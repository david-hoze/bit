{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @bit cas gc@ — mark-and-sweep garbage collection of orphaned CAS content.
--
-- A blob or chunk is /live/ if some version of some tracked file references it.
-- The mark set is built from:
--
--   * every commit reachable from any ref (@git rev-list --all@), so content
--     needed to check out history is never collected, and
--   * the current @.bit/index/@ on disk, so blobs staged by @bit add@ but not
--     yet committed are never collected.
--
-- For each live whole-file hash that is chunked, the manifest's chunk hashes
-- (and the manifest itself) are also marked live. Everything in @.bit/cas/@ not
-- in the mark set is an orphan and is removed (or just listed, with @--dry-run@).
module Bit.Core.CasGc (casGc) where

import Control.Monad (forM, foldM, filterM)
import Data.Char (isSpace)
import Data.List (isSuffixOf)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import qualified System.Directory as Dir

import Bit.Types (Hash, HashAlgo(..), hashToText)
import Bit.IO.Concurrency (Concurrency(..))
import qualified Bit.Scan.Verify as Verify
import Bit.CAS (stripHashPrefix)
import Bit.CDC.Manifest (readManifestFromCas)
import Bit.CDC.Types (ChunkManifest(..), ChunkRef(..))
import qualified Bit.Git.Run as Git

-- | Run garbage collection. When @dryRun@ is True, report orphans without
-- deleting them.
casGc :: Bool -> FilePath -> IO ()
casGc dryRun _cwd = do
  indexDir <- Git.getIndexPath
  let bitDir = takeDirectory indexDir
      casDir = bitDir </> "cas"
  casExists <- Dir.doesDirectoryExist casDir
  if not casExists
    then putStrLn "No CAS directory — nothing to collect."
    else do
      liveWhole <- markLiveWholeHashes indexDir
      (liveChunks, liveManifests) <- resolveChunked casDir liveWhole
      let liveBlobHex     = Set.map hashHex (Set.union liveWhole liveChunks)
          liveManifestHex = Set.map hashHex liveManifests
      casFiles <- listCasFiles casDir
      let orphans = [ path | (path, name) <- casFiles, isOrphan liveBlobHex liveManifestHex name ]
      sweep dryRun orphans

-- | Whole-file hashes referenced by any commit or by the current index.
markLiveWholeHashes :: FilePath -> IO (Set (Hash 'MD5))
markLiveWholeHashes indexDir = do
  (code, revOut, _) <- Git.runGitAt indexDir ["rev-list", "--all"]
  let commits = case code of
        ExitSuccess -> filter (not . null) (lines revOut)
        _           -> []
  commitEntries <- forM commits $ \rev ->
    Verify.loadMetadata (Verify.FromCommit (filter (not . isSpace) rev)) Sequential
  indexEntries <- Verify.loadMetadata (Verify.FromFilesystem indexDir) Sequential
  pure $ Set.fromList
    [ h | entries <- indexEntries : commitEntries
        , Verify.BinaryEntry _ h _ <- entries ]

-- | For each live whole-file hash that has a manifest, collect its chunk hashes
-- and record that the manifest itself is live.
resolveChunked :: FilePath -> Set (Hash 'MD5) -> IO (Set (Hash 'MD5), Set (Hash 'MD5))
resolveChunked casDir = foldM step (Set.empty, Set.empty) . Set.toList
  where
    step (chunks, manifests) h = do
      mManifest <- readManifestFromCas casDir h
      pure $ case mManifest of
        Just manifest ->
          ( foldr (Set.insert . crHash) chunks (cmChunks manifest)
          , Set.insert h manifests )
        Nothing -> (chunks, manifests)

-- | Is a CAS file an orphan? Blobs are matched against the live blob set,
-- @.manifest@ files against the live manifest set.
isOrphan :: Set String -> Set String -> String -> Bool
isOrphan liveBlobHex liveManifestHex name
  | ".manifest" `isSuffixOf` name =
      not (Set.member (dropSuffix ".manifest" name) liveManifestHex)
  | otherwise = not (Set.member name liveBlobHex)

-- | List every regular file under @cas/<prefix>/@ as @(absolutePath, baseName)@.
listCasFiles :: FilePath -> IO [(FilePath, String)]
listCasFiles casDir = do
  prefixes <- listSubdirs casDir
  fmap concat $ forM prefixes $ \prefix -> do
    let prefixDir = casDir </> prefix
    names <- Dir.listDirectory prefixDir
    files <- filterM (Dir.doesFileExist . (prefixDir </>)) names
    pure [ (prefixDir </> n, n) | n <- files ]

-- | Subdirectories of a directory (the 2-char CAS fan-out prefixes).
listSubdirs :: FilePath -> IO [FilePath]
listSubdirs dir = do
  entries <- Dir.listDirectory dir
  filterM (Dir.doesDirectoryExist . (dir </>)) entries

-- | Delete (or, in dry-run mode, just report) the orphan files.
sweep :: Bool -> [FilePath] -> IO ()
sweep dryRun orphans
  | null orphans = putStrLn "CAS is clean — no orphan blobs."
  | otherwise = do
      sizes <- mapM Dir.getFileSize orphans
      let total = sum sizes
          n     = length orphans
      if dryRun
        then do
          putStrLn $ "Would remove " ++ show n ++ " orphan blob(s), reclaiming "
            ++ showBytes total ++ " (dry run — nothing deleted)."
          mapM_ (putStrLn . ("  " ++) . takeFileName) orphans
        else do
          mapM_ removeQuiet orphans
          putStrLn $ "Removed " ++ show n ++ " orphan blob(s), reclaimed "
            ++ showBytes total ++ "."
  where
    removeQuiet p = Dir.removeFile p

-- | Hex portion of a hash (the CAS filename), stripping any @<algo>:@ prefix.
hashHex :: Hash 'MD5 -> String
hashHex = stripHashPrefix . T.unpack . hashToText

dropSuffix :: String -> String -> String
dropSuffix suf s
  | suf `isSuffixOf` s = take (length s - length suf) s
  | otherwise          = s

-- | Human-readable byte count.
showBytes :: Integer -> String
showBytes n
  | n < 1024          = show n ++ " B"
  | n < 1024 * 1024   = show (n `div` 1024) ++ " KB"
  | n < 1024 * 1024 * 1024 = show (n `div` (1024 * 1024)) ++ " MB"
  | otherwise         = show (n `div` (1024 * 1024 * 1024)) ++ " GB"
