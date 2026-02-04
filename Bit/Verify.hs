{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bit.Verify
  ( verifyLocal
  , verifyRemote
  , VerifyIssue(..)
  , loadMetadataIndex
  , loadMetadataFromBundle
  ) where

import Data.Traversable (traverse)
import Bit.Types (Hash(..), HashAlgo(..), Path, FileEntry(..), EntryKind(..), syncHash, hashToText)
import Bit.Utils (filterOutBitPaths)
import System.FilePath ((</>), makeRelative, normalise)
import System.Directory (doesFileExist, listDirectory, doesDirectoryExist, removeFile)
import Data.List (isPrefixOf)
import Data.Maybe (maybeToList)
import Data.Either (either)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Internal.Git as Git
import Bit.Internal.Metadata (MetaContent(..), parseMetadata, readMetadataOrComputeHash, hashFile)
import qualified Bit.Remote.Scan as Remote.Scan
import qualified Bit.Remote
import qualified Internal.Transport as Transport
import Internal.Config (fetchedBundle, fetchedBundlePath, bitIndexPath, bundleCwdPath, fromCwdPath, BundleName)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.Char (isSpace)
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import Control.Monad (when, unless)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO (hPutStrLn, stderr)

-- | Result of comparing one file to metadata.
data VerifyIssue
  = HashMismatch Path String String Integer Integer  -- path, expectedHash, actualHash, expectedSize, actualSize
  | Missing Path                                      -- path (in metadata but no actual file)
  deriving (Show, Eq)

-- | List all regular files under dir, with paths relative to baseDir.
listFilesRecursive :: FilePath -> FilePath -> IO [FilePath]
listFilesRecursive baseDir dir = do
  entries <- listDirectory dir
  concat <$> mapM (\name -> do
    let full = dir </> name
    isDir <- doesDirectoryExist full
    if isDir
      then listFilesRecursive baseDir full
      else return [makeRelative baseDir full]
    ) entries

-- | Check if a path is within the .git directory.
isGitPath :: FilePath -> Bool
isGitPath path = ".git" `isPrefixOf` normalise path || normalise path == ".git"

-- | Load all metadata from .rgit/index: list of (relative path, expected hash, expected size).
-- Handles both text files (content stored directly) and binary files (metadata stored).
loadMetadataIndex :: FilePath -> IO [(Path, Hash 'MD5, Integer)]
loadMetadataIndex indexDir = do
  exists <- doesDirectoryExist indexDir
  if not exists
    then return []
    else do
      relPaths <- listFilesRecursive indexDir indexDir
      pairs <- mapM (\relPath -> do
        let fullPath = indexDir </> relPath
        mc <- readMetadataOrComputeHash fullPath
        return $ case mc of
          Just mc' -> [(relPath, metaHash mc', metaSize mc')]
          Nothing -> []
        ) relPaths
      return (concat pairs)

-- | Verify local working tree against metadata in .rgit/index.
-- Returns (number of files checked, list of issues).
verifyLocal :: FilePath -> IO (Int, [VerifyIssue])
verifyLocal cwd = do
  let indexDir = cwd </> ".bit/index"
  meta <- loadMetadataIndex indexDir
  -- Filter out .git directory entries
  let filteredMeta = filter (\(relPath, _, _) -> not (isGitPath relPath)) meta
  issues <- mapM (checkOne cwd) filteredMeta
  return (length filteredMeta, concat issues)
  where
    checkOne root (relPath, expectedHash, expectedSize) = do
      let actualPath = root </> relPath
      exists <- doesFileExist actualPath
      if not exists
        then return [Missing relPath]
        else do
          actualHash <- hashFile actualPath
          actualSize <- fromIntegral . BS.length <$> BS.readFile actualPath
          if actualHash == expectedHash && actualSize == expectedSize
            then return []
            else return [HashMismatch relPath (T.unpack (hashToText expectedHash)) (T.unpack (hashToText actualHash)) expectedSize actualSize]

-- | Extract metadata from a bundle's HEAD commit.
-- First fetches the bundle into the repo, then reads metadata from refs/remotes/origin/main.
-- Returns list of (relative path, hash, size) for files under index/.
loadMetadataFromBundle :: BundleName -> IO [(Path, Hash 'MD5, Integer)]
loadMetadataFromBundle bundleName = do
  -- First, fetch the bundle into the repo so we can read from it
  fetchCode <- Git.fetchFromBundle bundleName
  if fetchCode /= ExitSuccess
    then return []
    else do
      -- Get the remote HEAD hash (now available as refs/remotes/origin/main)
      (code, out, _) <- readProcessWithExitCode "git"
        [ "-C", bitIndexPath
        , "rev-parse"
        , "refs/remotes/origin/main"
        ] ""
      case filter (not . isSpace) out of
        [] -> return []
        headHash -> do
          -- List all files in the commit that are under index/
          (code2, out2, _) <- readProcessWithExitCode "git"
            [ "-C", bitIndexPath
            , "ls-tree"
            , "-r"
            , "--name-only"
            , headHash
            , "--"
            , "index/"
            ] ""
          if code2 /= ExitSuccess
            then return []
            else do
              -- Filter to get only paths under index/ and remove the "index/" prefix
              let paths = filter (not . null) $ map (drop 6) $ filter ("index/" `isPrefixOf`) $ lines out2
              -- Read each metadata file from the commit
              metaList <- mapM (readMetadataFromCommit headHash) paths
              return $ concat metaList
  where
    -- Read a metadata file from a specific commit
    readMetadataFromCommit :: String -> FilePath -> IO [(Path, Hash 'MD5, Integer)]
    readMetadataFromCommit commitHash relPath = do
      let gitPath = "index/" ++ relPath
      (code, content, _) <- readProcessWithExitCode "git"
        [ "-C", bitIndexPath
        , "show"
        , commitHash ++ ":" ++ gitPath
        ] ""
      if code /= ExitSuccess
        then return []
        else case parseMetadata content of
          Nothing -> return []
          Just (MetaContent { metaHash = h, metaSize = sz }) -> return [(relPath, h, sz)]

-- | Verify remote files match remote metadata.
-- Returns (number of files checked, list of issues).
verifyRemote :: FilePath -> Bit.Remote.Remote -> IO (Int, [VerifyIssue])
verifyRemote cwd remote = do
  -- 1. Fetch the remote bundle if needed
  let fetchedPath = fromCwdPath (bundleCwdPath fetchedBundle)
  bundleExists <- doesFileExist fetchedPath
  when (not bundleExists) $ do
    let localDest = ".bit/temp_remote.bundle"
    fetchResult <- Transport.copyFromRemoteDetailed remote ".bit/bit.bundle" localDest
    case fetchResult of
      Transport.CopySuccess -> do
        -- Copy to fetchedPath for consistency
        BS.readFile localDest >>= BS.writeFile fetchedPath
        when (localDest /= fetchedPath) $ safeRemove localDest
      _ -> do
        hPutStrLn stderr "Error: Could not fetch remote bundle."
        return ()
  
  -- Check if bundle exists now (if fetch failed, we can't continue)
  bundleExistsNow <- doesFileExist fetchedPath
  if not bundleExistsNow
    then return (0, [])
    else do
      -- 2. Load metadata from the bundle
      remoteMeta <- loadMetadataFromBundle fetchedBundle
      
      -- 3. Fetch actual remote files
      Remote.Scan.fetchRemoteFiles remote >>= either
        (\_ -> hPutStrLn stderr "Error: Could not fetch remote file list." >> return (0, []))
        (\remoteFiles -> do
          let filteredRemoteFiles = filterOutBitPaths remoteFiles
          
          -- 4. Build maps for comparison (both use MD5 hashes)
          let remoteFileMap = Map.fromList
                [ (normalise e.path, (h, e.kind))
                | e <- filteredRemoteFiles
                , h <- maybeToList (syncHash e.kind)
                ]
              remoteMetaMap = Map.fromList [(normalise p, (h, sz)) | (p, h, sz) <- remoteMeta]
          
          -- 5. Compare metadata with actual files
          issues <- traverse (checkRemoteFile remoteFileMap) remoteMeta
          
          -- 6. Check for files on remote that aren't in metadata
          let metaPaths = Set.fromList (Map.keys remoteMetaMap)
              filePaths = Set.fromList (Map.keys remoteFileMap)
              extraPaths = filePaths `Set.difference` metaPaths
              extraIssues = map (\p -> HashMismatch p "(not in metadata)" "(exists on remote)" 0 0) (Set.toList extraPaths)
          
          return (length remoteMeta, concat issues ++ extraIssues))
  where
    -- Check one file from metadata against remote (both use MD5)
    checkRemoteFile :: Map.Map FilePath (Hash 'MD5, EntryKind) -> (Path, Hash 'MD5, Integer) -> IO [VerifyIssue]
    checkRemoteFile remoteFileMap (relPath, expectedHash, expectedSize) = do
      let normalizedPath = normalise relPath
      case Map.lookup normalizedPath remoteFileMap of
        Nothing -> return [Missing relPath]
        Just (actualHash, File _ actualSize _) ->
          if actualHash == expectedHash && actualSize == expectedSize
            then return []
            else return [HashMismatch relPath (T.unpack (hashToText expectedHash)) (T.unpack (hashToText actualHash)) expectedSize actualSize]
        Just _ -> return []

-- Helper to safely remove a file
safeRemove :: FilePath -> IO ()
safeRemove path = do
  exists <- doesFileExist path
  when exists $ removeFile path
