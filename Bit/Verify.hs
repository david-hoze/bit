{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

module Bit.Verify
  ( verifyLocal
  , verifyLocalAt
  , verifyRemote
  , VerifyIssue(..)
  , loadBinaryMetadata
  , loadCommittedBinaryMetadata
  , loadMetadataFromBundle
  , MetadataEntry(..)
  , MetadataSource(..)
  , loadMetadata
  , entryPath
  , binaryEntries
  , allEntryPaths
  ) where

import Bit.Types (Hash(..), HashAlgo(..), Path(..), FileEntry(..), EntryKind(..), syncHash, hashToText)
import Bit.Utils (filterOutBitPaths)
import Bit.Concurrency (Concurrency(..), runConcurrently, ioConcurrency)
import System.FilePath ((</>), makeRelative, normalise)
import System.Directory (doesFileExist, listDirectory, doesDirectoryExist, removeFile)
import Data.List (isPrefixOf)
import Data.Maybe (maybeToList)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Internal.Git as Git
import Bit.Internal.Metadata (MetaContent(..), parseMetadata, readMetadataOrComputeHash, parseMetadataFile, hashFile)
import qualified Bit.Remote.Scan as Remote.Scan
import qualified Bit.Remote
import qualified Internal.Transport as Transport
import Internal.Config (fetchedBundle, bitIndexPath, bundleCwdPath, fromCwdPath, BundleName)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.Char (isSpace)
import System.IO (hPutStrLn, stderr)
import Control.Monad (when, unless)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.IORef (IORef, atomicModifyIORef')
import qualified Bit.Scan as Scan

-- | Result of comparing one file to metadata.
data VerifyIssue
  = HashMismatch Path String String Integer Integer  -- path, expectedHash, actualHash, expectedSize, actualSize
  | Missing Path                                      -- path (in metadata but no actual file)
  deriving (Show, Eq)

-- | A metadata entry loaded from any source.
-- Binary files have hash+size metadata that can be verified.
-- Text files are known to exist but their hashes may not be reliably comparable
-- across sources (git normalizes line endings in blobs).
data MetadataEntry
  = BinaryEntry Path (Hash 'MD5) Integer   -- ^ Hash-verifiable: path, hash, size
  | TextEntry Path                          -- ^ Exists but not hash-comparable
  deriving (Show, Eq)

-- | Source for reading metadata entries.
data MetadataSource
  = FromFilesystem FilePath            -- ^ Read .bit/index/ directory on disk
  | FromCommit String                  -- ^ Read from a git commit hash (e.g. refs/remotes/origin/main)
  deriving (Show, Eq)

-- | Extract path from any entry.
entryPath :: MetadataEntry -> Path
entryPath (BinaryEntry p _ _) = p
entryPath (TextEntry p) = p

-- | Extract verifiable (binary) entries only.
binaryEntries :: [MetadataEntry] -> [(Path, Hash 'MD5, Integer)]
binaryEntries = concatMap go
  where go (BinaryEntry p h s) = [(p, h, s)]
        go (TextEntry _) = []

-- | Extract all known paths (binary + text).
allEntryPaths :: [MetadataEntry] -> Set.Set Path
allEntryPaths = Set.fromList . map entryPath

-- | Filter to user files only (exclude .git internals and .gitignore).
-- Used by BOTH filesystem and commit-tree paths.
isUserFile :: FilePath -> Bool
isUserFile filePath = not (isGitPath filePath) && filePath /= ".gitignore"

-- | Resolve concurrency setting to a concrete bound.
resolveConcurrency :: Concurrency -> IO Int
resolveConcurrency Sequential = pure 1
resolveConcurrency (Parallel 0) = ioConcurrency
resolveConcurrency (Parallel n) = pure n

-- | List all regular files under dir, with paths relative to baseDir.
listFilesRecursive :: FilePath -> FilePath -> IO [FilePath]
listFilesRecursive baseDir dir = do
  entries <- listDirectory dir
  concat <$> mapM (\name -> do
    let full = dir </> name
    isDir <- doesDirectoryExist full
    if isDir
      then listFilesRecursive baseDir full
      else pure [makeRelative baseDir full]
    ) entries

-- | Check if a path is within the .git directory.
isGitPath :: FilePath -> Bool
isGitPath filePath = ".git" `isPrefixOf` normalise filePath || normalise filePath == ".git"

-- | Load metadata entries from any source.
-- Handles file enumeration, parsing, and binary/text classification uniformly.
loadMetadata :: MetadataSource -> Concurrency -> IO [MetadataEntry]
loadMetadata (FromFilesystem indexDir) concurrency = do
  exists <- doesDirectoryExist indexDir
  if not exists
    then pure []
    else do
      relPaths <- listFilesRecursive indexDir indexDir
      let userPaths = filter isUserFile relPaths
      bound <- resolveConcurrency concurrency
      runConcurrently (Parallel bound) (readEntryFromFilesystem indexDir) userPaths

loadMetadata (FromCommit commitHash) _concurrency = do
  -- ls-tree at ROOT level (no prefix!) to enumerate all files
  (code, out, _) <- readProcessWithExitCode "git"
    [ "-C", bitIndexPath, "-c", "core.quotePath=false", "ls-tree", "-r", "--name-only", commitHash ] ""
  case code of
    ExitSuccess -> do
      let paths = filter isUserFile $ filter (not . null) $ lines out
      mapM (readEntryFromCommit commitHash) paths
    _ -> pure []

-- | Read a single metadata entry from a filesystem path.
readEntryFromFilesystem :: FilePath -> FilePath -> IO MetadataEntry
readEntryFromFilesystem indexDir relPath = do
  let fullPath = indexDir </> relPath
  mc <- readMetadataOrComputeHash fullPath
  case mc of
    Just (MetaContent { metaHash = h, metaSize = sz }) ->
      -- Check if this was parsed as metadata (binary) or computed from content (text)
      -- by trying parseMetadata on the raw content
      parseMetadataFile fullPath >>= \case
        Just _ -> pure (BinaryEntry (Path relPath) h sz)   -- has hash:/size: format → binary
        Nothing -> pure (TextEntry (Path relPath))          -- content IS the file → text
    Nothing -> pure (TextEntry (Path relPath))  -- shouldn't happen, but safe fallback

-- | Read a single metadata entry from a git commit tree.
readEntryFromCommit :: String -> FilePath -> IO MetadataEntry
readEntryFromCommit commitHash relPath = do
  -- NOTE: path is at root level in the commit tree, NOT under index/
  (code, content, _) <- readProcessWithExitCode "git"
    [ "-C", bitIndexPath, "show", commitHash ++ ":" ++ relPath ] ""
  pure $ case code of
    ExitSuccess -> case parseMetadata content of
      Just (MetaContent { metaHash = h, metaSize = sz }) ->
        BinaryEntry (Path relPath) h sz
      Nothing ->
        TextEntry (Path relPath)  -- text file: content IS the data, skip hash verify
    _ -> TextEntry (Path relPath)

-- | Load only binary (hash-verifiable) metadata entries from the index.
-- Text files are excluded. If you need all entries, use 'loadMetadata' directly.
loadBinaryMetadata :: FilePath -> Concurrency -> IO [(Path, Hash 'MD5, Integer)]
loadBinaryMetadata indexDir concurrency =
  binaryEntries <$> loadMetadata (FromFilesystem indexDir) concurrency

-- | Load binary metadata from the committed (HEAD) state of a .bit/index repo.
-- This returns what the metadata *should* be, immune to scan updates.
loadCommittedBinaryMetadata :: FilePath -> IO [(Path, Hash 'MD5, Integer)]
loadCommittedBinaryMetadata indexDir = do
  (code, out, _) <- Git.runGitAt indexDir ["rev-parse", "HEAD"]
  case code of
    ExitSuccess -> do
      let headHash = filter (not . isSpace) out
      binaryEntries <$> loadMetadata (FromCommit headHash) Sequential
    _ -> pure []

-- | Verify working tree at an arbitrary root path against its committed metadata.
-- Scans the working directory to update .bit/index/ metadata, then uses
-- git diff to find files whose metadata changed from the committed state.
-- Returns (number of files checked, list of issues).
-- If an IORef counter is provided, it will be incremented after each file is checked.
verifyLocalAt :: FilePath -> Maybe (IORef Int) -> Concurrency -> IO (Int, [VerifyIssue])
verifyLocalAt root mCounter _concurrency = do
  let indexDir = root </> bitIndexPath

  -- 1. Scan working directory and update .bit/index/ metadata
  entries <- Scan.scanWorkingDir root
  Scan.writeMetadataFiles root entries

  -- 2. git diff in the index repo to find files changed from committed state
  (diffCode, diffOut, _) <- Git.runGitAt indexDir ["diff", "--name-only"]
  let changedPaths
        | diffCode == ExitSuccess = filter (not . null) (lines diffOut)
        | otherwise               = []

  -- 3. Also check for missing files: committed paths not in working tree
  (lsCode, lsOut, _) <- Git.runGitAt indexDir ["ls-tree", "-r", "--name-only", "HEAD"]
  let committedPaths
        | lsCode == ExitSuccess = filter isUserFile $ filter (not . null) (lines lsOut)
        | otherwise             = []

  -- 4. Build issues from changed files (hash mismatches)
  mismatchIssues <- concat <$> mapM (checkChanged indexDir) changedPaths

  -- 5. Build issues from missing files (committed but not in working tree)
  missingFiltered <- fmap concat $ mapM (\p -> do
    exists <- doesFileExist (root </> p)
    pure [Missing (Path p) | not exists]
    ) committedPaths

  let allIssues = mismatchIssues ++ missingFiltered
      totalChecked = length committedPaths

  -- Update counter
  traverse_ (\ref -> atomicModifyIORef' ref (\_ -> (totalChecked, ()))) mCounter

  pure (totalChecked, allIssues)
  where
    checkChanged indexDir relPath = do
      -- Read expected metadata from committed state
      (showCode, committedContent, _) <- Git.runGitAt indexDir ["show", "HEAD:" ++ relPath]
      -- Read actual metadata from filesystem (updated by scan)
      let fsPath = indexDir </> relPath
      fsExists <- doesFileExist fsPath
      case (showCode, fsExists) of
        (ExitSuccess, True) -> do
          let mExpected = parseMetadata committedContent
          mActual <- parseMetadataFile fsPath
          case (mExpected, mActual) of
            (Just expected, Just actual) ->
              -- Binary file: compare hash+size metadata
              pure [HashMismatch (Path relPath)
                      (T.unpack (hashToText (metaHash expected)))
                      (T.unpack (hashToText (metaHash actual)))
                      (metaSize expected)
                      (metaSize actual)]
            _ -> do
              -- Text file: git diff says it changed, report mismatch
              actualHash <- hashFile fsPath
              actualSize <- fromIntegral . BS.length <$> BS.readFile fsPath
              pure [HashMismatch (Path relPath)
                      "(committed)"
                      (T.unpack (hashToText actualHash))
                      0
                      actualSize]
        _ -> pure []

-- | Verify local working tree against committed metadata in .bit/index.
-- Returns (number of files checked, list of issues).
-- If an IORef counter is provided, it will be incremented after each file is checked.
verifyLocal :: FilePath -> Maybe (IORef Int) -> Concurrency -> IO (Int, [VerifyIssue])
verifyLocal cwd = verifyLocalAt cwd

-- | Extract metadata from a bundle's HEAD commit.
-- First fetches the bundle into the repo, then reads metadata from refs/remotes/origin/main.
-- Returns (binaryMetadata, allKnownPaths):
--   binaryMetadata: list of (path, hash, size) for binary files (verifiable)
--   allKnownPaths: set of all file paths in the bundle (binary + text, for "extra files" check)
-- Text files are NOT hash-verified because git may normalize line endings (CRLF→LF),
-- causing hash mismatches with the actual files on the remote.
loadMetadataFromBundle :: BundleName -> IO ([(Path, Hash 'MD5, Integer)], Set.Set Path)
loadMetadataFromBundle bundleName = do
  -- First, fetch the bundle into the repo so we can read from it
  fetchCode <- Git.fetchFromBundle bundleName
  case fetchCode of
    ExitSuccess -> do
      -- Get the remote HEAD hash (now available as refs/remotes/origin/main)
      (_code, out, _) <- readProcessWithExitCode "git"
        [ "-C", bitIndexPath
        , "rev-parse"
        , "refs/remotes/origin/main"
        ] ""
      case filter (not . isSpace) out of
        [] -> pure ([], Set.empty)
        headHash -> do
          entries <- loadMetadata (FromCommit headHash) Sequential
          pure (binaryEntries entries, allEntryPaths entries)
    _ -> pure ([], Set.empty)

-- | Verify remote files match remote metadata.
-- Returns (number of files checked, list of issues).
-- If an IORef counter is provided, it will be incremented after each file is checked.
verifyRemote :: FilePath -> Bit.Remote.Remote -> Maybe (IORef Int) -> Concurrency -> IO (Int, [VerifyIssue])
verifyRemote _cwd remote mCounter _concurrency = do
  -- 1. Fetch the remote bundle if needed
  let fetchedPath = fromCwdPath (bundleCwdPath fetchedBundle)
  bundleExists <- doesFileExist fetchedPath
  unless bundleExists $ do
    let localDest = ".bit/temp_remote.bundle"
    fetchResult <- Transport.copyFromRemoteDetailed remote ".bit/bit.bundle" localDest
    case fetchResult of
      Transport.CopySuccess -> do
        -- Copy to fetchedPath for consistency
        BS.readFile localDest >>= BS.writeFile fetchedPath
        when (localDest /= fetchedPath) $ safeRemove localDest
      _ -> do
        hPutStrLn stderr "Error: Could not fetch remote bundle."
        pure ()
  
  -- Check if bundle exists now (if fetch failed, we can't continue)
  bundleExistsNow <- doesFileExist fetchedPath
  if not bundleExistsNow
    then pure (0, [])
    else do
      -- 2. Load metadata from the bundle (binary metadata + all known paths)
      (remoteMeta, allKnownPaths) <- loadMetadataFromBundle fetchedBundle

      -- 3. Fetch actual remote files
      Remote.Scan.fetchRemoteFiles remote >>= either
        (const $ hPutStrLn stderr "Error: Could not fetch remote file list." >> pure (0, []))
        (\remoteFiles -> do
          let filteredRemoteFiles = filterOutBitPaths remoteFiles
          
          -- 4. Build maps for comparison (both use MD5 hashes)
          let remoteFileMap = Map.fromList
                [ (normalise (unPath e.path), (h, e.kind))
                | e <- filteredRemoteFiles
                , h <- maybeToList (syncHash e.kind)
                ]
          
          -- 5. Compare binary file metadata with actual files on remote
          issues <- traverse (checkRemoteFile remoteFileMap) remoteMeta
          
          -- 6. Check for files on remote that aren't known to the bundle
          -- Use allKnownPaths (binary + text) to avoid false positives for text files
          let filePaths = Set.fromList (map Path (Map.keys remoteFileMap))
              extraPaths = filePaths `Set.difference` allKnownPaths
              extraIssues = map (\p -> HashMismatch p "(not in metadata)" "(exists on remote)" 0 0) (Set.toList extraPaths)
          
          pure (length remoteMeta, concat issues ++ extraIssues))
  where
    -- Check one file from metadata against remote (both use MD5)
    checkRemoteFile :: Map.Map FilePath (Hash 'MD5, EntryKind) -> (Path, Hash 'MD5, Integer) -> IO [VerifyIssue]
    checkRemoteFile remoteFileMap (relPath, expectedHash, expectedSize) = do
      let normalizedPath = normalise (unPath relPath)
      result <- case Map.lookup normalizedPath remoteFileMap of
        Nothing -> pure [Missing relPath]
        Just (actualHash, File _ actualSize _) ->
          if actualHash == expectedHash && actualSize == expectedSize
            then pure []
            else pure [HashMismatch relPath (T.unpack (hashToText expectedHash)) (T.unpack (hashToText actualHash)) expectedSize actualSize]
        Just _ -> pure []
      -- Increment counter after checking file
      traverse_ (\ref -> atomicModifyIORef' ref (\n -> (n + 1, ()))) mCounter
      pure result

-- Helper to safely remove a file
safeRemove :: FilePath -> IO ()
safeRemove filePath = do
  exists <- doesFileExist filePath
  when exists $ removeFile filePath
