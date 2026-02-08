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
import Bit.Internal.Metadata (MetaContent(..), parseMetadata, readMetadataOrComputeHash, hashFile, parseMetadataFile)
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
    [ "-C", bitIndexPath, "ls-tree", "-r", "--name-only", commitHash ] ""
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

-- | Verify working tree at an arbitrary root path against metadata at that path.
-- This is the core verification function used for both local and filesystem remote verification.
-- Returns (number of files checked, list of issues).
-- If an IORef counter is provided, it will be incremented after each file is checked.
verifyLocalAt :: FilePath -> Maybe (IORef Int) -> Concurrency -> IO (Int, [VerifyIssue])
verifyLocalAt root mCounter concurrency = do
  let indexDir = root </> ".bit/index"
  entries <- loadMetadata (FromFilesystem indexDir) concurrency
  -- Filter out .git directory entries
  let filteredEntries = filter (\entry -> not (isGitPath (unPath (entryPath entry)))) entries
  
  -- Determine concurrency level
  bound <- resolveConcurrency concurrency
  
  -- Parallelize verification (IO-bound: file reads and hashing)
  issues <- runConcurrently (Parallel bound) (checkOne root indexDir) filteredEntries
  pure (length filteredEntries, concat issues)
  where
    checkOne rootPath indexPath entry = do
      let relPath = entryPath entry
          actualPath = rootPath </> unPath relPath
      exists <- doesFileExist actualPath
      result <- if not exists
        then pure [Missing relPath]
        else case entry of
          BinaryEntry _ expectedHash expectedSize -> do
            actualHash <- hashFile actualPath
            actualSize <- fromIntegral . BS.length <$> BS.readFile actualPath
            if actualHash == expectedHash && actualSize == expectedSize
              then pure []
              else pure [HashMismatch relPath (T.unpack (hashToText expectedHash)) (T.unpack (hashToText actualHash)) expectedSize actualSize]
          TextEntry _ -> do
            -- Text files: just verify they exist (already checked above)
            -- Could optionally verify content matches index, but that's redundant
            -- since the index content IS the file content for text files
            actualHash <- hashFile actualPath
            actualSize <- fromIntegral . BS.length <$> BS.readFile actualPath
            -- For text files, we need to check against what's in the index
            let indexFilePath = indexPath </> unPath relPath
            indexHash <- hashFile indexFilePath
            indexSize <- fromIntegral . BS.length <$> BS.readFile indexFilePath
            if actualHash == indexHash && actualSize == indexSize
              then pure []
              else pure [HashMismatch relPath (T.unpack (hashToText indexHash)) (T.unpack (hashToText actualHash)) indexSize actualSize]
      -- Increment counter after checking file (atomicModifyIORef' is thread-safe)
      traverse_ (\ref -> atomicModifyIORef' ref (\n -> (n + 1, ()))) mCounter
      pure result

-- | Verify local working tree against metadata in .rgit/index.
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
