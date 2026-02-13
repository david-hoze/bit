{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

module Bit.Verify
  ( verifyLocal
  , verifyLocalAt
  , verifyWithAbort
  , verifyRemote
  , VerifyIssue(..)
  , VerifyResult(..)
  , BinaryFileMeta(..)
  , loadBinaryMetadata
  , loadCommittedBinaryMetadata
  , loadMetadataFromBundle
  , MetadataEntry(..)
  , MetadataSource(..)
  , loadMetadata
  , entryPath
  , binaryEntries
  , textEntryPaths
  , allEntryPaths
  , loadCommittedTextPaths
  , Scan.ScanPhase(..)
  ) where

import Bit.Types (Hash(..), HashAlgo(..), Path(..), FileEntry(..), EntryKind(..), ContentType(..), syncHash, hashToText)
import Bit.Utils (filterOutBitPaths, toPosix)
import Bit.Concurrency (Concurrency(..), runConcurrently, ioConcurrency)
import System.FilePath ((</>), makeRelative, normalise, takeDirectory)
import System.Directory (doesFileExist, listDirectory, doesDirectoryExist, removeFile, createDirectoryIfMissing, removeDirectoryRecursive, getPermissions, setPermissions, setOwnerWritable)
import Data.List (isPrefixOf)
import Data.Maybe (maybeToList, isJust)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Internal.Git as Git
import Bit.Internal.Metadata (MetaContent(..), parseMetadata, parseMetadataFile, hashFile, serializeMetadata)
import qualified Bit.Remote.Scan as Remote.Scan
import qualified Bit.Remote
import qualified Internal.Transport as Transport
import Internal.Config (bundleForRemote, bitIndexPath, bundleCwdPath, fromCwdPath, BundleName)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.Char (isSpace)
import System.IO (hPutStrLn, stderr)
import Control.Monad (when, unless, void)
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

-- | Result of a verification run: count of files checked and list of issues.
-- Replaces bare (Int, [VerifyIssue]) tuple to prevent transposition.
data VerifyResult = VerifyResult
  { vrCount :: Int
  , vrIssues :: [VerifyIssue]
  }
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

-- | Binary file metadata: path, hash, size. Replaces bare (Path, Hash, Integer) tuple.
data BinaryFileMeta = BinaryFileMeta
  { bfmPath :: Path
  , bfmHash :: Hash 'MD5
  , bfmSize :: Integer
  }
  deriving (Show, Eq)

-- | Extract verifiable (binary) entries only.
binaryEntries :: [MetadataEntry] -> [BinaryFileMeta]
binaryEntries = concatMap go
  where go (BinaryEntry p h s) = [BinaryFileMeta p h s]
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
readEntryFromFilesystem indexDir relPath =
  classifyMetadataFile (Path relPath) (indexDir </> relPath)

-- | Read a single metadata entry from a git commit tree.
readEntryFromCommit :: String -> FilePath -> IO MetadataEntry
readEntryFromCommit commitHash relPath = do
  -- NOTE: path is at root level in the commit tree, NOT under index/
  (code, content, _) <- readProcessWithExitCode "git"
    [ "-C", bitIndexPath, "show", commitHash ++ ":" ++ relPath ] ""
  pure $ case code of
    ExitSuccess -> classifyMetadata (Path relPath) content
    _           -> TextEntry (Path relPath)

-- | Classify metadata content string as binary or text entry.
classifyMetadata :: Path -> String -> MetadataEntry
classifyMetadata p content =
  case parseMetadata content of
    Just mc -> BinaryEntry p (metaHash mc) (metaSize mc)
    Nothing -> TextEntry p

-- | Classify a metadata file on disk as binary or text entry.
classifyMetadataFile :: Path -> FilePath -> IO MetadataEntry
classifyMetadataFile p filePath =
  parseMetadataFile filePath >>= \case
    Just mc -> pure (BinaryEntry p (metaHash mc) (metaSize mc))
    Nothing -> pure (TextEntry p)

-- | Load only binary (hash-verifiable) metadata entries from the index.
-- Text files are excluded. If you need all entries, use 'loadMetadata' directly.
loadBinaryMetadata :: FilePath -> Concurrency -> IO [BinaryFileMeta]
loadBinaryMetadata indexDir concurrency =
  binaryEntries <$> loadMetadata (FromFilesystem indexDir) concurrency

-- | Load binary metadata from the committed (HEAD) state of a .bit/index repo.
-- This returns what the metadata *should* be, immune to scan updates.
loadCommittedBinaryMetadata :: FilePath -> IO [BinaryFileMeta]
loadCommittedBinaryMetadata indexDir = do
  (code, out, _) <- Git.runGitAt indexDir ["rev-parse", "HEAD"]
  case code of
    ExitSuccess -> do
      let headHash = filter (not . isSpace) out
      binaryEntries <$> loadMetadata (FromCommit headHash) Sequential
    _ -> pure []

-- | Extract text-only entry paths.
textEntryPaths :: [MetadataEntry] -> [Path]
textEntryPaths entries = [p | TextEntry p <- entries]

-- | Load committed text file paths from the HEAD of a .bit/index repo.
loadCommittedTextPaths :: FilePath -> IO (Set.Set Path)
loadCommittedTextPaths indexDir = do
  (code, out, _) <- Git.runGitAt indexDir ["rev-parse", "HEAD"]
  case code of
    ExitSuccess -> do
      let headHash = filter (not . isSpace) out
      Set.fromList . textEntryPaths <$> loadMetadata (FromCommit headHash) Sequential
    _ -> pure Set.empty

-- | Given scan entries for a root path, write metadata, defeat racy git, and
-- find issues via git diff. Shared logic for all verify variants.
findIssuesFromScan :: FilePath -> [FileEntry] -> IO VerifyResult
findIssuesFromScan root entries = do
  let indexDir = root </> bitIndexPath

  -- 1. Write metadata
  Scan.writeMetadataFiles root entries

  -- 2. Reset git's index to HEAD to clear cached stat data.
  --    git diff caches (mtime, size) for unchanged files.  read-tree HEAD
  --    recreates all index entries with zeroed stat fields (size=0), so
  --    git always sees a size mismatch against the actual metadata files
  --    and is forced to re-read their content on the subsequent diff.
  void $ Git.runGitAt indexDir ["read-tree", "HEAD"]

  -- 3. git diff in the index repo to find files changed from committed state
  (diffCode, diffOut, _) <- Git.runGitAt indexDir ["diff", "--name-only"]
  let changedPaths
        | diffCode == ExitSuccess = filter (not . null) (lines diffOut)
        | otherwise               = []

  -- 5. Also check for missing files: committed paths not in working tree
  (lsCode, lsOut, _) <- Git.runGitAt indexDir ["ls-tree", "-r", "--name-only", "HEAD"]
  let committedPaths
        | lsCode == ExitSuccess = filter isUserFile $ filter (not . null) (lines lsOut)
        | otherwise             = []

  -- 6. Build issues from changed files (hash mismatches)
  mismatchIssues <- concat <$> mapM (checkChanged indexDir) changedPaths

  -- 7. Build issues from missing files (committed but not in working tree)
  missingFiltered <- fmap concat $ mapM (\p -> do
    exists <- doesFileExist (root </> p)
    pure [Missing (Path p) | not exists]
    ) committedPaths

  let allIssues = mismatchIssues ++ missingFiltered
      totalChecked = length committedPaths

  pure (VerifyResult totalChecked allIssues)
  where
    checkChanged indexDir relPath = do
      (showCode, committedContent, _) <- Git.runGitAt indexDir ["show", "HEAD:" ++ relPath]
      let fsPath = indexDir </> relPath
      fsExists <- doesFileExist fsPath
      case (showCode, fsExists) of
        (ExitSuccess, True) -> do
          let committed = classifyMetadata (Path relPath) committedContent
          actual <- classifyMetadataFile (Path relPath) fsPath
          case (committed, actual) of
            (BinaryEntry _ eh es, BinaryEntry _ ah as') ->
              pure [HashMismatch (Path relPath)
                      (T.unpack (hashToText eh))
                      (T.unpack (hashToText ah))
                      es
                      as']
            (BinaryEntry _ eh es, TextEntry _) -> do
              actualHash <- hashFile fsPath
              actualSize <- fromIntegral . BS.length <$> BS.readFile fsPath
              pure [HashMismatch (Path relPath)
                      (T.unpack (hashToText eh))
                      (T.unpack (hashToText actualHash))
                      es
                      actualSize]
            (TextEntry _, TextEntry _) ->
              reportTextMismatch fsPath relPath
            (TextEntry _, BinaryEntry _ _ _) ->
              reportTextMismatch fsPath relPath
        _ -> pure []

    reportTextMismatch fsPath relPath = do
      actualHash <- hashFile fsPath
      actualSize <- fromIntegral . BS.length <$> BS.readFile fsPath
      pure [HashMismatch (Path relPath)
              "(committed)"
              (T.unpack (hashToText actualHash))
              0
              actualSize]

-- | Verify working tree at an arbitrary root path against its committed metadata.
-- Scans the working directory (always hashes all files) then finds issues via git diff.
verifyLocalAt :: FilePath -> Maybe (IORef Int) -> Concurrency -> IO VerifyResult
verifyLocalAt root mCounter concurrency = do
  entries <- Scan.scanWorkingDir root concurrency
  result <- findIssuesFromScan root entries
  traverse_ (\ref -> atomicModifyIORef' ref (\_ -> (vrCount result, ()))) mCounter
  pure result

-- | Verify local working tree against committed metadata in .bit/index.
verifyLocal :: FilePath -> Maybe (IORef Int) -> Concurrency -> IO VerifyResult
verifyLocal cwd = verifyLocalAt cwd

-- | Verify with bandwidth detection. Uses scanWorkingDirWithAbort to measure
-- throughput and optionally skip hashing on slow storage.
-- Returns the verify result plus a list of files that were skipped.
verifyWithAbort :: FilePath -> Maybe (IORef Int) -> Concurrency -> Maybe (Scan.ScanPhase -> IO ()) -> IO (VerifyResult, [FilePath])
verifyWithAbort root mCounter concurrency mCallback = do
  Scan.ScanResult entries skipped <- Scan.scanWorkingDirWithAbort root concurrency mCallback

  -- For skipped files, create size-only entries: committed hash + actual file size.
  -- If the size changed, git diff will detect the mismatch.
  sizeEntries <- if null skipped
      then pure []
      else do
          committedMeta <- loadCommittedBinaryMetadata (root </> bitIndexPath)
          let metaMap = Map.fromList
                [(unPath (bfmPath m), (bfmHash m, bfmSize m)) | m <- committedMeta]
          pure $ concatMap (\(rel, actualSize) ->
              case Map.lookup rel metaMap of
                  Just (h, _) -> [FileEntry (Path rel) (File h actualSize BinaryContent)]
                  Nothing     -> []
              ) skipped

  when (isJust mCallback) $ hPutStrLn stderr "Comparing against committed metadata..."
  result <- findIssuesFromScan root (entries ++ sizeEntries)
  traverse_ (\ref -> atomicModifyIORef' ref (\_ -> (vrCount result, ()))) mCounter
  pure (result, [rel | (rel, _) <- skipped])

-- | Extract metadata from a bundle's HEAD commit.
-- Reads the hash from the bundle, then loads metadata from that commit.
-- The objects must already be in the repo (via git fetch <name> in saveFetchedBundle).
-- Falls back to fetching from the bundle file directly if objects aren't available.
-- Returns all metadata entries (binary + text). Callers extract what they need
-- via 'binaryEntries' or 'allEntryPaths'.
loadMetadataFromBundle :: BundleName -> IO [MetadataEntry]
loadMetadataFromBundle bundleName = do
  -- Get the hash from the bundle
  mHash <- Git.getHashFromBundle bundleName
  case mHash of
    Nothing -> pure []
    Just headHash -> do
      -- Try to load from the commit (objects should already be fetched)
      entries <- loadMetadata (FromCommit headHash) Sequential
      if null entries
        then do
          -- Objects not in repo yet — fetch from bundle file directly
          let bundlePath = fromCwdPath (bundleCwdPath bundleName)
          (fetchCode, _, _) <- readProcessWithExitCode "git"
            ["-C", bitIndexPath, "fetch", bundlePath, "+refs/heads/*:refs/remotes/bundle/*"] ""
          case fetchCode of
            ExitSuccess -> loadMetadata (FromCommit headHash) Sequential
            _ -> pure []
        else pure entries

-- | Verify remote files match remote metadata.
-- Sets up a temporary working tree in .bit/vremotes/<name>/, checks out the
-- expected state from the bundle, overwrites with actual remote data (rclone
-- metadata for binaries, downloaded content for text files), then runs a single
-- git diff to find all mismatches.
-- Returns (number of files checked, list of issues).
-- If an IORef counter is provided, it will be incremented after each file is checked.
verifyRemote :: FilePath -> Bit.Remote.Remote -> Maybe (IORef Int) -> Concurrency -> IO VerifyResult
verifyRemote cwd remote mCounter concurrency = do
  -- 1. Fetch the remote bundle if needed (per-remote bundle path)
  let bundleName = bundleForRemote (Bit.Remote.remoteName remote)
      fetchedPath = fromCwdPath (bundleCwdPath bundleName)
  bundleExists <- doesFileExist fetchedPath
  unless bundleExists $ do
    let localDest = ".bit/temp_remote.bundle"
    fetchResult <- Transport.copyFromRemoteDetailed remote ".bit/bit.bundle" localDest
    case fetchResult of
      Transport.CopySuccess -> do
        createDirectoryIfMissing True (takeDirectory fetchedPath)
        BS.readFile localDest >>= BS.writeFile fetchedPath
        when (localDest /= fetchedPath) $ safeRemove localDest
      _ -> do
        hPutStrLn stderr "Error: Could not fetch remote bundle."
        pure ()

  bundleExistsNow <- doesFileExist fetchedPath
  if not bundleExistsNow
    then pure (VerifyResult 0 [])
    else do
      -- 2. Load metadata from bundle (classifies entries; fetches bundle into .bit/index)
      entries <- loadMetadataFromBundle bundleName
      let allKnownPaths = allEntryPaths entries

      -- 3. Fetch remote file list via rclone ls
      Remote.Scan.fetchRemoteFiles remote >>= either
        (const $ hPutStrLn stderr "Error: Could not fetch remote file list." >> pure (VerifyResult 0 []))
        (\remoteFiles -> do
          let filteredRemoteFiles = filterOutBitPaths remoteFiles
              remoteFileMap = Map.fromList
                [ (normalise (unPath e.path), (h, e.kind))
                | e <- filteredRemoteFiles
                , h <- maybeToList (syncHash e.kind)
                ]

          -- 4. Set up verification working tree
          let vremoteDir = cwd </> ".bit" </> "vremotes" </> Bit.Remote.remoteName remote
          forceRemoveDir vremoteDir
          createDirectoryIfMissing True vremoteDir

          -- Checkout expected state from the remote's bundle
          let absBundlePath = cwd </> fetchedPath
          void $ Git.runGitAt vremoteDir ["init", "-q"]
          void $ Git.runGitAt vremoteDir ["config", "core.quotePath", "false"]
          void $ Git.runGitAt vremoteDir ["fetch", "-q", absBundlePath, "refs/heads/main:refs/heads/main"]
          void $ Git.runGitAt vremoteDir ["checkout", "-q", "main"]

          -- 5. Overwrite working tree with actual remote state:
          --    Binary: construct metadata from rclone ls data
          --    Text: download actual content from remote
          --    Missing: delete the checked-out file
          let processEntry entry = do
                let p = entryPath entry
                    destFile = vremoteDir </> unPath p
                case entry of
                  BinaryEntry _ _ _ ->
                    case Map.lookup (normalise (unPath p)) remoteFileMap of
                      Just (h, File _ sz _) ->
                        writeFile destFile (serializeMetadata (MetaContent h sz))
                      _ -> safeRemove destFile
                  TextEntry _ -> do
                    code <- Transport.copyFromRemote remote (toPosix (unPath p)) destFile
                    when (code /= ExitSuccess) $ safeRemove destFile
                traverse_ (\ref -> atomicModifyIORef' ref (\n -> (n + 1, ()))) mCounter
          void $ runConcurrently concurrency (\e -> processEntry e) entries

          -- 6. Single git diff: compares actual working tree against expected (HEAD)
          (_, diffOut, _) <- Git.runGitAt vremoteDir
            ["diff", "--ignore-cr-at-eol", "--name-status", "HEAD"]
          let entryMap = Map.fromList
                [(normalise (unPath (entryPath e)), e) | e <- entries]
              issues = concatMap (parseDiffLine entryMap remoteFileMap)
                (filter (not . null) (lines diffOut))

          -- 7. Extra files on remote not in bundle metadata
          let filePaths = Set.fromList (map Path (Map.keys remoteFileMap))
              extraPaths = filePaths `Set.difference` allKnownPaths
              extraIssues = map (\p -> HashMismatch p "(not in metadata)" "(exists on remote)" 0 0) (Set.toList extraPaths)

          -- 8. Clean up (.git/objects are read-only, need forceRemoveDir)
          forceRemoveDir vremoteDir

          pure (VerifyResult (length entries) (issues ++ extraIssues)))
  where
    parseDiffLine entryMap remoteFileMap line =
      let (status, rest) = break (== '\t') line
          path = drop 1 rest
          npath = normalise path
      in case status of
        "D" -> [Missing (Path path)]
        "M" -> case (Map.lookup npath entryMap, Map.lookup npath remoteFileMap) of
          (Just (BinaryEntry p eh es), Just (ah, File _ as' _)) ->
            [HashMismatch p (T.unpack (hashToText eh)) (T.unpack (hashToText ah)) es as']
          _ -> [HashMismatch (Path path) "(committed)" "(remote differs)" 0 0]
        _ -> []

-- Helper to safely remove a file
safeRemove :: FilePath -> IO ()
safeRemove filePath = do
  exists <- doesFileExist filePath
  when exists $ removeFile filePath

-- | Remove a directory tree, handling read-only files (e.g. .git/objects).
forceRemoveDir :: FilePath -> IO ()
forceRemoveDir dir = do
  exists <- doesDirectoryExist dir
  when exists $ do
    makeWritable dir
    removeDirectoryRecursive dir
  where
    makeWritable d = do
      contents <- listDirectory d
      mapM_ (\name -> do
        let full = d </> name
        isDir <- doesDirectoryExist full
        if isDir
          then makeWritable full
          else do
            perms <- getPermissions full
            setPermissions full (setOwnerWritable True perms)
        ) contents
