{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Rclone.Sync
    ( -- Shared action derivation
      deriveActions
    , nameStatusToAction
      -- Working directory sync operations
    , syncAllFilesFromHEAD
    , applyMergeToWorkingDir
    , safeDeleteWorkFile
    , copyFromIndexToWorkTree
    , isTextFileInIndex
    , isTextMetadataFile
    , syncBinariesAfterMerge
    , executeCommand
    ) where

import qualified System.Directory as Dir
import Bit.IO.Platform (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Control.Monad (when, void, forM, forM_, unless, filterM)
import Control.Exception (try, SomeException)
import Bit.IO.Concurrency (ioConcurrency, mapConcurrentlyBounded)
import Data.Foldable (traverse_)
import qualified Bit.Git.Run as Git
import qualified Bit.Rclone.Run as Transport
import Bit.Config.Paths (bundleForRemote)
import Bit.Config.Metadata (parseMetadataFile, MetaContent(..))
import Bit.CAS (casBlobPath, hasBlobInCas)
import Bit.CDC.Manifest (casManifestPath, writeManifestToCas, parseManifest)
import Bit.CDC.Types (ChunkRef(..), ChunkManifest(..))
import Bit.CDC.Reassemble (reassembleFile)
import Data.List (isPrefixOf)
import Bit.Utils (toPosix)
import Bit.Domain.Plan (RcloneAction(..), resolveSwaps)
import Bit.Remote (Remote, remoteName, remoteUrl)
import qualified Bit.Device.Identity as Device
import Bit.Types (BitM, BitEnv(..), Path(..), unPath)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import qualified Bit.Rclone.Progress as CopyProgress
-- Strict IO imports to avoid Windows file locking issues
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Bit.Core.Helpers
    ( getLocalHeadE
    , readFileMaybe
    , isFilesystemRemote
    )
import qualified Bit.IO.Platform as Platform
import System.Exit (ExitCode(..))

-- ============================================================================
-- Internal types
-- ============================================================================

-- | File classified for sync: text content is in the index, binary must come from rclone.
data FileToSync
    = TextToSync   FilePath
    | BinaryToSync FilePath
    deriving (Show, Eq)

-- | Probe a remote for a file's manifest. Returns (filePath, MetaContent, Maybe manifest).
-- If a manifest exists on the remote, parses it. Otherwise returns Nothing (whole blob).
classifyForPull :: FilePath -> FilePath -> Remote -> FilePath -> IO (FilePath, Maybe MetaContent, Maybe ChunkManifest)
classifyForPull indexDir casDir remote filePath = do
    mMeta <- parseMetadataFile (indexDir </> filePath)
    case mMeta of
        Just mc -> do
            let remoteManifestPath = toPosix (casManifestPath "cas" (metaHash mc))
                localManifestTmp = casDir </> "tmp_manifest"
            manifestCode <- Transport.copyFromRemote remote remoteManifestPath localManifestTmp
            manifestExists <- case manifestCode of
                ExitSuccess -> Dir.doesFileExist localManifestTmp
                _           -> pure False
            if manifestExists
              then do
                manifestBytes <- BS.readFile localManifestTmp
                safeRemoveFile localManifestTmp
                let manifestContent = case decodeUtf8' manifestBytes of
                        Right t -> T.unpack t
                        Left _  -> ""
                case parseManifest manifestContent of
                    Just manifest -> pure (filePath, Just mc, Just manifest)
                    Nothing       -> pure (filePath, Just mc, Nothing)
              else do
                safeRemoveFile localManifestTmp
                pure (filePath, Just mc, Nothing)
        Nothing -> pure (filePath, Nothing, Nothing)

-- ============================================================================
-- Shared action derivation (used by both push and pull)
-- ============================================================================

-- | Derive rclone actions from a git ref range. Nothing = first sync (all files).
deriveActions :: Maybe String -> String -> IO [RcloneAction]
deriveActions mOldRef newRef = do
    changes <- case mOldRef of
        Just oldRef -> Git.getDiffNameStatus oldRef newRef
        Nothing     -> map Git.Added <$> Git.getFilesAtCommit newRef
    pure $ resolveSwaps (map nameStatusToAction changes)

-- | Convert a git metadata diff entry to an rclone action.
nameStatusToAction :: Git.NameStatusChange -> RcloneAction
nameStatusToAction (Git.Added p)         = Copy (Path p) (Path p)
nameStatusToAction (Git.Modified p)      = Copy (Path p) (Path p)
nameStatusToAction (Git.Deleted p)       = Delete (Path p)
nameStatusToAction (Git.Renamed old new) = Move (Path old) (Path new)
nameStatusToAction (Git.Copied _ new)    = Copy (Path new) (Path new)

-- | Execute pull actions: apply rclone actions to the local working directory.
-- Deletions are processed first (from Delete, Move src, Swap), then copies
-- (from Copy, Move dest, Swap both).
-- Text files are copied from the index; binary files from readable paths (full) or CAS (bare).
executePullActions :: String -> FilePath -> Device.RemoteLayout -> Maybe Remote -> [RcloneAction] -> IO ()
executePullActions remoteRoot cwd layout mRemote actions = do
    -- Phase 1: Process all deletions
    forM_ actions $ \action -> case action of
        Delete p            -> safeDeleteWorkFile cwd (unPath p)
        Move src _          -> safeDeleteWorkFile cwd (unPath src)
        Swap _ src dest     -> do
            safeDeleteWorkFile cwd (unPath src)
            safeDeleteWorkFile cwd (unPath dest)
        _                   -> pure ()

    -- Phase 2: Collect all files to copy
    let filesToCopy = concatMap copyTargets actions
    classifyAndSync remoteRoot cwd layout mRemote filesToCopy
  where
    copyTargets (Copy _ dest)       = [unPath dest]
    copyTargets (Move _ dest)       = [unPath dest]
    copyTargets (Swap _ src dest)   = [unPath src, unPath dest]
    copyTargets (Delete _)          = []

-- | Classify files as text/binary and sync them to the working directory.
-- Text files are copied from the index; binary files from readable paths (full) or CAS (bare).
classifyAndSync :: String -> FilePath -> Device.RemoteLayout -> Maybe Remote -> [FilePath] -> IO ()
classifyAndSync remoteRoot cwd layout mRemote filePaths = do
    -- Classify text vs binary
    let classify filePath = do
            fromIndex <- isTextFileInIndex cwd filePath
            pure $ if fromIndex then TextToSync filePath else BinaryToSync filePath
    fileInfo <- forM filePaths classify

    -- Copy text from index in parallel
    let textPaths = [p | TextToSync p <- fileInfo]
    concLevel <- ioConcurrency
    void $ mapConcurrentlyBounded concLevel (copyFromIndexToWorkTree cwd) textPaths

    -- Binary files: full layout = rclone from readable paths; bare = copy from remote CAS by hash
    let binaryPaths = [p | BinaryToSync p <- fileInfo]
    unless (null binaryPaths) $
        case (layout, mRemote) of
            (Device.LayoutBare, Just remote) -> do
                indexDir <- Git.getIndexPath
                let casDir = takeDirectory indexDir </> "cas"
                    bitDir = takeDirectory indexDir
                createDirectoryIfMissing True casDir

                -- Phase 1: probe manifests and classify files as chunked or whole-blob.
                -- Manifests are small, so individual downloads are fine here.
                classified <- mapM (classifyForPull indexDir casDir remote) binaryPaths
                let chunkedFiles  = [(fp, mc, m) | (fp, Just mc, Just m)  <- classified]
                    wholeBlobFiles = [(fp, mc)    | (fp, Just mc, Nothing) <- classified]

                -- Phase 2: batch-download only chunks/blobs not already in local CAS.
                chunkRelPaths <- fmap concat $ mapM (\(_, _, m) ->
                    filterM (\cr -> not <$> hasBlobInCas casDir (crHash cr)) (cmChunks m) >>= \needed ->
                        pure (map (\cr -> casBlobPath "cas" (crHash cr)) needed)) chunkedFiles
                wholeRelPaths <- filterM (\(_, mc) -> not <$> hasBlobInCas casDir (metaHash mc)) wholeBlobFiles
                    >>= pure . map (\(_, mc) -> casBlobPath "cas" (metaHash mc))
                let allRelPaths = chunkRelPaths ++ wholeRelPaths
                    totalBlobs = sum (map (\(_, _, m) -> length (cmChunks m)) chunkedFiles) + length wholeBlobFiles
                    cached = totalBlobs - length allRelPaths
                putStrLn $ "Downloading " ++ show (length allRelPaths) ++ " file(s) from CAS"
                    ++ if cached > 0 then " (" ++ show cached ++ " already cached)..." else "..."
                unless (null allRelPaths) $ do
                    -- Ensure CAS subdirectories exist for downloaded files
                    mapM_ (createDirectoryIfMissing True . (bitDir </>) . takeDirectory) allRelPaths
                    -- CAS chunks are small (~128KB) — use high parallelism to beat latency.
                    progress <- CopyProgress.newSyncProgress (length allRelPaths)
                    CopyProgress.withSyncProgressReporter progress $
                        CopyProgress.rcloneCopyFilesWithFlags ["--transfers", "32"]
                            (remoteUrl remote) bitDir allRelPaths progress

                -- Phase 3: write manifests locally and reassemble chunked files.
                forM_ chunkedFiles $ \(filePath, mc, manifest) -> do
                    writeManifestToCas casDir (metaHash mc) manifest
                    let localPath = cwd </> filePath
                    createDirectoryIfMissing True (takeDirectory localPath)
                    void $ reassembleFile casDir manifest localPath

                -- Phase 4: copy whole blobs from local CAS to working tree.
                forM_ wholeBlobFiles $ \(filePath, mc) -> do
                    let localPath = cwd </> filePath
                        casSrc = casBlobPath casDir (metaHash mc)
                    createDirectoryIfMissing True (takeDirectory localPath)
                    Dir.copyFile casSrc localPath
            _ -> do
                progress <- CopyProgress.newSyncProgress (length binaryPaths)
                CopyProgress.withSyncProgressReporter progress $
                    CopyProgress.rcloneCopyFiles remoteRoot cwd binaryPaths progress

-- ============================================================================
-- Unified sync operations (work for both cloud and filesystem remotes)
-- ============================================================================

-- | Sync ALL files from current HEAD to local working directory.
-- Used for first pull when there's no old HEAD to diff against.
-- After git checkout has updated the index, HEAD lists the remote's tree.
--
-- Works for both cloud (remoteRoot = remoteUrl remote, e.g. "gdrive:path")
-- and filesystem (remoteRoot = local path to remote).
-- Text files are copied from the index; binary from readable paths (full) or CAS (bare).
syncAllFilesFromHEAD :: String -> FilePath -> Device.RemoteLayout -> Maybe Remote -> IO ()
syncAllFilesFromHEAD remoteRoot localRoot layout mRemote = do
    actions <- deriveActions Nothing "HEAD"
    executePullActions remoteRoot localRoot layout mRemote actions

-- | After a merge, mirror git's metadata changes onto the actual working directory.
-- Uses `git diff --name-status oldHead newHead` to determine what changed,
-- then downloads/deletes/moves actual files accordingly.
--
-- remoteRoot: rclone source for binary files (full layout); bare uses CAS by hash.
-- Text files are copied from the local index; binary from readable paths or CAS.
--
-- CRITICAL: Always reads the actual HEAD after merge from git (via getLocalHeadE).
-- Never accepts newHead as a parameter - this prevents the bug where remoteHash
-- was passed instead of the merged HEAD, causing local-only files to appear deleted.
applyMergeToWorkingDir :: String -> FilePath -> String -> Device.RemoteLayout -> Maybe Remote -> IO ()
applyMergeToWorkingDir remoteRoot cwd oldHead layout mRemote = do
    newHead <- getLocalHeadE
    traverse_ (\newH -> do
        actions <- deriveActions (Just oldHead) newH
        putStrLn "--- Pulling changes from remote ---"
        if null actions
            then putStrLn "Working tree already up to date with remote."
            else executePullActions remoteRoot cwd layout mRemote actions
        ) newHead

-- ============================================================================
-- Working tree helpers
-- ============================================================================

-- | Safely delete a file from the working directory.
safeDeleteWorkFile :: FilePath -> FilePath -> IO ()
safeDeleteWorkFile cwd filePath = do
    let fullPath = cwd </> filePath
    exists <- Dir.doesFileExist fullPath
    when exists $ Dir.removeFile fullPath

-- | Remove a file if it exists. No-op if file doesn't exist.
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile fp = do
    exists <- Dir.doesFileExist fp
    when exists $ Dir.removeFile fp

-- ============================================================================
-- Helper functions
-- ============================================================================

-- | True if the path is a text file in the index (content stored in metadata, not hash/size).
-- Used during pull to avoid re-downloading from rclone when content is already in the bundle.
isTextFileInIndex :: FilePath -> FilePath -> IO Bool
isTextFileInIndex _localRoot filePath = do
    indexDir <- Git.getIndexPath
    let metaPath = indexDir </> filePath
    exists <- Platform.doesFileExist metaPath
    if not exists then pure False
    else do
        mcontent <- readFileMaybe metaPath
        pure $ maybe False (\content -> not (any ("hash: " `isPrefixOf`) (lines content))) mcontent

-- | Copy a file from the index to the working tree. Call only when the path
-- is a text file (content in index). Creates parent dirs as needed.
copyFromIndexToWorkTree :: FilePath -> FilePath -> IO ()
copyFromIndexToWorkTree localRoot filePath = do
    indexDir <- Git.getIndexPath
    let metaPath = indexDir </> filePath
        workPath = localRoot </> filePath
    createDirectoryIfMissing True (takeDirectory workPath)
    copyFile metaPath workPath

-- | Check if a metadata file is a text file (content stored directly) or binary (hash/size stored).
-- Text files don't have "hash:" lines, binary files do.
-- Uses Platform.doesFileExist for UNC path compatibility on Windows.
isTextMetadataFile :: FilePath -> IO Bool
isTextMetadataFile metaPath = do
    exists <- Platform.doesFileExist metaPath
    if not exists then pure False
    else do
        -- Use strict ByteString reading to avoid Windows file locking issues.
        -- try: BS.readFile can fail on UNC paths due to GHC's \\?\ prefix;
        -- treat read failure as "not text" (binary) so the file goes through rclone.
        result <- try (BS.readFile metaPath)
        case result of
            Left (_ :: SomeException) -> pure False
            Right bs -> do
                let content = either (const "") T.unpack (decodeUtf8' bs)
                pure $ not (any ("hash: " `isPrefixOf`) (lines content))


-- | Sync binaries after a successful merge commit.
-- Uses remoteUrl to determine the rclone source (full) or CAS (bare), works for both cloud and filesystem.
syncBinariesAfterMerge :: Remote -> Maybe String -> BitM ()
syncBinariesAfterMerge remote oldHead = do
    cwd <- asks envCwd
    isFs <- isFilesystemRemote remote
    layout <- if isFs then pure Device.LayoutFull else liftIO (Device.readRemoteLayout cwd (remoteName remote))
    let name = remoteName remote
        remoteRoot = remoteUrl remote
        mRemote = if isFs then Nothing else Just remote
    liftIO $ putStrLn "Syncing binaries... done."
    -- Apply diff-based sync or full sync depending on whether we have an old HEAD
    liftIO $ maybe (syncAllFilesFromHEAD remoteRoot cwd layout mRemote) (\oh -> applyMergeToWorkingDir remoteRoot cwd oh layout mRemote) oldHead
    maybeRemoteHash <- liftIO $ Git.getHashFromBundle (bundleForRemote (remoteName remote))
    liftIO $ traverse_ (void . Git.updateRemoteTrackingBranchToHash name) maybeRemoteHash

-- | Execute a non-copy rclone action (push: local -> remote).
-- Copy actions are handled by batch rcloneCopyFiles in Push.syncRemoteFiles.
executeCommand :: FilePath -> Remote -> RcloneAction -> IO ()
executeCommand _localRoot remote action = case action of
        Copy _ _ -> pure ()  -- Handled by batch rcloneCopyFiles

        Move src dest ->
            void $ Transport.moveRemote remote (toPosix (unPath src)) (toPosix (unPath dest))

        Delete p ->
            void $ Transport.deleteRemote remote (toPosix (unPath p))

        Swap tmp src dest -> do
            void $ Transport.moveRemote remote (toPosix (unPath src)) (toPosix (unPath tmp))
            void $ Transport.moveRemote remote (toPosix (unPath dest)) (toPosix (unPath src))
            void $ Transport.moveRemote remote (toPosix (unPath tmp)) (toPosix (unPath dest))
