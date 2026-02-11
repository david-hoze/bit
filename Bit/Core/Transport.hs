{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Core.Transport
    ( -- Working directory sync operations
      syncAllFilesFromHEAD
    , applyMergeToWorkingDir
    , filesystemDeleteFileAtRemote
    , filesystemSyncAllFiles
    , filesystemSyncChangedFiles
    , safeDeleteWorkFile
    , copyFromIndexToWorkTree
    , isTextFileInIndex
    , isTextMetadataFile
    , syncBinariesAfterMerge
    , executeCommand
    ) where

import qualified System.Directory as Dir
import Bit.Platform (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Control.Monad (when, void, forM, forM_, unless)
import Bit.Concurrency (ioConcurrency, mapConcurrentlyBounded)
import Data.Foldable (traverse_)
import System.Exit (ExitCode(..))
import qualified Internal.Git as Git
import Internal.Git (NameStatusChange(Added, Deleted, Modified, Renamed, Copied))
import qualified Internal.Transport as Transport
import Internal.Config (bitIndexPath, bundleForRemote)
import Data.List (isPrefixOf)
import Bit.Utils (toPosix)
import Bit.Plan (RcloneAction(..))
import Bit.Remote (Remote, remoteName, remoteUrl, RemotePath(..))
import Bit.Types (BitM, BitEnv(..), unPath)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import qualified Bit.CopyProgress as CopyProgress
-- Strict IO imports to avoid Windows file locking issues
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Bit.Core.Helpers
    ( getLocalHeadE
    , readFileMaybe
    )
import qualified Bit.Platform as Platform

-- ============================================================================
-- Internal types
-- ============================================================================

-- | File classified for sync: text content is in the index, binary must come from rclone.
data FileToSync
    = TextToSync   FilePath
    | BinaryToSync FilePath
    deriving (Show, Eq)

-- ============================================================================
-- Unified sync operations (work for both cloud and filesystem remotes)
-- ============================================================================

-- | Sync ALL files from current HEAD to local working directory.
-- Used for first pull when there's no old HEAD to diff against.
-- After git checkout has updated the index, HEAD lists the remote's tree.
--
-- Works for both cloud (remoteRoot = remoteUrl remote, e.g. "gdrive:path")
-- and filesystem (remoteRoot = local path to remote).
-- Text files are copied from the index; binary files are batched via rclone.
syncAllFilesFromHEAD :: String -> FilePath -> IO ()
syncAllFilesFromHEAD remoteRoot localRoot = do
    let localIndex = localRoot </> ".bit" </> "index"
    (code, out, _) <- Git.runGitWithOutput ["ls-tree", "-r", "--name-only", "HEAD"]
    when (code == ExitSuccess) $ do
        let paths = filter (not . null) (lines out)

        -- Classify text vs binary
        fileInfo <- forM paths $ \filePath -> do
            let metaPath = localIndex </> filePath
            isText <- isTextMetadataFile metaPath
            pure $ if isText then TextToSync filePath else BinaryToSync filePath

        -- Copy text from index in parallel
        let textPaths = [p | TextToSync p <- fileInfo]
        concLevel <- ioConcurrency
        void $ mapConcurrentlyBounded concLevel (copyFromIndexToWorkTree localRoot) textPaths

        -- Batch binary via rcloneCopyFiles
        let binaryPaths = [p | BinaryToSync p <- fileInfo]
        unless (null binaryPaths) $ do
            progress <- CopyProgress.newSyncProgress (length binaryPaths)
            CopyProgress.withSyncProgressReporter progress $
                CopyProgress.rcloneCopyFiles remoteRoot localRoot binaryPaths progress

-- | After a merge, mirror git's metadata changes onto the actual working directory.
-- Uses `git diff --name-status oldHead newHead` to determine what changed,
-- then downloads/deletes/moves actual files accordingly.
--
-- remoteRoot: rclone source for binary files (cloud URL or filesystem path).
-- Text files are copied from the local index; binary files are batched via rclone.
--
-- CRITICAL: Always reads the actual HEAD after merge from git (via getLocalHeadE).
-- Never accepts newHead as a parameter - this prevents the bug where remoteHash
-- was passed instead of the merged HEAD, causing local-only files to appear deleted.
applyMergeToWorkingDir :: String -> FilePath -> String -> IO ()
applyMergeToWorkingDir remoteRoot cwd oldHead = do
    newHead <- getLocalHeadE
    traverse_ (\newH -> do
        changes <- Git.getDiffNameStatus oldHead newH
        putStrLn "--- Pulling changes from remote ---"
        if null changes
            then putStrLn "Working tree already up to date with remote."
            else do
                -- Process deletions first
                forM_ changes $ \change -> case change of
                    Deleted p -> safeDeleteWorkFile cwd p
                    Renamed oldPath _ -> safeDeleteWorkFile cwd oldPath
                    _ -> pure ()

                -- Collect files to copy
                let filesToCopy = concatMap (\change -> case change of
                        Added p -> [p]
                        Modified p -> [p]
                        Renamed _ newPath -> [newPath]
                        Copied _ newPath -> [newPath]
                        Deleted _ -> []) changes

                -- Classify text vs binary
                let classify filePath = do
                        fromIndex <- isTextFileInIndex cwd filePath
                        pure $ if fromIndex then TextToSync filePath else BinaryToSync filePath
                fileInfo <- forM filesToCopy classify

                -- Copy text from index in parallel
                let textPaths = [p | TextToSync p <- fileInfo]
                concLevel <- ioConcurrency
                void $ mapConcurrentlyBounded concLevel (copyFromIndexToWorkTree cwd) textPaths

                -- Batch binary via rcloneCopyFiles
                let binaryPaths = [p | BinaryToSync p <- fileInfo]
                unless (null binaryPaths) $ do
                    progress <- CopyProgress.newSyncProgress (length binaryPaths)
                    CopyProgress.withSyncProgressReporter progress $
                        CopyProgress.rcloneCopyFiles remoteRoot cwd binaryPaths progress
        ) newHead

-- ============================================================================
-- Filesystem remote push operations
-- ============================================================================

-- | Delete a file at the remote working tree.
filesystemDeleteFileAtRemote :: RemotePath -> FilePath -> IO ()
filesystemDeleteFileAtRemote (RemotePath remotePath) filePath = do
    let fullPath = remotePath </> filePath
    exists <- Platform.doesFileExist fullPath
    when exists $ Platform.removeFile fullPath

-- | Sync all files from a commit to the filesystem remote (first push).
-- Text files are copied individually from remote index to remote working tree.
-- Binary files are batched via rcloneCopyFiles (local -> remote).
filesystemSyncAllFiles :: FilePath -> RemotePath -> String -> IO ()
filesystemSyncAllFiles localRoot (RemotePath remotePath) commitHash = do
    let remoteIndex = remotePath </> ".bit" </> "index"
    files <- Git.runGitAt remoteIndex ["ls-tree", "-r", "--name-only", commitHash]
    case files of
        (ExitSuccess, out, _) -> do
            let paths = filter (not . null) (lines out)

            -- Classify text vs binary (only include binaries that exist locally)
            fileInfo <- fmap concat $ forM paths $ \filePath -> do
                let metaPath = remoteIndex </> filePath
                isText <- isTextMetadataFile metaPath
                if isText
                    then pure [TextToSync filePath]
                    else do
                        let srcPath = localRoot </> filePath
                        srcExists <- Dir.doesFileExist srcPath
                        pure [BinaryToSync filePath | srcExists]

            -- Copy text files from remote index to remote working tree in parallel
            let textPaths = [p | TextToSync p <- fileInfo]
                copyText p = do
                    let metaPath = remoteIndex </> p
                        workPath = remotePath </> p
                    createDirectoryIfMissing True (takeDirectory workPath)
                    copyFile metaPath workPath
            concLevel <- ioConcurrency
            void $ mapConcurrentlyBounded concLevel copyText textPaths

            -- Batch binary via rcloneCopyFiles (localRoot -> remotePath)
            let binaryPaths = [p | BinaryToSync p <- fileInfo]
            unless (null binaryPaths) $ do
                progress <- CopyProgress.newSyncProgress (length binaryPaths)
                CopyProgress.withSyncProgressReporter progress $
                    CopyProgress.rcloneCopyFiles localRoot remotePath binaryPaths progress
        _ -> pure ()

-- | Sync only changed files between two commits (subsequent push).
-- Text files are copied individually; binary files are batched via rcloneCopyFiles.
filesystemSyncChangedFiles :: FilePath -> RemotePath -> String -> String -> IO ()
filesystemSyncChangedFiles localRoot (RemotePath remotePath) oldHead newHead = do
    let remoteIndex = remotePath </> ".bit" </> "index"
    changes <- Git.runGitAt remoteIndex ["diff", "--name-status", oldHead, newHead]
    case changes of
        (ExitSuccess, out, _) -> do
            let parsedChanges = Git.parseNameStatusOutput out

            -- Process deletions individually
            let rp = RemotePath remotePath
            forM_ parsedChanges $ \change -> case change of
                Deleted p -> filesystemDeleteFileAtRemote rp p
                Renamed oldPath _ -> filesystemDeleteFileAtRemote rp oldPath
                _ -> pure ()

            -- Collect files to copy
            let filesToCopy = concatMap (\change -> case change of
                    Added p -> [p]
                    Modified p -> [p]
                    Renamed _ newPath -> [newPath]
                    Copied _ newPath -> [newPath]
                    Deleted _ -> []) parsedChanges

            -- Classify text vs binary (only include binaries that exist locally)
            fileInfo <- fmap concat $ forM filesToCopy $ \p -> do
                let metaPath = remoteIndex </> p
                isText <- isTextMetadataFile metaPath
                if isText
                    then pure [TextToSync p]
                    else do
                        let srcPath = localRoot </> p
                        srcExists <- Dir.doesFileExist srcPath
                        pure [BinaryToSync p | srcExists]

            -- Copy text from remote index to remote working tree in parallel
            let textPaths = [p | TextToSync p <- fileInfo]
                copyText p = do
                    let metaPath = remoteIndex </> p
                        workPath = remotePath </> p
                    createDirectoryIfMissing True (takeDirectory workPath)
                    copyFile metaPath workPath
            concLevel <- ioConcurrency
            void $ mapConcurrentlyBounded concLevel copyText textPaths

            -- Batch binary via rcloneCopyFiles (localRoot -> remotePath)
            let binaryPaths = [p | BinaryToSync p <- fileInfo]
            unless (null binaryPaths) $ do
                progress <- CopyProgress.newSyncProgress (length binaryPaths)
                CopyProgress.withSyncProgressReporter progress $
                    CopyProgress.rcloneCopyFiles localRoot remotePath binaryPaths progress
        _ -> pure ()

-- ============================================================================
-- Working tree helpers
-- ============================================================================

-- | Safely delete a file from the working directory.
safeDeleteWorkFile :: FilePath -> FilePath -> IO ()
safeDeleteWorkFile cwd filePath = do
    let fullPath = cwd </> filePath
    exists <- Dir.doesFileExist fullPath
    when exists $ Dir.removeFile fullPath

-- ============================================================================
-- Helper functions
-- ============================================================================

-- | True if the path is a text file in the index (content stored in metadata, not hash/size).
-- Used during pull to avoid re-downloading from rclone when content is already in the bundle.
isTextFileInIndex :: FilePath -> FilePath -> IO Bool
isTextFileInIndex localRoot filePath = do
    let metaPath = localRoot </> bitIndexPath </> filePath
    exists <- Dir.doesFileExist metaPath
    if not exists then pure False
    else do
        mcontent <- readFileMaybe metaPath
        pure $ maybe False (\content -> not (any ("hash: " `isPrefixOf`) (lines content))) mcontent

-- | Copy a file from the index to the working tree. Call only when the path
-- is a text file (content in index). Creates parent dirs as needed.
copyFromIndexToWorkTree :: FilePath -> FilePath -> IO ()
copyFromIndexToWorkTree localRoot filePath = do
    let metaPath = localRoot </> bitIndexPath </> filePath
        workPath = localRoot </> filePath
    createDirectoryIfMissing True (takeDirectory workPath)
    copyFile metaPath workPath

-- | Check if a metadata file is a text file (content stored directly) or binary (hash/size stored).
-- Text files don't have "hash:" lines, binary files do.
isTextMetadataFile :: FilePath -> IO Bool
isTextMetadataFile metaPath = do
    exists <- Dir.doesFileExist metaPath
    if not exists then pure False
    else do
        -- Use strict ByteString reading to avoid Windows file locking issues
        bs <- BS.readFile metaPath
        let content = either (const "") T.unpack (decodeUtf8' bs)
        pure $ not (any ("hash: " `isPrefixOf`) (lines content))


-- | Sync binaries after a successful merge commit.
-- Uses remoteUrl to determine the rclone source, works for both cloud and filesystem.
syncBinariesAfterMerge :: Remote -> Maybe String -> BitM ()
syncBinariesAfterMerge remote oldHead = do
    cwd <- asks envCwd
    let name = remoteName remote
        remoteRoot = remoteUrl remote
    liftIO $ putStrLn "Syncing binaries... done."
    -- Apply diff-based sync or full sync depending on whether we have an old HEAD
    liftIO $ maybe (syncAllFilesFromHEAD remoteRoot cwd) (applyMergeToWorkingDir remoteRoot cwd) oldHead
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
