{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Core.Transport
    ( -- File transport abstraction
      FileTransport(..)
    , mkCloudTransport
    , mkFilesystemTransport
      -- Working directory sync operations
    , applyMergeToWorkingDir
    , downloadOrCopyFromIndex
    , filesystemDownloadOrCopyFromIndex
    , filesystemSyncRemoteFilesToLocalFromHEAD
    , filesystemCopyFileToRemote
    , filesystemDeleteFileAtRemote
    , filesystemSyncAllFiles
    , filesystemSyncChangedFiles
    , safeDeleteWorkFile
    , copyFromIndexToWorkTree
    , isTextFileInIndex
    , isTextMetadataFile
    , syncBinariesAfterMerge
    , executeCommand
    , executePullCommand
    ) where

import qualified System.Directory as Dir
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import Control.Monad (when, void, forM)
import Data.Foldable (traverse_)
import Data.Maybe (maybeToList)
import System.Exit (ExitCode(..))
import qualified Internal.Git as Git
import qualified Internal.Transport as Transport
import Internal.Config (bitIndexPath, fetchedBundle)
import qualified Bit.Scan as Scan
import qualified Bit.Pipeline as Pipeline
import qualified Bit.Remote.Scan as Remote.Scan
import Data.List (isPrefixOf)
import Bit.Concurrency (runConcurrentlyBounded)
import Control.Concurrent (getNumCapabilities)
import System.IO (stderr, hPutStrLn)
import Bit.Utils (toPosix)
import Bit.Plan (RcloneAction(..))
import Bit.Remote (Remote)
import Bit.Types (BitM, BitEnv(..), unPath)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import qualified Bit.CopyProgress as CopyProgress
import Bit.CopyProgress (SyncProgress)
import Data.IORef (writeIORef)
-- Strict IO imports to avoid Windows file locking issues
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Bit.Core.Helpers
    ( getLocalHeadE
    , parseFilesystemDiffOutput
    , readFileMaybe
    )

-- ============================================================================
-- FILE TRANSPORT ABSTRACTION
-- ============================================================================

-- | Abstracts how files are transferred during pull.
-- Cloud remotes use rclone; filesystem remotes use direct file copy.
data FileTransport = FileTransport
  { -- | Copy/download a single file to the working directory.
    -- Args: cwd, relative path, progress tracker
    transportDownloadFile :: FilePath -> FilePath -> SyncProgress -> IO ()
    -- | Sync ALL files from remote to local (used for first pull when there's no oldHead to diff against).
    -- Called after git checkout has updated the index, so files are listed from current HEAD.
    -- Args: cwd
  , transportSyncAllFiles :: FilePath -> IO ()
  }

-- | Build a cloud transport that uses rclone to copy files.
mkCloudTransport :: Remote -> FileTransport
mkCloudTransport remote = FileTransport
  { transportDownloadFile = \cwd filePath _progress -> downloadOrCopyFromIndex cwd remote filePath
  , transportSyncAllFiles = \cwd -> do
      -- Cloud path uses the existing syncRemoteFilesToLocal logic
      -- which scans remote via rclone and syncs to local.
      localFiles <- Scan.scanWorkingDir cwd
      remoteResult <- Remote.Scan.fetchRemoteFiles remote
      case remoteResult of
        Left _ -> hPutStrLn stderr "Error: Failed to fetch remote file list."
        Right remoteFiles -> do
          let actions = Pipeline.pullSyncFiles localFiles remoteFiles
          putStrLn "--- Pulling changes from remote ---"
          if null actions
            then putStrLn "Working tree already up to date with remote."
            else do
              -- Create progress tracker for cloud operations (file-count only)
              progress <- CopyProgress.newSyncProgress (length actions)
              CopyProgress.withSyncProgressReporter progress $ do
                -- Use lower concurrency for network/subprocess operations
                caps <- getNumCapabilities
                let concurrency = min 8 (max 2 (caps * 2))
                void $ runConcurrentlyBounded concurrency (\a -> do
                  executePullCommand cwd remote a
                  CopyProgress.incrementFilesComplete progress
                  ) actions
  }

-- | Build a filesystem transport that uses direct file copy.
mkFilesystemTransport :: FilePath -> FileTransport
mkFilesystemTransport remotePath = FileTransport
  { transportDownloadFile = filesystemDownloadOrCopyFromIndex' remotePath
  , transportSyncAllFiles = filesystemSyncRemoteFilesToLocal' remotePath
  }
  where
    -- Wrapper that matches the signature expected by FileTransport
    filesystemDownloadOrCopyFromIndex' remPath cwd filePath progress =
        filesystemDownloadOrCopyFromIndex cwd remPath filePath progress
    filesystemSyncRemoteFilesToLocal' _remPath cwd =
      filesystemSyncRemoteFilesToLocalFromHEAD cwd remotePath

-- ============================================================================
-- Working tree synchronization
-- ============================================================================

-- | After a merge, mirror git's metadata changes onto the actual working directory.
-- Uses `git diff --name-status oldHead newHead` to determine what changed,
-- then downloads/deletes/moves actual files accordingly.
-- This replaces syncRemoteFilesToLocal for merge pulls.
--
-- CRITICAL: Always reads the actual HEAD after merge from git (via getLocalHeadE).
-- Never accepts newHead as a parameter - this prevents the bug where remoteHash
-- was passed instead of the merged HEAD, causing local-only files to appear deleted.
applyMergeToWorkingDir :: FileTransport -> FilePath -> String -> IO ()
applyMergeToWorkingDir transport cwd oldHead = do
    newHead <- getLocalHeadE
    case newHead of
        Nothing -> pure ()  -- shouldn't happen after merge commit
        Just newH -> do
            changes <- Git.getDiffNameStatus oldHead newH
            putStrLn "--- Pulling changes from remote ---"
            if null changes
                then putStrLn "Working tree already up to date with remote."
                else do
                    -- First pass: collect paths that will be copied and their sizes
                    filesToCopy <- fmap concat $ forM changes $ \(fileStatus, filePath, mNewPath) -> case fileStatus of
                        'A' -> pure [filePath]
                        'M' -> pure [filePath]
                        'R' -> pure (maybeToList mNewPath)
                        _ -> pure []
                    
                    -- Gather file sizes for binary files (for progress tracking)
                    fileInfo <- forM filesToCopy $ \filePath -> do
                        fromIndex <- isTextFileInIndex cwd filePath
                        if fromIndex
                            then pure (filePath, True, (0 :: Integer))
                            else do
                                -- Binary file: try to get size for progress
                                -- (size might not be available yet, that's ok)
                                let _destPath = cwd </> filePath
                                pure (filePath, False, 0)  -- Size will be tracked during copy
                    
                    let binaryFiles = [(p, s) | (p, False, s) <- fileInfo]
                        totalFiles = length binaryFiles
                    
                    -- Create progress tracker
                    progress <- CopyProgress.newSyncProgress totalFiles
                    
                    -- Second pass: apply changes with progress (parallelized)
                    CopyProgress.withSyncProgressReporter progress $ do
                        -- Use lower concurrency for file operations to avoid thrashing
                        caps <- getNumCapabilities
                        let concurrency = max 2 (caps * 2)
                        void $ runConcurrentlyBounded concurrency (\(fileStatus, filePath, mNewPath) -> case fileStatus of
                            'A' -> (transportDownloadFile transport) cwd filePath progress
                            'M' -> (transportDownloadFile transport) cwd filePath progress
                            'D' -> safeDeleteWorkFile cwd filePath
                            'R' -> case mNewPath of
                                Just newPath -> do
                                    safeDeleteWorkFile cwd filePath
                                    (transportDownloadFile transport) cwd newPath progress
                                Nothing -> pure ()
                            _ -> pure ()
                            ) changes

-- | Download a file from remote, or copy from index if it's a text file.
-- Used by cloud transport.
downloadOrCopyFromIndex :: FilePath -> Remote -> FilePath -> IO ()
downloadOrCopyFromIndex cwd remote filePath = do
    fromIndex <- isTextFileInIndex cwd filePath
    if fromIndex
        then copyFromIndexToWorkTree cwd filePath
        else do
            let localPath = cwd </> filePath
            createDirectoryIfMissing True (takeDirectory localPath)
            void $ Transport.copyFromRemote remote (toPosix filePath) (toPosix localPath)

-- | Download a file from remote or copy from index for filesystem pull.
-- Parameter order: localRoot, remotePath, filePath (relative), progress.
filesystemDownloadOrCopyFromIndex :: FilePath -> FilePath -> FilePath -> SyncProgress -> IO ()
filesystemDownloadOrCopyFromIndex localRoot remotePath filePath progress = do
    fromIndex <- isTextFileInIndex localRoot filePath
    if fromIndex
        then copyFromIndexToWorkTree localRoot filePath
        else do
            -- Binary file: copy from remote working tree
            let srcPath = remotePath </> filePath
            let destPath = localRoot </> filePath
            srcExists <- Dir.doesFileExist srcPath
            when srcExists $ do
                size <- Dir.getFileSize srcPath
                writeIORef (CopyProgress.spCurrentFile progress) filePath
                CopyProgress.copyFileWithProgress srcPath destPath (fromIntegral size) progress
                CopyProgress.incrementFilesComplete progress

-- | Sync all files from remote to local using current HEAD (after checkout).
-- This is called after git checkout has updated the index, so we list files from HEAD.
filesystemSyncRemoteFilesToLocalFromHEAD :: FilePath -> FilePath -> IO ()
filesystemSyncRemoteFilesToLocalFromHEAD localRoot remotePath = do
    let localIndex = localRoot </> ".bit" </> "index"
    -- Use HEAD to list files (after checkout, HEAD points to the remote branch)
    (code, out, _) <- Git.runGitWithOutput ["ls-tree", "-r", "--name-only", "HEAD"]
    when (code == ExitSuccess) $ do
        let paths = filter (not . null) (lines out)
        
        -- First pass: classify files and gather sizes for binary files
        fileInfo <- forM paths $ \filePath -> do
            let metaPath = localIndex </> filePath
            isText <- isTextMetadataFile metaPath
            if isText
                then pure (filePath, True, 0)
                else do
                    -- Binary file: get size from remote file
                    let srcPath = remotePath </> filePath
                    srcExists <- Dir.doesFileExist srcPath
                    if srcExists
                        then do
                            size <- Dir.getFileSize srcPath
                            pure (filePath, False, fromIntegral size)
                        else pure (filePath, False, 0)
        
        let binaryFiles = [(p, s) | (p, False, s) <- fileInfo, s > 0]
            totalBytes = sum [s | (_, s) <- binaryFiles]
        
        -- Create progress tracker
        progress <- CopyProgress.newSyncProgress (length binaryFiles)
        writeIORef (CopyProgress.spBytesTotal progress) totalBytes
        
        -- Second pass: copy files with progress (parallelized)
        CopyProgress.withSyncProgressReporter progress $ do
            -- Use lower concurrency for file copies to avoid disk thrashing
            caps <- getNumCapabilities
            let concurrency = max 2 (caps * 2)
            void $ runConcurrentlyBounded concurrency (\(filePath, isText, size) -> do
                let metaPath = localIndex </> filePath
                if isText
                    then do
                        -- Text file: metadata IS the content, copy from local index to working tree
                        let workPath = localRoot </> filePath
                        createDirectoryIfMissing True (takeDirectory workPath)
                        copyFile metaPath workPath
                    else do
                        -- Binary file: metadata is hash/size, copy actual file from remote working tree
                        let srcPath = remotePath </> filePath
                        let destPath = localRoot </> filePath
                        srcExists <- Dir.doesFileExist srcPath
                        when srcExists $ do
                            writeIORef (CopyProgress.spCurrentFile progress) filePath
                            CopyProgress.copyFileWithProgress srcPath destPath size progress
                            CopyProgress.incrementFilesComplete progress
                ) fileInfo

-- | Copy a file from local to remote (handles both text and binary).
-- Parameter order: localRoot, remotePath, remoteIndex, filePath (relative).
-- TRANSPOSITION NOTE: 4 FilePaths — rely on naming conventions.
filesystemCopyFileToRemote :: FilePath -> FilePath -> FilePath -> FilePath -> SyncProgress -> IO ()
filesystemCopyFileToRemote localRoot remotePath remoteIndex filePath progress = do
    -- Check if it's a text file (content in index) or binary (hash/size in index)
    let metaPath = remoteIndex </> filePath
    isText <- isTextMetadataFile metaPath
    if isText
        then do
            -- Text file: metadata IS the content, copy from remote index to working tree
            let workPath = remotePath </> filePath
            createDirectoryIfMissing True (takeDirectory workPath)
            copyFile metaPath workPath
        else do
            -- Binary file: metadata is hash/size, copy actual file from local working tree
            let srcPath = localRoot </> filePath
            let destPath = remotePath </> filePath
            srcExists <- Dir.doesFileExist srcPath
            when srcExists $ do
                size <- Dir.getFileSize srcPath
                writeIORef (CopyProgress.spCurrentFile progress) filePath
                CopyProgress.copyFileWithProgress srcPath destPath (fromIntegral size) progress
                CopyProgress.incrementFilesComplete progress

-- | Delete a file at the remote working tree.
filesystemDeleteFileAtRemote :: FilePath -> FilePath -> IO ()
filesystemDeleteFileAtRemote remotePath filePath = do
    let fullPath = remotePath </> filePath
    exists <- Dir.doesFileExist fullPath
    when exists $ Dir.removeFile fullPath

-- | Sync all files from a commit to the filesystem remote (first push).
filesystemSyncAllFiles :: FilePath -> FilePath -> String -> IO ()
filesystemSyncAllFiles localRoot remotePath commitHash = do
    let remoteIndex = remotePath </> ".bit" </> "index"
    files <- Git.runGitAt remoteIndex ["ls-tree", "-r", "--name-only", commitHash]
    case files of
        (ExitSuccess, out, _) -> do
            let paths = filter (not . null) (lines out)
            
            -- First pass: classify files and gather sizes for binary files
            fileInfo <- forM paths $ \filePath -> do
                let metaPath = remoteIndex </> filePath
                isText <- isTextMetadataFile metaPath
                if isText
                    then pure (filePath, True, 0)
                    else do
                        -- Binary file: get size from local file
                        let srcPath = localRoot </> filePath
                        srcExists <- Dir.doesFileExist srcPath
                        if srcExists
                            then do
                                size <- Dir.getFileSize srcPath
                                pure (filePath, False, fromIntegral size)
                            else pure (filePath, False, 0)
            
            let binaryFiles = [(p, s) | (p, False, s) <- fileInfo, s > 0]
                totalBytes = sum [s | (_, s) <- binaryFiles]
            
            -- Create progress tracker
            progress <- CopyProgress.newSyncProgress (length binaryFiles)
            writeIORef (CopyProgress.spBytesTotal progress) totalBytes
            
            -- Second pass: copy files with progress (parallelized)
            CopyProgress.withSyncProgressReporter progress $ do
                -- Use lower concurrency for file copies to avoid disk thrashing
                caps <- getNumCapabilities
                let concurrency = max 2 (caps * 2)
                void $ runConcurrentlyBounded concurrency (\(filePath, isText, size) -> do
                    let metaPath = remoteIndex </> filePath
                    if isText
                        then do
                            -- Text file: metadata IS the content, copy from remote index to working tree
                            let workPath = remotePath </> filePath
                            createDirectoryIfMissing True (takeDirectory workPath)
                            copyFile metaPath workPath
                        else do
                            -- Binary file: metadata is hash/size, copy actual file from local working tree
                            let srcPath = localRoot </> filePath
                            let destPath = remotePath </> filePath
                            srcExists <- Dir.doesFileExist srcPath
                            when srcExists $ do
                                writeIORef (CopyProgress.spCurrentFile progress) filePath
                                CopyProgress.copyFileWithProgress srcPath destPath size progress
                                CopyProgress.incrementFilesComplete progress
                    ) fileInfo
        _ -> pure ()

-- | Sync only changed files between two commits.
-- Parameter order: localRoot, remotePath, oldHead (commit hash), newHead (commit hash).
filesystemSyncChangedFiles :: FilePath -> FilePath -> String -> String -> IO ()
filesystemSyncChangedFiles localRoot remotePath oldHead newHead = do
    let remoteIndex = remotePath </> ".bit" </> "index"
    changes <- Git.runGitAt remoteIndex ["diff", "--name-status", oldHead, newHead]
    case changes of
        (ExitSuccess, out, _) -> do
            let parsedChanges = parseFilesystemDiffOutput out
            
            -- First pass: collect paths that will be copied and their sizes
            filesToCopy <- fmap concat $ forM parsedChanges $ \(fileStatus, filePath, mNewPath) -> case fileStatus of
                'A' -> pure [filePath]
                'M' -> pure [filePath]
                'R' -> pure (maybeToList mNewPath)
                _ -> pure []
            
            -- Gather file sizes for binary files
            fileInfo <- forM filesToCopy $ \p -> do
                let metaPath = remoteIndex </> p
                isText <- isTextMetadataFile metaPath
                if isText
                    then pure (p, True, 0)
                    else do
                        let srcPath = localRoot </> p
                        srcExists <- Dir.doesFileExist srcPath
                        if srcExists
                            then do
                                size <- Dir.getFileSize srcPath
                                pure (p, False, fromIntegral size)
                            else pure (p, False, 0)
            
            let binaryFiles = [(p, s) | (p, False, s) <- fileInfo, s > 0]
                totalBytes = sum [s | (_, s) <- binaryFiles]
            
            -- Create progress tracker
            progress <- CopyProgress.newSyncProgress (length binaryFiles)
            writeIORef (CopyProgress.spBytesTotal progress) totalBytes
            
            -- Second pass: apply changes with progress (parallelized)
            CopyProgress.withSyncProgressReporter progress $ do
                -- Use lower concurrency for file copies to avoid disk thrashing
                caps <- getNumCapabilities
                let concurrency = max 2 (caps * 2)
                void $ runConcurrentlyBounded concurrency (\(st, p, mNewPath) -> case st of
                    'A' -> filesystemCopyFileToRemote localRoot remotePath remoteIndex p progress
                    'M' -> filesystemCopyFileToRemote localRoot remotePath remoteIndex p progress
                    'D' -> filesystemDeleteFileAtRemote remotePath p
                    'R' -> case mNewPath of
                        Just newPath -> do
                            filesystemDeleteFileAtRemote remotePath p
                            filesystemCopyFileToRemote localRoot remotePath remoteIndex newPath progress
                        Nothing -> pure ()
                    _ -> pure ()
                    ) parsedChanges
        _ -> pure ()

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

-- | Sync binaries after a successful merge commit
syncBinariesAfterMerge :: FileTransport -> Remote -> Maybe String -> BitM ()
syncBinariesAfterMerge transport _remote oldHead = do
    cwd <- asks envCwd
    liftIO $ putStrLn "Syncing binaries... done."
    -- Apply diff-based sync or full sync depending on whether we have an old HEAD
    liftIO $ maybe (transportSyncAllFiles transport cwd) (applyMergeToWorkingDir transport cwd) oldHead
    maybeRemoteHash <- liftIO $ Git.getHashFromBundle fetchedBundle
    liftIO $ traverse_ (void . Git.updateRemoteTrackingBranchToHash) maybeRemoteHash

-- | Executes/Prints the command to be run in the shell (push: local -> remote).
executeCommand :: FilePath -> Remote -> RcloneAction -> IO ()
executeCommand localRoot remote action = case action of
        Copy src dest -> do
            let localPath = toPosix (localRoot </> unPath src)
            void $ Transport.copyToRemote localPath remote (toPosix (unPath dest))

        Move src dest ->
            void $ Transport.moveRemote remote (toPosix (unPath src)) (toPosix (unPath dest))

        Delete p ->
            void $ Transport.deleteRemote remote (toPosix (unPath p))

        Swap _ _ _ -> pure ()  -- not produced by planAction; future-proofing

-- | Execute a single pull action: copy from remote to local or delete local file.
-- Text files are already in the git bundle (index); copy from index to work dir instead of rclone.
executePullCommand :: FilePath -> Remote -> RcloneAction -> IO ()
executePullCommand localRoot remote action = case action of
        Copy _src dest -> do
            fromIndex <- isTextFileInIndex localRoot (unPath dest)
            if fromIndex
            then copyFromIndexToWorkTree localRoot (unPath dest)
            else do
                let localPath = toPosix (localRoot </> unPath dest)
                createDirectoryIfMissing True (takeDirectory (localRoot </> unPath dest))
                void $ Transport.copyFromRemote remote (toPosix (unPath dest)) localPath
        Move src dest -> do
            fromIndex <- isTextFileInIndex localRoot (unPath src)
            if fromIndex
            then copyFromIndexToWorkTree localRoot (unPath src)
            else do
                let localSrcPath = localRoot </> unPath src
                createDirectoryIfMissing True (takeDirectory localSrcPath)
                void $ Transport.copyFromRemote remote (toPosix (unPath src)) (toPosix localSrcPath)
            let localDestPath = localRoot </> unPath dest
            exists <- Dir.doesFileExist localDestPath
            when exists $ Dir.removeFile localDestPath
        Delete filePath -> do
            let localPath = localRoot </> unPath filePath
            exists <- Dir.doesFileExist localPath
            when exists $ Dir.removeFile localPath
        Swap _ _ _ -> pure ()
