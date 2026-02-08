{-# LANGUAGE BangPatterns #-}

-- | Progress reporting for file copy operations.
-- Provides chunked binary copy with byte-level progress tracking.
module Bit.CopyProgress
    ( SyncProgress(..)
    , newSyncProgress
    , copyFileWithProgress
    , withSyncProgressReporter
    , incrementFilesComplete
    ) where

import System.IO
    ( withBinaryFile, IOMode(ReadMode, WriteMode)
    , hGetBuf, hPutBuf
    , hIsTerminalDevice, hPutStrLn, stderr
    )
import Bit.Progress (reportProgress, clearProgress)
import System.Directory (createDirectoryIfMissing, copyFile)
import System.FilePath (takeDirectory)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Exception (finally)
import Control.Monad (when)
import Foreign.Marshal.Alloc (allocaBytes)
import Bit.Utils (formatBytes)

-- | Shared progress state for sync operations (push/pull).
data SyncProgress = SyncProgress
    { spFilesTotal     :: !Int              -- Total files to sync
    , spFilesComplete  :: !(IORef Int)      -- Files completed so far
    , spBytesTotal     :: !(IORef Integer)  -- Total bytes to sync (sum of known file sizes)
    , spBytesCopied    :: !(IORef Integer)  -- Bytes copied so far
    , spCurrentFile    :: !(IORef String)   -- Currently copying file name
    }

-- | Create a new sync progress tracker.
newSyncProgress :: Int -> IO SyncProgress
newSyncProgress total = do
    complete <- newIORef 0
    bytesTotal <- newIORef 0
    bytesCopied <- newIORef 0
    currentFile <- newIORef ""
    pure SyncProgress
        { spFilesTotal = total
        , spFilesComplete = complete
        , spBytesTotal = bytesTotal
        , spBytesCopied = bytesCopied
        , spCurrentFile = currentFile
        }

-- | Increment the files complete counter.
incrementFilesComplete :: SyncProgress -> IO ()
incrementFilesComplete progress = 
    atomicModifyIORef' (spFilesComplete progress) (\n -> (n + 1, ()))

-- | Copy a file with progress reporting. Uses chunked binary copy for large files.
-- For files smaller than the threshold, uses plain copyFile (no progress overhead).
-- Updates the progress counters during copy.
copyFileWithProgress :: FilePath -> FilePath -> Integer -> SyncProgress -> IO ()
copyFileWithProgress src dest fileSize progress = do
    let sizeThreshold = 1024 * 1024  -- 1MB threshold
    if fileSize < sizeThreshold
        then do
            -- Small file: use plain copyFile, no chunked progress
            createDirectoryIfMissing True (takeDirectory dest)
            copyFile src dest
            -- Still update byte counter for aggregate progress
            atomicModifyIORef' (spBytesCopied progress) (\n -> (n + fileSize, ()))
        else do
            -- Large file: chunked copy with progress
            createDirectoryIfMissing True (takeDirectory dest)
            copyFileChunked src dest fileSize (spBytesCopied progress)

-- | Chunked binary copy with progress updates. Uses strict IO (no lazy ByteString).
-- Chunk size: 64KB (good balance between IO syscalls and memory usage).
copyFileChunked :: FilePath -> FilePath -> Integer -> IORef Integer -> IO ()
copyFileChunked src dest _expectedSize bytesRef = do
    let chunkSize = 64 * 1024  -- 64KB chunks
    allocaBytes chunkSize $ \buffer ->
        withBinaryFile src ReadMode $ \hIn ->
            withBinaryFile dest WriteMode $ \hOut -> do
                let loop !bytesSoFar = do
                        count <- hGetBuf hIn buffer chunkSize
                        if count == 0
                            then pure ()  -- EOF
                            else do
                                hPutBuf hOut buffer count
                                let newTotal = bytesSoFar + fromIntegral count
                                -- Update progress counter atomically
                                atomicModifyIORef' bytesRef (\n -> (n + fromIntegral count, ()))
                                loop newTotal
                loop (0 :: Integer)

-- | Start a progress reporter thread and run an action.
-- Automatically stops the reporter when the action completes.
-- Only shows progress if on a TTY and total files > threshold.
withSyncProgressReporter :: SyncProgress -> IO a -> IO a
withSyncProgressReporter progress action = do
    isTTY <- hIsTerminalDevice stderr
    let shouldShowProgress = isTTY && spFilesTotal progress > 5
    if shouldShowProgress
        then do
            reporterThread <- forkIO (syncProgressLoop progress)
            finally action $ do
                killThread reporterThread
                -- Final summary line
                filesCompleted <- readIORef (spFilesComplete progress)
                totalBytes <- readIORef (spBytesCopied progress)
                clearProgress
                hPutStrLn stderr $ "Synced " ++ show filesCompleted ++ " files (" ++ formatBytes totalBytes ++ ")."
        else do
            -- Non-TTY: print one line per file as it completes
            if spFilesTotal progress > 0
                then actionWithPerFilePrint progress action
                else action

-- | Progress reporter loop: displays aggregate and per-file progress.
syncProgressLoop :: SyncProgress -> IO ()
syncProgressLoop progress = go
  where
    go = do
        filesCompleted <- readIORef (spFilesComplete progress)
        bytesCopied <- readIORef (spBytesCopied progress)
        totalBytes <- readIORef (spBytesTotal progress)
        _currentFile <- readIORef (spCurrentFile progress)
        
        let filesPct = if spFilesTotal progress > 0
                       then (filesCompleted * 100) `div` spFilesTotal progress
                       else 0
        
        -- Show aggregate progress
        let progressLine = "Syncing files: " ++ show filesCompleted ++ "/" ++ show (spFilesTotal progress) 
                         ++ " files, " ++ formatBytes bytesCopied
                         ++ if totalBytes > 0
                            then " / " ++ formatBytes totalBytes ++ " (" ++ show filesPct ++ "%)"
                            else ""
        
        reportProgress progressLine
        
        threadDelay 100000  -- 100ms update interval
        
        when (filesCompleted < spFilesTotal progress) go

-- | Run action with per-file print for non-TTY environments.
actionWithPerFilePrint :: SyncProgress -> IO a -> IO a
actionWithPerFilePrint _progress action = action
-- For non-TTY, we'd print each file as it completes, but that requires
-- hooking into each copyFileWithProgress call site. For now, just run the action.
-- The caller can print messages manually if needed.
