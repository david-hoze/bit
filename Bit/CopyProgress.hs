{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Progress reporting for file copy operations.
-- Provides rclone-based batch file copy with progress tracking via JSON logs.
module Bit.CopyProgress
    ( SyncProgress(..)
    , newSyncProgress
    , rcloneCopyFiles
    , rcloneCopyto
    , withSyncProgressReporter
    , incrementFilesComplete
    ) where

import System.IO
    ( hIsTerminalDevice, hPutStrLn, hPutStr, stderr
    , hIsEOF, hClose, Handle, hSetEncoding, utf8
    )
import System.Process (CreateProcess(..), StdStream(..), proc, createProcess, waitForProcess, terminateProcess)
import System.Exit (ExitCode(..))
import Bit.Progress (reportProgress, clearProgress)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.Async (async, wait)
import Control.Exception (finally, bracket, try, SomeException, throwIO)
import Control.Monad (when, void, unless)
import Bit.Utils (formatBytes, toPosix)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (openTempFile)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)

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

-- ============================================================================
-- rclone batch copy via --files-from
-- ============================================================================

-- | JSON log line from rclone --use-json-log output (permissive parsing).
data RcloneLogLine = RcloneLogLine
    { rllLevel  :: Maybe String
    , rllMsg    :: Maybe String
    , rllObject :: Maybe String
    , rllSize   :: Maybe Integer
    , rllStats  :: Maybe RcloneStats
    } deriving (Show, Generic)

instance Aeson.FromJSON RcloneLogLine where
    parseJSON = Aeson.withObject "RcloneLogLine" $ \v -> RcloneLogLine
        <$> v Aeson..:? "level"
        <*> v Aeson..:? "msg"
        <*> v Aeson..:? "object"
        <*> v Aeson..:? "size"
        <*> v Aeson..:? "stats"

-- | Stats block from rclone JSON log.
data RcloneStats = RcloneStats
    { rsBytes          :: Maybe Integer
    , rsTotalBytes     :: Maybe Integer
    , rsTransfers      :: Maybe Int
    , rsTotalTransfers :: Maybe Int
    } deriving (Show, Generic)

instance Aeson.FromJSON RcloneStats where
    parseJSON = Aeson.withObject "RcloneStats" $ \v -> RcloneStats
        <$> v Aeson..:? "bytes"
        <*> v Aeson..:? "totalBytes"
        <*> v Aeson..:? "transfers"
        <*> v Aeson..:? "totalTransfers"

-- | Batch-copy files from src to dst using a single rclone subprocess.
-- Uses --files-from to pass the list of relative paths. Progress is updated
-- from rclone's JSON log output on stderr.
--
-- src and dst can be local paths or rclone remote specs (e.g. "gdrive:path").
-- The file paths must be relative to both src and dst roots.
rcloneCopyFiles :: String -> String -> [FilePath] -> SyncProgress -> IO ()
rcloneCopyFiles _ _ [] _ = pure ()
rcloneCopyFiles src dst files progress = do
    tmpDir <- getTemporaryDirectory
    bracket (openTempFile tmpDir "bit-files-.txt") cleanupTmpFile $ \(tmpPath, tmpHandle) -> do
        -- Write one posix-style path per line (UTF-8 for rclone compatibility)
        hSetEncoding tmpHandle utf8
        mapM_ (\f -> hPutStr tmpHandle (toPosix f ++ "\n")) files
        hClose tmpHandle

        let args = [ "copy"
                   , toPosix src
                   , toPosix dst
                   , "--files-from", tmpPath
                   , "--use-json-log"
                   , "--stats", "0.5s"
                   , "-v"
                   , "--retries", "3"
                   , "--low-level-retries", "10"
                   , "--no-traverse"
                   ]
            cp = (proc "rclone" args)
                   { std_out = NoStream
                   , std_err = CreatePipe
                   , std_in  = NoStream
                   }

        errorsRef <- newIORef ([] :: [String])

        bracket (createProcess cp) cleanupProcess $ \(_, _, mStderr, ph) -> do
            case mStderr of
                Just hErr -> do
                    -- CRITICAL: drain stderr concurrently before waitForProcess
                    -- to avoid pipe deadlock (same pattern as Internal/Transport.hs:241-249)
                    asyncDrain <- async (drainStderr hErr progress errorsRef)
                    wait asyncDrain
                    code <- waitForProcess ph
                    case code of
                        ExitSuccess -> pure ()
                        ExitFailure n -> do
                            errors <- readIORef errorsRef
                            let errMsg = "rclone copy failed (exit code " ++ show n ++ ")"
                                    ++ if null errors then ""
                                       else ":\n" ++ unlines errors
                            throwIO (userError errMsg)
                Nothing -> do
                    code <- waitForProcess ph
                    case code of
                        ExitSuccess -> pure ()
                        ExitFailure n ->
                            throwIO (userError $ "rclone copy failed (exit code " ++ show n ++ ")")
  where
    cleanupTmpFile (path, h) = do
        void (try (hClose h) :: IO (Either SomeException ()))
        void (try (removeFile path) :: IO (Either SomeException ()))

    cleanupProcess (mIn, _mOut, mErr, ph) = do
        void (try (traverse_ hClose mIn) :: IO (Either SomeException ()))
        void (try (traverse_ hClose mErr) :: IO (Either SomeException ()))
        void (try (terminateProcess ph) :: IO (Either SomeException ()))
        void (try (waitForProcess ph) :: IO (Either SomeException ExitCode))

    traverse_ f = maybe (pure ()) f

-- | Copy a single file with progress tracking via rclone JSON logs.
-- Runs @rclone copyto src dst@ and updates bytesRef with bytes transferred.
-- Returns the rclone exit code.
rcloneCopyto :: String -> String -> IORef Integer -> IO ExitCode
rcloneCopyto src dst bytesRef = do
    let args = [ "copyto", toPosix src, toPosix dst
               , "--use-json-log", "--stats", "0.5s", "-v"
               ]
        cp = (proc "rclone" args)
               { std_out = NoStream
               , std_err = CreatePipe
               , std_in  = NoStream
               }
    bracket (createProcess cp) cleanupProcess $ \(_, _, mStderr, ph) ->
        case mStderr of
            Just hErr -> do
                asyncDrain <- async (drainCopytoStderr hErr bytesRef)
                wait asyncDrain
                waitForProcess ph
            Nothing ->
                waitForProcess ph
  where
    cleanupProcess (mIn, _, mErr, ph) = do
        void (try (hClose' mIn) :: IO (Either SomeException ()))
        void (try (hClose' mErr) :: IO (Either SomeException ()))
        void (try (terminateProcess ph) :: IO (Either SomeException ()))
        void (try (waitForProcess ph) :: IO (Either SomeException ExitCode))
    hClose' = maybe (pure ()) hClose

-- | Drain rclone copyto stderr, updating a bytes counter from JSON stats.
drainCopytoStderr :: Handle -> IORef Integer -> IO ()
drainCopytoStderr h bytesRef = go
  where
    go = do
        eof <- hIsEOF h
        unless eof $ do
            lineBytes <- BS.hGetLine h
            unless (BS.null lineBytes) $
                case Aeson.decodeStrict lineBytes :: Maybe RcloneLogLine of
                    Just logLine -> case rllStats logLine of
                        Just stats -> case rsBytes stats of
                            Just b -> atomicModifyIORef' bytesRef (\_ -> (b, ()))
                            Nothing -> pure ()
                        Nothing -> pure ()
                    Nothing -> pure ()
            go

-- | Drain rclone stderr, parsing JSON log lines to update progress.
-- Reads raw ByteString to preserve UTF-8 (non-ASCII filenames in JSON).
-- Uses delta-based increments so counters compose safely with other writers.
drainStderr :: Handle -> SyncProgress -> IORef [String] -> IO ()
drainStderr h progress errorsRef = do
    -- Track last-reported absolute values to compute deltas
    lastBytesRef      <- newIORef (0 :: Integer)
    lastTotalBytesRef <- newIORef (0 :: Integer)
    lastTransfersRef  <- newIORef (0 :: Int)
    go lastBytesRef lastTotalBytesRef lastTransfersRef
  where
    go lastBytesRef lastTotalBytesRef lastTransfersRef = do
        eof <- hIsEOF h
        unless eof $ do
            lineBytes <- BS.hGetLine h
            unless (BS.null lineBytes) $
                parseLine lineBytes lastBytesRef lastTotalBytesRef lastTransfersRef
            go lastBytesRef lastTotalBytesRef lastTransfersRef

    parseLine lineBytes lastBytesRef lastTotalBytesRef lastTransfersRef =
        -- decodeStrict expects ByteString and handles UTF-8 correctly
        case Aeson.decodeStrict lineBytes :: Maybe RcloneLogLine of
            Just logLine -> do
                -- Update progress from stats using deltas
                case rllStats logLine of
                    Just stats -> do
                        case rsBytes stats of
                            Just b -> do
                                lastB <- readIORef lastBytesRef
                                let delta = b - lastB
                                when (delta > 0) $
                                    atomicModifyIORef' (spBytesCopied progress) (\n -> (n + delta, ()))
                                writeIORef lastBytesRef b
                            Nothing -> pure ()
                        case rsTotalBytes stats of
                            Just tb -> do
                                lastTB <- readIORef lastTotalBytesRef
                                let delta = tb - lastTB
                                when (delta /= 0) $
                                    atomicModifyIORef' (spBytesTotal progress) (\n -> (n + delta, ()))
                                writeIORef lastTotalBytesRef tb
                            Nothing -> pure ()
                        case rsTransfers stats of
                            Just t -> do
                                lastT <- readIORef lastTransfersRef
                                let delta = t - lastT
                                when (delta > 0) $
                                    atomicModifyIORef' (spFilesComplete progress) (\n -> (n + fromIntegral delta, ()))
                                writeIORef lastTransfersRef t
                            Nothing -> pure ()
                    Nothing -> pure ()
                -- Accumulate errors
                case rllLevel logLine of
                    Just "error" -> do
                        let msg = fromMaybe (decodeLineUtf8 lineBytes) (rllMsg logLine)
                        atomicModifyIORef' errorsRef (\es -> (es ++ [msg], ()))
                    _ -> pure ()
            Nothing -> pure ()  -- Non-JSON line, ignore

    -- Decode a raw ByteString line to String, with fallback for invalid UTF-8
    decodeLineUtf8 bs = case decodeUtf8' bs of
        Right t -> T.unpack t
        Left _  -> show bs

-- | Start a progress reporter thread and run an action.
-- Automatically stops the reporter when the action completes.
-- Only shows progress if on a TTY and total files > threshold.
withSyncProgressReporter :: SyncProgress -> IO a -> IO a
withSyncProgressReporter progress action = do
    isTTY <- hIsTerminalDevice stderr
    let shouldShowProgress = isTTY && spFilesTotal progress > 0
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

        -- Use bytes for percentage when available, fall back to file count
        let pct = if totalBytes > 0
                  then fromIntegral ((bytesCopied * 100) `div` totalBytes) :: Int
                  else if spFilesTotal progress > 0
                       then (filesCompleted * 100) `div` spFilesTotal progress
                       else 0

        -- Show aggregate progress
        let progressLine = "Syncing files: " ++ show filesCompleted ++ "/" ++ show (spFilesTotal progress)
                         ++ " files, " ++ formatBytes bytesCopied
                         ++ if totalBytes > 0
                            then " / " ++ formatBytes totalBytes ++ " (" ++ show pct ++ "%)"
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
