{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Progress reporting for file copy operations.
-- Provides rclone-based batch file copy with progress tracking via JSON logs.
module Bit.Rclone.Progress
    ( SyncProgress(..)
    , newSyncProgress
    , newSyncProgressWithBytes
    , rcloneCopyFiles
    , rcloneCopyFilesWithFlags
    , rcloneCopyto
    , withSyncProgressReporter
    , incrementFilesComplete
    , computeTotalBytes
    ) where

import System.IO
    ( hIsTerminalDevice, hPutStrLn, hPutStr, stderr
    , hIsEOF, hClose, Handle, hSetEncoding, utf8
    )
import System.Process (CreateProcess(..), StdStream(..), proc, createProcess, waitForProcess, terminateProcess)
import System.Exit (ExitCode(..))
import Bit.Progress.Report (reportProgress, clearProgress)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.Async (async, wait)
import Control.Exception (finally, bracket, try, SomeException, throwIO)
import Control.Monad (when, void, unless)
import Data.List (foldl')
import System.FilePath ((</>))
import Bit.Utils (formatBytes, toPosix)
import System.Directory (getTemporaryDirectory, removeFile, getFileSize)
import System.IO (openTempFile)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
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
    , spStartTime      :: !UTCTime          -- When the sync started
    }

-- | Create a new sync progress tracker.
newSyncProgress :: Int -> IO SyncProgress
newSyncProgress total = newSyncProgressWithBytes total 0

-- | Create a sync progress tracker with a known total byte count.
newSyncProgressWithBytes :: Int -> Integer -> IO SyncProgress
newSyncProgressWithBytes total totalBytes = do
    complete <- newIORef 0
    bytesTotalRef <- newIORef totalBytes
    bytesCopied <- newIORef 0
    currentFile <- newIORef ""
    startTime <- getCurrentTime
    pure SyncProgress
        { spFilesTotal = total
        , spFilesComplete = complete
        , spBytesTotal = bytesTotalRef
        , spBytesCopied = bytesCopied
        , spCurrentFile = currentFile
        , spStartTime = startTime
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
rcloneCopyFiles = rcloneCopyFilesWithFlags []

-- | Compute total bytes for a list of files relative to a source root.
-- Silently skips files that don't exist (e.g. missing CAS blobs in lite mode).
computeTotalBytes :: FilePath -> [FilePath] -> IO Integer
computeTotalBytes srcRoot files = foldl' (+) 0 <$> mapM getSize files
  where
    getSize f = do
        result <- try (getFileSize (srcRoot </> f)) :: IO (Either SomeException Integer)
        pure $ either (const 0) id result

-- | Like 'rcloneCopyFiles' but with extra rclone flags prepended to the command.
-- Use this when chunk uploads need different parallelism than whole-file copies,
-- e.g. @["--transfers", "32"]@ for many small CAS chunks.
rcloneCopyFilesWithFlags :: [String] -> String -> String -> [FilePath] -> SyncProgress -> IO ()
rcloneCopyFilesWithFlags _ _ _ [] _ = pure ()
rcloneCopyFilesWithFlags extraFlags src dst files progress = do
    -- Set total bytes upfront if not already set
    currentTotal <- readIORef (spBytesTotal progress)
    when (currentTotal == 0) $ do
        total <- computeTotalBytes src files
        writeIORef (spBytesTotal progress) total
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
                   ] ++ extraFlags
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
                        -- Update total bytes from rclone only if we don't have a precomputed total
                        precomputed <- readIORef (spBytesTotal progress)
                        when (precomputed == 0) $
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

        now <- getCurrentTime
        let elapsed = realToFrac (diffUTCTime now (spStartTime progress)) :: Double
            mbps = if elapsed > 0.5
                   then fromIntegral bytesCopied / (1024 * 1024 * elapsed)
                   else 0 :: Double
            etaSecs = if mbps > 0.01 && totalBytes > bytesCopied
                      then round (fromIntegral (totalBytes - bytesCopied) / (mbps * 1024 * 1024)) :: Int
                      else 0

        -- Use bytes for percentage when available, fall back to file count
        let pct = if totalBytes > 0
                  then fromIntegral ((bytesCopied * 100) `div` totalBytes) :: Int
                  else if spFilesTotal progress > 0
                       then (filesCompleted * 100) `div` spFilesTotal progress
                       else 0

        -- Show aggregate progress with speed and ETA
        let speedStr = if mbps > 0.01
                       then ", " ++ showSpeed mbps ++ " MB/s"
                       else ""
            etaStr = if etaSecs > 0
                     then ", " ++ formatEta etaSecs ++ " remaining"
                     else ""
            progressLine = "Syncing files: " ++ show filesCompleted ++ "/" ++ show (spFilesTotal progress)
                         ++ " files, " ++ formatBytes bytesCopied
                         ++ (if totalBytes > 0
                             then " / " ++ formatBytes totalBytes ++ " (" ++ show pct ++ "%)"
                             else "")
                         ++ speedStr ++ etaStr

        reportProgress progressLine

        threadDelay 500000  -- 500ms update interval

        when (filesCompleted < spFilesTotal progress) go

    showSpeed :: Double -> String
    showSpeed n
        | n >= 10   = show (round n :: Int)
        | otherwise = let d = round (n * 10) :: Int
                      in show (d `div` 10) ++ "." ++ show (d `mod` 10)

    formatEta :: Int -> String
    formatEta secs
        | secs < 60  = show secs ++ "s"
        | secs < 3600 = show (secs `div` 60) ++ "m " ++ show (secs `mod` 60) ++ "s"
        | otherwise   = show (secs `div` 3600) ++ "h " ++ show ((secs `mod` 3600) `div` 60) ++ "m"

-- | Run action with per-file print for non-TTY environments.
actionWithPerFilePrint :: SyncProgress -> IO a -> IO a
actionWithPerFilePrint _progress action = action
-- For non-TTY, we'd print each file as it completes, but that requires
-- hooking into each copyFileWithProgress call site. For now, just run the action.
-- The caller can print messages manually if needed.
