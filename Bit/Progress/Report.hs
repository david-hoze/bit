{-# LANGUAGE BangPatterns #-}

-- | Centralized progress reporting for terminal operations.
-- Eliminates duplicated progress loops and ensures consistent output behavior.
module Bit.Progress.Report
  ( reportProgress
  , clearProgress
  , enableProgressMode
  , disableProgressMode
  , withProgressReporter
  , simpleProgressLoop
  ) where

import System.IO (hPutStr, hFlush, hIsTerminalDevice, hSetBinaryMode, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Exception (finally, try, SomeException)
import Control.Monad (when)
import System.Environment (lookupEnv)
import Data.Char (isDigit)
import System.Process (readProcess)

-- | Cached terminal width. 0 means not yet queried.
{-# NOINLINE termWidthRef #-}
termWidthRef :: IORef Int
termWidthRef = unsafePerformIO (newIORef 0)

-- | Get terminal width, caching the result. Falls back to 80 if detection fails.
getTermWidth :: IO Int
getTermWidth = do
    cached <- readIORef termWidthRef
    if cached > 0
        then pure cached
        else do
            w <- detectTermWidth
            writeIORef termWidthRef w
            pure w

-- | Detect terminal width from COLUMNS env var, tput, or stty, defaulting to 80.
detectTermWidth :: IO Int
detectTermWidth = do
    -- Try COLUMNS env var first (set by most shells including bash on MINGW64)
    mCols <- lookupEnv "COLUMNS"
    case mCols >>= parsePositiveInt of
        Just w  -> pure w
        Nothing -> do
            -- Try `tput cols` (works on MINGW64/mintty)
            tputResult <- try (readProcess "tput" ["cols"] "") :: IO (Either SomeException String)
            case tputResult >>= Right . parsePositiveInt . filter isDigit of
                Right (Just w) -> pure w
                _ -> do
                    -- Try `stty size` which returns "rows cols"
                    sttyResult <- try (readProcess "stty" ["size"] "") :: IO (Either SomeException String)
                    case sttyResult of
                        Right out -> case words out of
                            [_, cols] -> pure (maybe 80 id $ parsePositiveInt cols)
                            _         -> pure 80
                        Left _ -> pure 80
  where
    parsePositiveInt s = case reads (filter isDigit s) of
        [(n, "")] | n > 0 -> Just n
        _                  -> Nothing

-- | Write a progress line to stderr. Overwrites the current line using carriage return.
-- Truncates the message to terminal width to prevent line wrapping (which breaks \r overwriting).
reportProgress :: String -> IO ()
reportProgress msg = do
  w <- getTermWidth
  -- Reserve 1 column for safety; \r\ESC[K prefix doesn't count toward visible width
  let maxVisible = max 20 (w - 1)
      truncated = if length msg > maxVisible
                  then take (maxVisible - 3) msg ++ "..."
                  else msg
  hPutStr stderr ("\r\ESC[K" ++ truncated)
  hFlush stderr

-- | Clear the current progress line. Used in cleanup.
clearProgress :: IO ()
clearProgress = do
  hPutStr stderr "\r\ESC[K"
  hFlush stderr

-- | Set stderr to binary mode so \r is not translated to \r\n on Windows.
-- Call once before starting any progress reporting.
enableProgressMode :: IO ()
enableProgressMode = hSetBinaryMode stderr True

-- | Restore stderr to text mode after progress reporting is done.
disableProgressMode :: IO ()
disableProgressMode = hSetBinaryMode stderr False

-- | Bracket pattern for progress reporting. Handles TTY detection, thread management, and cleanup.
-- Parameters:
--   threshold: minimum count to show progress (below this, no reporter thread is spawned)
--   total: total item count
--   action: action that receives the counter IORef
-- Returns: the result of the action
withProgressReporter :: Int -> Int -> (IORef Int -> IO a) -> IO a
withProgressReporter threshold total action = do
  isTTY <- hIsTerminalDevice stderr
  counter <- newIORef (0 :: Int)
  
  let shouldShowProgress = isTTY && total > threshold
  
  if shouldShowProgress
    then do
      -- Binary mode prevents \r → \r\n translation on Windows
      enableProgressMode
      -- Fork reporter thread, run action with cleanup
      reporterThread <- forkIO (simpleProgressLoop "Processing..." counter total 100000)
      finally
        (action counter)
        (do
          killThread reporterThread
          clearProgress
          disableProgressMode
        )
    else
      -- No progress reporting, just run the action
      action counter

-- | Generic progress loop that formats and reports progress.
-- Parameters:
--   label: prefix for the progress message (e.g., "Scanning...", "Checking files:")
--   counter: IORef tracking current progress
--   total: total item count
--   interval: update interval in microseconds
simpleProgressLoop :: String -> IORef Int -> Int -> Int -> IO ()
simpleProgressLoop label counter total interval = go
  where
    go = do
      n <- readIORef counter
      let pct = (n * 100) `div` max 1 total
      reportProgress $ label ++ " " ++ show n ++ "/" ++ show total ++ " (" ++ show pct ++ "%)"
      threadDelay interval
      when (n < total) go
