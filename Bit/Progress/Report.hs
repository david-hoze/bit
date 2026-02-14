{-# LANGUAGE BangPatterns #-}

-- | Centralized progress reporting for terminal operations.
-- Eliminates duplicated progress loops and ensures consistent output behavior.
module Bit.Progress.Report
  ( reportProgress
  , clearProgress
  , withProgressReporter
  , simpleProgressLoop
  ) where

import System.IO (hPutStr, hFlush, hIsTerminalDevice, stderr)
import Data.IORef (IORef, newIORef, readIORef)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Exception (finally)
import Control.Monad (when)

-- | Write a progress line to stderr. Overwrites the current line using carriage return.
-- This is the single correct way to update a progress indicator.
reportProgress :: String -> IO ()
reportProgress msg = do
  hPutStr stderr ("\r\ESC[K" ++ msg)
  hFlush stderr

-- | Clear the current progress line. Used in cleanup.
clearProgress :: IO ()
clearProgress = do
  hPutStr stderr "\r\ESC[K"
  hFlush stderr

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
      -- Fork reporter thread, run action with cleanup
      reporterThread <- forkIO (simpleProgressLoop "Processing..." counter total 100000)
      finally
        (action counter)
        (do
          killThread reporterThread
          clearProgress
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
