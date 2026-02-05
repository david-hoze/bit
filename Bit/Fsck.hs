{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Fsck
  ( doFsck
  ) where

import qualified Bit.Verify as Verify
import qualified Bit.Utils as Utils
import qualified Internal.Git as Git
import Bit.Concurrency (Concurrency(..))
import System.FilePath ((</>))
import System.Exit (ExitCode(..), exitWith)
import Control.Monad (unless, when)
import System.IO (hPutStr, hPutStrLn, hFlush, hSetBuffering, BufferMode(..), stderr, hIsTerminalDevice)
import Data.IORef (newIORef, readIORef, IORef)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Exception (finally)
import Bit.Progress (reportProgress, clearProgress)

-- | Run local-only integrity check in the spirit of git fsck: no network.
-- [1/2] Local working tree vs local metadata (rgit equivalent of checking objects).
-- [2/2] git fsck on .rgit/index/.git (metadata history integrity).
-- Prints one line per problem (git-style: "missing <path>", "hash mismatch <path>")
-- and passes through git fsck output. Exits 1 if any check finds issues.
doFsck :: FilePath -> Concurrency -> IO ()
doFsck cwd concurrency = do
  hSetBuffering stderr NoBuffering
  
  -- [1/2] Working tree vs local metadata
  -- Get file count first
  let indexDir = cwd </> ".bit/index"
  meta <- Verify.loadMetadataIndex indexDir concurrency
  let fileCount = length meta
  
  -- Run verification with progress if enough files
  (actualCount, localIssues) <- if fileCount > 5
    then do
      isTTY <- hIsTerminalDevice stderr
      counter <- newIORef (0 :: Int)
      let shouldShowProgress = isTTY
      
      -- Start progress reporter thread if in TTY
      reporterThread <- if shouldShowProgress
        then Just <$> forkIO (fsckProgressLoop counter fileCount)
        else return Nothing
      
      -- Run verification with progress
      result <- finally
        (Verify.verifyLocal cwd (Just counter) concurrency)
        (do
          -- Clean up: kill reporter thread and clear line
          maybe (return ()) killThread reporterThread
          when shouldShowProgress clearProgress
        )
      return result
    else
      -- Few files, no progress needed
      Verify.verifyLocal cwd Nothing concurrency
  
  let localOk = null localIssues
  if localOk
    then hPutStrLn stderr $ "[1/2] Checked " ++ show actualCount ++ " files. OK."
    else do
      hPutStrLn stderr $ "[1/2] Checked " ++ show actualCount ++ " files. Issues found:"
      mapM_ (printIssue Utils.toPosix) localIssues
      hFlush stderr

  -- [2/2] Metadata history (git fsck in .rgit/index/.git)
  hPutStrLn stderr "[2/2] Checking metadata history..."
  (gitCode, gitOut, gitErr) <- Git.fsck
  let gitOk = gitCode == ExitSuccess
  if gitOk
    then hPutStrLn stderr "[2/2] Metadata history OK."
    else do
      putStr gitOut
      hPutStr stderr gitErr

  when (not localOk || not gitOk) $
    exitWith (ExitFailure 1)

  where
    printIssue :: (FilePath -> FilePath) -> Verify.VerifyIssue -> IO ()
    printIssue toP = \case
      Verify.HashMismatch path _ _ _ _ ->
        hPutStrLn stderr $ "hash mismatch " ++ toP path
      Verify.Missing path ->
        hPutStrLn stderr $ "missing " ++ toP path
    
    -- Progress reporter loop for fsck operation
    fsckProgressLoop :: IORef Int -> Int -> IO ()
    fsckProgressLoop counter total = go
      where
        go = do
          n <- readIORef counter
          let pct = (n * 100) `div` max 1 total
          reportProgress $ "[1/2] Checking working tree: " ++ show n ++ "/" ++ show total ++ " (" ++ show pct ++ "%)"
          threadDelay 100000  -- 100ms
          when (n < total) go
