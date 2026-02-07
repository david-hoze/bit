{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Strict process output capture utilities.
--
-- This module provides safe alternatives to using lazy IO with processes.
-- All functions strictly read process output before returning, preventing
-- "delayed read on closed handle" errors.
--
-- This module intentionally does NOT expose 'System.IO.hGetContents'.
-- Use 'readProcessStrict' or 'readProcessStrictWithStderr' instead.
--
-- All operations:
-- * Use strict 'ByteString' to ensure handles are fully read before closing
-- * Read stdout and stderr concurrently to avoid deadlocks
-- * Properly clean up handles and wait for process termination
-- * Work correctly on Windows (no handle leaks or timing issues)
--
-- Import this module for process operations that need output capture:
--
-- @
-- import Bit.Process (readProcessStrict, readProcessStrictWithStderr)
-- @
module Bit.Process
  ( -- * Strict process execution
    readProcessStrict
  , readProcessStrictWithStderr
  
    -- * Re-exports (safe operations only)
  , BS.ByteString
  , ExitCode(..)
  ) where

import Prelude (FilePath, IO, String, Maybe(..), Either(..), ($), pure, return, error)
import Control.Concurrent.Async (async, wait)
import Control.Exception (bracket, try, SomeException)
import Control.Monad (void)
import System.Exit (ExitCode(..))
import System.IO (hClose)
import System.Process
  ( CreateProcess(..)
  , StdStream(..)
  , proc
  , createProcess
  , waitForProcess
  )
import qualified Data.ByteString as BS

-- ============================================================================
-- Strict process execution
-- ============================================================================

-- | Run a process and strictly capture its stdout and stderr.
-- Returns (exitCode, stdout, stderr).
--
-- Both stdout and stderr are read concurrently to avoid deadlocks when
-- the process fills either buffer. All handles are properly closed before
-- returning, even if an exception occurs.
--
-- This is the preferred way to capture process output. Unlike using
-- 'createProcess' with 'System.IO.hGetContents', this function:
--
-- * Reads all output strictly (no lazy IO)
-- * Reads stdout and stderr concurrently (no deadlock)
-- * Closes all handles before 'waitForProcess'
-- * Works correctly on Windows
--
-- Example:
--
-- @
-- (exitCode, out, err) <- readProcessStrict "git" ["status"]
-- @
readProcessStrict :: FilePath -> [String] -> IO (ExitCode, BS.ByteString, BS.ByteString)
readProcessStrict cmd args = do
  let cp = (proc cmd args)
        { std_out = CreatePipe
        , std_err = CreatePipe
        , std_in  = CreatePipe
        }
  
  bracket (createProcess cp) cleanupProcess $ \(mStdin, mStdout, _mStderr, ph) -> do
    case (mStdin, mStdout, _mStderr) of
      (Just hIn, Just hOut, Just hErr) -> do
        -- Close stdin immediately (we don't send input)
        hClose hIn
        
        -- Read stdout and stderr concurrently to avoid deadlocks
        -- Both reads are strict and will fully drain the handles
        asyncOut <- async (BS.hGetContents hOut)
        asyncErr <- async (BS.hGetContents hErr)
        
        -- Wait for both reads to complete
        -- hGetContents will close the handles when done
        out <- wait asyncOut
        err <- wait asyncErr
        
        -- Now it's safe to wait for the process
        exitCode <- waitForProcess ph
        
        return (exitCode, out, err)
      
      _ -> error "Bit.Process.readProcessStrict: failed to create pipes"
  where
    cleanupProcess (mStdin, mStdout, _mStderr, ph) = do
      -- Try to close any handles that are still open
      -- (BS.hGetContents closes the handle, but we need cleanup for stdin)
      case mStdin of
        Just h -> void (try (hClose h) :: IO (Either SomeException ()))
        Nothing -> pure ()
      
      case mStdout of
        Just h -> void (try (hClose h) :: IO (Either SomeException ()))
        Nothing -> pure ()
      
      case _mStderr of
        Just h -> void (try (hClose h) :: IO (Either SomeException ()))
        Nothing -> pure ()
      
      -- Ensure process is cleaned up
      void (try (waitForProcess ph) :: IO (Either SomeException ExitCode))

-- | Run a process, strictly capture stdout, and inherit stderr to the terminal.
-- Returns (exitCode, stdout).
--
-- This is useful when you want to capture the command's output but still see
-- progress or error messages in real-time on the terminal (e.g., git clone,
-- rclone copy with --progress).
--
-- The stdout is read strictly before waiting for the process to complete,
-- ensuring no "delayed read on closed handle" errors.
--
-- Example:
--
-- @
-- -- Capture git output while showing progress on terminal
-- (exitCode, out) <- readProcessStrictWithStderr "git" ["clone", "--progress", url]
-- @
readProcessStrictWithStderr :: FilePath -> [String] -> IO (ExitCode, BS.ByteString)
readProcessStrictWithStderr cmd args = do
  let cp = (proc cmd args)
        { std_out = CreatePipe
        , std_err = Inherit  -- stderr goes to terminal
        , std_in  = CreatePipe
        }
  
  bracket (createProcess cp) cleanupProcess $ \(mStdin, mStdout, _mStderr, ph) -> do
    case (mStdin, mStdout) of
      (Just hIn, Just hOut) -> do
        -- Close stdin immediately (we don't send input)
        hClose hIn
        
        -- Read stdout strictly
        -- We don't need async here since stderr is inherited (not a pipe)
        out <- BS.hGetContents hOut
        -- hGetContents closes the handle
        
        -- Now it's safe to wait for the process
        exitCode <- waitForProcess ph
        
        return (exitCode, out)
      
      _ -> error "Bit.Process.readProcessStrictWithStderr: failed to create pipes"
  where
    cleanupProcess (mStdin, mStdout, _mStderr, ph) = do
      -- Try to close any handles that are still open
      case mStdin of
        Just h -> void (try (hClose h) :: IO (Either SomeException ()))
        Nothing -> pure ()
      
      case mStdout of
        Just h -> void (try (hClose h) :: IO (Either SomeException ()))
        Nothing -> pure ()
      
      -- Ensure process is cleaned up
      void (try (waitForProcess ph) :: IO (Either SomeException ExitCode))
