{-# LANGUAGE ScopedTypeVariables #-}

-- | Atomic file writes with Windows retry logic.
--
-- This module provides:
-- 
-- * 'atomicWriteFile' - Atomic write using temp file + rename pattern
-- * 'DirWriteLock' - Directory-level locking for coordinating concurrent writes
-- * 'LockRegistry' - Global registry of directory locks for process-wide coordination
--
-- On Windows, atomic writes use retry logic to handle transient "permission denied"
-- errors caused by antivirus, Windows Search, or other processes holding handles.
--
-- This module has no dependencies on other Bit modules to avoid circular imports.
module Bit.IO.AtomicWrite
  ( -- * Atomic file writes
    atomicWriteFile
  , atomicWriteFileStr
  , atomicWriteFileWithLock
  
    -- * Directory locking (for coordinated concurrent writes)
  , DirWriteLock
  , newDirWriteLock
  , withDirWriteLock
  
    -- * Lock registry (process-wide lock coordination)
  , LockRegistry
  , newLockRegistry
  , withLockedDir
  ) where

import System.Directory (renameFile, removeFile)
import System.IO (openTempFile, hClose)
import System.FilePath (takeDirectory, (</>))
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import Control.Exception (bracketOnError, catch, IOException, try)
import Control.Monad (void)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar, modifyMVar)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Atomic File Writes
-- ============================================================================

-- | Write content to target atomically (temp file + rename). Spec § Atomic Operations.
-- Uses bracketOnError to clean up temp file if an exception occurs.
-- On Windows, retries the rename a few times if the target is locked.
atomicWriteFile :: FilePath -> BS.ByteString -> IO ()
atomicWriteFile target content = do
  let tempDir = takeDirectory target
  bracketOnError
    (openTempFile tempDir ".bit-tmp")
    (\(tempPath, handle) -> do
        hClose handle
        removeFile tempPath `catch` \(_ :: IOException) -> pure ())
    (\(tempPath, handle) -> do
        BS.hPut handle content
        hClose handle
        -- Small delay to ensure handle is fully released on Windows
        threadDelay 10000  -- 10ms
        renameWithRetry tempPath target 5)

-- | Retry rename up to n times with increasing delays (for Windows file locking).
-- If target file exists and is locked, try to delete it first.
renameWithRetry :: FilePath -> FilePath -> Int -> IO ()
renameWithRetry src dest 0 = do
  -- Final attempt: try to remove target first, then rename
  removeFile dest `catch` \(_ :: IOException) -> pure ()
  renameFile src dest
renameWithRetry src dest n = do
  result <- try (renameFile src dest)
  case result of
    Right () -> pure ()
    Left (_ :: IOException) -> do
      -- Try to remove the locked target file
      void (try (removeFile dest) :: IO (Either IOException ()))
      threadDelay (50000 * (6 - n))  -- 50ms, 100ms, 150ms, 200ms, 250ms
      renameWithRetry src dest (n - 1)

-- | Atomic write of a String (UTF-8).
atomicWriteFileStr :: FilePath -> String -> IO ()
atomicWriteFileStr path str = atomicWriteFile path (encodeUtf8 (pack str))

-- ============================================================================
-- Directory Write Locks
-- ============================================================================

-- | Lock for coordinating writes to files in a directory.
--
-- Combines MVar (thread-level coordination within this process) with
-- retry logic for Windows OS-level conflicts. For process-level coordination,
-- use 'LockRegistry'.
--
-- The temp-file-rename pattern has contention at the directory level when
-- creating temp files and calling 'renameFile', so we lock at the directory
-- level rather than the file level.
data DirWriteLock = DirWriteLock
  { dwlMVar    :: MVar ()     -- ^ Thread-level coordination
  , _dwlDir     :: FilePath    -- ^ Directory being protected
  , _dwlLockFile :: FilePath   -- ^ Path to .lock file (for future file locking)
  }

-- | Create a new directory write lock.
--
-- The lock file path is @dir </> ".bit-write.lock"@.
newDirWriteLock :: FilePath -> IO DirWriteLock
newDirWriteLock dir = do
  mvar <- newMVar ()
  let lockFile = dir </> ".bit-write.lock"
  pure $ DirWriteLock mvar dir lockFile

-- | Execute action while holding the directory lock.
--
-- This coordinates Haskell threads within the same process.
-- For OS-level coordination (multiple processes), the retry logic in
-- 'atomicWriteFile' handles transient conflicts.
withDirWriteLock :: DirWriteLock -> IO a -> IO a
withDirWriteLock dwl action = withMVar (dwlMVar dwl) $ \() -> action

-- | Atomically write a file using temp + rename pattern with directory locking.
--
-- Thread-safe within this process and handles Windows quirks with retry logic.
atomicWriteFileWithLock :: DirWriteLock -> FilePath -> BS.ByteString -> IO ()
atomicWriteFileWithLock dwl destPath content =
  withDirWriteLock dwl $ atomicWriteFile destPath content

-- ============================================================================
-- Lock Registry (process-wide)
-- ============================================================================

-- | Registry of directory locks for process-wide coordination.
--
-- Use this when you have multiple workers that may write to different
-- directories and need to coordinate their writes:
--
-- @
-- registry <- newLockRegistry
-- forConcurrently_ files $ \file ->
--   withLockedDir registry file $
--     atomicWriteFile file content
-- @
newtype LockRegistry = LockRegistry (MVar (Map.Map FilePath DirWriteLock))

-- | Create a new lock registry.
newLockRegistry :: IO LockRegistry
newLockRegistry = LockRegistry <$> newMVar Map.empty

-- | Execute action while holding the lock for a file's directory.
--
-- Gets or creates a lock for the directory containing the file,
-- then runs the action while holding that lock.
withLockedDir :: LockRegistry -> FilePath -> IO a -> IO a
withLockedDir (LockRegistry mvar) path action = do
  let dir = takeDirectory path
  lock <- modifyMVar mvar $ \locks ->
    case Map.lookup dir locks of
      Just lock -> pure (locks, lock)
      Nothing -> do
        lock <- newDirWriteLock dir
        pure (Map.insert dir lock locks, lock)
  withDirWriteLock lock action
