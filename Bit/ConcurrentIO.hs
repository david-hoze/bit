{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Type-safe concurrent IO operations.
--
-- This module provides a 'ConcurrentIO' newtype that restricts IO to
-- strict, concurrent-safe operations. The constructor is NOT exported,
-- preventing smuggling of arbitrary lazy IO via 'liftIO'.
--
-- All file operations use strict 'ByteString' to ensure file handles
-- are closed before returning, eliminating Windows "file is locked" errors.
--
-- Usage:
--
-- @
-- import Bit.ConcurrentIO
--
-- scanFiles :: [FilePath] -> ConcurrentIO [FileHash]
-- scanFiles paths = do
--   sem <- newQSemC 8
--   mapConcurrentlyBoundedC sem hashFile paths
--
-- hashFile :: FilePath -> ConcurrentIO FileHash
-- hashFile path = do
--   contents <- readFileStrict path
--   pure $ FileHash path (sha256 contents)
-- @
module Bit.ConcurrentIO
  ( -- * The ConcurrentIO monad
    ConcurrentIO  -- Type exported, constructor hidden
  , runConcurrentIO
  
    -- * Strict ByteString file operations
  , readFileStrict
  , writeFileStrict
  , readFileMaybeC
  
    -- * Concurrency primitives
  , mapConcurrentlyBoundedC
  , newQSemC
  , forkIOC
  , threadDelayC
  
    -- * IORef operations (strict)
  , newIORefC
  , readIORefC
  , atomicModifyIORefC'
  
    -- * Exception handling
  , tryC
  , catchC
  , bracketC
  , finallyC
  ) where

import qualified Data.ByteString as BS
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem (QSem, newQSem, waitQSem, signalQSem)
import Control.Exception (Exception, SomeException, try, catch, bracket, bracket_, finally)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')

-- | A restricted IO monad that only permits strict, concurrent-safe operations.
--
-- The constructor 'UnsafeConcurrentIO' is NOT exported. This prevents users
-- from lifting arbitrary lazy IO operations into 'ConcurrentIO'.
--
-- To add new operations to 'ConcurrentIO', add them to this module with
-- explicit wrappers around the safe IO functions.
newtype ConcurrentIO a = UnsafeConcurrentIO { runConcurrentIO :: IO a }
  deriving (Functor, Applicative, Monad)
  -- NOTE: No MonadIO instance! This is intentional.
  -- Deriving MonadIO would allow 'liftIO' to smuggle arbitrary lazy IO.

-- ============================================================================
-- Strict ByteString file operations
-- ============================================================================

-- | Read entire file strictly into memory as 'ByteString'.
-- File handle is closed before returning.
readFileStrict :: FilePath -> ConcurrentIO BS.ByteString
readFileStrict = UnsafeConcurrentIO . BS.readFile

-- | Write 'ByteString' to file strictly.
-- File handle is closed before returning.
writeFileStrict :: FilePath -> BS.ByteString -> ConcurrentIO ()
writeFileStrict path = UnsafeConcurrentIO . BS.writeFile path

-- | Read file, returning 'Nothing' on any error.
-- Useful for "check if exists and read" patterns.
readFileMaybeC :: FilePath -> ConcurrentIO (Maybe BS.ByteString)
readFileMaybeC path = UnsafeConcurrentIO $
  either (const Nothing) Just <$> (try (BS.readFile path) :: IO (Either SomeException BS.ByteString))

-- ============================================================================
-- Concurrency primitives
-- ============================================================================

-- | Create a new quantity semaphore for bounding parallelism.
newQSemC :: Int -> ConcurrentIO QSem
newQSemC = UnsafeConcurrentIO . newQSem

-- | Bounded parallel map: runs up to the semaphore's bound concurrently.
--
-- Each action acquires the semaphore before running and releases after.
-- Uses 'mapConcurrently' from the async package internally.
mapConcurrentlyBoundedC 
  :: Traversable t 
  => QSem 
  -> (a -> ConcurrentIO b) 
  -> t a 
  -> ConcurrentIO (t b)
mapConcurrentlyBoundedC sem f xs = UnsafeConcurrentIO $
  mapConcurrently (withSem . runConcurrentIO . f) xs
  where
    withSem action = bracket_ (waitQSem sem) (signalQSem sem) action

-- | Fork a new thread.
forkIOC :: ConcurrentIO () -> ConcurrentIO ThreadId
forkIOC action = UnsafeConcurrentIO $ forkIO (runConcurrentIO action)

-- | Delay for the specified number of microseconds.
threadDelayC :: Int -> ConcurrentIO ()
threadDelayC = UnsafeConcurrentIO . threadDelay

-- ============================================================================
-- IORef operations (strict)
-- ============================================================================

-- | Create a new 'IORef'.
newIORefC :: a -> ConcurrentIO (IORef a)
newIORefC = UnsafeConcurrentIO . newIORef

-- | Read an 'IORef'.
readIORefC :: IORef a -> ConcurrentIO a
readIORefC = UnsafeConcurrentIO . readIORef

-- | Strictly modify an 'IORef'. The function is applied strictly.
atomicModifyIORefC' :: IORef a -> (a -> (a, b)) -> ConcurrentIO b
atomicModifyIORefC' ref f = UnsafeConcurrentIO $ atomicModifyIORef' ref f

-- ============================================================================
-- Exception handling
-- ============================================================================

-- | Try an action, catching exceptions.
tryC :: Exception e => ConcurrentIO a -> ConcurrentIO (Either e a)
tryC action = UnsafeConcurrentIO $ try (runConcurrentIO action)

-- | Catch exceptions from an action.
catchC :: Exception e => ConcurrentIO a -> (e -> ConcurrentIO a) -> ConcurrentIO a
catchC action handler = UnsafeConcurrentIO $
  runConcurrentIO action `catch` (runConcurrentIO . handler)

-- | Bracket pattern for resource management.
bracketC :: ConcurrentIO a -> (a -> ConcurrentIO b) -> (a -> ConcurrentIO c) -> ConcurrentIO c
bracketC acquire release use = UnsafeConcurrentIO $
  bracket (runConcurrentIO acquire) (runConcurrentIO . release) (runConcurrentIO . use)

-- | Run cleanup action regardless of success or failure.
finallyC :: ConcurrentIO a -> ConcurrentIO b -> ConcurrentIO a
finallyC action cleanup = UnsafeConcurrentIO $
  runConcurrentIO action `finally` runConcurrentIO cleanup
