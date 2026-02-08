{-# LANGUAGE BangPatterns #-}

-- | Concurrency utilities for bounded parallel execution.
--
-- This module provides helpers for running IO-bound operations in parallel
-- with bounded concurrency to avoid backpressure (too many concurrent IO
-- operations overwhelming the OS).
--
-- The concurrency levels are based on the number of available CPU cores
-- (from 'getNumCapabilities') and scaled appropriately for different workload
-- types (file IO, network IO, etc.).
module Bit.Concurrency
  ( -- * Concurrency level calculation
    ioConcurrency
  , networkConcurrency
  
    -- * Concurrency control
  , Concurrency(..)
  , runConcurrently
  , runConcurrentlyBounded
  
    -- * Re-exports from Scan.hs for compatibility
  , mapConcurrentlyBounded
  ) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.QSem (newQSem, waitQSem, signalQSem)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (bracket_)

-- | Concurrency mode for running operations.
data Concurrency
  = Parallel Int  -- ^ Run up to N operations in parallel
  | Sequential    -- ^ Run operations one at a time (for debugging or benchmarking)
  deriving (Show, Eq)

-- | Standard concurrency level for IO-bound file operations.
-- Scales with available cores, bounded to avoid backpressure.
--
-- For IO-bound work (file reads, hashing), threads spend most time waiting
-- on IO, so we use a multiplier of 4× cores. This provides good parallelism
-- without overwhelming the disk subsystem.
--
-- Minimum of 4 ensures reasonable performance even on single-core systems.
ioConcurrency :: IO Int
ioConcurrency = do
    caps <- getNumCapabilities
    pure (max 4 (caps * 4))

-- | Lower concurrency for network/subprocess operations.
--
-- Network and subprocess operations have additional constraints:
-- - Each subprocess has OS overhead
-- - Cloud APIs may have rate limits
-- - Network bandwidth is shared
--
-- We use a lower multiplier (2× cores) and cap at 8 to avoid overwhelming
-- remote services or the network stack.
networkConcurrency :: IO Int
networkConcurrency = do
    caps <- getNumCapabilities
    pure (min 8 (max 2 (caps * 2)))

-- | Run an action over a list with the specified concurrency level.
--
-- When 'Sequential', operations run one at a time with 'mapM'.
-- When 'Parallel n', operations run with bounded parallelism using 'mapConcurrentlyBounded'.
runConcurrently :: Concurrency -> (a -> IO b) -> [a] -> IO [b]
runConcurrently Sequential f xs = mapM f xs
runConcurrently (Parallel n) f xs = mapConcurrentlyBounded n f xs

-- | Like 'runConcurrently' but takes the bound as a direct Int parameter.
-- Useful when you already have a computed concurrency level.
runConcurrentlyBounded :: Int -> (a -> IO b) -> [a] -> IO [b]
runConcurrentlyBounded = mapConcurrentlyBounded

-- | Bounded parallel map: runs up to @bound@ actions concurrently.
--
-- Uses a 'QSem' (quantity semaphore) to limit the number of concurrent
-- operations. Each action acquires the semaphore before running and
-- releases it after completion (even on exception).
--
-- This is the same implementation used in Bit.Scan and is re-exported
-- here for consistency.
mapConcurrentlyBounded :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyBounded bound f xs = do
    sem <- newQSem bound
    mapConcurrently (\x -> bracket_ (waitQSem sem) (signalQSem sem) (f x)) xs
