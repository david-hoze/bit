{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility functions for bit.
--
-- This module provides path utilities and re-exports atomic file operations
-- from 'Bit.AtomicWrite'.
module Bit.Utils
  ( -- * Path utilities
    toPosix
  , isBitPath
  , filterOutBitPaths
  
    -- * Formatting utilities
  , formatBytes
  
    -- * Atomic file writes (re-exported from Bit.AtomicWrite)
  , atomicWriteFile
  , atomicWriteFileStr
  , atomicWriteFileWithLock
  
    -- * Directory locking (re-exported from Bit.AtomicWrite)
  , DirWriteLock
  , newDirWriteLock
  , withDirWriteLock
  
    -- * Lock registry (re-exported from Bit.AtomicWrite)
  , LockRegistry
  , newLockRegistry
  , withLockedDir
  ) where

import Data.List (isPrefixOf, isInfixOf)
import Bit.Types (FileEntry(..))
import Bit.AtomicWrite
    ( atomicWriteFile
    , atomicWriteFileStr
    , atomicWriteFileWithLock
    , DirWriteLock
    , newDirWriteLock
    , withDirWriteLock
    , LockRegistry
    , newLockRegistry
    , withLockedDir
    )

-- | Convert Windows backslashes to forward slashes (e.g. for rclone paths).
toPosix :: FilePath -> FilePath
toPosix = map (\c -> if c == '\\' then '/' else c)

-- | True if the path is or is under .bit (bit metadata, not user content).
isBitPath :: FilePath -> Bool
isBitPath p = p == ".bit" || ".bit" `isPrefixOf` p || "/.bit" `isInfixOf` p || "\\.bit" `isInfixOf` p

-- | Remove .bit paths from a list of file entries (e.g. remote file list).
filterOutBitPaths :: [FileEntry] -> [FileEntry]
filterOutBitPaths = filter (\e -> not (isBitPath e.path))

-- | Format bytes in human-readable form (B, KB, MB, GB, TB).
-- Uses 1 decimal place for KB and above, 1024 base.
formatBytes :: Integer -> String
formatBytes bytes
    | bytes < 1024                  = show bytes ++ " B"
    | bytes < 1024 * 1024           = formatWith (fromIntegral bytes / 1024) ++ " KB"
    | bytes < 1024 * 1024 * 1024    = formatWith (fromIntegral bytes / (1024 * 1024)) ++ " MB"
    | bytes < 1024 * 1024 * 1024 * 1024 = formatWith (fromIntegral bytes / (1024 * 1024 * 1024)) ++ " GB"
    | otherwise                     = formatWith (fromIntegral bytes / (1024 * 1024 * 1024 * 1024)) ++ " TB"
  where
    formatWith :: Double -> String
    formatWith n = showFixed 1 n
    
    -- Show a double with exactly 1 decimal place
    showFixed :: Int -> Double -> String
    showFixed decimals n =
        let multiplier = 10 ^ decimals
            rounded = fromIntegral (round (n * multiplier) :: Integer) / multiplier
        in show rounded
