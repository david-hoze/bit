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
