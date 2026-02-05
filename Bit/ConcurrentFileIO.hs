{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Strict, concurrent-safe file IO operations.
--
-- This module intentionally does NOT export lazy IO functions.
-- Use 'readFileBinaryStrict' instead of 'Prelude.readFile'.
--
-- All operations:
-- * Use strict 'ByteString' to ensure file handles close immediately
-- * Are safe for concurrent access (no lazy IO handle retention)
-- * Work correctly on Windows (no "file is locked" errors)
--
-- Import this module instead of 'Prelude' for file operations:
--
-- @
-- import Bit.ConcurrentFileIO (readFileBinaryStrict, writeFileBinaryStrict)
-- @
module Bit.ConcurrentFileIO
  ( -- * Reading (strict)
    readFileBinaryStrict
  , readFileUtf8Strict
  , readFileMaybe
  , readFileUtf8Maybe
  
    -- * Writing (strict)
  , writeFileBinaryStrict
  , writeFileUtf8Strict
  
    -- * Re-exports (safe operations only)
  , BS.ByteString
  , T.Text
  ) where

import Prelude (FilePath, IO, Maybe(..), Either(..), String, pure, ($), (.))
import Control.Exception (try, SomeException, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- ============================================================================
-- Reading (strict)
-- ============================================================================

-- | Read entire file strictly into memory as 'ByteString'.
-- File handle is closed before returning.
--
-- This is the preferred way to read files in concurrent code.
-- Unlike 'Prelude.readFile', this does not use lazy IO.
readFileBinaryStrict :: MonadIO m => FilePath -> m BS.ByteString
readFileBinaryStrict = liftIO . BS.readFile

-- | Read entire file as strict UTF-8 'Text'.
-- File handle is closed before returning.
--
-- Throws 'T.UnicodeException' on invalid UTF-8.
readFileUtf8Strict :: MonadIO m => FilePath -> m T.Text
readFileUtf8Strict path = liftIO $ do
  bs <- BS.readFile path
  case T.decodeUtf8' bs of
    Left err -> throwIO err
    Right t  -> pure t

-- | Read file, returning 'Nothing' on any error.
-- Useful for "check if exists and read" patterns.
--
-- This combines file existence check and read into one atomic operation,
-- avoiding TOCTOU race conditions.
readFileMaybe :: MonadIO m => FilePath -> m (Maybe BS.ByteString)
readFileMaybe path = liftIO $ do
  result <- try (BS.readFile path)
  pure $ case result of
    Left (_ :: SomeException) -> Nothing
    Right bs -> Just bs

-- | Read file as UTF-8, returning 'Nothing' on any error (including invalid UTF-8).
readFileUtf8Maybe :: MonadIO m => FilePath -> m (Maybe T.Text)
readFileUtf8Maybe path = liftIO $ do
  result <- try (BS.readFile path)
  pure $ case result of
    Left (_ :: SomeException) -> Nothing
    Right bs -> case T.decodeUtf8' bs of
      Left _ -> Nothing
      Right t -> Just t

-- ============================================================================
-- Writing (strict)
-- ============================================================================

-- | Write 'ByteString' to file strictly.
-- File handle is closed before returning.
--
-- NOTE: This is NOT atomic. For atomic writes, use 'Bit.Utils.atomicWriteFile'.
writeFileBinaryStrict :: MonadIO m => FilePath -> BS.ByteString -> m ()
writeFileBinaryStrict path = liftIO . BS.writeFile path

-- | Write 'Text' to file as UTF-8.
-- File handle is closed before returning.
--
-- NOTE: This is NOT atomic. For atomic writes, use 'Bit.Utils.atomicWriteFile'.
writeFileUtf8Strict :: MonadIO m => FilePath -> T.Text -> m ()
writeFileUtf8Strict path = liftIO . BS.writeFile path . T.encodeUtf8
