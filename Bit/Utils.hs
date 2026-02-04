{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bit.Utils
  ( toPosix
  , isBitPath
  , filterOutBitPaths
  , atomicWriteFile
  , atomicWriteFileStr
  ) where

import Data.List (isPrefixOf, isInfixOf)
import Bit.Types (FileEntry(..))
import System.Directory (renameFile, removeFile)
import System.IO (openTempFile, hClose)
import System.FilePath (takeDirectory)
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import Control.Exception (bracketOnError, catch, IOException, try)
import Control.Monad (void)
import Control.Concurrent (threadDelay)

-- | Convert Windows backslashes to forward slashes (e.g. for rclone paths).
toPosix :: FilePath -> FilePath
toPosix = map (\c -> if c == '\\' then '/' else c)

-- | True if the path is or is under .bit (bit metadata, not user content).
isBitPath :: FilePath -> Bool
isBitPath p = p == ".bit" || ".bit" `isPrefixOf` p || "/.bit" `isInfixOf` p || "\\.bit" `isInfixOf` p

-- | Remove .bit paths from a list of file entries (e.g. remote file list).
filterOutBitPaths :: [FileEntry] -> [FileEntry]
filterOutBitPaths = filter (\e -> not (isBitPath e.path))

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
        removeFile tempPath `catch` \(_ :: IOException) -> return ())
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
  removeFile dest `catch` \(_ :: IOException) -> return ()
  renameFile src dest
renameWithRetry src dest n = do
  result <- try (renameFile src dest)
  case result of
    Right () -> return ()
    Left (_ :: IOException) -> do
      -- Try to remove the locked target file
      void (try (removeFile dest) :: IO (Either IOException ()))
      threadDelay (50000 * (6 - n))  -- 50ms, 100ms, 150ms, 200ms, 250ms
      renameWithRetry src dest (n - 1)

-- | Atomic write of a String (UTF-8).
atomicWriteFileStr :: FilePath -> String -> IO ()
atomicWriteFileStr path str = atomicWriteFile path (encodeUtf8 (pack str))
