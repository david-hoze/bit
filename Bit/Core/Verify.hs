{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bit.Core.Verify
    ( VerifyTarget(..)
    , verify
    , fsck
    ) where

import System.FilePath ((</>))
import Control.Monad (when)
import Data.Foldable (traverse_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Control.Exception (finally)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Data.IORef (IORef, newIORef, readIORef)
import System.IO (stderr, hIsTerminalDevice)

import Bit.Types (BitM, BitEnv(..))
import Bit.Concurrency (Concurrency)
import qualified Bit.Verify as Verify
import qualified Bit.Fsck as Fsck
import Internal.Config (fetchedBundle)
import Bit.Progress (reportProgress, clearProgress)

import Bit.Core.Helpers (withRemote, printVerifyIssue, isFilesystemRemote)
import Bit.Remote (Remote, remoteUrl, RemotePath(..))

-- | Whether to verify local working tree or remote.
data VerifyTarget = VerifyLocal | VerifyRemote
  deriving (Show, Eq)

verify :: VerifyTarget -> Concurrency -> BitM ()
verify target concurrency = case target of
  VerifyRemote -> withRemote $ \remote -> do
      cwd <- asks envCwd
      isFs <- isFilesystemRemote remote
      if isFs
        then verifyFilesystemRemote (RemotePath (remoteUrl remote)) concurrency
        else verifyCloudRemote cwd remote concurrency

  VerifyLocal -> do
      cwd <- asks envCwd
      let indexDir = cwd </> ".bit/index"
      meta <- liftIO $ Verify.loadBinaryMetadata indexDir concurrency
      let fileCount = length meta

      if fileCount > 5
        then liftIO $ do
          isTTY <- hIsTerminalDevice stderr
          counter <- newIORef (0 :: Int)
          let shouldShowProgress = isTTY

          reporterThread <- if shouldShowProgress
            then Just <$> forkIO (verifyProgressLoop counter fileCount)
            else pure Nothing

          result <- finally
            (Verify.verifyLocal cwd (Just counter) concurrency)
            (do
              traverse_ killThread reporterThread
              when shouldShowProgress clearProgress
            )

          printVerifyResult truncateHash " Run 'bit status' for details." result
        else liftIO $ do
          result <- Verify.verifyLocal cwd Nothing concurrency
          printVerifyResult truncateHash " Run 'bit status' for details." result

-- | Verify a filesystem remote by scanning its working directory.
verifyFilesystemRemote :: RemotePath -> Concurrency -> BitM ()
verifyFilesystemRemote (RemotePath remotePath) concurrency = liftIO $ do
    putStrLn "Verifying remote files..."
    result <- Verify.verifyLocalAt remotePath Nothing concurrency
    printVerifyResult truncateHash "" result

-- | Verify a cloud remote using the fetched bundle.
verifyCloudRemote :: FilePath -> Remote -> Concurrency -> BitM ()
verifyCloudRemote cwd remote concurrency = liftIO $ do
    putStrLn "Fetching remote metadata..."
    putStrLn "Scanning remote files..."

    entries <- Verify.loadMetadataFromBundle fetchedBundle
    let fileCount = length entries

    if fileCount > 5
      then do
        isTTY <- hIsTerminalDevice stderr
        counter <- newIORef (0 :: Int)
        let shouldShowProgress = isTTY

        reporterThread <- if shouldShowProgress
          then Just <$> forkIO (verifyProgressLoop counter fileCount)
          else pure Nothing

        result <- finally
          (Verify.verifyRemote cwd remote (Just counter) concurrency)
          (do
            traverse_ killThread reporterThread
            when shouldShowProgress clearProgress
          )

        printVerifyResult truncateHash "" result
      else do
        result <- Verify.verifyRemote cwd remote Nothing concurrency
        printVerifyResult truncateHash "" result

verifyProgressLoop :: IORef Int -> Int -> IO ()
verifyProgressLoop counter total = go
  where
    go = do
      n <- readIORef counter
      let pct = (n * 100) `div` max 1 total
      reportProgress $ "Checking files: " ++ show n ++ "/" ++ show total ++ " (" ++ show pct ++ "%)"
      threadDelay 100000
      when (n < total) go

-- | Truncate a hash string to 16 characters with ellipsis.
truncateHash :: String -> String
truncateHash s = take 16 s ++ if length s > 16 then "..." else ""

-- | Print VerifyResult: success message or issues plus summary.
-- hashFn: how to display hashes (e.g. truncateHash or id).
-- suffix: appended to the failure line (e.g. " Run 'bit status' for details." or "").
printVerifyResult :: (String -> String) -> String -> Verify.VerifyResult -> IO ()
printVerifyResult hashFn suffix result =
  if null result.vrIssues
    then putStrLn $ "[OK] All " ++ show result.vrCount ++ " files match metadata."
    else do
      mapM_ (printVerifyIssue hashFn) result.vrIssues
      putStrLn $ "Checked " ++ show result.vrCount ++ " files. " ++ show (length result.vrIssues) ++ " issues found." ++ suffix

fsck :: FilePath -> IO ()
fsck = Fsck.doFsck

