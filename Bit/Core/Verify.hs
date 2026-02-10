{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Core.Verify
    ( verify
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

import qualified Bit.Device as Device
import Bit.Core.Helpers (withRemote, printVerifyIssue, getRemoteTargetType)
import Bit.Remote (Remote, remoteName, remoteUrl)

verify :: Bool -> Concurrency -> BitM ()
verify isRemote concurrency
  | isRemote = withRemote $ \remote -> do
      cwd <- asks envCwd
      mTarget <- liftIO $ getRemoteTargetType cwd (remoteName remote)
      let isFilesystem = maybe False Device.isFilesystemTarget mTarget
      if isFilesystem
        then verifyFilesystemRemote (remoteUrl remote) concurrency
        else verifyCloudRemote cwd remote concurrency

  | otherwise = do
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

          (actualCount, issues) <- finally
            (Verify.verifyLocal cwd (Just counter) concurrency)
            (do
              traverse_ killThread reporterThread
              when shouldShowProgress clearProgress
            )

          if null issues
            then putStrLn $ "[OK] All " ++ show actualCount ++ " files match metadata."
            else do
              mapM_ (printVerifyIssue truncateHash) issues
              putStrLn $ "Checked " ++ show actualCount ++ " files. " ++ show (length issues) ++ " issues found. Run 'bit status' for details."
        else liftIO $ do
          (actualCount, issues) <- Verify.verifyLocal cwd Nothing concurrency
          if null issues
            then putStrLn $ "[OK] All " ++ show actualCount ++ " files match metadata."
            else do
              mapM_ (printVerifyIssue truncateHash) issues
              putStrLn $ "Checked " ++ show actualCount ++ " files. " ++ show (length issues) ++ " issues found. Run 'bit status' for details."

-- | Verify a filesystem remote by scanning its working directory.
verifyFilesystemRemote :: FilePath -> Concurrency -> BitM ()
verifyFilesystemRemote remotePath concurrency = liftIO $ do
    putStrLn "Verifying remote files..."
    (actualCount, issues) <- Verify.verifyLocalAt remotePath Nothing concurrency
    if null issues
      then putStrLn $ "[OK] All " ++ show actualCount ++ " files match metadata."
      else do
        mapM_ (printVerifyIssue truncateHash) issues
        putStrLn $ "Checked " ++ show actualCount ++ " files. " ++ show (length issues) ++ " issues found."

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

        (actualCount, issues) <- finally
          (Verify.verifyRemote cwd remote (Just counter) concurrency)
          (do
            traverse_ killThread reporterThread
            when shouldShowProgress clearProgress
          )

        if null issues
          then putStrLn $ "[OK] All " ++ show actualCount ++ " files match metadata."
          else do
            mapM_ (printVerifyIssue truncateHash) issues
            putStrLn $ "Checked " ++ show actualCount ++ " files. " ++ show (length issues) ++ " issues found."
      else do
        (actualCount, issues) <- Verify.verifyRemote cwd remote Nothing concurrency
        if null issues
          then putStrLn $ "[OK] All " ++ show actualCount ++ " files match metadata."
          else do
            mapM_ (printVerifyIssue truncateHash) issues
            putStrLn $ "Checked " ++ show actualCount ++ " files. " ++ show (length issues) ++ " issues found."

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

fsck :: FilePath -> IO ()
fsck = Fsck.doFsck

