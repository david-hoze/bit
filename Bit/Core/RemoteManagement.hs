{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Bit.Core.RemoteManagement
    ( remoteAdd
    , addRemote
    , addRemoteFilesystem
    , promptDeviceName
    , remoteShow
    , remoteCheck
    , formatRemoteDisplay
    , showRemoteStatusFromBundle
    ) where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import Control.Monad (unless, void, when, forM_)
import Data.Foldable (traverse_)
import System.Exit (ExitCode(..), exitWith)
import qualified Internal.Git as Git
import qualified Internal.Transport as Transport
import qualified Bit.Device as Device
import qualified Bit.DevicePrompt as DevicePrompt
import System.IO (stderr, hPutStrLn)
import Control.Exception (try, IOException)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Data.IORef (IORef, newIORef, readIORef)
import System.IO (hIsTerminalDevice)
import Data.Maybe (fromMaybe)
import Internal.Config (bitDevicesDir, bitRemotesDir, bitDir, fetchedBundle, bundleCwdPath, fromCwdPath, BundleName(..))
import Bit.Types (BitM, BitEnv(..))
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Bit.Progress (reportProgress, clearProgress)
import Bit.Remote (remoteUrl, displayRemote, resolveRemote)
import Bit.Core.Helpers (formatPathList)
import qualified Bit.Core.Fetch as Fetch
import Bit.Utils (atomicWriteFileStr)

-- ============================================================================
-- Remote Management
-- ============================================================================

remoteAdd :: String -> String -> IO ()
remoteAdd = addRemote

addRemote :: String -> String -> IO ()
addRemote name pathOrUrl = do
    cwd <- Dir.getCurrentDirectory
    Dir.createDirectoryIfMissing True bitDevicesDir
    Dir.createDirectoryIfMissing True bitRemotesDir
    pathType <- Device.classifyRemotePath pathOrUrl
    case pathType of
        Device.CloudRemote url -> do
            Device.writeRemoteFile cwd name (Device.TargetCloud url)
            void $ Git.addRemote name url
            putStrLn $ "Remote '" ++ name ++ "' added (" ++ url ++ ")."
        Device.FilesystemPath filePath -> addRemoteFilesystem cwd name filePath

promptDeviceName :: FilePath -> FilePath -> Maybe String -> IO String
promptDeviceName cwd _volRoot mLabel =
    DevicePrompt.acquireDeviceNameAuto mLabel $ \name -> (name `elem`) <$> Device.listDeviceNames cwd

addRemoteFilesystem :: FilePath -> String -> FilePath -> IO ()
addRemoteFilesystem cwd name filePath = do
    absPath <- Dir.makeAbsolute filePath
    exists <- Dir.doesDirectoryExist absPath
    unless exists $ do
        hPutStrLn stderr ("fatal: Path does not exist or is not accessible: " ++ filePath)
        exitWith (ExitFailure 1)
    volRoot <- Device.getVolumeRoot absPath
    let relPath = Device.getRelativePath volRoot absPath
    mStoreUuid <- Device.readBitStore volRoot
    mExistingDevice <- maybe (pure Nothing) (Device.findDeviceByUuid cwd) mStoreUuid
    result <- try @IOException $ case (mStoreUuid, mExistingDevice) of
        (Just _u, Just dev) -> do
            putStrLn $ "Using existing device '" ++ dev ++ "'."
            _mInfo <- Device.readDeviceFile cwd dev
            Device.writeRemoteFile cwd name (Device.TargetDevice dev relPath)
            putStrLn $ "Remote '" ++ name ++ "' → " ++ dev ++ ":" ++ relPath
            putStrLn $ "(using existing device '" ++ dev ++ "')"
            pure ()
        (Just u, Nothing) -> do
            mLabel <- Device.getVolumeLabel volRoot
            deviceName' <- promptDeviceName cwd volRoot mLabel
            storeType' <- Device.detectStorageType volRoot
            mSerial <- case storeType' of
                Device.Physical -> Device.getHardwareSerial volRoot
                Device.Network -> pure Nothing
            Device.writeDeviceFile cwd deviceName' (Device.DeviceInfo u storeType' mSerial)
            Device.writeRemoteFile cwd name (Device.TargetDevice deviceName' relPath)
            putStrLn $ "Remote '" ++ name ++ "' → " ++ deviceName' ++ ":" ++ relPath
            putStrLn $ "Device '" ++ deviceName' ++ "' registered (" ++ (case storeType' of Device.Physical -> "physical"; Device.Network -> "network") ++ ")."
            pure ()
        (Nothing, _) -> do
            mLabel <- Device.getVolumeLabel volRoot
            deviceName' <- promptDeviceName cwd volRoot mLabel
            u <- Device.generateStoreUuid
            Device.writeBitStore volRoot u
            storeType' <- Device.detectStorageType volRoot
            mSerial <- case storeType' of
                Device.Physical -> Device.getHardwareSerial volRoot
                Device.Network -> pure Nothing
            Device.writeDeviceFile cwd deviceName' (Device.DeviceInfo u storeType' mSerial)
            Device.writeRemoteFile cwd name (Device.TargetDevice deviceName' relPath)
            putStrLn $ "Remote '" ++ name ++ "' → " ++ deviceName' ++ ":" ++ relPath
            putStrLn $ "Device '" ++ deviceName' ++ "' registered (" ++ (case storeType' of Device.Physical -> "physical"; Device.Network -> "network") ++ ")."
            pure ()
    case result of
        Right () -> pure ()
        Left _err -> do
            -- Cannot create .bit-store at volume root (e.g. permission denied on C:\)
            -- Fall back to path-based storage for local directories
            Device.writeRemoteFile cwd name (Device.TargetLocalPath absPath)
            void $ Git.addRemote name absPath
            putStrLn $ "Remote '" ++ name ++ "' added (" ++ absPath ++ ")."

-- ============================================================================
-- Remote show / check
-- ============================================================================

-- | Progress reporter loop for remote check operations
checkProgressLoop :: IORef Int -> Int -> IO ()
checkProgressLoop counter total = go
  where
    go = do
      n <- readIORef counter
      let pct = (n * 100) `div` max 1 total
      reportProgress $ "Checking files: " ++ show n ++ "/" ++ show total ++ " (" ++ show pct ++ "%)"
      threadDelay 100000  -- 100ms
      when (n < total) go

remoteShow :: Maybe String -> BitM ()
remoteShow mRemoteName = do
    cwd <- asks envCwd
    case mRemoteName of
        Nothing -> do
            let remotesDir = cwd </> bitRemotesDir
            dirExists <- liftIO $ Dir.doesDirectoryExist remotesDir
            if not dirExists
                then liftIO $ putStrLn "No remotes configured. Use 'bit remote add <name> <url>' to add one."
                else do
                    remoteNames <- liftIO $ Dir.listDirectory remotesDir
                    if null remoteNames
                        then liftIO $ putStrLn "No remotes configured. Use 'bit remote add <name> <url>' to add one."
                        else liftIO $ forM_ remoteNames $ \name -> do
                            mTarget <- Device.readRemoteFile cwd name
                            display <- formatRemoteDisplay cwd name mTarget
                            putStrLn display
        Just name -> do
            (mRemote, mTarget) <- liftIO $ (,) <$> resolveRemote cwd name <*> Device.readRemoteFile cwd name
            display <- liftIO $ case mTarget of
                Just _ -> formatRemoteDisplay cwd name mTarget
                Nothing -> pure (name ++ " → " ++ maybe "(not configured)" displayRemote mRemote)
            case mRemote of
                Nothing -> liftIO $ putStrLn "No remotes configured. Use 'bit remote add <name> <url>' to add one."
                Just remote -> do
                    liftIO $ do
                        putStrLn display
                        putStrLn ""
                    let fetchedPath = fromCwdPath (bundleCwdPath fetchedBundle)
                    hasBundle <- liftIO $ Dir.doesFileExist fetchedPath
                    if hasBundle
                        then liftIO $ showRemoteStatusFromBundle name (Just (remoteUrl remote))
                        else do
                            maybeBundlePath <- liftIO $ Fetch.fetchRemoteBundle remote
                            case maybeBundlePath of
                                Just bPath -> do
                                    outcome <- liftIO $ Fetch.saveFetchedBundle remote (Just bPath)
                                    case outcome of
                                        Fetch.FetchError err -> liftIO $ hPutStrLn stderr $ "Warning: " ++ err
                                        _ -> pure ()  -- No need to render fetch output in remote show
                                    liftIO $ showRemoteStatusFromBundle name (Just (remoteUrl remote))
                                Nothing -> liftIO $ do
                                    putStrLn $ "  Fetch URL: " ++ remoteUrl remote
                                    putStrLn $ "  Push  URL: " ++ remoteUrl remote
                                    putStrLn ""
                                    putStrLn "  HEAD branch: (unknown)"
                                    putStrLn ""
                                    putStrLn "  Local branch configured for 'bit pull':"
                                    putStrLn "    main merges with remote (unknown)"
                                    putStrLn ""
                                    putStrLn "  Local refs configured for 'bit push':"
                                    putStrLn "    main pushes to main (unknown)"

remoteCheck :: Maybe String -> BitM ()
remoteCheck mName = do
    cwd <- asks envCwd
    (mRemote, _name) <- liftIO $ do
        name <- maybe Git.getTrackedRemoteName pure mName
        mRemote <- resolveRemote cwd name
        pure (mRemote, name)
    case mRemote of
        Nothing -> liftIO $ do
            maybe
                (hPutStrLn stderr "fatal: No remote configured.")
                (\name -> hPutStrLn stderr $ "fatal: '" ++ name ++ "' does not appear to be a git remote.")
                mName
            hPutStrLn stderr "hint: Set remote with 'bit remote add <name> <url>'"
            exitWith (ExitFailure 1)
        Just remote -> do
            liftIO $ do
                putStrLn $ "Checking local against remote: " ++ displayRemote remote
                putStrLn ""

            (_, filesOutput, _) <- liftIO $ Git.runGitWithOutput ["ls-files"]
            let fileCount = length . filter (not . null) . lines $ filesOutput

            if fileCount > 5
                then liftIO $ do
                    isTTY <- hIsTerminalDevice stderr
                    counter <- newIORef (0 :: Int)
                    let shouldShowProgress = isTTY

                    reporterThread <- if shouldShowProgress
                        then do
                            putStrLn "Running remote check..."
                            Just <$> forkIO (checkProgressLoop counter fileCount)
                        else do
                            putStrLn "Running remote check..."
                            pure Nothing

                    res <- try @IOException (Transport.checkRemote cwd remote (Just counter))

                    traverse_ killThread reporterThread
                    when shouldShowProgress clearProgress

                    either (const $ do
                            hPutStrLn stderr "fatal: rclone not found. Install rclone: https://rclone.org/install/"
                            exitWith (ExitFailure 1))
                        (processCheckResult cwd) res
                else liftIO $ do
                    putStrLn "Running remote check..."
                    res <- try @IOException (Transport.checkRemote cwd remote Nothing)
                    either (const $ do
                            hPutStrLn stderr "fatal: rclone not found. Install rclone: https://rclone.org/install/"
                            exitWith (ExitFailure 1))
                        (processCheckResult cwd) res
  where
    processCheckResult cwd cr = do
        let reportPath = cwd </> bitDir </> "last-check.txt"
        Dir.createDirectoryIfMissing True (cwd </> bitDir)
        atomicWriteFileStr reportPath (Transport.checkRawOutput cr)
        let matches = Transport.checkMatches cr
            differs = Transport.checkDiffers cr
            missingDest = Transport.checkMissingDest cr
            missingSrc = Transport.checkMissingSrc cr
            errs = Transport.checkErrors cr
            nMatch = length matches
            hasDiff = not (null differs && null missingDest && null missingSrc && null errs)
        let exitCode = Transport.checkExitCode cr
        if | not hasDiff && exitCode == ExitSuccess -> do
                putStrLn $ show nMatch ++ " files match between local and remote."
                exitWith ExitSuccess
           | exitCode /= ExitSuccess && exitCode /= ExitFailure 1 -> do
                hPutStrLn stderr "fatal: Could not read from remote."
                hPutStrLn stderr ""
                hPutStrLn stderr "Please make sure you have the correct access rights"
                hPutStrLn stderr "and the remote exists."
                unless (null (Transport.checkStderr cr)) $ hPutStrLn stderr (Transport.checkStderr cr)
                exitWith (ExitFailure 1)
           | otherwise -> do
                putStrLn ""
                unless (null differs) $ mapM_ putStrLn ("  content differs:" : formatPathList differs)
                unless (null missingDest) $ mapM_ putStrLn ("  local only (not on remote):" : formatPathList missingDest)
                unless (null missingSrc) $ mapM_ putStrLn ("  remote only (not in local):" : formatPathList missingSrc)
                unless (null errs) $ mapM_ putStrLn ("  errors:" : formatPathList errs)
                let errWord = if length errs == 1 then "1 error" else show (length errs) ++ " errors"
                putStrLn $ show (length differs + length missingDest + length missingSrc) ++ " differences, "
                    ++ errWord ++ ". " ++ show nMatch ++ " files matched."
                putStrLn ""
                hPutStrLn stderr "hint: Content differences may indicate an incomplete push or pull."
                hPutStrLn stderr "hint: Run 'bit verify' and 'bit verify --remote' to check metadata consistency."
                hPutStrLn stderr "hint: Full report saved to .bit/last-check.txt"
                exitWith (ExitFailure 1)

-- | Format remote display line (e.g. "origin → black_usb:Backup (physical, connected at E:\)")
formatRemoteDisplay :: FilePath -> String -> Maybe Device.RemoteTarget -> IO String
formatRemoteDisplay cwd name = maybe (pure (name ++ " → (no target)")) $ \case
    Device.TargetLocalPath p -> pure (name ++ " → " ++ p ++ " (local path)")
    Device.TargetDevice dev devPath -> do
        res <- Device.resolveRemoteTarget cwd (Device.TargetDevice dev devPath)
        mInfo <- Device.readDeviceFile cwd dev
        let typ = maybe "unknown" (\i -> case Device.deviceType i of Device.Physical -> "physical"; Device.Network -> "network") mInfo
        case res of
            Device.Resolved mount -> pure (name ++ " → " ++ dev ++ ":" ++ devPath ++ " (" ++ typ ++ ", connected at " ++ mount ++ ")")
            Device.NotConnected _ -> pure (name ++ " → " ++ dev ++ ":" ++ devPath ++ " (" ++ typ ++ ", NOT CONNECTED)")
    Device.TargetCloud u -> pure (name ++ " → " ++ u ++ " (cloud)")

showRemoteStatusFromBundle :: String -> Maybe String -> IO ()
showRemoteStatusFromBundle name mUrl = do
    maybeLocal <- Git.getLocalHead
    let url = fromMaybe "?" mUrl
    putStrLn $ "* remote " ++ name
    putStrLn $ "  Fetch URL: " ++ url
    putStrLn $ "  Push  URL: " ++ url
    putStrLn ""
    compareHistory maybeLocal fetchedBundle

compareHistory :: Maybe String -> BundleName -> IO ()
compareHistory maybeLocal bundleName = do
    maybeRemote <- Git.getHashFromBundle bundleName
    case (maybeLocal, maybeRemote) of
        (Nothing, Just _) -> do
            putStrLn "  HEAD branch: (unknown)"
            putStrLn ""
            putStrLn "  Local branch configured for 'bit pull':"
            putStrLn "    main merges with remote (unknown)"
            putStrLn ""
            putStrLn "  Local refs configured for 'bit push':"
            putStrLn "    main pushes to main (local out of date)"

        (Just lHash, Just rHash) -> do
            putStrLn "  HEAD branch: main"
            putStrLn ""
            if lHash == rHash
                then do
                    putStrLn "  Local branch configured for 'bit pull':"
                    putStrLn "    main merges with remote main"
                    putStrLn ""
                    putStrLn "  Local refs configured for 'bit push':"
                    putStrLn "    main pushes to main (up to date)"
                else do
                    localAhead  <- Git.checkIsAhead rHash lHash
                    remoteAhead <- Git.checkIsAhead lHash rHash

                    putStrLn "  Local branch configured for 'bit pull':"
                    putStrLn "    main merges with remote main"
                    putStrLn ""
                    putStrLn "  Local refs configured for 'bit push':"
                    case (localAhead, remoteAhead) of
                        (True, False) -> putStrLn "    main pushes to main (fast-forwardable)"
                        (False, True) -> putStrLn "    main pushes to main (local out of date)"
                        (False, False) -> putStrLn "    main pushes to main (local out of date)"
                        (True, True)   -> putStrLn "    main pushes to main (up to date)"
        _ -> pure ()
