{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Bit.Core.RemoteManagement
    ( remoteAdd
    , addRemote
    , addRemoteFilesystem
    , promptDeviceName
    , remoteShow
    , formatRemoteDisplay
    , showRemoteStatusFromBundle
    ) where

import qualified System.Directory as Dir
import qualified Bit.IO.Platform as Platform
import System.FilePath ((</>))
import Control.Monad (unless, when, void, forM_)
import System.Exit (ExitCode(..), exitWith)
import Bit.Git.Run (AncestorQuery(..))
import qualified Bit.Git.Run as Git
import qualified Bit.Device.Identity as Device
import qualified Bit.Device.Prompt as DevicePrompt
import Data.UUID (UUID)
import System.IO (stderr, hPutStrLn)
import Control.Exception (try, IOException)
import Data.Maybe (fromMaybe)
import Bit.Config.Paths (bundleForRemote, bundleCwdPath, fromCwdPath, BundleName(..), bundleGitRelPath, fromGitRelPath)
import Bit.Types (BitM, BitEnv(..))
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Bit.Remote (Remote, remoteUrl, displayRemote, resolveRemote)
import qualified Bit.Core.Fetch as Fetch

-- ============================================================================
-- Remote Management
-- ============================================================================

remoteAdd :: String -> String -> Bool -> IO ()
remoteAdd = addRemote

addRemote :: String -> String -> Bool -> IO ()
addRemote name pathOrUrl bare = do
    cwd <- Dir.getCurrentDirectory
    pathType <- Device.classifyRemotePath pathOrUrl
    case pathType of
        Device.CloudRemote url -> do
            let layout = if bare then Device.LayoutBare else Device.LayoutFull
            Device.writeRemoteFile cwd name Device.RemoteCloud (Just url) (Just layout)
            -- Register bundle path as named git remote (bundle may not exist yet)
            let bundleGitPath = fromGitRelPath (bundleGitRelPath (bundleForRemote name))
            void $ Git.addRemote name bundleGitPath
            putStrLn $ "Remote '" ++ name ++ "' added (" ++ url ++ ")" ++ (if bare then " (bare layout)." else ".")
        Device.FilesystemPath filePath -> do
            when bare $ do
                hPutStrLn stderr "warning: --bare is only valid for cloud remotes; ignoring."
            addRemoteFilesystem cwd name filePath

promptDeviceName :: FilePath -> FilePath -> Maybe String -> IO String
promptDeviceName cwd _volRoot mLabel =
    DevicePrompt.acquireDeviceNameAuto mLabel $ \name -> (name `elem`) <$> Device.listDeviceNames cwd

addRemoteFilesystem :: FilePath -> String -> FilePath -> IO ()
addRemoteFilesystem cwd name filePath = do
    absPath <- Dir.makeAbsolute filePath
    exists <- Platform.doesDirectoryExist absPath
    unless exists $ do
        hPutStrLn stderr ("fatal: Path does not exist or is not accessible: " ++ filePath)
        case filePath of
            ('\\':_) -> uncHint
            ('/':'/':_) -> uncHint
            _ -> pure ()
        exitWith (ExitFailure 1)
    volRoot <- Device.getVolumeRoot absPath
    isFixed <- Device.isFixedDrive volRoot
    if isFixed
        then addRemoteFixed cwd name absPath
        else addRemoteDevice cwd name absPath volRoot
  where
    uncHint = hPutStrLn stderr "hint: For UNC paths under Git Bash / MINGW, use forward slashes: //server/share/path"

-- | Add a fixed-drive filesystem remote. Path stored only in git config.
addRemoteFixed :: FilePath -> String -> FilePath -> IO ()
addRemoteFixed cwd name absPath = do
    void $ Git.addRemote name (absPath </> ".bit" </> "index")
    Device.writeRemoteFile cwd name Device.RemoteFilesystem Nothing Nothing
    putStrLn $ "Remote '" ++ name ++ "' added (" ++ absPath ++ ")."

-- | Add a removable/network device remote. Uses device UUID tracking.
addRemoteDevice :: FilePath -> String -> FilePath -> FilePath -> IO ()
addRemoteDevice cwd name absPath volRoot = do
    let relPath = Device.getRelativePath volRoot absPath
    mStoreUuid <- Device.readBitStore volRoot
    mExistingDevice <- maybe (pure Nothing) (Device.findDeviceByUuid cwd) mStoreUuid
    result <- try @IOException $ case (mStoreUuid, mExistingDevice) of
        (Just _u, Just dev) -> do
            putStrLn $ "Using existing device '" ++ dev ++ "'."
            _mInfo <- Device.readDeviceFile cwd dev
            Device.writeRemoteFile cwd name Device.RemoteDevice (Just (dev ++ ":" ++ relPath)) Nothing
            void $ Git.addRemote name (absPath </> ".bit" </> "index")
            putStrLn $ "Remote '" ++ name ++ "' → " ++ dev ++ ":" ++ relPath
            putStrLn $ "(using existing device '" ++ dev ++ "')"
            pure ()
        (Just u, Nothing) -> do
            mLabel <- Device.getVolumeLabel volRoot
            deviceName' <- promptDeviceName cwd volRoot mLabel
            registerDevice cwd name volRoot deviceName' relPath u
            void $ Git.addRemote name (absPath </> ".bit" </> "index")
        (Nothing, _) -> do
            mLabel <- Device.getVolumeLabel volRoot
            deviceName' <- promptDeviceName cwd volRoot mLabel
            u <- Device.generateStoreUuid
            Device.writeBitStore volRoot u
            registerDevice cwd name volRoot deviceName' relPath u
            void $ Git.addRemote name (absPath </> ".bit" </> "index")
    case result of
        Right () -> pure ()
        Left _err -> do
            -- Cannot create .bit-store at volume root (e.g. permission denied on C:\)
            -- Fall back to path-based storage for local directories
            Device.writeRemoteFile cwd name Device.RemoteFilesystem Nothing Nothing
            void $ Git.addRemote name (absPath </> ".bit" </> "index")
            putStrLn $ "Remote '" ++ name ++ "' added (" ++ absPath ++ ")."

-- | Detect storage type, get serial, write device + remote files, and print confirmation.
-- Shared between the "existing UUID / no device" and "no UUID" branches.
registerDevice :: FilePath -> String -> FilePath -> String -> FilePath -> UUID -> IO ()
registerDevice cwd name volRoot deviceName' relPath u = do
    storeType' <- Device.detectStorageType volRoot
    mSerial <- case storeType' of
        Device.Physical -> Device.getHardwareSerial volRoot
        Device.Network -> pure Nothing
    Device.writeDeviceFile cwd deviceName' (Device.DeviceInfo u storeType' mSerial)
    Device.writeRemoteFile cwd name Device.RemoteDevice (Just (deviceName' ++ ":" ++ relPath)) Nothing
    putStrLn $ "Remote '" ++ name ++ "' → " ++ deviceName' ++ ":" ++ relPath
    putStrLn $ "Device '" ++ deviceName' ++ "' registered (" ++ displayStorageType storeType' ++ ")."

displayStorageType :: Device.StorageType -> String
displayStorageType Device.Physical = "physical"
displayStorageType Device.Network  = "network"

-- ============================================================================
-- Remote show
-- ============================================================================

remoteShow :: Maybe String -> BitM ()
remoteShow mRemoteName = do
    cwd <- asks envCwd
    case mRemoteName of
        Nothing -> do
            bitDir <- asks envBitDir
            let remotesDir = bitDir </> "remotes"
            dirExists <- liftIO $ Dir.doesDirectoryExist remotesDir
            if not dirExists
                then liftIO $ putStrLn "No remotes configured. Use 'bit remote add <name> <url>' to add one."
                else do
                    remoteNames <- liftIO $ Dir.listDirectory remotesDir
                    if null remoteNames
                        then liftIO $ putStrLn "No remotes configured. Use 'bit remote add <name> <url>' to add one."
                        else liftIO $ forM_ remoteNames $ \rName -> do
                            mRemote' <- resolveRemote cwd rName
                            mType' <- Device.readRemoteType cwd rName
                            display <- case mRemote' of
                                Just r  -> formatRemoteDisplayByType cwd rName mType' r
                                Nothing -> do
                                    mTarget' <- Device.readRemoteFile cwd rName
                                    formatRemoteDisplay cwd rName mTarget'
                            putStrLn display
        Just name -> do
            (mRemote, mType) <- liftIO $ (,) <$> resolveRemote cwd name <*> Device.readRemoteType cwd name
            case mRemote of
                Nothing -> liftIO $ putStrLn "No remotes configured. Use 'bit remote add <name> <url>' to add one."
                Just remote -> do
                    let isFs = maybe False Device.isFilesystemType mType
                    -- Spec format for single-remote show: cloud = Remote/Type/Target/Layout; filesystem = one-line then status
                    if mType == Just Device.RemoteCloud
                        then liftIO $ do
                            layout <- Device.readRemoteLayout cwd name
                            let layoutStr = case layout of Device.LayoutBare -> "bare"; _ -> "full"
                            putStrLn $ "  Remote: " ++ name
                            putStrLn "  Type: cloud"
                            putStrLn $ "  Target: " ++ remoteUrl remote
                            putStrLn $ "  Layout: " ++ layoutStr
                            putStrLn ""
                        else liftIO $ do
                            display <- formatRemoteDisplayByType cwd name mType remote
                            putStrLn display
                            putStrLn ""
                    -- Show status: ref-based for filesystem/device, bundle-based for cloud
                    if isFs
                        then liftIO $ showRefBasedRemoteStatus name (remoteUrl remote)
                        else do
                            let fetchedPath = fromCwdPath (bundleCwdPath (bundleForRemote name))
                            hasBundle <- liftIO $ Dir.doesFileExist fetchedPath
                            if hasBundle
                                then liftIO $ showRemoteStatusFromBundle name (Just (remoteUrl remote)) (mType == Just Device.RemoteCloud)
                                else do
                                    maybeBundlePath <- liftIO $ Fetch.fetchRemoteBundle remote
                                    case maybeBundlePath of
                                        Just bPath -> do
                                            outcome <- liftIO $ Fetch.saveFetchedBundle remote (Just bPath)
                                            case outcome of
                                                Fetch.FetchError err -> liftIO $ hPutStrLn stderr $ "Warning: " ++ err
                                                _ -> pure ()
                                            liftIO $ showRemoteStatusFromBundle name (Just (remoteUrl remote)) (mType == Just Device.RemoteCloud)
                                        Nothing -> liftIO $ do
                                            when (mType /= Just Device.RemoteCloud) $
                                                printRemoteUrls (remoteUrl remote)
                                            putStrLn ""
                                            putStrLn "  HEAD branch: (unknown)"
                                            putStrLn ""
                                            putStrLn "  Local branch configured for 'bit pull':"
                                            putStrLn "    main merges with remote (unknown)"
                                            putStrLn ""
                                            putStrLn "  Local refs configured for 'bit push':"
                                            putStrLn "    main pushes to main (unknown)"


-- | Format remote display line (e.g. "origin → black_usb:Backup (physical, connected at E:\)")
formatRemoteDisplay :: FilePath -> String -> Maybe Device.RemoteTarget -> IO String
formatRemoteDisplay cwd name = maybe (pure (name ++ " → (no target)")) $ \case
    Device.TargetLocalPath p -> pure (name ++ " → " ++ p ++ " (local path)")
    Device.TargetDevice dev devPath -> do
        res <- Device.resolveRemoteTarget cwd (Device.TargetDevice dev devPath)
        mInfo <- Device.readDeviceFile cwd dev
        let typ = maybe "unknown" (displayStorageType . Device.deviceType) mInfo
        case res of
            Device.Resolved mount -> pure (name ++ " → " ++ dev ++ ":" ++ devPath ++ " (" ++ typ ++ ", connected at " ++ mount ++ ")")
            Device.NotConnected _ -> pure (name ++ " → " ++ dev ++ ":" ++ devPath ++ " (" ++ typ ++ ", NOT CONNECTED)")
    Device.TargetCloud u -> pure (name ++ " → " ++ u ++ " (cloud)")

-- | Format remote display line using RemoteType instead of RemoteTarget.
formatRemoteDisplayByType :: FilePath -> String -> Maybe Device.RemoteType -> Remote -> IO String
formatRemoteDisplayByType cwd name mType remote = case mType of
    Just Device.RemoteFilesystem -> pure (name ++ " → " ++ remoteUrl remote ++ " (filesystem)")
    Just Device.RemoteDevice -> do
        -- Read the target to get device info
        mTarget <- Device.readRemoteFile cwd name
        case mTarget of
            Just (Device.TargetDevice dev devPath) -> do
                mInfo <- Device.readDeviceFile cwd dev
                let typ = maybe "unknown" (displayStorageType . Device.deviceType) mInfo
                pure (name ++ " → " ++ dev ++ ":" ++ devPath ++ " (" ++ typ ++ ", connected at " ++ remoteUrl remote ++ ")")
            _ -> pure (name ++ " → " ++ displayRemote remote ++ " (device)")
    Just Device.RemoteCloud -> do
        layout <- Device.readRemoteLayout cwd name
        let layoutStr = case layout of Device.LayoutBare -> "bare"; _ -> "full"
        pure (name ++ " → " ++ remoteUrl remote ++ " (cloud, Layout: " ++ layoutStr ++ ")")
    Nothing -> pure (name ++ " → " ++ displayRemote remote)

-- | Print "  Fetch URL:" and "  Push  URL:" lines for remote show output.
printRemoteUrls :: String -> IO ()
printRemoteUrls url = do
    putStrLn $ "  Fetch URL: " ++ url
    putStrLn $ "  Push  URL: " ++ url

-- | Print "* remote <name>", Fetch/Push URL lines, and blank line (remote show banner).
printRemoteShowBanner :: String -> String -> IO ()
printRemoteShowBanner name url = do
    putStrLn $ "* remote " ++ name
    printRemoteUrls url
    putStrLn ""

-- | Show remote status using git tracking refs (for filesystem/device remotes).
-- No bundle needed — reads directly from refs/remotes/<name>/main.
showRefBasedRemoteStatus :: String -> String -> IO ()
showRefBasedRemoteStatus name url = do
    maybeLocal <- Git.getLocalHead
    (refCode, refOut, _) <- Git.runGitWithOutput ["rev-parse", Git.remoteTrackingRef name]
    let maybeRemote = case refCode of
            ExitSuccess -> Just (filter (/= '\n') refOut)
            _ -> Nothing
    printRemoteShowBanner name url
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
                    status <- classifyPushStatus lHash rHash
                    putStrLn "  Local branch configured for 'bit pull':"
                    putStrLn "    main merges with remote main"
                    putStrLn ""
                    putStrLn "  Local refs configured for 'bit push':"
                    case status of
                        PushRefFastForwardable -> putStrLn "    main pushes to main (fast-forwardable)"
                        PushRefLocalOutOfDate  -> putStrLn "    main pushes to main (local out of date)"
                        PushRefDiverged       -> putStrLn "    main pushes to main (diverged)"
                        PushRefUpToDate       -> putStrLn "    main pushes to main (up to date)"
        _ -> do
            putStrLn "  HEAD branch: (unknown)"
            putStrLn ""
            putStrLn "  Local branch configured for 'bit pull':"
            putStrLn "    main merges with remote (unknown)"
            putStrLn ""
            putStrLn "  Local refs configured for 'bit push':"
            putStrLn "    main pushes to main (unknown)"

-- | When skipSpecHeader is True, caller already printed spec-format header (Remote/Type/Target/Layout) for cloud.
showRemoteStatusFromBundle :: String -> Maybe String -> Bool -> IO ()
showRemoteStatusFromBundle name mUrl skipSpecHeader = do
    maybeLocal <- Git.getLocalHead
    let url = fromMaybe "?" mUrl
    unless skipSpecHeader $ printRemoteShowBanner name url
    compareHistory maybeLocal (bundleForRemote name)

-- | Status of local ref relative to remote (for 'bit remote show' push message).
data PushRefStatus
  = PushRefUpToDate        -- ^ Same commit
  | PushRefFastForwardable -- ^ Local ahead of remote
  | PushRefLocalOutOfDate  -- ^ Remote ahead of local
  | PushRefDiverged        -- ^ Both have commits the other doesn't; merge/rebase needed
  deriving (Show, Eq)

-- | Classify push status from local and remote commit hashes. Calls git internally;
-- callers never see raw boolean ancestry flags.
classifyPushStatus :: String -> String -> IO PushRefStatus
classifyPushStatus localHash remoteHash = do
  localAhead  <- Git.checkIsAhead (AncestorQuery { aqAncestor = remoteHash, aqDescendant = localHash })
  remoteAhead <- Git.checkIsAhead (AncestorQuery { aqAncestor = localHash, aqDescendant = remoteHash })
  pure $ case (localAhead, remoteAhead) of
    (True, False) -> PushRefFastForwardable
    (False, True) -> PushRefLocalOutOfDate
    (False, False) -> PushRefDiverged
    (True, True)   -> PushRefUpToDate

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
                    status <- classifyPushStatus lHash rHash

                    putStrLn "  Local branch configured for 'bit pull':"
                    putStrLn "    main merges with remote main"
                    putStrLn ""
                    putStrLn "  Local refs configured for 'bit push':"
                    case status of
                        PushRefFastForwardable -> putStrLn "    main pushes to main (fast-forwardable)"
                        PushRefLocalOutOfDate  -> putStrLn "    main pushes to main (local out of date)"
                        PushRefDiverged       -> putStrLn "    main pushes to main (diverged)"
                        PushRefUpToDate       -> putStrLn "    main pushes to main (up to date)"
        _ -> pure ()
