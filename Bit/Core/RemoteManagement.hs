{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bit.Core.RemoteManagement
    ( remoteAdd
    , addRemote
    , addRemoteFilesystem
    , promptDeviceName
    , remoteShow
    , remoteRepair
    , formatRemoteDisplay
    , showRemoteStatusFromBundle
    ) where

import qualified System.Directory as Dir
import qualified Bit.Platform as Platform
import System.FilePath ((</>), takeDirectory)
import Control.Monad (unless, void, when, forM_)
import System.Exit (ExitCode(..), exitWith)
import Internal.Git (AncestorQuery(..))
import qualified Internal.Git as Git
import qualified Internal.Transport as Transport
import qualified Bit.Device as Device
import qualified Bit.DevicePrompt as DevicePrompt
import Data.UUID (UUID)
import System.IO (stderr, hPutStrLn)
import Control.Exception (try, IOException)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Internal.Config (bitDevicesDir, bitRemotesDir, fetchedBundle, bundleCwdPath, fromCwdPath, BundleName(..), bitIndexPath, bundleGitRelPath, fromGitRelPath)
import Bit.Types (BitM, BitEnv(..), Path(..), Hash(..), HashAlgo(..), hashToText)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Bit.Remote (Remote, remoteUrl, remoteName, displayRemote, resolveRemote)
import Bit.Core.Helpers (getRemoteType, formatVerifyCounts)
import qualified Bit.Core.Fetch as Fetch
import qualified Bit.Verify as Verify
import Bit.Concurrency (Concurrency(..))
import Bit.Utils (toPosix, atomicWriteFileStr)
import Bit.Internal.Metadata (MetaContent(..), serializeMetadata)

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
            Device.writeRemoteFile cwd name Device.RemoteCloud (Just url)
            -- Register bundle path as named git remote (bundle may not exist yet)
            let bundleGitPath = fromGitRelPath (bundleGitRelPath fetchedBundle)
            void $ Git.addRemote name bundleGitPath
            putStrLn $ "Remote '" ++ name ++ "' added (" ++ url ++ ")."
        Device.FilesystemPath filePath -> addRemoteFilesystem cwd name filePath

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
    Device.writeRemoteFile cwd name Device.RemoteFilesystem Nothing
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
            Device.writeRemoteFile cwd name Device.RemoteDevice (Just (dev ++ ":" ++ relPath))
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
            Device.writeRemoteFile cwd name Device.RemoteFilesystem Nothing
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
    Device.writeRemoteFile cwd name Device.RemoteDevice (Just (deviceName' ++ ":" ++ relPath))
    putStrLn $ "Remote '" ++ name ++ "' → " ++ deviceName' ++ ":" ++ relPath
    putStrLn $ "Device '" ++ deviceName' ++ "' registered (" ++ displayStorageType storeType' ++ ")."

displayStorageType :: Device.StorageType -> String
displayStorageType Device.Physical = "physical"
displayStorageType Device.Network  = "network"

-- ============================================================================
-- Remote show / repair
-- ============================================================================

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
                    -- Display the remote line
                    display <- liftIO $ formatRemoteDisplayByType cwd name mType remote
                    liftIO $ do
                        putStrLn display
                        putStrLn ""
                    -- Show status: ref-based for filesystem/device, bundle-based for cloud
                    let isFs = maybe False Device.isFilesystemType mType
                    if isFs
                        then liftIO $ showRefBasedRemoteStatus name (remoteUrl remote)
                        else do
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
                                                _ -> pure ()
                                            liftIO $ showRemoteStatusFromBundle name (Just (remoteUrl remote))
                                        Nothing -> liftIO $ do
                                            printRemoteUrls (remoteUrl remote)
                                            putStrLn ""
                                            putStrLn "  HEAD branch: (unknown)"
                                            putStrLn ""
                                            putStrLn "  Local branch configured for 'bit pull':"
                                            putStrLn "    main merges with remote (unknown)"
                                            putStrLn ""
                                            putStrLn "  Local refs configured for 'bit push':"
                                            putStrLn "    main pushes to main (unknown)"

-- ============================================================================
-- Remote repair
-- ============================================================================

-- | Repair action: copy a file from one side to repair the other.
-- Carries the expected hash+size so metadata can be restored.
data RepairAction
    = RepairLocal  Path Path (Hash 'MD5) Integer  -- ^ Copy from remote sourcePath to fix local destPath, with expected hash+size
    | RepairRemote Path Path (Hash 'MD5) Integer  -- ^ Copy from local sourcePath to fix remote destPath, with expected hash+size
    deriving (Show)

-- | Result of executing a single repair action.
data RepairResult = Repaired Path | RepairFailed Path String
    deriving (Show)

remoteRepair :: Maybe String -> Concurrency -> BitM ()
remoteRepair mName concurrency = do
    cwd <- asks envCwd
    -- Resolve remote
    (mRemote, _resolvedName) <- liftIO $ do
        name <- maybe Git.getTrackedRemoteName pure mName
        mRemote <- resolveRemote cwd name
        pure (mRemote, name)
    case mRemote of
        Nothing -> liftIO $ do
            maybe
                (hPutStrLn stderr "fatal: No remote configured.")
                (\n -> hPutStrLn stderr $ "fatal: '" ++ n ++ "' does not appear to be a git remote.")
                mName
            hPutStrLn stderr "hint: Set remote with 'bit remote add <name> <url>'"
            exitWith (ExitFailure 1)
        Just remote -> liftIO $ do
            putStrLn $ "Repairing against remote: " ++ displayRemote remote
            putStrLn ""

            -- Determine if filesystem or cloud remote
            mType <- getRemoteType cwd (remoteName remote)
            let isFilesystem = maybe False Device.isFilesystemType mType
                remotePath = remoteUrl remote

            if isFilesystem
                then repairFilesystem cwd remote remotePath concurrency
                else repairCloud cwd remote concurrency

-- | Repair against a filesystem remote (direct file access, no rclone).
repairFilesystem :: FilePath -> Remote -> FilePath -> Concurrency -> IO ()
repairFilesystem cwd _remote remotePath concurrency = do
    -- Load committed metadata from both sides (immune to scan updates)
    let localIndexDir = cwd </> bitIndexPath
        remoteIndexDir = remotePath </> bitIndexPath
    localMeta <- Verify.loadCommittedBinaryMetadata localIndexDir
    remoteMeta <- Verify.loadCommittedBinaryMetadata remoteIndexDir

    -- Verify both sides
    putStrLn "Verifying local files..."
    localResult <- Verify.verifyLocal cwd Nothing concurrency
    putStrLn $ formatVerifyCounts localResult.vrCount (length localResult.vrIssues)

    putStrLn "Verifying remote files..."
    remoteResult <- Verify.verifyLocalAt remotePath Nothing concurrency
    putStrLn $ formatVerifyCounts remoteResult.vrCount (length remoteResult.vrIssues)

    runRepairLogic localMeta remoteMeta localResult.vrIssues remoteResult.vrIssues
        (executeFilesystemRepair cwd remotePath)

-- | Repair against a cloud remote (bundle-based, uses rclone).
repairCloud :: FilePath -> Remote -> Concurrency -> IO ()
repairCloud cwd remote concurrency = do
    -- Fetch bundle
    maybeBundlePath <- Fetch.fetchRemoteBundle remote
    case maybeBundlePath of
        Nothing -> do
            hPutStrLn stderr "fatal: Could not fetch remote bundle."
            exitWith (ExitFailure 1)
        Just bPath -> do
            outcome <- Fetch.saveFetchedBundle remote (Just bPath)
            case outcome of
                Fetch.FetchError err -> do
                    hPutStrLn stderr $ "fatal: " ++ err
                    exitWith (ExitFailure 1)
                _ -> pure ()

    -- Load committed metadata (immune to scan updates)
    let localIndexDir = cwd </> bitIndexPath
    localMeta <- Verify.loadCommittedBinaryMetadata localIndexDir
    entries <- Verify.loadMetadataFromBundle fetchedBundle
    let remoteMeta = Verify.binaryEntries entries

    -- Verify both sides
    putStrLn "Verifying local files..."
    localResult <- Verify.verifyLocal cwd Nothing concurrency
    putStrLn $ formatVerifyCounts localResult.vrCount (length localResult.vrIssues)

    putStrLn "Verifying remote files..."
    remoteResult <- Verify.verifyRemote cwd remote Nothing concurrency
    putStrLn $ formatVerifyCounts remoteResult.vrCount (length remoteResult.vrIssues)

    runRepairLogic localMeta remoteMeta localResult.vrIssues remoteResult.vrIssues
        (executeRepair cwd remote)

-- | Common repair logic shared between filesystem and cloud remotes.
runRepairLogic :: [Verify.BinaryFileMeta]    -- local metadata
              -> [Verify.BinaryFileMeta]    -- remote metadata
              -> [Verify.VerifyIssue]             -- local issues
              -> [Verify.VerifyIssue]             -- remote issues
              -> (RepairAction -> IO RepairResult) -- repair executor
              -> IO ()
runRepairLogic localMeta remoteMeta localIssues remoteIssues executeAction =
    if null localIssues && null remoteIssues
        then putStrLn "\nAll files verified. Nothing to repair."
        else do
            putStrLn ""

            -- Build sets of broken paths
            let localIssueSet = Set.fromList (map issuePath localIssues)
                remoteIssueSet = Set.fromList (map issuePath remoteIssues)

            -- Build content indexes from VERIFIED files
            let remoteVerified = buildContentIndex remoteMeta remoteIssueSet
                localVerified  = buildContentIndex localMeta localIssueSet

            -- Build metadata maps for lookup
            let localMetaMap = Map.fromList [(m.bfmPath, (m.bfmHash, m.bfmSize)) | m <- localMeta]
                remoteMetaMap = Map.fromList [(m.bfmPath, (m.bfmHash, m.bfmSize)) | m <- remoteMeta]

            -- Plan repairs: use the OTHER side's metadata as source of truth
            -- (local metadata may reflect corrupted state after a scan)
            let (localRepairs, localUnrepairable) =
                    planRepairs localIssues remoteMetaMap remoteVerified RepairLocal
                (remoteRepairs, remoteUnrepairable) =
                    planRepairs remoteIssues localMetaMap localVerified RepairRemote
                allRepairs = localRepairs ++ remoteRepairs
                allUnrepairable = localUnrepairable ++ remoteUnrepairable

            when (null allRepairs && null allUnrepairable) $
                putStrLn "No repairable issues found (issues may be in text files or untracked files)."

            -- Execute repairs
            unless (null allRepairs) $ do
                putStrLn $ "Repairing " ++ show (length allRepairs) ++ " file(s)..."
                results <- mapM executeAction allRepairs

                let repaired = [p | Repaired p <- results]
                    failed   = [(p, e) | RepairFailed p e <- results]

                forM_ repaired $ \p ->
                    putStrLn $ "  [REPAIRED] " ++ toPosix (unPath p)
                forM_ failed $ \(p, e) ->
                    hPutStrLn stderr $ "  [FAILED]   " ++ toPosix (unPath p) ++ " (" ++ e ++ ")"

                -- Summary
                putStrLn ""
                putStrLn $ show (length repaired) ++ " repaired, "
                    ++ show (length failed) ++ " failed, "
                    ++ show (length allUnrepairable) ++ " unrepairable."

                unless (null failed && null allUnrepairable) $
                    exitWith (ExitFailure 1)

            unless (null allUnrepairable) $ do
                when (null allRepairs) $ do
                    forM_ allUnrepairable $ \p ->
                        hPutStrLn stderr $ "  [UNREPAIRABLE] " ++ toPosix (unPath p)
                    putStrLn ""
                    putStrLn $ "0 repaired, 0 failed, " ++ show (length allUnrepairable) ++ " unrepairable."
                    exitWith (ExitFailure 1)

-- | Extract the path from a VerifyIssue.
issuePath :: Verify.VerifyIssue -> Path
issuePath (Verify.HashMismatch p _ _ _ _) = p
issuePath (Verify.Missing p) = p

-- | Build a content-addressable index from verified metadata entries.
-- Maps (hashString, size) to a Path that is known to be good.
-- Excludes any paths that are in the issue set (those are broken).
buildContentIndex :: [Verify.BinaryFileMeta] -> Set.Set Path -> Map.Map (String, Integer) Path
buildContentIndex entries issueSet =
    Map.fromList
        [ ((T.unpack (hashToText m.bfmHash), m.bfmSize), m.bfmPath)
        | m <- entries
        , not (Set.member m.bfmPath issueSet)
        ]

-- | Plan repair actions for a list of issues.
-- For each issue, looks up the expected (hash, size) in the opposite side's content index.
-- Returns (repair actions, unrepairable paths).
planRepairs :: [Verify.VerifyIssue]
            -> Map.Map Path (Hash 'MD5, Integer)
            -> Map.Map (String, Integer) Path
            -> (Path -> Path -> Hash 'MD5 -> Integer -> RepairAction)
            -> ([RepairAction], [Path])
planRepairs issues metaMap contentIndex mkAction = foldr go ([], []) issues
  where
    go issue (repairs, unrepairables) =
        let p = issuePath issue
        in case Map.lookup p metaMap of
            Nothing -> (repairs, unrepairables)  -- not in binary metadata, skip
            Just (expectedHash, expectedSize) ->
                let key = (T.unpack (hashToText expectedHash), expectedSize)
                in case Map.lookup key contentIndex of
                    Just sourcePath -> (mkAction sourcePath p expectedHash expectedSize : repairs, unrepairables)
                    Nothing -> (repairs, p : unrepairables)

-- | Restore the metadata file in .bit/index/ to match the expected hash+size.
restoreLocalMetadata :: FilePath -> Path -> Hash 'MD5 -> Integer -> IO ()
restoreLocalMetadata cwd destPath expectedHash expectedSize = do
    let metaPath = cwd </> bitIndexPath </> unPath destPath
    Dir.createDirectoryIfMissing True (takeDirectory metaPath)
    atomicWriteFileStr metaPath (serializeMetadata (MetaContent expectedHash expectedSize))

-- | Execute a single repair action for cloud remotes (uses rclone).
executeRepair :: FilePath -> Remote -> RepairAction -> IO RepairResult
executeRepair cwd remote (RepairLocal sourcePath destPath expectedHash expectedSize) = do
    -- Copy from remote to fix local
    let remoteRelPath = toPosix (unPath sourcePath)
        localFullPath = cwd </> unPath destPath
    Dir.createDirectoryIfMissing True (takeDirectory localFullPath)
    code <- Transport.copyFromRemote remote remoteRelPath localFullPath
    case code of
        ExitSuccess -> do
            restoreLocalMetadata cwd destPath expectedHash expectedSize
            pure (Repaired destPath)
        _ -> pure (RepairFailed destPath "copy from remote failed")
executeRepair cwd remote (RepairRemote sourcePath destPath _ _) = do
    -- Copy from local to fix remote
    let localFullPath = cwd </> unPath sourcePath
        remoteRelPath = toPosix (unPath destPath)
    code <- Transport.copyToRemote localFullPath remote remoteRelPath
    pure $ case code of
        ExitSuccess -> Repaired destPath
        _ -> RepairFailed destPath "copy to remote failed"

-- | Execute a single repair action for filesystem remotes (direct file copy).
executeFilesystemRepair :: FilePath -> FilePath -> RepairAction -> IO RepairResult
executeFilesystemRepair cwd remotePath (RepairLocal sourcePath destPath expectedHash expectedSize) = do
    -- Copy from remote filesystem to fix local
    let srcFullPath = remotePath </> unPath sourcePath
        dstFullPath = cwd </> unPath destPath
    Dir.createDirectoryIfMissing True (takeDirectory dstFullPath)
    result <- try @IOException $ Platform.copyFile srcFullPath dstFullPath
    case result of
        Right () -> do
            restoreLocalMetadata cwd destPath expectedHash expectedSize
            pure (Repaired destPath)
        Left e -> pure (RepairFailed destPath (show e))
executeFilesystemRepair cwd remotePath (RepairRemote sourcePath destPath expectedHash expectedSize) = do
    -- Copy from local to fix remote filesystem, also restore remote metadata
    let srcFullPath = cwd </> unPath sourcePath
        dstFullPath = remotePath </> unPath destPath
        remoteMetaPath = remotePath </> bitIndexPath </> unPath destPath
    Platform.createDirectoryIfMissing True (takeDirectory dstFullPath)
    Platform.createDirectoryIfMissing True (takeDirectory remoteMetaPath)
    result <- try @IOException $ do
        Platform.copyFile srcFullPath dstFullPath
        atomicWriteFileStr remoteMetaPath (serializeMetadata (MetaContent expectedHash expectedSize))
    pure $ case result of
        Right () -> Repaired destPath
        Left e -> RepairFailed destPath (show e)

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
    Just Device.RemoteCloud -> pure (name ++ " → " ++ remoteUrl remote ++ " (cloud)")
    Nothing -> pure (name ++ " → " ++ displayRemote remote)

-- | Print "  Fetch URL:" and "  Push  URL:" lines for remote show output.
printRemoteUrls :: String -> IO ()
printRemoteUrls url = do
    putStrLn $ "  Fetch URL: " ++ url
    putStrLn $ "  Push  URL: " ++ url

-- | Show remote status using git tracking refs (for filesystem/device remotes).
-- No bundle needed — reads directly from refs/remotes/<name>/main.
showRefBasedRemoteStatus :: String -> String -> IO ()
showRefBasedRemoteStatus name url = do
    maybeLocal <- Git.getLocalHead
    (refCode, refOut, _) <- Git.runGitWithOutput ["rev-parse", Git.remoteTrackingRef name]
    let maybeRemote = case refCode of
            ExitSuccess -> Just (filter (/= '\n') refOut)
            _ -> Nothing
    putStrLn $ "* remote " ++ name
    printRemoteUrls url
    putStrLn ""
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

showRemoteStatusFromBundle :: String -> Maybe String -> IO ()
showRemoteStatusFromBundle name mUrl = do
    maybeLocal <- Git.getLocalHead
    let url = fromMaybe "?" mUrl
    putStrLn $ "* remote " ++ name
    printRemoteUrls url
    putStrLn ""
    compareHistory maybeLocal fetchedBundle

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
