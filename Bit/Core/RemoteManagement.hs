{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DataKinds #-}

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
import System.FilePath ((</>), takeDirectory)
import Control.Monad (unless, void, when, forM_)
import System.Exit (ExitCode(..), exitWith)
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
import Internal.Config (bitDevicesDir, bitRemotesDir, fetchedBundle, bundleCwdPath, fromCwdPath, BundleName(..), bitIndexPath)
import Bit.Types (BitM, BitEnv(..), Path(..), Hash(..), HashAlgo(..), hashToText)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Bit.Remote (Remote, remoteUrl, remoteName, displayRemote, resolveRemote)
import Bit.Core.Helpers (getRemoteTargetType)
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
            registerDevice cwd name volRoot deviceName' relPath u
        (Nothing, _) -> do
            mLabel <- Device.getVolumeLabel volRoot
            deviceName' <- promptDeviceName cwd volRoot mLabel
            u <- Device.generateStoreUuid
            Device.writeBitStore volRoot u
            registerDevice cwd name volRoot deviceName' relPath u
    case result of
        Right () -> pure ()
        Left _err -> do
            -- Cannot create .bit-store at volume root (e.g. permission denied on C:\)
            -- Fall back to path-based storage for local directories
            Device.writeRemoteFile cwd name (Device.TargetLocalPath absPath)
            void $ Git.addRemote name absPath
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
    Device.writeRemoteFile cwd name (Device.TargetDevice deviceName' relPath)
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
            mTarget <- getRemoteTargetType cwd (remoteName remote)
            let isFilesystem = maybe False Device.isFilesystemTarget mTarget
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
    (localCount, localIssues) <- Verify.verifyLocal cwd Nothing concurrency
    putStrLn $ "  " ++ show localCount ++ " files checked, " ++ show (length localIssues) ++ " issues"

    putStrLn "Verifying remote files..."
    (remoteCount, remoteIssues) <- Verify.verifyLocalAt remotePath Nothing concurrency
    putStrLn $ "  " ++ show remoteCount ++ " files checked, " ++ show (length remoteIssues) ++ " issues"

    runRepairLogic localMeta remoteMeta localIssues remoteIssues
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
    (localCount, localIssues) <- Verify.verifyLocal cwd Nothing concurrency
    putStrLn $ "  " ++ show localCount ++ " files checked, " ++ show (length localIssues) ++ " issues"

    putStrLn "Verifying remote files..."
    (remoteCount, remoteIssues) <- Verify.verifyRemote cwd remote Nothing concurrency
    putStrLn $ "  " ++ show remoteCount ++ " files checked, " ++ show (length remoteIssues) ++ " issues"

    runRepairLogic localMeta remoteMeta localIssues remoteIssues
        (executeRepair cwd remote)

-- | Common repair logic shared between filesystem and cloud remotes.
runRepairLogic :: [(Path, Hash 'MD5, Integer)]    -- local metadata
              -> [(Path, Hash 'MD5, Integer)]    -- remote metadata
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
            let localMetaMap = Map.fromList [(p, (h, s)) | (p, h, s) <- localMeta]
                remoteMetaMap = Map.fromList [(p, (h, s)) | (p, h, s) <- remoteMeta]

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
buildContentIndex :: [(Path, Hash 'MD5, Integer)] -> Set.Set Path -> Map.Map (String, Integer) Path
buildContentIndex entries issueSet =
    Map.fromList
        [ ((T.unpack (hashToText h), sz), p)
        | (p, h, sz) <- entries
        , not (Set.member p issueSet)
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
    result <- try @IOException $ Dir.copyFile srcFullPath dstFullPath
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
    Dir.createDirectoryIfMissing True (takeDirectory dstFullPath)
    Dir.createDirectoryIfMissing True (takeDirectory remoteMetaPath)
    result <- try @IOException $ do
        Dir.copyFile srcFullPath dstFullPath
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

showRemoteStatusFromBundle :: String -> Maybe String -> IO ()
showRemoteStatusFromBundle name mUrl = do
    maybeLocal <- Git.getLocalHead
    let url = fromMaybe "?" mUrl
    putStrLn $ "* remote " ++ name
    putStrLn $ "  Fetch URL: " ++ url
    putStrLn $ "  Push  URL: " ++ url
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
  localAhead  <- Git.checkIsAhead remoteHash localHash  -- is local ahead of remote?
  remoteAhead <- Git.checkIsAhead localHash remoteHash  -- is remote ahead of local?
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
