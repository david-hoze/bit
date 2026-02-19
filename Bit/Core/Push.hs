{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bit.Core.Push
    ( push
    , pushBundle
    , uploadToRemote
    , cleanupTemp
    , syncRemoteFiles
    ) where

import qualified System.Directory as Dir
import qualified Bit.IO.Platform as Platform
import System.FilePath ((</>), takeDirectory)
import Control.Monad (when, unless, void, forM_)
import System.Exit (ExitCode(..), exitWith)
import qualified Bit.Git.Run as Git
import qualified Bit.Rclone.Run as Transport
import Bit.Config.Paths (bundleForRemote, bundleCwdPath, fromCwdPath, bundleGitRelPath, fromGitRelPath)
import Bit.Utils (toPosix)
import qualified Data.List as List
import System.IO (stderr, hPutStrLn)
import Bit.Remote (Remote, remoteName, remoteUrl, RemoteState(..), FetchResult(..), displayRemote, RemotePath(..))
import Bit.Domain.Plan (RcloneAction(..))
import Bit.Types (BitM, BitEnv(..), ForceMode(..), unPath, Hash(..), HashAlgo(..))
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Bit.Rclone.Progress as CopyProgress
import qualified Bit.Scan.Verify as Verify
import Bit.IO.Concurrency (Concurrency(..))
import Bit.IO.Platform (copyFile)
import Bit.Core.Helpers
    ( AncestorQuery(..)
    , isFilesystemRemote
    , withRemote
    , getLocalHeadE
    , checkIsAheadE
    , tell
    , tellErr
    , printVerifyIssue
    , safeRemove
    )
import Bit.Core.Init (initializeRemoteRepoAt)
import Bit.Rclone.Sync (deriveActions, executeCommand)
import Bit.Core.Fetch (classifyRemoteState, fetchBundle)
import qualified Bit.Device.Identity as Device
import Bit.CAS (casBlobPath, hasBlobInCas, writeBlobToCas)
import Bit.Config.Metadata (parseMetadataFile, MetaContent(..))
import Bit.CDC.Manifest (readManifestFromCas, casManifestPath)
import Bit.CDC.Types (ChunkRef(..), ChunkManifest(..))
import qualified Data.Set as Set
import Data.Set (Set)
import Bit.Remote.ChunkIndex (queryRemoteBlobs)

-- ============================================================================
-- Push seam: the only difference between cloud and filesystem push
-- ============================================================================

-- | Transport seam for push. Exactly the two operations that differ between
-- cloud (bundle-based) and filesystem (git-native) transports. Everything
-- else — classifyRemoteState, syncRemoteFiles, ancestry checks, force modes,
-- verification — is shared code.
data PushSeam = PushSeam
    { ptFetchHistory :: IO (Maybe String)  -- ^ Fetch remote history, return remote hash
    , ptPushMetadata :: IO ()              -- ^ Push metadata to remote after file sync
    }

-- | Build a cloud push seam (bundle-based metadata transport via rclone).
mkCloudSeam :: Remote -> PushSeam
mkCloudSeam remote = PushSeam
    { ptFetchHistory = cloudFetchHistory remote
    , ptPushMetadata = cloudPushMetadata remote
    }

-- | Build a filesystem push seam (native git fetch/pull for metadata transport).
mkFilesystemSeam :: FilePath -> Remote -> PushSeam
mkFilesystemSeam cwd remote = PushSeam
    { ptFetchHistory = filesystemFetchHistory cwd remote
    , ptPushMetadata = filesystemPushMetadata cwd remote
    }

-- | Build a git-native push seam for metadata-only remotes (GitHub, GitLab, bare repos).
-- Uses git fetch/push directly against the remote URL.
mkGitSeam :: Remote -> PushSeam
mkGitSeam remote = PushSeam
    { ptFetchHistory = do
        let name = remoteName remote
        (fetchCode, _, _) <- Git.runGitWithOutput ["fetch", name]
        if fetchCode == ExitSuccess
            then Git.getRemoteTrackingHash name
            else pure Nothing
    , ptPushMetadata = do
        let name = remoteName remote
        (code, _, err) <- Git.runGitWithOutput ["push", name, "main"]
        when (code /= ExitSuccess) $ do
            hPutStrLn stderr $ "error: git push failed: " ++ err
            exitWith code
    }

-- ============================================================================
-- Cloud seam implementations
-- ============================================================================

-- | Cloud: download bundle, register as git remote, read tracking hash.
cloudFetchHistory :: Remote -> IO (Maybe String)
cloudFetchHistory remote = do
    let name = remoteName remote
        bundleName = bundleForRemote name
        fetchedPath = fromCwdPath (bundleCwdPath bundleName)
        bundleGitPath = fromGitRelPath (bundleGitRelPath bundleName)
    result <- fetchBundle remote
    case result of
        BundleFound bPath -> do
            Dir.createDirectoryIfMissing True (takeDirectory fetchedPath)
            copyFile bPath fetchedPath
            safeRemove bPath
            -- Register bundle as named git remote and fetch objects + refs
            void $ Git.addRemote name bundleGitPath
            (fetchCode, _, _) <- Git.runGitWithOutput ["fetch", name]
            if fetchCode == ExitSuccess
                then Git.getRemoteTrackingHash name
                else Git.getHashFromBundle bundleName  -- fallback
        _ -> pure Nothing

-- | Cloud: create bundle at per-remote path and upload to remote.
-- The local copy persists so bit status can compare against it.
cloudPushMetadata :: Remote -> IO ()
cloudPushMetadata remote = do
    let name = remoteName remote
        bundleName = bundleForRemote name
        bundlePath = fromCwdPath (bundleCwdPath bundleName)
    -- Ensure bundles directory exists
    Dir.createDirectoryIfMissing True (takeDirectory bundlePath)
    code <- Git.createBundle bundleName
    case code of
        ExitSuccess -> uploadToRemote bundlePath remote
        _ -> do hPutStrLn stderr "Error creating bundle"
                exitWith (ExitFailure 1)

-- ============================================================================
-- Filesystem seam implementations
-- ============================================================================

-- | Filesystem: register remote index as git remote, fetch, read tracking hash.
filesystemFetchHistory :: FilePath -> Remote -> IO (Maybe String)
filesystemFetchHistory _cwd remote = do
    let name = remoteName remote
        remotePath = remoteUrl remote
    -- Ensure git remote URL is current (device may have moved)
    void $ Git.addRemote name (remotePath </> ".bit" </> "index")
    -- Native git fetch
    (fetchCode, _, fetchErr) <- Git.runGitWithOutput ["fetch", name]
    when (fetchCode /= ExitSuccess) $ do
        hPutStrLn stderr $ "Error fetching from remote: " ++ fetchErr
        exitWith fetchCode
    Git.getRemoteTrackingHash name

-- | Filesystem: pull local metadata into remote's index via git pull --ff-only.
filesystemPushMetadata :: FilePath -> Remote -> IO ()
filesystemPushMetadata _cwd remote = do
    let remotePath = remoteUrl remote
        remoteIndex = remotePath </> ".bit" </> "index"
    localIndexPath <- Git.getIndexPath
    let localIndexGit = localIndexPath </> ".git"
    (code, _, err) <- Git.runGitAt remoteIndex
        ["pull", "--ff-only", localIndexGit, "main"]
    when (code /= ExitSuccess) $ do
        hPutStrLn stderr $ "error: Failed to update remote metadata: " ++ err
        exitWith (ExitFailure 1)

-- ============================================================================
-- Unified push entry point
-- ============================================================================

push :: BitM ()
push = withRemote $ \remote -> do
    cwd <- asks envCwd

    -- Read layout for all remote types
    mType <- liftIO $ Device.readRemoteType cwd (remoteName remote)
    isFs <- isFilesystemRemote remote
    layout <- liftIO $ Device.readRemoteLayout cwd (remoteName remote)

    -- 1. Proof of possession — skip for bare and metadata-only remotes
    when (layout == Device.LayoutFull || (isFs && layout /= Device.LayoutMetadata)) $ do
        liftIO $ putStrLn "Verifying local files..."
        result <- liftIO $ Verify.verifyLocal cwd Nothing (Parallel 0)
        if null result.vrIssues
            then liftIO $ putStrLn $ "Verified " ++ show result.vrCount ++ " files. All match metadata."
            else liftIO $ do
                hPutStrLn stderr $ "error: Working tree does not match metadata (" ++ show (length result.vrIssues) ++ " issues)."
                mapM_ (printVerifyIssue id) result.vrIssues
                hPutStrLn stderr "hint: Run 'bit verify' to see all mismatches."
                hPutStrLn stderr "hint: Run 'bit add' to update metadata, or 'bit restore' to restore files."
                exitWith (ExitFailure 1)

    -- 2. Detect transport mode and build seam
    let seam = case (mType, layout) of
            (Just Device.RemoteGit, _)       -> mkGitSeam remote
            (_, Device.LayoutMetadata)        -> case mType of
                Just Device.RemoteCloud       -> mkCloudSeam remote
                _                             -> mkGitSeam remote
            _ | isFs                          -> mkFilesystemSeam cwd remote
            _                                 -> mkCloudSeam remote

    -- 3. For git/metadata-only remotes, skip classifyRemoteState (uses rclone listing).
    -- Instead, try fetch directly and dispatch based on result.
    if layout == Device.LayoutMetadata
        then do
            liftIO $ putStrLn $ "Pushing metadata to: " ++ displayRemote remote
            preHash <- liftIO $ Git.getRemoteTrackingHash (remoteName remote)
            mRemoteHash <- liftIO $ ptFetchHistory seam
            case mRemoteHash of
                Just _ -> processExistingRemote remote seam preHash mRemoteHash layout
                Nothing -> executePush seam remote Nothing layout
        else do
            -- Classify remote state (works for both cloud and filesystem via rclone)
            liftIO $ putStrLn $ "Inspecting remote: " ++ displayRemote remote
            state <- liftIO $ classifyRemoteState remote

            -- 4. Handle states
            case state of
                StateEmpty -> do
                    liftIO $ putStrLn "Remote is empty. Initializing..."
                    -- Filesystem: create and initialize remote repo structure
                    when isFs $ liftIO $ do
                        let remotePath = remoteUrl remote
                        Platform.createDirectoryIfMissing True remotePath
                        void $ initializeRemoteRepoAt (RemotePath remotePath)
                    executePush seam remote Nothing layout

                StateValidBit -> do
                    liftIO $ putStrLn "Remote is a bit repo. Checking history..."
                    preHash <- liftIO $ Git.getRemoteTrackingHash (remoteName remote)
                    mRemoteHash <- liftIO $ ptFetchHistory seam
                    case mRemoteHash of
                        Just _ -> processExistingRemote remote seam preHash mRemoteHash layout
                        Nothing -> executePush seam remote Nothing layout

                StateNonBitOccupied samples -> handleNonBit seam remote samples layout

                StateNetworkError err ->
                    liftIO $ hPutStrLn stderr $ "Aborting: Network error -> " ++ err

-- ============================================================================
-- Push state handlers
-- ============================================================================

-- | Handle existing remote with history. Performs ancestry checks and
-- force mode logic using the pre-fetch and post-fetch tracking hashes.
processExistingRemote :: Remote -> PushSeam -> Maybe String -> Maybe String -> Device.RemoteLayout -> BitM ()
processExistingRemote remote seam preHash mRemoteHash layout = do
    fMode <- asks envForceMode
    case fMode of
        Force -> do
            lift $ tellErr "Warning: --force used. Overwriting remote history..."
            executePush seam remote mRemoteHash layout

        ForceWithLease -> case (preHash, mRemoteHash) of
            (Just pre, Just post) | pre == post -> do
                lift $ tell "Remote check passed (--force-with-lease). Proceeding..."
                executePush seam remote mRemoteHash layout
            (Nothing, _) -> do
                lift $ tellErr "Warning: No previous tracking ref. Proceeding..."
                executePush seam remote mRemoteHash layout
            (Just _, Just _) -> liftIO $ do
                hPutStrLn stderr "---------------------------------------------------"
                hPutStrLn stderr "ERROR: Remote has changed since last fetch!"
                hPutStrLn stderr "Someone else pushed to the remote."
                hPutStrLn stderr "Run 'bit fetch' to update your local view."
                hPutStrLn stderr "---------------------------------------------------"
                exitWith (ExitFailure 1)
            (_, Nothing) -> liftIO $ do
                hPutStrLn stderr "Error: Could not determine remote state."
                exitWith (ExitFailure 1)

        NoForce -> do
            maybeLocalHash <- lift getLocalHeadE
            case (maybeLocalHash, mRemoteHash) of
                (Just lHash, Just rHash) -> do
                    isAhead <- lift $ checkIsAheadE (AncestorQuery { aqAncestor = rHash, aqDescendant = lHash })
                    if isAhead
                        then do
                            lift $ tell "Remote check passed. Proceeding with push..."
                            executePush seam remote mRemoteHash layout
                        else liftIO $ do
                            hPutStrLn stderr "---------------------------------------------------"
                            hPutStrLn stderr "error: Remote history has diverged or is ahead!"
                            hPutStrLn stderr "Please run 'bit pull' before pushing."
                            hPutStrLn stderr "---------------------------------------------------"
                            exitWith (ExitFailure 1)
                _ -> liftIO $ do
                    hPutStrLn stderr "Error: Could not extract hashes for comparison."
                    exitWith (ExitFailure 1)

-- | Handle non-bit-occupied remote. Only --force allows overwriting.
handleNonBit :: PushSeam -> Remote -> [String] -> Device.RemoteLayout -> BitM ()
handleNonBit seam remote samples layout = do
    fMode <- asks envForceMode
    case fMode of
        Force -> do
            liftIO $ hPutStrLn stderr "Warning: --force used. Overwriting non-bit remote..."
            executePush seam remote Nothing layout
        _ -> liftIO $ do
            hPutStrLn stderr "-------------------------------------------------------"
            hPutStrLn stderr "[!] STOP: Remote is NOT a bit repository!"
            hPutStrLn stderr $ "Found existing files: " ++ List.intercalate ", " samples
            hPutStrLn stderr "To initialize anyway (destructive): bit push --force"
            hPutStrLn stderr "-------------------------------------------------------"

-- ============================================================================
-- Push execution (shared code for both transports)
-- ============================================================================

-- | Execute a push: sync files, push metadata, update local tracking ref.
-- mRemoteHash: the remote's HEAD hash (Nothing for first push / empty remote).
executePush :: PushSeam -> Remote -> Maybe String -> Device.RemoteLayout -> BitM ()
executePush seam remote mRemoteHash layout = do
    unless (layout == Device.LayoutMetadata) $
        syncRemoteFiles mRemoteHash layout
    -- Skip metadata push when remote is already at our HEAD (avoids slow
    -- no-op git pull over network paths like \\tsclient\...)
    localHead <- liftIO getLocalHeadE
    let alreadyAtHead = case (localHead, mRemoteHash) of
            (Just lh, Just rh) -> lh == rh
            _                  -> False
    unless alreadyAtHead $ liftIO $ ptPushMetadata seam
    liftIO $ void $ Git.updateRemoteTrackingBranchToHead (remoteName remote)
    liftIO $ putStrLn "Push complete."

-- ============================================================================
-- File sync: derive actions from git metadata diff, execute via rclone
-- Always upload binaries to remote CAS; for full layout also sync readable paths.
-- ============================================================================

syncRemoteFiles :: Maybe String -> Device.RemoteLayout -> BitM ()
syncRemoteFiles mRemoteHash layout = withRemote $ \remote -> do
    cwd <- asks envCwd
    bitDir <- asks envBitDir
    let indexDir = bitDir </> "index"
    actions <- liftIO $ deriveActions mRemoteHash "HEAD"
    liftIO $ putStrLn "--- Pushing Changes to Remote ---"
    if null actions
        then liftIO $ putStrLn "Remote is already up to date."
        else do
            let (copies, others) = List.partition isCopy actions
                copyPaths = [unPath src | Copy src _ <- copies]
            -- Always upload binary files to remote CAS (both full and bare).
            -- TODO: Full layout then syncs same files to readable paths (double bandwidth).
            --       Future optimization: skip CAS upload for files that already exist at readable path.
            let casDir = bitDir </> "cas"
            -- Query remote CAS to skip blobs that already exist (dedup).
            remoteBlobs <- liftIO $ do
                unless (null copyPaths) $ putStrLn "Querying remote CAS for dedup..."
                queryRemoteBlobs remote
            -- Collect all CAS-relative paths to upload in one batch.
            -- In lite mode, CAS blobs may not exist locally — stage them from the working tree.
            casRelPaths <- fmap concat $ liftIO $ mapM (collectCasPaths remoteBlobs cwd indexDir casDir) copyPaths
            unless (null copyPaths) $
                liftIO $ putStrLn $ "Uploading " ++ show (length casRelPaths) ++ " file(s) to CAS"
                    ++ if not (Set.null remoteBlobs)
                       then " (" ++ show (Set.size remoteBlobs) ++ " already on remote)..."
                       else "..."
            -- Batch upload: one rclone subprocess with --files-from.
            -- CAS chunks are small (~128KB), so latency dominates — use high parallelism.
            unless (null casRelPaths) $ liftIO $ do
                progress <- CopyProgress.newSyncProgress (length casRelPaths)
                CopyProgress.withSyncProgressReporter progress $
                    CopyProgress.rcloneCopyFilesWithFlags ["--transfers", "32"]
                        bitDir (remoteUrl remote) casRelPaths progress
            -- Full layout: also sync readable paths and apply move/delete.
            -- Bare layout: no readable tree, so Move/Delete are intentionally not applied.
            when (layout == Device.LayoutFull) $ do
                let copyPathsPosix = map toPosix copyPaths
                progress <- liftIO $ CopyProgress.newSyncProgress (length actions)
                liftIO $ CopyProgress.withSyncProgressReporter progress $ do
                    unless (null copyPathsPosix) $
                        CopyProgress.rcloneCopyFiles cwd (remoteUrl remote) copyPathsPosix progress
                    forM_ others $ \a -> do
                        executeCommand cwd remote a
                        CopyProgress.incrementFilesComplete progress
  where
    isCopy (Copy _ _) = True
    isCopy _          = False

-- | Collect CAS-relative paths for a file: chunk blobs + manifest (if chunked)
-- or whole blob (if not). Paths are relative to the .bit/ directory.
-- Filters out blobs already present on the remote (remoteBlobs set).
-- In lite mode, CAS blobs may not exist locally. If a whole blob is missing,
-- stage it from the working tree so the upload succeeds.
collectCasPaths :: Set (Hash 'MD5) -> FilePath -> FilePath -> FilePath -> FilePath -> IO [FilePath]
collectCasPaths remoteBlobs cwd indexDir casDir filePath = do
    mMeta <- parseMetadataFile (indexDir </> filePath)
    case mMeta of
        Just mc -> do
            mManifest <- readManifestFromCas casDir (metaHash mc)
            case mManifest of
                Just manifest -> do
                    -- Chunked: upload only chunks not already on remote + always the manifest
                    let newChunkPaths = [ casBlobPath "cas" (crHash cr)
                                        | cr <- cmChunks manifest
                                        , not (Set.member (crHash cr) remoteBlobs) ]
                        manifestPath = casManifestPath "cas" (metaHash mc)
                    pure (newChunkPaths ++ [manifestPath])
                Nothing -> do
                    -- Whole blob — skip if already on remote
                    let h = metaHash mc
                    if Set.member h remoteBlobs
                        then pure []
                        else do
                            exists <- hasBlobInCas casDir h
                            unless exists $
                                writeBlobToCas (cwd </> filePath) casDir h
                            pure [casBlobPath "cas" h]
        Nothing -> pure []

-- ============================================================================
-- Bundle helpers (cloud transport)
-- ============================================================================

-- | Push the git bundle to remote. Uses bracket to ensure temp bundle cleanup.
pushBundle :: Remote -> IO ()
pushBundle remote = do
    let name = remoteName remote
        bundleName = bundleForRemote name
        bundlePath = fromCwdPath (bundleCwdPath bundleName)
    -- Ensure bundles directory exists
    Dir.createDirectoryIfMissing True (takeDirectory bundlePath)
    code <- Git.createBundle bundleName
    case code of
        ExitSuccess -> uploadToRemote bundlePath remote
        _ -> do hPutStrLn stderr "Error creating bundle"
                exitWith (ExitFailure 1)

-- | Upload a bundle file to the remote.
uploadToRemote :: FilePath -> Remote -> IO ()
uploadToRemote src remote = do
    putStrLn "Uploading bundle to remote..."
    rCode <- Transport.copyToRemote src remote ".bit/bit.bundle"
    case rCode of
        ExitSuccess -> putStrLn "Metadata push complete."
        _ -> do hPutStrLn stderr "Error uploading bundle."
                exitWith (ExitFailure 1)

-- | Helper for cleanup that doesn't crash if the file was never made.
cleanupTemp :: FilePath -> IO ()
cleanupTemp filePath = do
    exists <- Dir.doesFileExist filePath
    when exists (Dir.removeFile filePath)
