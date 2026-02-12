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
import qualified Bit.Platform as Platform
import System.FilePath ((</>), takeDirectory)
import Control.Monad (when, unless, void, forM_)
import System.Exit (ExitCode(..), exitWith)
import qualified Internal.Git as Git
import qualified Internal.Transport as Transport
import Internal.Config (bundleForRemote, bundleCwdPath, fromCwdPath, bundleGitRelPath, fromGitRelPath)
import Bit.Utils (toPosix)
import qualified Data.List as List
import System.IO (stderr, hPutStrLn)
import Bit.Remote (Remote, remoteName, remoteUrl, RemoteState(..), FetchResult(..), displayRemote, RemotePath(..))
import Bit.Plan (RcloneAction(..))
import Bit.Types (BitM, BitEnv(..), ForceMode(..), unPath)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Bit.CopyProgress as CopyProgress
import qualified Bit.Verify as Verify
import Bit.Concurrency (Concurrency(..))
import Bit.Platform (copyFile)
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
import Bit.Core.Transport (deriveActions, executeCommand)
import Bit.Core.Fetch (classifyRemoteState, fetchBundle)

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
        _ -> hPutStrLn stderr "Error creating bundle"

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
filesystemPushMetadata cwd remote = do
    let remotePath = remoteUrl remote
        localIndexGit = cwd </> ".bit" </> "index" </> ".git"
        remoteIndex = remotePath </> ".bit" </> "index"
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

    -- 1. Proof of possession — always verify local before pushing
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
    isFs <- isFilesystemRemote remote
    let seam = if isFs
            then mkFilesystemSeam cwd remote
            else mkCloudSeam remote

    -- 3. Classify remote state (works for both via rclone)
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
                initializeRemoteRepoAt (RemotePath remotePath)
            executePush seam remote Nothing

        StateValidBit -> do
            liftIO $ putStrLn "Remote is a bit repo. Checking history..."
            -- Capture pre-fetch tracking hash BEFORE ptFetchHistory.
            -- ptFetchHistory updates the tracking ref as a side effect of git fetch.
            -- ForceWithLease needs the pre-fetch hash to detect concurrent pushes.
            preHash <- liftIO $ Git.getRemoteTrackingHash (remoteName remote)
            mRemoteHash <- liftIO $ ptFetchHistory seam
            case mRemoteHash of
                Just _ -> processExistingRemote remote seam preHash mRemoteHash
                -- No remote commits yet (initialized but empty) — treat as first push
                Nothing -> executePush seam remote Nothing

        StateNonBitOccupied samples -> handleNonBit seam remote samples

        StateNetworkError err ->
            liftIO $ hPutStrLn stderr $ "Aborting: Network error -> " ++ err

-- ============================================================================
-- Push state handlers
-- ============================================================================

-- | Handle existing remote with history. Performs ancestry checks and
-- force mode logic using the pre-fetch and post-fetch tracking hashes.
processExistingRemote :: Remote -> PushSeam -> Maybe String -> Maybe String -> BitM ()
processExistingRemote remote seam preHash mRemoteHash = do
    fMode <- asks envForceMode
    case fMode of
        Force -> do
            lift $ tellErr "Warning: --force used. Overwriting remote history..."
            executePush seam remote mRemoteHash

        ForceWithLease -> case (preHash, mRemoteHash) of
            (Just pre, Just post) | pre == post -> do
                lift $ tell "Remote check passed (--force-with-lease). Proceeding..."
                executePush seam remote mRemoteHash
            (Nothing, _) -> do
                lift $ tellErr "Warning: No previous tracking ref. Proceeding..."
                executePush seam remote mRemoteHash
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
                            executePush seam remote mRemoteHash
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
handleNonBit :: PushSeam -> Remote -> [String] -> BitM ()
handleNonBit seam remote samples = do
    fMode <- asks envForceMode
    case fMode of
        Force -> do
            liftIO $ hPutStrLn stderr "Warning: --force used. Overwriting non-bit remote..."
            executePush seam remote Nothing
        _ -> liftIO $ do
            hPutStrLn stderr "-------------------------------------------------------"
            hPutStrLn stderr "[!] STOP: Remote is NOT a bit repository!"
            hPutStrLn stderr $ "Found existing files: " ++ List.intercalate ", " samples
            hPutStrLn stderr "To initialize anyway (destructive): bit init --force"
            hPutStrLn stderr "-------------------------------------------------------"

-- ============================================================================
-- Push execution (shared code for both transports)
-- ============================================================================

-- | Execute a push: sync files, push metadata, update local tracking ref.
-- mRemoteHash: the remote's HEAD hash (Nothing for first push / empty remote).
executePush :: PushSeam -> Remote -> Maybe String -> BitM ()
executePush seam remote mRemoteHash = do
    syncRemoteFiles mRemoteHash
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
-- ============================================================================

syncRemoteFiles :: Maybe String -> BitM ()
syncRemoteFiles mRemoteHash = withRemote $ \remote -> do
    cwd <- asks envCwd
    actions <- liftIO $ deriveActions mRemoteHash "HEAD"
    liftIO $ putStrLn "--- Pushing Changes to Remote ---"
    if null actions
        then liftIO $ putStrLn "Remote is already up to date."
        else do
            let (copies, others) = List.partition isCopy actions
                copyPaths = [toPosix (unPath src) | Copy src _ <- copies]
            -- Create progress tracker for all actions
            progress <- liftIO $ CopyProgress.newSyncProgress (length actions)
            liftIO $ CopyProgress.withSyncProgressReporter progress $ do
                -- Batch all copies into a single rclone subprocess
                unless (null copyPaths) $
                    CopyProgress.rcloneCopyFiles cwd (remoteUrl remote) copyPaths progress
                -- Non-copy actions individually
                forM_ others $ \a -> do
                    executeCommand cwd remote a
                    CopyProgress.incrementFilesComplete progress
  where
    isCopy (Copy _ _) = True
    isCopy _          = False

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
        _ -> hPutStrLn stderr "Error creating bundle"

-- | Upload a bundle file to the remote.
uploadToRemote :: FilePath -> Remote -> IO ()
uploadToRemote src remote = do
    putStrLn "Uploading bundle to remote..."
    rCode <- Transport.copyToRemote src remote ".bit/bit.bundle"
    case rCode of
        ExitSuccess -> putStrLn "Metadata push complete."
        _ -> hPutStrLn stderr "Error uploading bundle."

-- | Helper for cleanup that doesn't crash if the file was never made.
cleanupTemp :: FilePath -> IO ()
cleanupTemp filePath = do
    exists <- Dir.doesFileExist filePath
    when exists (Dir.removeFile filePath)
