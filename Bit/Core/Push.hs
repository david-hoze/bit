{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Core.Push
    ( push
    , cloudPush
    , filesystemPush
    , pushBundle
    , uploadToRemote
    , cleanupTemp
    , pushToRemote
    , updateLocalBundleAfterPush
    , syncRemoteFiles
    , processExistingRemote
    ) where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import Control.Monad (when, unless, void)
import System.Exit (ExitCode(..), exitWith)
import qualified Internal.Git as Git
import qualified Internal.Transport as Transport
import Internal.Config (fetchedBundle, BundleName(..), bundleCwdPath, fromCwdPath)
import Data.Char (isSpace)
import qualified Bit.Pipeline as Pipeline
import qualified Bit.Remote.Scan as Remote.Scan
import qualified Data.List as List
import Bit.Concurrency (runConcurrentlyBounded)
import Control.Concurrent (getNumCapabilities)
import System.IO (stderr, hPutStrLn)
import Control.Exception (bracket)
import Bit.Remote (Remote, remoteName, remoteUrl, RemoteState(..), FetchResult(..), displayRemote)
import Bit.Types (BitM, BitEnv(..), ForceMode(..))
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Bit.CopyProgress as CopyProgress
import qualified Bit.Verify as Verify
import Bit.Concurrency (Concurrency(..))
import qualified Bit.Device as Device
import System.Directory (copyFile)
import Bit.Core.Helpers
    ( getRemoteTargetType
    , withRemote
    , getLocalHeadE
    , checkIsAheadE
    , fileExistsE
    , tell
    , tellErr
    , printVerifyIssue
    , safeRemove
    )
import Bit.Core.Init (initializeRepoAt)
import Bit.Core.Transport (executeCommand, filesystemSyncAllFiles, filesystemSyncChangedFiles)
import Bit.Core.Fetch (classifyRemoteState, fetchBundle)

-- ============================================================================
-- Push operations
-- ============================================================================

push :: BitM ()
push = withRemote $ \remote -> do
    cwd <- asks envCwd
    fMode <- asks envForceMode
    skipVerify <- asks envSkipVerify
    
    -- NEW: Proof of possession — verify local before pushing
    unless (fMode == Force || skipVerify) $ do
        liftIO $ putStrLn "Verifying local files..."
        (fileCount, issues) <- liftIO $ Verify.verifyLocal cwd Nothing (Parallel 0)
        if null issues
            then liftIO $ putStrLn $ "Verified " ++ show fileCount ++ " files. All match metadata."
            else liftIO $ do
                hPutStrLn stderr $ "error: Working tree does not match metadata (" ++ show (length issues) ++ " issues)."
                mapM_ (printVerifyIssue id) issues  -- full hash, no truncation
                hPutStrLn stderr "hint: Run 'bit verify' to see all mismatches."
                hPutStrLn stderr "hint: Run 'bit add' to update metadata, or 'bit restore' to restore files."
                hPutStrLn stderr "hint: Run 'bit push --force' to push anyway (unsafe)."
                exitWith (ExitFailure 1)
    
    -- Determine if this is a filesystem or cloud remote
    mTarget <- liftIO $ getRemoteTargetType cwd (remoteName remote)
    case mTarget of
        Just (Device.TargetDevice _ _) -> liftIO $ filesystemPush cwd remote
        Just (Device.TargetLocalPath _) -> liftIO $ filesystemPush cwd remote
        _ -> cloudPush remote  -- Cloud remote or no target info (use cloud flow)

-- | Push to a cloud remote (original flow, unchanged).
cloudPush :: Remote -> BitM ()
cloudPush remote = do
    fMode <- asks envForceMode
    state <- liftIO $ do
        putStrLn $ "Inspecting remote: " ++ displayRemote remote
        classifyRemoteState remote

    case state of
        StateEmpty -> do
            liftIO $ putStrLn "Remote is empty. Initializing..."
            syncRemoteFiles
            liftIO $ pushBundle remote
            updateLocalBundleAfterPush

        StateValidRgit -> do
            fetchResult <- liftIO $ do
                putStrLn "Remote is a bit repo. Checking history..."
                fetchBundle remote
            case fetchResult of
                BundleFound bPath -> do
                    let fetchedPath = fromCwdPath (bundleCwdPath fetchedBundle)
                    liftIO $ do
                        copyFile bPath fetchedPath
                        safeRemove bPath
                    processExistingRemote
                _ -> liftIO $ hPutStrLn stderr "Error: Remote .bit found but metadata is missing."

        StateNonRgitOccupied samples -> do
            if fMode == Force
                then do
                    liftIO $ hPutStrLn stderr "Warning: --force used. Overwriting non-bit remote..."
                    syncRemoteFiles
                    liftIO $ pushBundle remote
                    updateLocalBundleAfterPush
                else liftIO $ do
                    hPutStrLn stderr "-------------------------------------------------------"
                    hPutStrLn stderr "[!] STOP: Remote is NOT a bit repository!"
                    hPutStrLn stderr $ "Found existing files: " ++ List.intercalate ", " samples
                    hPutStrLn stderr "To initialize anyway (destructive): bit init --force"
                    hPutStrLn stderr "-------------------------------------------------------"

        StateNetworkError err ->
            liftIO $ hPutStrLn stderr $ "Aborting: Network error -> " ++ err

        StateCorruptedRgit msg ->
            liftIO $ hPutStrLn stderr $ "Aborting: [X] Corrupted remote -> " ++ msg

-- | Push to a filesystem remote. Creates a full bit repo at the remote location.
filesystemPush :: FilePath -> Remote -> IO ()
filesystemPush cwd remote = do
    let remotePath = remoteUrl remote
    putStrLn $ "Pushing to filesystem remote: " ++ remotePath
    
    -- 1. Check if remote has .bit/ directory (first push vs subsequent)
    let remoteBitDir = remotePath </> ".bit"
    remoteHasBit <- Dir.doesDirectoryExist remoteBitDir
    
    unless remoteHasBit $ do
        putStrLn "First push: initializing bit repo at remote..."
        initializeRepoAt remotePath
    
    -- 2. Fetch local into remote
    let localIndexGit = cwd </> ".bit" </> "index" </> ".git"
    let remoteIndex = remotePath </> ".bit" </> "index"
    
    putStrLn "Fetching local commits into remote..."
    (fetchCode, _fetchOut, fetchErr) <- Git.runGitAt remoteIndex 
        ["fetch", localIndexGit, "main:refs/remotes/origin/main"]
    
    when (fetchCode /= ExitSuccess) $ do
        hPutStrLn stderr $ "Error fetching into remote: " ++ fetchErr
        exitWith fetchCode
    
    -- 3. Capture remote HEAD before merge
    (oldHeadCode, oldHeadOut, _) <- Git.runGitAt remoteIndex ["rev-parse", "HEAD"]
    let mOldHead = if oldHeadCode == ExitSuccess 
                   then Just (filter (not . isSpace) oldHeadOut)
                   else Nothing
    
    -- 4. Check if remote HEAD is ancestor of what we're pushing (fast-forward check)
    case mOldHead of
        Just _oldHead -> do
            (checkCode, _, _) <- Git.runGitAt remoteIndex 
                ["merge-base", "--is-ancestor", "HEAD", "refs/remotes/origin/main"]
            when (checkCode /= ExitSuccess) $ do
                hPutStrLn stderr "error: Remote has local commits that you don't have."
                hPutStrLn stderr "hint: Run 'bit pull' to merge remote changes first, then push again."
                exitWith (ExitFailure 1)
        Nothing -> pure ()  -- First push, no check needed
    
    -- 5. Merge at remote (ff-only)
    putStrLn "Merging at remote (fast-forward only)..."
    (mergeCode, _mergeOut, mergeErr) <- Git.runGitAt remoteIndex 
        ["merge", "--ff-only", "refs/remotes/origin/main"]
    
    if mergeCode /= ExitSuccess
        then do
            hPutStrLn stderr $ "error: Failed to merge at remote: " ++ mergeErr
            exitWith (ExitFailure 1)
        else do
            -- 6. Get new HEAD at remote
            (newHeadCode, newHeadOut, _) <- Git.runGitAt remoteIndex ["rev-parse", "HEAD"]
            when (newHeadCode /= ExitSuccess) $ do
                hPutStrLn stderr "Error: Could not get remote HEAD after merge"
                exitWith (ExitFailure 1)
            
            let newHead = filter (not . isSpace) newHeadOut
            
            -- 7. Sync actual files based on what changed
            maybe (do
                    -- First push: sync all files from new HEAD
                    putStrLn "First push: syncing all files to remote..."
                    filesystemSyncAllFiles cwd remotePath newHead)
                (\oldHead -> do
                    -- Subsequent push: sync only changed files
                    putStrLn "Syncing changed files to remote..."
                    filesystemSyncChangedFiles cwd remotePath oldHead newHead)
                mOldHead
            
            -- 8. Update local tracking ref
            putStrLn "Updating local tracking ref..."
            void $ Git.updateRemoteTrackingBranchToHead
            
            putStrLn "Push complete."

-- ============================================================================
-- Push helper functions
-- ============================================================================

-- | Push the git bundle to remote. Uses bracket to ensure temp bundle cleanup.
pushBundle :: Remote -> IO ()
pushBundle remote = do
    let tempBundle = BundleName "bit"
        tempBundleCwdPath = fromCwdPath (bundleCwdPath tempBundle)

    -- bracket <setup> <cleanup> <action>
    bracket
        (Git.createBundle tempBundle) -- 1. Acquire
        (\_ -> cleanupTemp tempBundleCwdPath) -- 2. Release (Always runs)
        (\gCode -> do                 -- 3. Work
            if gCode /= ExitSuccess
                then hPutStrLn stderr "Error creating bundle"
                else uploadToRemote tempBundleCwdPath remote
        )

-- Helper for the upload logic to keep the bracket clean
uploadToRemote :: FilePath -> Remote -> IO ()
uploadToRemote src remote = do
    putStrLn "Uploading bundle to remote..."
    rCode <- Transport.copyToRemote src remote ".bit/bit.bundle"
    if rCode == ExitSuccess
        then putStrLn "Metadata push complete."
        else hPutStrLn stderr "Error uploading bundle."

-- Helper for cleanup that doesn't crash if the file was never made
cleanupTemp :: FilePath -> IO ()
cleanupTemp filePath = do
    exists <- Dir.doesFileExist filePath
    when exists (Dir.removeFile filePath)

-- | Sync files, push bundle, and update local tracking. Used after remote checks pass.
pushToRemote :: Remote -> BitM ()
pushToRemote remote = do
  syncRemoteFiles
  liftIO $ pushBundle remote
  updateLocalBundleAfterPush

-- | After a successful push, update the local fetched_remote.bundle to current HEAD
-- so rgit status shows up to date instead of "ahead of remote".
updateLocalBundleAfterPush :: BitM ()
updateLocalBundleAfterPush = do
    code <- liftIO $ Git.createBundle fetchedBundle
    when (code == ExitSuccess) $ do
        void $ liftIO $ Git.updateRemoteTrackingBranch fetchedBundle

syncRemoteFiles :: BitM ()
syncRemoteFiles = withRemote $ \remote -> do
    cwd <- asks envCwd
    localFiles <- asks envLocalFiles
    remoteResult <- liftIO $ Remote.Scan.fetchRemoteFiles remote
    either
        (\_ -> liftIO $ hPutStrLn stderr "Error: Failed to fetch remote file list.")
        (\remoteFiles -> do
            let actions = Pipeline.pushSyncFiles localFiles remoteFiles
            liftIO $ putStrLn "--- Pushing Changes to Remote ---"
            if null actions
                then liftIO $ putStrLn "Remote is already up to date."
                else do
                    -- Create progress tracker for cloud operations (file-count only)
                    progress <- liftIO $ CopyProgress.newSyncProgress (length actions)
                    liftIO $ CopyProgress.withSyncProgressReporter progress $ do
                        -- Use lower concurrency for network/subprocess operations
                        caps <- getNumCapabilities
                        let concurrency = min 8 (max 2 (caps * 2))
                        void $ runConcurrentlyBounded concurrency (\a -> do
                            executeCommand cwd remote a
                            CopyProgress.incrementFilesComplete progress
                            ) actions)
        remoteResult

processExistingRemote :: BitM ()
processExistingRemote = do
    fMode <- asks envForceMode
    mRemote <- asks envRemote
    case fMode of
      Force -> do
            lift $ tellErr "Warning: --force used. Overwriting remote history..."
            maybe (lift $ tellErr "Error: No remote configured.") pushToRemote mRemote
      ForceWithLease -> do
                    maybeRemoteHash <- liftIO $ Git.getHashFromBundle fetchedBundle
                    let fetchedPath = fromCwdPath (bundleCwdPath fetchedBundle)
                    hasFetchedBundle <- lift $ fileExistsE fetchedPath

                    case (maybeRemoteHash, hasFetchedBundle) of
                        (Just rHash, True) -> do
                            maybeFetchedHash <- liftIO $ Git.getHashFromBundle fetchedBundle
                            case maybeFetchedHash of
                                Just fileHash | rHash == fileHash -> do
                                    lift $ tell "Remote check passed (--force-with-lease). Proceeding with push..."
                                    maybe (lift $ tellErr "Error: No remote configured.") pushToRemote mRemote
                                Just _fHash -> lift $ do
                                    tellErr "---------------------------------------------------"
                                    tellErr "ERROR: Remote has changed since last fetch!"
                                    tellErr "Someone else pushed to the remote."
                                    tellErr "Run 'bit fetch' to update your local view of the remote."
                                    tellErr "---------------------------------------------------"
                                Nothing -> lift $ tellErr "Error: Could not extract hash from fetched bundle."
                        (Just _, False) -> do
                            lift $ tellErr "Warning: No local fetched bundle found. Proceeding with push (--force-with-lease)..."
                            maybe (lift $ tellErr "Error: No remote configured.") pushToRemote mRemote
                        (Nothing, _) -> lift $ tellErr "Error: Could not extract hash from remote bundle."
      NoForce -> do
                    maybeRemoteHash <- liftIO $ Git.getHashFromBundle fetchedBundle
                    maybeLocalHash <- lift getLocalHeadE

                    case (maybeLocalHash, maybeRemoteHash) of
                        (Just lHash, Just rHash) -> do
                            isAhead <- lift $ checkIsAheadE rHash lHash

                            if isAhead
                                then do
                                    lift $ tell "Remote check passed. Proceeding with push..."
                                    maybe (lift $ tellErr "Error: No remote configured.") pushToRemote mRemote
                                else lift $ do
                                    tellErr "---------------------------------------------------"
                                    tellErr "ERROR: Remote history has diverged or is ahead!"
                                    tellErr "Please run 'bit pull' before pushing."
                                    tellErr "---------------------------------------------------"

                        _ -> lift $ tellErr "Error: Could not extract hashes for comparison."
