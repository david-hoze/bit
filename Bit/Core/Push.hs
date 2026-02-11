{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
import qualified Bit.Platform as Platform
import System.FilePath ((</>))
import Control.Monad (when, unless, void, forM_)
import Data.Foldable (traverse_)
import System.Exit (ExitCode(..), exitWith)
import qualified Internal.Git as Git
import qualified Internal.Transport as Transport
import Internal.Config (fetchedBundle, BundleName(..), bundleCwdPath, fromCwdPath)
import Bit.Utils (trimGitOutput, toPosix)
import qualified Bit.Pipeline as Pipeline
import qualified Bit.Remote.Scan as Remote.Scan
import qualified Data.List as List
import System.IO (stderr, hPutStrLn)
import Control.Exception (bracket)
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
    , fileExistsE
    , tell
    , tellErr
    , printVerifyIssue
    , safeRemove
    )
import Bit.Core.Init (initializeRemoteRepoAt)
import Bit.Core.Transport (executeCommand, filesystemSyncAllFiles, filesystemSyncChangedFiles)
import Bit.Core.Fetch (classifyRemoteState, fetchBundle)

-- ============================================================================
-- Push operations
-- ============================================================================

push :: BitM ()
push = withRemote $ \remote -> do
    cwd <- asks envCwd

    -- Proof of possession — always verify local before pushing
    liftIO $ putStrLn "Verifying local files..."
    result <- liftIO $ Verify.verifyLocal cwd Nothing (Parallel 0)
    if null result.vrIssues
        then liftIO $ putStrLn $ "Verified " ++ show result.vrCount ++ " files. All match metadata."
        else liftIO $ do
            hPutStrLn stderr $ "error: Working tree does not match metadata (" ++ show (length result.vrIssues) ++ " issues)."
            mapM_ (printVerifyIssue id) result.vrIssues  -- full hash, no truncation
            hPutStrLn stderr "hint: Run 'bit verify' to see all mismatches."
            hPutStrLn stderr "hint: Run 'bit add' to update metadata, or 'bit restore' to restore files."
            exitWith (ExitFailure 1)

    -- Determine if this is a filesystem or cloud remote
    isFs <- isFilesystemRemote remote
    if isFs then liftIO $ filesystemPush cwd remote else cloudPush remote

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

        StateNonRgitOccupied samples ->
            case fMode of
                Force -> do
                    liftIO $ hPutStrLn stderr "Warning: --force used. Overwriting non-bit remote..."
                    syncRemoteFiles
                    liftIO $ pushBundle remote
                    updateLocalBundleAfterPush
                _ -> liftIO $ do
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
    let rp = RemotePath (remoteUrl remote)
        remotePath = unRemotePath rp
    putStrLn $ "Pushing to filesystem remote: " ++ remotePath

    -- 1. Check if remote has .bit/ directory (first push vs subsequent)
    let remoteBitDir = remotePath </> ".bit"
    remoteHasBit <- Platform.doesDirectoryExist remoteBitDir

    unless remoteHasBit $ do
        putStrLn "First push: initializing bit repo at remote..."
        initializeRemoteRepoAt rp

    -- 2. Fetch local into remote (at the remote, "origin" is the local side)
    let localIndexGit = cwd </> ".bit" </> "index" </> ".git"
    let remoteIndex = remotePath </> ".bit" </> "index"

    putStrLn "Fetching local commits into remote..."
    (fetchCode, _fetchOut, fetchErr) <- Git.runGitAt remoteIndex
        ["fetch", localIndexGit, "main:" ++ Git.remoteTrackingRef "origin"]

    when (fetchCode /= ExitSuccess) $ do
        hPutStrLn stderr $ "Error fetching into remote: " ++ fetchErr
        exitWith fetchCode

    -- 3. Capture remote HEAD before merge
    (oldHeadCode, oldHeadOut, _) <- Git.runGitAt remoteIndex ["rev-parse", "HEAD"]
    let mOldHead = case oldHeadCode of
            ExitSuccess -> Just (trimGitOutput oldHeadOut)
            _ -> Nothing

    -- 4. Check if remote HEAD is ancestor of what we're pushing (fast-forward check)
    traverse_ (const $ do
        (checkCode, _, _) <- Git.runGitAt remoteIndex
            ["merge-base", "--is-ancestor", "HEAD", Git.remoteTrackingRef "origin"]
        when (checkCode /= ExitSuccess) $ do
            hPutStrLn stderr "error: Remote has local commits that you don't have."
            hPutStrLn stderr "hint: Run 'bit pull' to merge remote changes first, then push again."
            exitWith (ExitFailure 1)
        ) mOldHead

    -- 5. Merge at remote (ff-only)
    putStrLn "Merging at remote (fast-forward only)..."
    (mergeCode, _mergeOut, mergeErr) <- Git.runGitAt remoteIndex
        ["merge", "--ff-only", Git.remoteTrackingRef "origin"]

    case mergeCode of
        ExitSuccess -> do
            -- 6. Get new HEAD at remote
            (newHeadCode, newHeadOut, _) <- Git.runGitAt remoteIndex ["rev-parse", "HEAD"]
            when (newHeadCode /= ExitSuccess) $ do
                hPutStrLn stderr "Error: Could not get remote HEAD after merge"
                exitWith (ExitFailure 1)

            let newHead = trimGitOutput newHeadOut

            -- 7. Sync actual files based on what changed
            case mOldHead of
                Nothing -> do
                    -- First push: sync all files from new HEAD
                    putStrLn "First push: syncing all files to remote..."
                    filesystemSyncAllFiles cwd rp newHead
                Just oldHead -> do
                    -- Subsequent push: sync only changed files
                    putStrLn "Syncing changed files to remote..."
                    filesystemSyncChangedFiles cwd rp oldHead newHead

            -- 8. Update local tracking ref
            putStrLn "Updating local tracking ref..."
            void $ Git.updateRemoteTrackingBranchToHead (remoteName remote)

            putStrLn "Push complete."
        _ -> do
            hPutStrLn stderr $ "error: Failed to merge at remote: " ++ mergeErr
            exitWith (ExitFailure 1)

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
        (Git.createBundle tempBundle)              -- 1. Acquire
        (const $ cleanupTemp tempBundleCwdPath)    -- 2. Release (Always runs)
        (\case                                     -- 3. Work
            ExitSuccess -> uploadToRemote tempBundleCwdPath remote
            _ -> hPutStrLn stderr "Error creating bundle"
        )

-- Helper for the upload logic to keep the bracket clean
uploadToRemote :: FilePath -> Remote -> IO ()
uploadToRemote src remote = do
    putStrLn "Uploading bundle to remote..."
    rCode <- Transport.copyToRemote src remote ".bit/bit.bundle"
    case rCode of
        ExitSuccess -> putStrLn "Metadata push complete."
        _ -> hPutStrLn stderr "Error uploading bundle."

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
-- so bit status shows up to date instead of "ahead of remote".
updateLocalBundleAfterPush :: BitM ()
updateLocalBundleAfterPush = do
    mRemote <- asks envRemote
    code <- liftIO $ Git.createBundle fetchedBundle
    case code of
        ExitSuccess -> case mRemote of
            Just remote -> void $ liftIO $ Git.updateRemoteTrackingBranchToHead (remoteName remote)
            Nothing     -> pure ()
        _ -> pure ()

syncRemoteFiles :: BitM ()
syncRemoteFiles = withRemote $ \remote -> do
    cwd <- asks envCwd
    localFiles <- asks envLocalFiles
    remoteResult <- liftIO $ Remote.Scan.fetchRemoteFiles remote
    either
        (const $ liftIO $ hPutStrLn stderr "Error: Failed to fetch remote file list.")
        (\remoteFiles -> do
            let actions = Pipeline.pushSyncFiles localFiles remoteFiles
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
                            CopyProgress.incrementFilesComplete progress)
        remoteResult
  where
    isCopy (Copy _ _) = True
    isCopy _          = False

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
                            isAhead <- lift $ checkIsAheadE (AncestorQuery { aqAncestor = rHash, aqDescendant = lHash })

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
