{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bit.Core.Pull
    ( pull
    , cloudPull
    , filesystemPull
    , filesystemPullLogicImpl
    , filesystemPullAcceptRemoteImpl
    , pullAcceptRemoteImpl
    , pullManualMergeImpl
    , pullWithCleanup
    , pullLogic
    , findDivergentFiles
    , createConflictDirectories
    , printConflictList
    ) where

import Prelude hiding (log)
import qualified System.Directory as Dir
import System.FilePath ((</>), normalise, takeDirectory)
import Control.Monad (when, unless, void, forM_)
import Data.Foldable (traverse_)
import System.Exit (ExitCode(..), exitWith)
import qualified Internal.Git as Git
import qualified Internal.Transport as Transport
import Internal.Config (bitIndexPath, fetchedBundle)
import Data.Char (isSpace)
import qualified Bit.Scan as Scan
import qualified Bit.Verify as Verify
import qualified Bit.Conflict as Conflict
import qualified Bit.Remote.Scan as Remote.Scan
import qualified Data.List as List
import qualified Data.Map as Map
import System.IO (stderr, hPutStrLn)
import Control.Exception (try, SomeException, throwIO)
import Bit.Utils (toPosix, filterOutBitPaths)
import Data.Maybe (maybeToList)
import Bit.Remote (Remote, remoteName, remoteUrl)
import Bit.Types (BitM, BitEnv(..), ForceMode(..), Hash, HashAlgo(..), EntryKind(..), syncHash, runBitM, unPath)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Bit.Internal.Metadata (MetaContent(..), serializeMetadata, displayHash, validateMetadataDir)
import Bit.Concurrency (Concurrency(..))
import qualified Bit.Device as Device
import Bit.Core.Helpers
    ( PullMode(..)
    , PullOptions(..)
    , defaultPullOptions
    , getRemoteTargetType
    , withRemote
    , getLocalHeadE
    , hasStagedChangesE
    , gitQuery
    , gitRaw
    , tell
    , tellErr
    , fileExistsE
    , createDirE
    , copyFileE
    , writeFileAtomicE
    , printVerifyIssue
    )
import Bit.Core.Transport
    ( FileTransport
    , mkCloudTransport
    , mkFilesystemTransport
    , applyMergeToWorkingDir
    , transportSyncAllFiles
    )
import Bit.Core.Fetch (fetchRemoteBundle, saveFetchedBundle, FetchOutcome(..))

-- ============================================================================
-- Pull operations
-- ============================================================================

pull :: PullOptions -> BitM ()
pull opts = withRemote $ \remote -> do
    cwd <- asks envCwd
    
    -- Determine if this is a filesystem or cloud remote
    mTarget <- liftIO $ getRemoteTargetType cwd (remoteName remote)
    case mTarget of
        Just (Device.TargetDevice _ _) -> liftIO $ filesystemPull cwd remote opts
        Just (Device.TargetLocalPath _) -> liftIO $ filesystemPull cwd remote opts
        _ -> cloudPull remote opts  -- Cloud remote or no target info (use cloud flow)

-- | Pull from a cloud remote (uses unified transport abstraction).
cloudPull :: Remote -> PullOptions -> BitM ()
cloudPull remote opts =
    let transport = mkCloudTransport remote
    in case pullMode opts of
        PullAcceptRemote -> pullAcceptRemoteImpl transport remote
        PullManualMerge  -> pullManualMergeImpl remote
        PullNormal       -> pullWithCleanup transport remote opts

-- | Pull from a filesystem remote. Fetches directly from the remote's .bit/index/.git.
filesystemPull :: FilePath -> Remote -> PullOptions -> IO ()
filesystemPull cwd remote opts = do
    let remotePath = remoteUrl remote
    putStrLn $ "Pulling from filesystem remote: " ++ remotePath
    
    -- Check if remote has .bit/ directory
    let remoteBitDir = remotePath </> ".bit"
    remoteHasBit <- Dir.doesDirectoryExist remoteBitDir
    unless remoteHasBit $ do
        hPutStrLn stderr "error: Remote is not a bit repository."
        exitWith (ExitFailure 1)
    
    -- 1. Fetch remote into local
    let remoteIndexGit = remotePath </> ".bit" </> "index" </> ".git"
    
    putStrLn "Fetching remote commits..."
    (fetchCode, _fetchOut, fetchErr) <- Git.runGitWithOutput 
        ["fetch", remoteIndexGit, "main:refs/remotes/origin/main"]
    
    when (fetchCode /= ExitSuccess) $ do
        hPutStrLn stderr $ "Error fetching from remote: " ++ fetchErr
        exitWith fetchCode
    
    -- Output fetch results similar to cloud pull
    hPutStrLn stderr $ "From " ++ remoteName remote
    hPutStrLn stderr $ " * [new branch]      main       -> origin/main"
    
    -- 3. Get remote HEAD hash
    (remoteHeadCode, remoteHeadOut, _) <- Git.runGitWithOutput ["rev-parse", "refs/remotes/origin/main"]
    when (remoteHeadCode /= ExitSuccess) $ do
        hPutStrLn stderr "Error: Could not get remote HEAD"
        exitWith (ExitFailure 1)
    
    let remoteHash = filter (not . isSpace) remoteHeadOut
    
    -- NEW: Proof of possession — verify filesystem remote before pulling
    unless (pullMode opts == PullAcceptRemote || pullSkipVerify opts) $ do
        putStrLn "Verifying remote repository..."
        (remoteCount, remoteIssues) <- Verify.verifyLocalAt remotePath Nothing (Parallel 0)
        if null remoteIssues
            then putStrLn $ "Verified " ++ show remoteCount ++ " remote files."
            else do
                hPutStrLn stderr $ "error: Remote working tree does not match remote metadata (" ++ show (length remoteIssues) ++ " issues)."
                mapM_ (printVerifyIssue id) remoteIssues
                hPutStrLn stderr "hint: Run 'bit verify' in the remote repo to see all mismatches."
                hPutStrLn stderr "hint: Run 'bit pull --accept-remote' to accept the remote's actual state."
                hPutStrLn stderr "hint: Run 'bit push --force' to overwrite remote with local state."
                exitWith (ExitFailure 1)
    
    -- 4. Build transport and delegate to unified pull logic
    let transport = mkFilesystemTransport remotePath
    
    -- Create a minimal BitEnv to call the shared logic
    localFiles <- Scan.scanWorkingDir cwd
    let env = BitEnv cwd localFiles (Just remote) NoForce (pullSkipVerify opts)
    
    -- Delegate to the unified path
    case pullMode opts of
        PullAcceptRemote -> runBitM env (filesystemPullAcceptRemoteImpl transport remoteHash)
        _                -> runBitM env (filesystemPullLogicImpl transport remote remoteHash)

-- | Filesystem pull logic (simplified - no bundle fetching, just merge + sync)
filesystemPullLogicImpl :: FileTransport -> Remote -> String -> BitM ()
filesystemPullLogicImpl transport _remote remoteHash = do
    cwd <- asks envCwd
    oldHash <- lift getLocalHeadE
    
    case oldHash of
        Nothing -> do
            lift $ putStrLn $ "Checking out " ++ take 7 remoteHash ++ " (first pull)"
            checkoutCode <- lift $ Git.checkoutRemoteAsMain
            if checkoutCode == ExitSuccess
                then do
                    lift $ transportSyncAllFiles transport cwd
                    lift $ putStrLn "Syncing binaries... done."
                    lift $ void $ Git.updateRemoteTrackingBranchToHash remoteHash
                else lift $ hPutStrLn stderr "Error: Failed to checkout remote branch."
        
        Just localHash -> do
            (mergeCode, mergeOut, mergeErr) <- lift $ Git.runGitWithOutput 
                ["merge", "--no-commit", "--no-ff", "refs/remotes/origin/main"]
            
            (finalMergeCode, finalMergeOut, finalMergeErr) <-
                lift $ if mergeCode /= ExitSuccess && "refusing to merge unrelated histories" `List.isInfixOf` (mergeOut ++ mergeErr)
                then do
                    putStrLn "Merging unrelated histories..."
                    Git.runGitWithOutput ["merge", "--no-commit", "--no-ff", "--allow-unrelated-histories", "refs/remotes/origin/main"]
                else pure (mergeCode, mergeOut, mergeErr)
            
            if finalMergeCode == ExitSuccess
                then do
                    lift $ putStrLn $ "Updating " ++ take 7 localHash ++ ".." ++ take 7 remoteHash
                    lift $ putStrLn "Merge made by the 'recursive' strategy."
                    hasChanges <- lift hasStagedChangesE
                    when hasChanges $ lift $ void $ Git.runGitRaw ["commit", "-m", "Merge remote"]
                    -- CRITICAL: Always read actual HEAD after merge, never use remoteHash
                    lift $ applyMergeToWorkingDir transport cwd localHash
                    lift $ putStrLn "Syncing binaries... done."
                    lift $ void $ Git.updateRemoteTrackingBranchToHash remoteHash
                else do
                    lift $ putStrLn finalMergeOut
                    lift $ hPutStrLn stderr finalMergeErr
                    lift $ putStrLn "Automatic merge failed."
                    lift $ putStrLn "bit requires you to pick a version for each conflict."
                    lift $ putStrLn ""
                    lift $ putStrLn "Resolving conflicts..."
                    
                    conflicts <- lift Conflict.getConflictedFilesE
                    resolutions <- lift $ Conflict.resolveAll conflicts
                    let total = length resolutions
                    
                    invalid <- lift $ validateMetadataDir (cwd </> bitIndexPath)
                    unless (null invalid) $ do
                        lift $ void $ Git.runGitRaw ["merge", "--abort"]
                        lift $ hPutStrLn stderr "fatal: Metadata files contain conflict markers. Merge aborted."
                        lift $ throwIO (userError "Invalid metadata")
                    
                    conflictsNow <- lift Conflict.getConflictedFilesE
                    if null conflictsNow
                        then do
                            lift $ void $ Git.runGitRaw ["commit", "-m", "Merge remote (resolved " ++ show total ++ " conflict(s))"]
                            lift $ putStrLn $ "Merge complete. " ++ show total ++ " conflict(s) resolved."
                            -- CRITICAL: Always read actual HEAD after merge, never use remoteHash
                            lift $ applyMergeToWorkingDir transport cwd localHash
                            lift $ putStrLn "Syncing binaries... done."
                            lift $ void $ Git.updateRemoteTrackingBranchToHash remoteHash
                        else pure ()

-- | Filesystem pull --accept-remote implementation
filesystemPullAcceptRemoteImpl :: FileTransport -> String -> BitM ()
filesystemPullAcceptRemoteImpl transport remoteHash = do
    cwd <- asks envCwd
    lift $ putStrLn "Accepting remote file state as truth..."
    
    -- Record current HEAD before checkout
    oldHead <- lift getLocalHeadE
    
    -- Force-checkout the remote branch
    checkoutCode <- lift Git.checkoutRemoteAsMain
    if checkoutCode /= ExitSuccess
        then lift $ hPutStrLn stderr "Error: Failed to checkout remote state."
        else do
            -- Sync actual files based on what changed
            case oldHead of
                Just oh -> lift $ applyMergeToWorkingDir transport cwd oh
                Nothing -> lift $ transportSyncAllFiles transport cwd
            
            -- Update tracking ref
            lift $ void $ Git.updateRemoteTrackingBranchToHash remoteHash
            lift $ putStrLn "Pull with --accept-remote completed."

-- | Pull with --accept-remote: force-checkout the remote branch, then sync files.
-- Git manages .bit/index/ (the metadata); we only sync actual files to the working tree.
pullAcceptRemoteImpl :: FileTransport -> Remote -> BitM ()
pullAcceptRemoteImpl transport remote = do
    cwd <- asks envCwd
    lift $ tell "Accepting remote file state as truth..."

    -- 1. Fetch the remote bundle so git has the remote's history
    maybeBundlePath <- lift $ fetchRemoteBundle remote
    case maybeBundlePath of
        Nothing -> lift $ tellErr "Error: Could not fetch remote bundle."
        Just bPath -> do
            outcome <- lift $ saveFetchedBundle remote (Just bPath)
            case outcome of
                FetchError err -> lift $ tellErr $ "Error: " ++ err
                _ -> pure ()  -- No need to render fetch output during pull

            -- 2. Record current HEAD before checkout (for diff-based sync)
            oldHead <- lift getLocalHeadE

            -- 3. Force-checkout the remote branch.
            --    This makes .bit/index/ match the remote's metadata exactly:
            --    text files get actual content, binary files get hash/size.
            lift $ tell "Scanning remote files..."
            checkoutCode <- lift Git.checkoutRemoteAsMain
            if checkoutCode /= ExitSuccess
                then lift $ tellErr "Error: Failed to checkout remote state."
                else do
                    -- 4. Sync actual files to working tree based on what changed in git
                    (_remoteCode, remoteOut, _) <- lift $ gitQuery ["rev-parse", "refs/remotes/origin/main"]
                    let _newHash = takeWhile (/= '\n') remoteOut
                    case oldHead of
                        Just oh -> lift $ applyMergeToWorkingDir transport cwd oh
                        Nothing -> lift $ transportSyncAllFiles transport cwd  -- First time, no diff available

                    -- 5. Update tracking ref
                    maybeRemoteHash <- lift $ Git.getHashFromBundle fetchedBundle
                    lift $ traverse_ (void . Git.updateRemoteTrackingBranchToHash) maybeRemoteHash

                    lift $ tell "Pull with --accept-remote completed."

-- | Pull with --manual-merge: detect remote divergence and create conflict directories.
pullManualMergeImpl :: Remote -> BitM ()
pullManualMergeImpl remote = do
    cwd <- asks envCwd
    lift $ tell "Fetching remote metadata... done."

    maybeBundlePath <- lift $ fetchRemoteBundle remote
    case maybeBundlePath of
        Nothing -> lift $ tellErr "Error: Could not fetch remote bundle."
        Just bPath -> do
            outcome <- lift $ saveFetchedBundle remote (Just bPath)
            case outcome of
                FetchError err -> lift $ tellErr $ "Error: " ++ err
                _ -> pure ()  -- No need to render fetch output during pull

            (remoteMeta, _allBundlePaths) <- lift $ Verify.loadMetadataFromBundle fetchedBundle
            lift $ tell "Scanning remote files... done."
            result <- lift $ Remote.Scan.fetchRemoteFiles remote
            case result of
                Left _ -> lift $ tellErr "Error: Could not fetch remote file list."
                Right remoteFiles -> do
                    let filteredRemoteFiles = filterOutBitPaths remoteFiles
                    localMeta <- lift $ Verify.loadBinaryMetadata (cwd </> bitIndexPath) (Parallel 0)

                    let remoteFileMap = Map.fromList
                          [ (normalise (unPath e.path), (h, e.kind))
                          | e <- filteredRemoteFiles
                          , h <- maybeToList (syncHash e.kind)
                          ]
                        remoteMetaMap = Map.fromList [(normalise (unPath p), (h, sz)) | (p, h, sz) <- remoteMeta]
                        localMetaMap = Map.fromList [(normalise (unPath p), (h, sz)) | (p, h, sz) <- localMeta]

                    lift $ tell "Comparing..."
                    let divergentFiles = findDivergentFiles remoteFileMap remoteMetaMap localMetaMap

                    if null divergentFiles
                        then do
                            lift $ tell "No remote divergence detected. Proceeding with normal pull..."
                            let transport = mkCloudTransport remote
                            pullWithCleanup transport remote defaultPullOptions
                        else do
                            _oldHash <- lift getLocalHeadE
                            (_remoteCode, remoteOut, _) <- lift $ gitQuery ["rev-parse", "refs/remotes/origin/main"]
                            let _newHash = takeWhile (/= '\n') remoteOut

                            (mergeCode, mergeOut, mergeErr) <- lift $ gitQuery ["merge", "--no-commit", "--no-ff", "refs/remotes/origin/main"]
                            (_finalMergeCode, _, _) <- lift $ if mergeCode /= ExitSuccess && "refusing to merge unrelated histories" `List.isInfixOf` (mergeOut ++ mergeErr)
                                then do tell "Merging unrelated histories (e.g. first pull)..."; gitQuery ["merge", "--no-commit", "--no-ff", "--allow-unrelated-histories", "refs/remotes/origin/main"]
                                else pure (mergeCode, mergeOut, mergeErr)

                            createConflictDirectories remote divergentFiles remoteFileMap remoteMetaMap localMetaMap

                            lift $ printConflictList divergentFiles remoteFileMap remoteMetaMap localMetaMap
                            lift $ do
                                tell ""
                                tell "To resolve:"
                                tell "  1. Examine files in .bit/conflicts/<path>/"
                                tell "  2. Copy your chosen version to <path>"
                                tell "  3. Run 'bit add <path>'"
                                tell "  4. Run 'bit merge --continue'"
                                tell ""
                                tell "Or abort: 'bit merge --abort'"

-- | Filesystem pull logic (simplified - no bundle fetching, just merge + sync)
pullWithCleanup :: FileTransport -> Remote -> PullOptions -> BitM ()
pullWithCleanup transport remote opts = do
    env <- asks id
    result <- liftIO $ try @SomeException (runBitM env (pullLogic transport remote opts))
    case result of
        Left ex -> do
            inProgress <- lift $ Git.isMergeInProgress
            if inProgress
                then do
                    lift $ void $ gitRaw ["merge", "--abort"]
                    lift $ tell "Merge aborted. Your working tree is unchanged."
                else lift $ throwIO ex
        Right _ -> pure ()

pullLogic :: FileTransport -> Remote -> PullOptions -> BitM ()
pullLogic transport remote opts = do
    cwd <- asks envCwd
    maybeBundlePath <- lift $ fetchRemoteBundle remote
    case maybeBundlePath of
        Nothing -> pure ()
        Just bPath -> do
            outcome <- lift $ saveFetchedBundle remote (Just bPath)
            case outcome of
                FetchError err -> lift $ tellErr $ "Error: " ++ err
                _ -> pure ()  -- No need to render fetch output during pull
            (_, countOut, _) <- lift $ gitQuery ["rev-list", "--count", "refs/remotes/origin/main"]
            let n = takeWhile (`elem` ['0'..'9']) (filter (/= '\n') countOut)
            lift $ tell $ "remote: Counting objects: " ++ (if null n then "0" else n) ++ ", done."

            -- NEW: Proof of possession — verify remote before pulling
            unless (pullSkipVerify opts) $ do
                lift $ putStrLn "Verifying remote files..."
                (remoteFileCount, remoteIssues) <- lift $ Verify.verifyRemote cwd remote Nothing (Parallel 0)
                if null remoteIssues
                    then lift $ putStrLn $ "Verified " ++ show remoteFileCount ++ " remote files."
                    else do
                        lift $ hPutStrLn stderr $ "error: Remote files do not match remote metadata (" ++ show (length remoteIssues) ++ " issues)."
                        lift $ mapM_ (printVerifyIssue id) remoteIssues
                        lift $ hPutStrLn stderr "hint: Run 'bit verify --remote' to see all mismatches."
                        lift $ hPutStrLn stderr "hint: Run 'bit pull --accept-remote' to accept the remote's actual state."
                        lift $ hPutStrLn stderr "hint: Run 'bit push --force' to overwrite remote with local state."
                        lift $ exitWith (ExitFailure 1)

            oldHash <- lift getLocalHeadE
            (_remoteCode, remoteOut, _) <- lift $ gitQuery ["rev-parse", "refs/remotes/origin/main"]
            let newHash = takeWhile (/= '\n') remoteOut

            case oldHash of
                Nothing -> do
                    lift $ tell $ "Checking out " ++ take 7 newHash ++ " (first pull)"
                    checkoutCode <- lift $ Git.checkoutRemoteAsMain
                    if checkoutCode == ExitSuccess
                        then do
                            lift $ transportSyncAllFiles transport cwd
                            lift $ tell "Syncing binaries... done."
                        else lift $ tellErr "Error: Failed to checkout remote branch."

                Just localHead -> do
                    (mergeCode, mergeOut, mergeErr) <- lift $ gitQuery ["merge", "--no-commit", "--no-ff", "refs/remotes/origin/main"]

                    (finalMergeCode, finalMergeOut, finalMergeErr) <-
                        lift $ if mergeCode /= ExitSuccess && "refusing to merge unrelated histories" `List.isInfixOf` (mergeOut ++ mergeErr)
                        then do tell "Merging unrelated histories..."; gitQuery ["merge", "--no-commit", "--no-ff", "--allow-unrelated-histories", "refs/remotes/origin/main"]
                        else pure (mergeCode, mergeOut, mergeErr)

                    if finalMergeCode == ExitSuccess
                    then do
                        lift $ tell $ "Updating " ++ take 7 localHead ++ ".." ++ take 7 newHash
                        lift $ tell "Merge made by the 'recursive' strategy."
                        hasChanges <- lift hasStagedChangesE
                        when hasChanges $ lift $ void $ gitRaw ["commit", "-m", "Merge remote"]
                        lift $ applyMergeToWorkingDir transport cwd localHead
                        lift $ tell "Syncing binaries... done."
                        maybeRemoteHash <- lift $ Git.getHashFromBundle fetchedBundle
                        lift $ traverse_ (void . Git.updateRemoteTrackingBranchToHash) maybeRemoteHash
                    else do
                        lift $ tell finalMergeOut
                        lift $ tellErr finalMergeErr
                        lift $ tell "Automatic merge failed."
                        lift $ tell "bit requires you to pick a version for each conflict."
                        lift $ tell ""
                        lift $ tell "Resolving conflicts..."

                        conflicts <- lift Conflict.getConflictedFilesE
                        resolutions <- lift $ Conflict.resolveAll conflicts
                        let total = length resolutions

                        invalid <- lift $ validateMetadataDir (cwd </> bitIndexPath)
                        unless (null invalid) $ do
                            lift $ void $ gitRaw ["merge", "--abort"]
                            lift $ tellErr "fatal: Metadata files contain conflict markers. Merge aborted."
                            lift $ throwIO (userError "Invalid metadata")

                        conflictsNow <- lift Conflict.getConflictedFilesE
                        if null conflictsNow
                            then do
                                lift $ void $ gitRaw ["commit", "-m", "Merge remote (resolved " ++ show total ++ " conflict(s))"]
                                lift $ tell $ "Merge complete. " ++ show total ++ " conflict(s) resolved."
                                lift $ applyMergeToWorkingDir transport cwd localHead
                                lift $ tell "Syncing binaries... done."
                                lift $ void $ Git.updateRemoteTrackingBranchToHash newHash
                            else pure ()

-- ============================================================================
-- Helper functions
-- ============================================================================

-- | Find files where remote actual files don't match remote metadata.
findDivergentFiles :: Map.Map FilePath (Hash 'MD5, EntryKind) -> Map.Map FilePath (Hash 'MD5, Integer) -> Map.Map FilePath (Hash 'MD5, Integer) -> [(FilePath, Hash 'MD5, Hash 'MD5, Integer, Integer)]
findDivergentFiles remoteFileMap remoteMetaMap _localMetaMap =
    Map.foldlWithKey (\acc filePath (expectedHash, expectedSize) ->
        let normalizedPath = normalise filePath
        in case Map.lookup normalizedPath remoteFileMap of
            Nothing -> acc  -- File missing on remote, skip
            Just (actualHash, entryKind) ->
                case entryKind of
                    File _ actualSize _ ->
                        if actualHash == expectedHash && actualSize == expectedSize
                            then acc  -- Matches, no divergence
                            else (filePath, expectedHash, actualHash, expectedSize, actualSize) : acc  -- Divergence!
                    _ -> acc
        ) [] remoteMetaMap

-- | Create conflict directories for divergent files.
createConflictDirectories :: Remote -> [(FilePath, Hash 'MD5, Hash 'MD5, Integer, Integer)] -> Map.Map FilePath (Hash 'MD5, EntryKind) -> Map.Map FilePath (Hash 'MD5, Integer) -> Map.Map FilePath (Hash 'MD5, Integer) -> BitM ()
createConflictDirectories remote divergentFiles _remoteFileMap _remoteMetaMap localMetaMap = do
    cwd <- asks envCwd
    let conflictsDir = cwd </> ".bit" </> "conflicts"
    lift $ createDirE conflictsDir

    forM_ divergentFiles $ \(filePath, _expectedHash, actualHash, _expectedSize, actualSize) -> do
        let conflictDir = conflictsDir </> filePath
        lift $ createDirE (takeDirectory conflictDir)

        let localPath = cwd </> filePath
        localExists <- lift $ fileExistsE localPath
        when localExists $ lift $ copyFileE localPath (conflictDir </> "LOCAL")

        code <- liftIO $ Transport.copyFromRemote remote (toPosix filePath) (conflictDir </> "REMOTE")
        when (code /= ExitSuccess) $ lift $ tellErr $ "Warning: Could not download remote file: " ++ filePath

        lift $ case Map.lookup (normalise filePath) localMetaMap of
            Just (localHash, localSize) ->
                writeFileAtomicE (conflictDir </> "METADATA_LOCAL") $
                    serializeMetadata (MetaContent localHash localSize)
            Nothing -> writeFileAtomicE (conflictDir </> "METADATA_LOCAL") "hash: (not tracked)\nsize: 0\n"

        lift $ writeFileAtomicE (conflictDir </> "METADATA_REMOTE") $
            serializeMetadata (MetaContent actualHash actualSize)

-- | Print conflict list in spec format.
printConflictList :: [(FilePath, Hash 'MD5, Hash 'MD5, Integer, Integer)] -> Map.Map FilePath (Hash 'MD5, EntryKind) -> Map.Map FilePath (Hash 'MD5, Integer) -> Map.Map FilePath (Hash 'MD5, Integer) -> IO ()
printConflictList divergentFiles _remoteFileMap _remoteMetaMap localMetaMap = do
    putStrLn ""
    putStrLn "✗ Remote divergence detected:"
    putStrLn ""
    
    forM_ divergentFiles $ \(filePath, expectedHash, remoteHash, expectedSize, remoteSize) -> do
        putStrLn $ "  " ++ toPosix filePath ++ ":"
        
        -- Get local metadata (use displayHash for Hash 'MD5 values)
        let localInfo = case Map.lookup (normalise filePath) localMetaMap of
                Just (localHash, localSize) -> (displayHash localHash, show localSize)
                Nothing -> ("(not tracked)", "0")
        
        putStrLn $ "    Local:           " ++ fst localInfo ++ " (" ++ snd localInfo ++ " bytes)"
        putStrLn $ "    Remote actual:   " ++ displayHash remoteHash ++ " (" ++ show remoteSize ++ " bytes)"
        putStrLn $ "    Remote metadata: " ++ displayHash expectedHash ++ " (" ++ show expectedSize ++ " bytes)"
        putStrLn $ ""
        putStrLn $ "    Files saved to: .bit/conflicts/" ++ toPosix filePath ++ "/"
        putStrLn ""
    
    putStrLn "This can happen when:"
    putStrLn "  - Files were modified directly on the remote (not via bit)"
    putStrLn "  - A partial push from another client"
    putStrLn "  - Remote storage corruption"
