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
    , DivergentFile(..)
    , findDivergentFiles
    , createConflictDirectories
    , printConflictList
    ) where

import Prelude hiding (log)
import System.FilePath ((</>), normalise, takeDirectory)
import Control.Monad (when, unless, void, forM_)
import Data.Foldable (traverse_)
import System.Exit (ExitCode(..), exitWith)
import qualified Internal.Git as Git
import qualified Internal.Transport as Transport
import Internal.Config (bitIndexPath, bundleForRemote)
import qualified Bit.Verify as Verify
import qualified Bit.Conflict as Conflict
import qualified Bit.Remote.Scan as Remote.Scan
import qualified Data.List as List
import qualified Data.Map as Map
import System.IO (stderr, hPutStrLn)
import Control.Exception (try, SomeException, throwIO)
import Bit.Utils (toPosix, filterOutBitPaths, trimGitOutput, shortRefDisplay)
import Data.Maybe (maybeToList)
import Bit.Remote (Remote, remoteName, remoteUrl, RemotePath(..))
import Bit.Types (BitM, BitEnv(..), ForceMode(..), Hash, HashAlgo(..), FileEntry(..), EntryKind(..), syncHash, runBitM, unPath)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Bit.Internal.Metadata (MetaContent(..), serializeMetadata, displayHash, validateMetadataDir)
import Bit.Concurrency (Concurrency(..))
import Bit.Core.Helpers
    ( PullMode(..)
    , PullOptions(..)
    , defaultPullOptions
    , isFilesystemRemote
    , withRemote
    , getLocalHeadE
    , gitQuery
    , gitRaw
    , tell
    , tellErr
    , fileExistsE
    , createDirE
    , copyFileE
    , writeFileAtomicE
    , printVerifyIssue
    , formatVerifiedRemoteFiles
    , checkFilesystemRemoteIsRepo
    )
import Bit.Core.Transport
    ( syncAllFilesFromHEAD
    , applyMergeToWorkingDir
    )
import Bit.Core.Fetch (fetchRemoteBundle, saveFetchedBundle, FetchOutcome(..), printFetchBanner)

-- ============================================================================
-- Pull operations
-- ============================================================================

hintPullAcceptRemote :: String
hintPullAcceptRemote = "hint: Run 'bit pull --accept-remote' to accept the remote's actual state."

-- | Print verify hint, pull-accept-remote hint, and exit with failure.
dieRemoteVerifyFailed :: String -> IO ()
dieRemoteVerifyFailed hintVerify = do
    hPutStrLn stderr hintVerify
    hPutStrLn stderr hintPullAcceptRemote
    exitWith (ExitFailure 1)

pull :: PullOptions -> BitM ()
pull opts = withRemote $ \remote -> do
    cwd <- asks envCwd

    -- Determine if this is a filesystem or cloud remote
    isFs <- isFilesystemRemote remote
    if isFs then liftIO $ filesystemPull cwd remote opts else cloudPull remote opts

-- | Pull from a cloud remote.
cloudPull :: Remote -> PullOptions -> BitM ()
cloudPull remote opts =
    case pullMode opts of
        PullAcceptRemote -> pullAcceptRemoteImpl remote
        PullManualMerge  -> pullManualMergeImpl remote
        PullNormal       -> pullWithCleanup remote opts

-- | Pull from a filesystem remote using named git remote.
filesystemPull :: FilePath -> Remote -> PullOptions -> IO ()
filesystemPull cwd remote opts = do
    let name = remoteName remote
        rp = RemotePath (remoteUrl remote)
        remotePath = unRemotePath rp
    putStrLn $ "Pulling from filesystem remote: " ++ remotePath

    -- Check if remote has .bit/ directory
    checkFilesystemRemoteIsRepo rp

    -- 1. Ensure git remote URL is current and fetch
    void $ Git.addRemote name (remotePath </> ".bit" </> "index")

    putStrLn "Fetching remote commits..."
    (fetchCode, _fetchOut, fetchErr) <- Git.runGitWithOutput ["fetch", name]

    when (fetchCode /= ExitSuccess) $ do
        hPutStrLn stderr $ "Error fetching from remote: " ++ fetchErr
        exitWith fetchCode

    printFetchBanner name (name ++ "/main")

    -- 3. Get remote HEAD hash
    (remoteHeadCode, remoteHeadOut, _) <- Git.runGitWithOutput ["rev-parse", Git.remoteTrackingRef name]
    when (remoteHeadCode /= ExitSuccess) $ do
        hPutStrLn stderr "Error: Could not get remote HEAD"
        exitWith (ExitFailure 1)

    let remoteHash = trimGitOutput remoteHeadOut

    -- Proof of possession — always verify filesystem remote before pulling
    unless (pullMode opts == PullAcceptRemote) $ do
        putStrLn "Verifying remote repository..."
        result <- Verify.verifyLocalAt remotePath Nothing (Parallel 0)
        if null result.vrIssues
            then putStrLn $ formatVerifiedRemoteFiles result.vrCount
            else do
                hPutStrLn stderr $ "error: Remote working tree does not match remote metadata (" ++ show (length result.vrIssues) ++ " issues)."
                mapM_ (printVerifyIssue id) result.vrIssues
                dieRemoteVerifyFailed "hint: Run 'bit verify' in the remote repo to see all mismatches."

    -- Create a minimal BitEnv to call the shared logic
    let env = BitEnv cwd (Just remote) NoForce

    -- Delegate to the unified path
    case pullMode opts of
        PullAcceptRemote -> runBitM env (filesystemPullAcceptRemoteImpl remotePath name remoteHash)
        _                -> runBitM env (filesystemPullLogicImpl remotePath remote remoteHash)

-- | Filesystem pull logic (simplified - no bundle fetching, just merge + sync)
filesystemPullLogicImpl :: String -> Remote -> String -> BitM ()
filesystemPullLogicImpl remoteRoot remote remoteHash = do
    cwd <- asks envCwd
    oldHash <- lift getLocalHeadE
    let name = remoteName remote

    case oldHash of
        Nothing -> do
            lift $ putStrLn $ "Checking out " ++ shortRefDisplay remoteHash ++ " (first pull)"
            checkoutCode <- lift $ Git.checkoutRemoteAsMain name
            case checkoutCode of
                ExitSuccess -> lift $ do
                    syncAllFilesFromHEAD remoteRoot cwd
                    putStrLn "Syncing binaries... done."
                    void $ Git.updateRemoteTrackingBranchToHash name remoteHash
                _ -> lift $ hPutStrLn stderr "Error: Failed to checkout remote branch."

        Just localHash -> do
            (mergeCode, mergeOut, mergeErr) <- lift $ Git.runGitWithOutput
                ["merge", "--no-commit", "--no-ff", Git.remoteTrackingRef name]

            (finalMergeCode, finalMergeOut, finalMergeErr) <-
                lift $ if mergeCode /= ExitSuccess && "refusing to merge unrelated histories" `List.isInfixOf` (mergeOut ++ mergeErr)
                then do
                    putStrLn "Merging unrelated histories..."
                    Git.runGitWithOutput ["merge", "--no-commit", "--no-ff", "--allow-unrelated-histories", Git.remoteTrackingRef name]
                else pure (mergeCode, mergeOut, mergeErr)

            case finalMergeCode of
                ExitSuccess -> do
                    lift $ putStrLn $ "Updating " ++ shortRefDisplay localHash ++ ".." ++ shortRefDisplay remoteHash
                    lift $ putStrLn "Merge made by the 'recursive' strategy."
                    -- Always commit when MERGE_HEAD exists — never guard with hasStagedChanges.
                    -- See spec.md "Design Decisions" #9: git commit succeeds when MERGE_HEAD
                    -- is present even if the tree is unchanged. Skipping would leave MERGE_HEAD
                    -- dangling and break the next push.
                    lift $ void $ Git.runGitRaw ["commit", "-m", "Merge remote"]
                    -- CRITICAL: Always read actual HEAD after merge, never use remoteHash
                    lift $ applyMergeToWorkingDir remoteRoot cwd localHash
                    lift $ putStrLn "Syncing binaries... done."
                    lift $ void $ Git.updateRemoteTrackingBranchToHash name remoteHash
                _ -> do
                    lift $ do
                        putStrLn finalMergeOut
                        hPutStrLn stderr finalMergeErr
                        putStrLn "Automatic merge failed."
                        putStrLn "bit requires you to pick a version for each conflict."
                        putStrLn ""
                        putStrLn "Resolving conflicts..."

                    conflicts <- lift Conflict.getConflictedFilesE
                    resolutions <- lift $ Conflict.resolveAll conflicts
                    let total = length resolutions

                    invalid <- lift $ validateMetadataDir (cwd </> bitIndexPath)
                    unless (null invalid) $ lift $ do
                        void $ Git.runGitRaw ["merge", "--abort"]
                        hPutStrLn stderr Conflict.conflictMarkersFatalMessage
                        throwIO (userError "Invalid metadata")

                    conflictsNow <- lift Conflict.getConflictedFilesE
                    when (null conflictsNow) $ lift $ do
                        void $ Git.runGitRaw ["commit", "-m", "Merge remote (resolved " ++ show total ++ " conflict(s))"]
                        putStrLn $ "Merge complete. " ++ show total ++ " conflict(s) resolved."
                        -- CRITICAL: Always read actual HEAD after merge, never use remoteHash
                        applyMergeToWorkingDir remoteRoot cwd localHash
                        putStrLn "Syncing binaries... done."
                        void $ Git.updateRemoteTrackingBranchToHash name remoteHash

-- | Filesystem pull --accept-remote implementation
filesystemPullAcceptRemoteImpl :: String -> String -> String -> BitM ()
filesystemPullAcceptRemoteImpl remoteRoot name remoteHash = do
    cwd <- asks envCwd
    lift $ putStrLn "Accepting remote file state as truth..."

    -- Record current HEAD before checkout
    oldHead <- lift getLocalHeadE

    -- Force-checkout the remote branch
    checkoutCode <- lift $ Git.checkoutRemoteAsMain name
    case checkoutCode of
        ExitSuccess -> do
            -- Sync actual files based on what changed
            maybe (lift $ syncAllFilesFromHEAD remoteRoot cwd)
                  (\oh -> lift $ applyMergeToWorkingDir remoteRoot cwd oh) oldHead

            -- Update tracking ref
            lift $ do
                void $ Git.updateRemoteTrackingBranchToHash name remoteHash
                putStrLn "Pull with --accept-remote completed."
        _ -> lift $ hPutStrLn stderr "Error: Failed to checkout remote state."

-- | Pull with --accept-remote: force-checkout the remote branch, then sync files.
-- Git manages .bit/index/ (the metadata); we only sync actual files to the working tree.
pullAcceptRemoteImpl :: Remote -> BitM ()
pullAcceptRemoteImpl remote = do
    cwd <- asks envCwd
    let name = remoteName remote
        remoteRoot = remoteUrl remote
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
            lift $ tell "Scanning remote files..."
            checkoutCode <- lift $ Git.checkoutRemoteAsMain name
            case checkoutCode of
                ExitSuccess -> do
                    -- 4. Sync actual files to working tree based on what changed in git
                    (_remoteCode, remoteOut, _) <- lift $ gitQuery ["rev-parse", Git.remoteTrackingRef name]
                    let _newHash = takeWhile (/= '\n') remoteOut
                    maybe (lift $ syncAllFilesFromHEAD remoteRoot cwd)
                          (\oh -> lift $ applyMergeToWorkingDir remoteRoot cwd oh) oldHead

                    -- 5. Update tracking ref
                    maybeRemoteHash <- lift $ Git.getHashFromBundle (bundleForRemote name)
                    lift $ traverse_ (void . Git.updateRemoteTrackingBranchToHash name) maybeRemoteHash

                    lift $ tell "Pull with --accept-remote completed."
                _ -> lift $ tellErr "Error: Failed to checkout remote state."

-- | Pull with --manual-merge: detect remote divergence and create conflict directories.
pullManualMergeImpl :: Remote -> BitM ()
pullManualMergeImpl remote = do
    cwd <- asks envCwd
    let name = remoteName remote
    lift $ tell "Fetching remote metadata... done."

    maybeBundlePath <- lift $ fetchRemoteBundle remote
    case maybeBundlePath of
        Nothing -> lift $ tellErr "Error: Could not fetch remote bundle."
        Just bPath -> do
            outcome <- lift $ saveFetchedBundle remote (Just bPath)
            case outcome of
                FetchError err -> lift $ tellErr $ "Error: " ++ err
                _ -> pure ()  -- No need to render fetch output during pull

            entries <- lift $ Verify.loadMetadataFromBundle (bundleForRemote name)
            let remoteMeta = Verify.binaryEntries entries
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
                        remoteMetaMap = Map.fromList [(normalise (unPath m.bfmPath), (m.bfmHash, m.bfmSize)) | m <- remoteMeta]
                        localMetaMap = Map.fromList [(normalise (unPath m.bfmPath), (m.bfmHash, m.bfmSize)) | m <- localMeta]

                    lift $ tell "Comparing..."
                    let divergentFiles = findDivergentFiles remoteFileMap remoteMetaMap

                    if null divergentFiles
                        then do
                            lift $ tell "No remote divergence detected. Proceeding with normal pull..."
                            pullWithCleanup remote defaultPullOptions
                        else do
                            _oldHash <- lift getLocalHeadE
                            (_remoteCode, remoteOut, _) <- lift $ gitQuery ["rev-parse", Git.remoteTrackingRef name]
                            let _newHash = takeWhile (/= '\n') remoteOut

                            (mergeCode, mergeOut, mergeErr) <- lift $ gitQuery ["merge", "--no-commit", "--no-ff", Git.remoteTrackingRef name]
                            (_finalMergeCode, _, _) <- lift $ if mergeCode /= ExitSuccess && "refusing to merge unrelated histories" `List.isInfixOf` (mergeOut ++ mergeErr)
                                then do tell "Merging unrelated histories (e.g. first pull)..."; gitQuery ["merge", "--no-commit", "--no-ff", "--allow-unrelated-histories", Git.remoteTrackingRef name]
                                else pure (mergeCode, mergeOut, mergeErr)

                            createConflictDirectories remote divergentFiles localMetaMap

                            lift $ printConflictList divergentFiles localMetaMap
                            lift $ do
                                tell ""
                                tell "To resolve:"
                                tell "  1. Examine files in .bit/conflicts/<path>/"
                                tell "  2. Copy your chosen version to <path>"
                                tell "  3. Run 'bit add <path>'"
                                tell "  4. Run 'bit merge --continue'"
                                tell ""
                                tell "Or abort: 'bit merge --abort'"

-- | Pull with cleanup: abort merge on failure.
pullWithCleanup :: Remote -> PullOptions -> BitM ()
pullWithCleanup remote opts = do
    env <- asks id
    result <- liftIO $ try @SomeException (runBitM env (pullLogic remote opts))
    either (\ex -> do
            inProgress <- lift $ Git.isMergeInProgress
            if inProgress
                then lift $ do
                    void $ gitRaw ["merge", "--abort"]
                    tell "Merge aborted. Your working tree is unchanged."
                else lift $ throwIO ex)
        (const $ pure ()) result

pullLogic :: Remote -> PullOptions -> BitM ()
pullLogic remote _opts = do
    cwd <- asks envCwd
    let name = remoteName remote
        remoteRoot = remoteUrl remote
    maybeBundlePath <- lift $ fetchRemoteBundle remote
    case maybeBundlePath of
        Nothing -> pure ()
        Just bPath -> do
            outcome <- lift $ saveFetchedBundle remote (Just bPath)
            case outcome of
                FetchError err -> lift $ tellErr $ "Error: " ++ err
                _ -> pure ()  -- No need to render fetch output during pull
            (_, countOut, _) <- lift $ gitQuery ["rev-list", "--count", Git.remoteTrackingRef name]
            let n = takeWhile (`elem` ['0'..'9']) (filter (/= '\n') countOut)
            lift $ tell $ "remote: Counting objects: " ++ (if null n then "0" else n) ++ ", done."

            -- Proof of possession — always verify remote before pulling
            lift $ putStrLn "Verifying remote files..."
            result <- lift $ Verify.verifyRemote cwd remote Nothing (Parallel 0)
            if null result.vrIssues
                then lift $ putStrLn $ formatVerifiedRemoteFiles result.vrCount
                else lift $ do
                    hPutStrLn stderr $ "error: Remote files do not match remote metadata (" ++ show (length result.vrIssues) ++ " issues)."
                    mapM_ (printVerifyIssue id) result.vrIssues
                    dieRemoteVerifyFailed "hint: Run 'bit --remote <name> verify' to see all mismatches."

            oldHash <- lift getLocalHeadE
            (_remoteCode, remoteOut, _) <- lift $ gitQuery ["rev-parse", Git.remoteTrackingRef name]
            let newHash = takeWhile (/= '\n') remoteOut

            case oldHash of
                Nothing -> do
                    lift $ tell $ "Checking out " ++ shortRefDisplay newHash ++ " (first pull)"
                    checkoutCode <- lift $ Git.checkoutRemoteAsMain name
                    case checkoutCode of
                        ExitSuccess -> lift $ do
                            syncAllFilesFromHEAD remoteRoot cwd
                            tell "Syncing binaries... done."
                        _ -> lift $ tellErr "Error: Failed to checkout remote branch."

                Just localHead -> do
                    (mergeCode, mergeOut, mergeErr) <- lift $ gitQuery ["merge", "--no-commit", "--no-ff", Git.remoteTrackingRef name]

                    (finalMergeCode, finalMergeOut, finalMergeErr) <-
                        lift $ if mergeCode /= ExitSuccess && "refusing to merge unrelated histories" `List.isInfixOf` (mergeOut ++ mergeErr)
                        then do tell "Merging unrelated histories..."; gitQuery ["merge", "--no-commit", "--no-ff", "--allow-unrelated-histories", Git.remoteTrackingRef name]
                        else pure (mergeCode, mergeOut, mergeErr)

                    case finalMergeCode of
                      ExitSuccess -> do
                        lift $ do
                            tell $ "Updating " ++ shortRefDisplay localHead ++ ".." ++ shortRefDisplay newHash
                            tell "Merge made by the 'recursive' strategy."
                        -- Always commit when MERGE_HEAD exists — never guard with hasStagedChanges.
                        -- When the tree is unchanged (e.g. identical content on both sides),
                        -- git commit still succeeds because MERGE_HEAD is present, and skipping
                        -- the commit would leave MERGE_HEAD dangling (breaking the next push).
                        lift $ void $ gitRaw ["commit", "-m", "Merge remote"]
                        lift $ do
                            applyMergeToWorkingDir remoteRoot cwd localHead
                            tell "Syncing binaries... done."
                        maybeRemoteHash <- lift $ Git.getHashFromBundle (bundleForRemote name)
                        lift $ traverse_ (void . Git.updateRemoteTrackingBranchToHash name) maybeRemoteHash
                      _ -> do
                        lift $ do
                            tell finalMergeOut
                            tellErr finalMergeErr
                            tell "Automatic merge failed."
                            tell "bit requires you to pick a version for each conflict."
                            tell ""
                            tell "Resolving conflicts..."

                        conflicts <- lift Conflict.getConflictedFilesE
                        resolutions <- lift $ Conflict.resolveAll conflicts
                        let total = length resolutions

                        invalid <- lift $ validateMetadataDir (cwd </> bitIndexPath)
                        unless (null invalid) $ lift $ do
                            void $ gitRaw ["merge", "--abort"]
                            tellErr Conflict.conflictMarkersFatalMessage
                            throwIO (userError "Invalid metadata")

                        conflictsNow <- lift Conflict.getConflictedFilesE
                        when (null conflictsNow) $ lift $ do
                            void $ gitRaw ["commit", "-m", "Merge remote (resolved " ++ show total ++ " conflict(s))"]
                            tell $ "Merge complete. " ++ show total ++ " conflict(s) resolved."
                            applyMergeToWorkingDir remoteRoot cwd localHead
                            tell "Syncing binaries... done."
                            void $ Git.updateRemoteTrackingBranchToHash name newHash

-- ============================================================================
-- Helper types and functions
-- ============================================================================

-- | A file where remote actual content doesn't match remote metadata.
-- Prevents transposition bugs vs bare (FilePath, Hash, Hash, Integer, Integer) tuple.
data DivergentFile = DivergentFile
    { dfPath         :: FilePath
    , dfExpectedHash :: Hash 'MD5
    , dfActualHash   :: Hash 'MD5
    , dfExpectedSize :: Integer
    , dfActualSize   :: Integer
    }
    deriving (Show, Eq)

-- | Find files where remote actual files don't match remote metadata.
findDivergentFiles :: Map.Map FilePath (Hash 'MD5, EntryKind) -> Map.Map FilePath (Hash 'MD5, Integer) -> [DivergentFile]
findDivergentFiles remoteFileMap remoteMetaMap =
    Map.foldlWithKey (\acc filePath (expectedHash, expectedSize) ->
        let normalizedPath = normalise filePath
        in case Map.lookup normalizedPath remoteFileMap of
            Nothing -> acc
            Just (actualHash, entryKind) ->
                case entryKind of
                    File _ actualSize _ ->
                        if actualHash == expectedHash && actualSize == expectedSize
                            then acc
                            else DivergentFile filePath expectedHash actualHash expectedSize actualSize : acc
                    _ -> acc
        ) [] remoteMetaMap

-- | Create conflict directories for divergent files.
createConflictDirectories :: Remote -> [DivergentFile] -> Map.Map FilePath (Hash 'MD5, Integer) -> BitM ()
createConflictDirectories remote divergentFiles localMetaMap = do
    cwd <- asks envCwd
    let conflictsDir = cwd </> ".bit" </> "conflicts"
    lift $ createDirE conflictsDir

    forM_ divergentFiles $ \df -> do
        let conflictDir = conflictsDir </> df.dfPath
        lift $ createDirE (takeDirectory conflictDir)

        let localPath = cwd </> df.dfPath
        localExists <- lift $ fileExistsE localPath
        when localExists $ lift $ copyFileE localPath (conflictDir </> "LOCAL")

        code <- liftIO $ Transport.copyFromRemote remote (toPosix df.dfPath) (conflictDir </> "REMOTE")
        when (code /= ExitSuccess) $ lift $ tellErr $ "Warning: Could not download remote file: " ++ df.dfPath

        lift $ case Map.lookup (normalise df.dfPath) localMetaMap of
            Just (localHash, localSize) ->
                writeFileAtomicE (conflictDir </> "METADATA_LOCAL") $
                    serializeMetadata (MetaContent localHash localSize)
            Nothing -> writeFileAtomicE (conflictDir </> "METADATA_LOCAL") "hash: (not tracked)\nsize: 0\n"

        lift $ writeFileAtomicE (conflictDir </> "METADATA_REMOTE") $
            serializeMetadata (MetaContent df.dfActualHash df.dfActualSize)

-- | Print conflict list in spec format.
printConflictList :: [DivergentFile] -> Map.Map FilePath (Hash 'MD5, Integer) -> IO ()
printConflictList divergentFiles localMetaMap = do
    putStrLn ""
    putStrLn "✗ Remote divergence detected:"
    putStrLn ""

    forM_ divergentFiles $ \df -> do
        putStrLn $ "  " ++ toPosix df.dfPath ++ ":"

        let localInfo = case Map.lookup (normalise df.dfPath) localMetaMap of
                Just (localHash, localSize) -> (displayHash localHash, show localSize)
                Nothing -> ("(not tracked)", "0")

        putStrLn $ "    Local:           " ++ fst localInfo ++ " (" ++ snd localInfo ++ " bytes)"
        putStrLn $ "    Remote actual:   " ++ displayHash df.dfActualHash ++ " (" ++ show df.dfActualSize ++ " bytes)"
        putStrLn $ "    Remote metadata: " ++ displayHash df.dfExpectedHash ++ " (" ++ show df.dfExpectedSize ++ " bytes)"
        putStrLn $ ""
        putStrLn $ "    Files saved to: .bit/conflicts/" ++ toPosix df.dfPath ++ "/"
        putStrLn ""

    putStrLn "This can happen when:"
    putStrLn "  - Files were modified directly on the remote (not via bit)"
    putStrLn "  - A partial push from another client"
    putStrLn "  - Remote storage corruption"
