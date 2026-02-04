{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

module Bit.Core
    ( -- Repo initialization
      init
    , initializeRepoAt

      -- Git passthrough (these take args from the CLI)
    , add
    , commit
    , diff
    , log
    , lsFiles
    , restore
    , checkout
    , status
    , reset
    , rm
    , mv
    , branch
    , merge

      -- Core sync operations
    , push
    , pull
    , fetch

      -- Verification
    , verify
    , fsck

      -- Remote management
    , remoteAdd
    , remoteShow
    , remoteCheck

      -- Merge management
    , mergeContinue
    , mergeAbort

      -- Branch management
    , unsetUpstream

      -- Types re-exported for Commands.hs
    , PullOptions(..)
    , defaultPullOptions
    ) where

import qualified Data.List as List
import Data.List (isPrefixOf)
import qualified System.Directory as Dir
import System.Directory (copyFile, removeFile, createDirectoryIfMissing, removeDirectory, listDirectory, doesDirectoryExist)
import System.FilePath ((</>), normalise, takeDirectory)
import Control.Monad (when, unless, void, forM_)
import System.Exit (ExitCode(..), exitWith)
import qualified Data.Map as Map
import qualified Internal.Git as Git
import qualified Internal.Transport as Transport
import Internal.Config (bitDir, bitGitDir, fetchedBundle, bitIndexPath, bitDevicesDir, bitRemotesDir, bundleCwdPath, bundleGitRelPath, fromCwdPath, fromGitRelPath, BundleName(..))
import qualified Bit.Internal.Metadata as Metadata
import Bit.Internal.Metadata (MetaContent(..), parseMetadata, displayHash, serializeMetadata)
import Data.Char (isSpace)
import qualified Bit.Scan as Scan
import qualified Bit.Diff as Diff
import qualified Bit.Plan as Plan
import qualified Bit.Pipeline as Pipeline
import qualified Bit.Verify as Verify
import qualified Bit.Fsck as Fsck
import Bit.Plan (RcloneAction(..))
import Bit.Types
import qualified Bit.Remote.Scan as Remote.Scan
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Exception (try, throwIO, SomeException, IOException)
import System.IO (hFlush, stdout, stderr, hPutStrLn, hIsTerminalDevice)
import System.Process (readProcessWithExitCode)
import Data.Maybe (fromMaybe, listToMaybe, maybe, maybeToList, mapMaybe)
import Data.Either (either)
import Bit.Utils (toPosix, filterOutBitPaths)
import qualified Bit.Device as Device
import qualified Bit.DevicePrompt as DevicePrompt
import qualified Bit.Conflict as Conflict
import Bit.Remote (Remote, remoteName, displayRemote, resolveRemote, remoteUrl, RemoteState(..), FetchResult(..))
import Prelude hiding (init, log)
import Control.Exception (bracket)

-- ============================================================================
-- Types
-- ============================================================================

data PullOptions = PullOptions
    { pullAcceptRemote :: Bool
    , pullManualMerge :: Bool
    } deriving (Show)

defaultPullOptions :: PullOptions
defaultPullOptions = PullOptions False False

-- ============================================================================
-- Git passthrough (thin wrappers)
-- ============================================================================

add :: [String] -> IO ExitCode
add args = Git.runGitRaw ("add" : args)

commit :: [String] -> IO ExitCode
commit args = Git.runGitRaw ("commit" : args)

diff :: [String] -> IO ExitCode
diff args = Git.runGitRaw ("diff" : args)

log :: [String] -> IO ExitCode
log args = Git.runGitRaw ("log" : args)

lsFiles :: [String] -> IO ExitCode
lsFiles args = Git.runGitRaw ("ls-files" : args)

reset :: [String] -> IO ExitCode
reset args = Git.runGitRaw ("reset" : args)

rm :: [String] -> IO ExitCode
rm args = Git.runGitRaw ("rm" : args)

mv :: [String] -> IO ExitCode
mv args = Git.runGitRaw ("mv" : args)

branch :: [String] -> IO ExitCode
branch args = Git.runGitRaw ("branch" : args)

merge :: [String] -> IO ExitCode
merge args = Git.runGitRaw ("merge" : args)

-- ============================================================================
-- Plain IO functions (no env needed)
-- ============================================================================

init :: IO ()
init = initializeRepo

fsck :: FilePath -> IO ()
fsck = Fsck.doFsck

mergeAbort :: IO ()
mergeAbort = doMergeAbort

unsetUpstream :: IO ()
unsetUpstream = void Git.unsetBranchUpstream

remoteAdd :: String -> String -> IO ()
remoteAdd = addRemote

-- ============================================================================
-- Plain IO functions (no env needed)
-- ============================================================================

initializeRepo :: IO ()
initializeRepo = do
    cwd <- Dir.getCurrentDirectory
    putStrLn $ "Initializing bit in: " ++ cwd
    initializeRepoAt cwd
    putStrLn "bit initialized successfully!"

-- | Initialize a bit repository at the specified target directory.
-- This is used both for local `bit init` and for creating filesystem remotes.
initializeRepoAt :: FilePath -> IO ()
initializeRepoAt targetDir = do
    let targetBitDir = targetDir </> ".bit"
    let targetBitIndexPath = targetBitDir </> "index"
    let targetBitGitDir = targetBitIndexPath </> ".git"
    let targetBitDevicesDir = targetBitDir </> "devices"
    let targetBitRemotesDir = targetBitDir </> "remotes"

    -- 1. Create .bit directory
    Dir.createDirectoryIfMissing True targetBitDir

    -- 2. Create .bit/index directory (needed before git init)
    Dir.createDirectoryIfMissing True targetBitIndexPath

    -- 3. Init Git in the index directory
    hasGit <- Dir.doesDirectoryExist targetBitGitDir
    unless hasGit $ do
        -- Initialize git in .bit/index, which will create .bit/index/.git
        void $ Git.runGitAt targetBitIndexPath ["init"]
        
        -- Fix for Windows external/USB drives: add to safe.directory
        -- git 2.35.2+ rejects directories with different ownership
        absIndex <- Dir.makeAbsolute targetBitIndexPath
        let safePath = map (\c -> if c == '\\' then '/' else c) absIndex
        void $ readProcessWithExitCode "git" ["config", "--global", "--add", "safe.directory", safePath] ""

    -- 3a. Create .git/bundles directory for storing bundle files
    Dir.createDirectoryIfMissing True (targetBitGitDir </> "bundles")

    -- 4. Configure default branch name to "main" (for the repo we just created)
    -- This affects future branch operations in this repo
    void $ Git.runGitAt targetBitIndexPath ["config", "init.defaultBranch", "main"]
    
    -- 5. Rename the initial branch to "main" if it's "master"
    -- Git init creates "master" by default, so we rename it
    (code, _, _) <- Git.runGitAt targetBitIndexPath ["branch", "-m", "master", "main"]
    when (code /= ExitSuccess) $
        -- If rename failed (e.g., no commits yet), that's okay - first commit will use "main"
        return ()

    -- 6. Create other .bit subdirectories (index already created above)
    Dir.createDirectoryIfMissing True targetBitDevicesDir
    Dir.createDirectoryIfMissing True targetBitRemotesDir

    -- 5a. Create config file with default values
    let configPath = targetBitDir </> "config"
    configExists <- Dir.doesFileExist configPath
    unless configExists $ do
        let defaultConfig = unlines
                [ "[text]"
                , "    size-limit = 1048576  # 1MB, files larger are always binary"
                , "    extensions = .txt,.md,.yaml,.yml,.json,.xml,.html,.css,.js,.py,.hs,.rs"
                ]
        writeFile configPath defaultConfig

    -- 5b. Merge driver: prevent Git from writing conflict markers; bit resolves whole-file only
    -- Use .bit/index/.git/info/attributes instead of .gitattributes in the working tree
    -- This way it doesn't conflict with files from the remote on first pull
    -- Also disable text/CRLF conversion (-text) to prevent spurious "modified" status
    void $ Git.runGitAt targetBitIndexPath ["config", "merge.bit-metadata.name", "bit metadata"]
    void $ Git.runGitAt targetBitIndexPath ["config", "merge.bit-metadata.driver", "false"]
    Dir.createDirectoryIfMissing True (targetBitGitDir </> "info")
    writeFile (targetBitGitDir </> "info" </> "attributes") "* merge=bit-metadata -text\n"

    -- 5c. The .git/info/exclude file is created in Commands.hs from .bitignore
    -- (this happens before each scan, not during init)

    -- Note: We do NOT create an initial commit here.
    -- This keeps the repo empty until first real commit or pull.
    -- On first pull, we simply checkout the remote's history (no merge needed).

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
            when (name == "origin") $ void Git.setupBranchTracking
            putStrLn $ "Remote '" ++ name ++ "' added (" ++ url ++ ")."
        Device.FilesystemPath path -> addRemoteFilesystem cwd name path

promptDeviceName :: FilePath -> FilePath -> Maybe String -> IO String
promptDeviceName cwd _volRoot mLabel =
    DevicePrompt.acquireDeviceNameAuto mLabel $ \name -> (name `elem`) <$> Device.listDeviceNames cwd

addRemoteFilesystem :: FilePath -> String -> FilePath -> IO ()
addRemoteFilesystem cwd name path = do
    absPath <- Dir.makeAbsolute path
    exists <- Dir.doesDirectoryExist absPath
    unless exists $ do
        hPutStrLn stderr ("fatal: Path does not exist or is not accessible: " ++ path)
        exitWith (ExitFailure 1)
    volRoot <- Device.getVolumeRoot absPath
    let relPath = Device.getRelativePath volRoot absPath
    mStoreUuid <- Device.readBitStore volRoot
    mExistingDevice <- case mStoreUuid of
        Just u -> Device.findDeviceByUuid cwd u
        Nothing -> return Nothing
    result <- try @IOException $ case (mStoreUuid, mExistingDevice) of
        (Just _u, Just dev) -> do
            putStrLn $ "Using existing device '" ++ dev ++ "'."
            mInfo <- Device.readDeviceFile cwd dev
            let storeType = maybe Device.Physical Device.deviceType mInfo
            Device.writeRemoteFile cwd name (Device.TargetDevice dev relPath)
            when (name == "origin") $ void Git.setupBranchTracking
            putStrLn $ "Remote '" ++ name ++ "' → " ++ dev ++ ":" ++ relPath
            putStrLn $ "(using existing device '" ++ dev ++ "')"
            return ()
        (Just u, Nothing) -> do
            mLabel <- Device.getVolumeLabel volRoot
            deviceName' <- promptDeviceName cwd volRoot mLabel
            storeType' <- Device.detectStorageType volRoot
            mSerial <- case storeType' of
                Device.Physical -> Device.getHardwareSerial volRoot
                Device.Network -> return Nothing
            Device.writeDeviceFile cwd deviceName' (Device.DeviceInfo u storeType' mSerial)
            Device.writeRemoteFile cwd name (Device.TargetDevice deviceName' relPath)
            when (name == "origin") $ void Git.setupBranchTracking
            putStrLn $ "Remote '" ++ name ++ "' → " ++ deviceName' ++ ":" ++ relPath
            putStrLn $ "Device '" ++ deviceName' ++ "' registered (" ++ (case storeType' of Device.Physical -> "physical"; Device.Network -> "network") ++ ")."
            return ()
        (Nothing, _) -> do
            mLabel <- Device.getVolumeLabel volRoot
            deviceName' <- promptDeviceName cwd volRoot mLabel
            u <- Device.generateStoreUuid
            Device.writeBitStore volRoot u
            storeType' <- Device.detectStorageType volRoot
            mSerial <- case storeType' of
                Device.Physical -> Device.getHardwareSerial volRoot
                Device.Network -> return Nothing
            Device.writeDeviceFile cwd deviceName' (Device.DeviceInfo u storeType' mSerial)
            Device.writeRemoteFile cwd name (Device.TargetDevice deviceName' relPath)
            when (name == "origin") $ void Git.setupBranchTracking
            putStrLn $ "Remote '" ++ name ++ "' → " ++ deviceName' ++ ":" ++ relPath
            putStrLn $ "Device '" ++ deviceName' ++ "' registered (" ++ (case storeType' of Device.Physical -> "physical"; Device.Network -> "network") ++ ")."
            return ()
    case result of
        Right () -> return ()
        Left _err -> do
            -- Cannot create .bit-store at volume root (e.g. permission denied on C:\)
            -- Fall back to path-based storage for local directories
            Device.writeRemoteFile cwd name (Device.TargetLocalPath absPath)
            void $ Git.addRemote name absPath
            when (name == "origin") $ void Git.setupBranchTracking
            putStrLn $ "Remote '" ++ name ++ "' added (" ++ absPath ++ ")."

doMergeAbort :: IO ()
doMergeAbort = do
    cwd <- Dir.getCurrentDirectory
    let conflictsDir = cwd </> ".bit" </> "conflicts"
    
    -- Abort git merge
    code <- Git.mergeAbort
    if code /= ExitSuccess
        then do
            hPutStrLn stderr "error: no merge in progress."
            exitWith (ExitFailure 1)
        else do
            putStrLn "Merge aborted. Your working tree is unchanged."
            
            -- Clean up conflict directories
            conflictsExist <- Dir.doesDirectoryExist conflictsDir
            when conflictsExist $ do
                removeDirectoryRecursive conflictsDir
                putStrLn "Conflict directories cleaned up."

-- | Remove directory recursively (helper function).
removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive dir = do
    exists <- doesDirectoryExist dir
    when exists $ do
        contents <- listDirectory dir
        forM_ contents $ \item -> do
            let path = dir </> item
            isDir <- doesDirectoryExist path
            if isDir
                then removeDirectoryRecursive path
                else removeFile path
        removeDirectory dir

-- ============================================================================
-- Stateful passthrough (needs BitEnv)
-- ============================================================================

status :: [String] -> BitM ExitCode
status args = do
    liftIO $ Git.runGitRaw ("status" : args)

restore :: [String] -> BitM ExitCode
restore = doRestore

checkout :: [String] -> BitM ExitCode
checkout = doCheckout

-- ============================================================================
-- Core operations (full business logic)
-- ============================================================================

push :: BitM ()
push = withRemote $ \remote -> do
    cwd <- asks envCwd
    force <- asks envForce
    
    -- Determine if this is a filesystem or cloud remote
    mTarget <- liftIO $ getRemoteTargetType cwd (remoteName remote)
    case mTarget of
        Just (Device.TargetDevice _ _) -> liftIO $ filesystemPush cwd remote
        Just (Device.TargetLocalPath _) -> liftIO $ filesystemPush cwd remote
        _ -> cloudPush remote  -- Cloud remote or no target info (use cloud flow)

-- | Push to a cloud remote (original flow, unchanged).
cloudPush :: Remote -> BitM ()
cloudPush remote = do
    force <- asks envForce
    liftIO $ putStrLn $ "Inspecting remote: " ++ displayRemote remote
    state <- liftIO $ classifyRemoteState remote

    case state of
        StateEmpty -> do
            liftIO $ putStrLn "Remote is empty. Initializing..."
            syncRemoteFiles
            liftIO $ pushBundle remote
            updateLocalBundleAfterPush

        StateValidRgit -> do
            liftIO $ putStrLn "Remote is a bit repo. Checking history..."
            fetchResult <- liftIO $ fetchBundle remote
            case fetchResult of
                BundleFound bPath -> do
                    let fetchedPath = fromCwdPath (bundleCwdPath fetchedBundle)
                    liftIO $ copyFile bPath fetchedPath
                    liftIO $ safeRemove bPath
                    processExistingRemote
                _ -> liftIO $ hPutStrLn stderr "Error: Remote .bit found but metadata is missing."

        StateNonRgitOccupied samples -> do
            if force
                then do
                    liftIO $ hPutStrLn stderr "Warning: --force used. Overwriting non-bit remote..."
                    syncRemoteFiles
                    liftIO $ pushBundle remote
                    updateLocalBundleAfterPush
                else do
                    liftIO $ hPutStrLn stderr "-------------------------------------------------------"
                    liftIO $ hPutStrLn stderr "[!] STOP: Remote is NOT a bit repository!"
                    liftIO $ hPutStrLn stderr $ "Found existing files: " ++ List.intercalate ", " samples
                    liftIO $ hPutStrLn stderr "To initialize anyway (destructive): bit init --force"
                    liftIO $ hPutStrLn stderr "-------------------------------------------------------"

        StateNetworkError err ->
            liftIO $ hPutStrLn stderr $ "Aborting: Network error -> " ++ err

        StateCorruptedRgit msg ->
            liftIO $ hPutStrLn stderr $ "Aborting: [X] Corrupted remote -> " ++ msg

pull :: PullOptions -> BitM ()
pull opts = withRemote $ \remote -> do
    cwd <- asks envCwd
    
    -- Determine if this is a filesystem or cloud remote
    mTarget <- liftIO $ getRemoteTargetType cwd (remoteName remote)
    case mTarget of
        Just (Device.TargetDevice _ _) -> liftIO $ filesystemPull cwd remote opts
        Just (Device.TargetLocalPath _) -> liftIO $ filesystemPull cwd remote opts
        _ -> cloudPull remote opts  -- Cloud remote or no target info (use cloud flow)

-- | Pull from a cloud remote (original flow, unchanged).
cloudPull :: Remote -> PullOptions -> BitM ()
cloudPull remote opts =
    if pullAcceptRemote opts
        then pullAcceptRemoteImpl remote
        else if pullManualMerge opts
            then pullManualMergeImpl remote
            else pullWithCleanup remote

fetch :: BitM ()
fetch = withRemote $ \remote -> do
    mb <- liftIO $ fetchRemoteBundle remote
    liftIO $ saveFetchedBundle remote mb

verify :: Bool -> BitM ()
verify isRemote
  | isRemote = withRemote $ \remote -> do
      cwd <- asks envCwd
      liftIO $ putStrLn "Fetching remote metadata... done."
      liftIO $ putStrLn "Scanning remote files... done."
      liftIO $ putStrLn "Comparing..."
      (fileCount, issues) <- liftIO $ Verify.verifyRemote cwd remote
      liftIO $ putStrLn $ "Verifying " ++ show fileCount ++ " files..."
      if null issues
        then liftIO $ putStrLn "[OK] All files match metadata."
        else do
          liftIO $ mapM_ (printVerifyIssue (\s -> take 16 s ++ if length s > 16 then "..." else "")) issues
          liftIO $ putStrLn $ show (length issues) ++ " issues found."
  | otherwise = do
      cwd <- asks envCwd
      (fileCount, issues) <- liftIO $ Verify.verifyLocal cwd
      liftIO $ putStrLn $ "Verifying " ++ show fileCount ++ " files..."
      if null issues
        then liftIO $ putStrLn "[OK] All files match metadata."
        else do
          liftIO $ mapM_ (printVerifyIssue (\s -> take 16 s ++ if length s > 16 then "..." else "")) issues
          liftIO $ putStrLn $ show (length issues) ++ " issues found. Run 'bit status' for details."

remoteShow :: Maybe String -> BitM ()
remoteShow mRemoteName = do
    cwd <- asks envCwd
    name <- case mRemoteName of
        Just n -> return n
        Nothing -> liftIO Git.getTrackedRemoteName
    mRemote <- liftIO $ resolveRemote cwd name
    mTarget <- liftIO $ Device.readRemoteFile cwd name
    display <- liftIO $ case mTarget of
        Just _ -> formatRemoteDisplay cwd name mTarget
        Nothing -> return (name ++ " → " ++ maybe "(not configured)" displayRemote mRemote)
    case mRemote of
        Nothing -> do
            liftIO $ putStrLn "No remote configured. (Use 'bit remote add <name> <url>')"
        Just remote -> do
            liftIO $ putStrLn display
            liftIO $ putStrLn ""
            let fetchedPath = fromCwdPath (bundleCwdPath fetchedBundle)
            hasBundle <- liftIO $ Dir.doesFileExist fetchedPath
            if hasBundle
                then liftIO $ showRemoteStatusFromBundle name (Just (remoteUrl remote))
                else do
                    maybeBundlePath <- liftIO $ fetchRemoteBundle remote
                    case maybeBundlePath of
                        Just bPath -> do
                            liftIO $ saveFetchedBundle remote (Just bPath)
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
    (mRemote, name) <- liftIO $ case mName of
        Nothing -> do
            name <- Git.getTrackedRemoteName
            mRemote <- resolveRemote cwd name
            return (mRemote, name)
        Just name -> do
            mRemote <- resolveRemote cwd name
            return (mRemote, name)
    case mRemote of
        Nothing -> do
            liftIO $ if maybe True (const False) mName
                then hPutStrLn stderr "fatal: No remote configured."
                else hPutStrLn stderr $ "fatal: '" ++ fromMaybe "" mName ++ "' does not appear to be a git remote."
            liftIO $ hPutStrLn stderr "hint: Set remote with 'bit remote add <name> <url>'"
            liftIO $ exitWith (ExitFailure 1)
        Just remote -> do
            liftIO $ putStrLn $ "Checking local against remote: " ++ displayRemote remote
            liftIO $ putStrLn ""
            liftIO $ putStrLn "Using: rclone check --combined"
            res <- liftIO $ try @IOException (Transport.checkRemote cwd remote)
            case res of
                Left _ -> do
                    liftIO $ hPutStrLn stderr "fatal: rclone not found. Install rclone: https://rclone.org/install/"
                    liftIO $ exitWith (ExitFailure 1)
                Right cr -> do
                    let reportPath = cwd </> bitDir </> "last-check.txt"
                    liftIO $ createDirectoryIfMissing True (cwd </> bitDir)
                    liftIO $ writeFile reportPath (Transport.checkRawOutput cr)
                    let matches = Transport.checkMatches cr
                        differs = Transport.checkDiffers cr
                        missingDest = Transport.checkMissingDest cr
                        missingSrc = Transport.checkMissingSrc cr
                        errs = Transport.checkErrors cr
                        nMatch = length matches
                        hasDiff = not (null differs && null missingDest && null missingSrc && null errs)
                    if not hasDiff && Transport.checkExitCode cr == ExitSuccess
                        then do
                            liftIO $ putStrLn $ show nMatch ++ " files match between local and remote."
                            liftIO $ exitWith ExitSuccess
                        else if Transport.checkExitCode cr /= ExitSuccess && Transport.checkExitCode cr /= ExitFailure 1
                        then do
                            liftIO $ hPutStrLn stderr "fatal: Could not read from remote."
                            liftIO $ hPutStrLn stderr ""
                            liftIO $ hPutStrLn stderr "Please make sure you have the correct access rights"
                            liftIO $ hPutStrLn stderr "and the remote exists."
                            unless (null (Transport.checkStderr cr)) $ liftIO $ hPutStrLn stderr (Transport.checkStderr cr)
                            liftIO $ exitWith (ExitFailure 1)
                        else do
                            liftIO $ putStrLn ""
                            when (not (null differs)) $ forM_ ("  content differs:" : formatPathList differs) $ \s -> liftIO $ putStrLn s
                            when (not (null missingDest)) $ forM_ ("  local only (not on remote):" : formatPathList missingDest) $ \s -> liftIO $ putStrLn s
                            when (not (null missingSrc)) $ forM_ ("  remote only (not in local):" : formatPathList missingSrc) $ \s -> liftIO $ putStrLn s
                            when (not (null errs)) $ forM_ ("  errors:" : formatPathList errs) $ \s -> liftIO $ putStrLn s
                            let errWord = if length errs == 1 then "1 error" else show (length errs) ++ " errors"
                            liftIO $ putStrLn $ show (length differs + length missingDest + length missingSrc) ++ " differences, "
                                ++ errWord ++ ". " ++ show nMatch ++ " files matched."
                            liftIO $ putStrLn ""
                            liftIO $ hPutStrLn stderr "hint: Content differences may indicate an incomplete push or pull."
                            liftIO $ hPutStrLn stderr "hint: Run 'bit verify' and 'bit verify --remote' to check metadata consistency."
                            liftIO $ hPutStrLn stderr "hint: Full report saved to .bit/last-check.txt"
                            liftIO $ exitWith (ExitFailure 1)

mergeContinue :: BitM ()
mergeContinue = do
    cwd <- asks envCwd
    mRemote <- asks envRemote
    let conflictsDir = cwd </> ".bit" </> "conflicts"
    conflictsExist <- liftIO $ Dir.doesDirectoryExist conflictsDir

    gitConflicts <- liftIO Conflict.getConflictedFilesE

    if not (null gitConflicts)
        then liftIO $ hPutStrLn stderr "error: you have not resolved your conflicts yet."
        else if not conflictsExist
            then do
                (code, _, _) <- liftIO $ Git.runGitWithOutput ["rev-parse", "--verify", "MERGE_HEAD"]
                if code == ExitSuccess
                    then do
                        oldHead <- liftIO getLocalHeadE
                        liftIO $ void $ Git.runGitRaw ["commit", "-m", "Merge remote"]
                        liftIO $ putStrLn "Merge complete."
                        maybe (return ()) (\remote -> do
                            liftIO $ putStrLn "Syncing binaries... done."
                            case oldHead of
                                Just oh -> applyMergeToWorkingDir remote oh
                                Nothing -> syncRemoteFilesToLocal  -- fallback
                            maybeRemoteHash <- liftIO $ Git.getHashFromBundle fetchedBundle
                            case maybeRemoteHash of
                                Just rHash -> liftIO $ void $ Git.updateRemoteTrackingBranchToHash rHash
                                Nothing    -> return ()) mRemote
                    else do
                        liftIO $ hPutStrLn stderr "error: no merge in progress."
                        liftIO $ exitWith (ExitFailure 1)
            else do
                invalid <- liftIO $ Metadata.validateMetadataDir (cwd </> bitIndexPath)
                unless (null invalid) $ do
                    liftIO $ hPutStrLn stderr "fatal: Metadata files contain conflict markers. Merge aborted."
                    liftIO $ throwIO (userError "Invalid metadata")

                oldHead <- liftIO getLocalHeadE
                (code, _, _) <- liftIO $ Git.runGitWithOutput ["rev-parse", "--verify", "MERGE_HEAD"]
                when (code /= ExitSuccess) $ do
                    (mergeCode, _, _) <- liftIO $ Git.runGitWithOutput ["merge", "--no-commit", "--no-ff", "refs/remotes/origin/main"]
                    when (mergeCode /= ExitSuccess) $
                        liftIO $ hPutStrLn stderr "warning: Could not start merge. Proceeding anyway."

                liftIO $ void $ Git.runGitRaw ["commit", "-m", "Merge remote (manual merge resolved)"]
                liftIO $ putStrLn "Merge complete."

                liftIO $ removeDirectoryRecursive conflictsDir
                liftIO $ putStrLn "Conflict directories cleaned up."

                maybe (return ()) (\remote -> do
                    liftIO $ putStrLn "Syncing binaries... done."
                    case oldHead of
                        Just oh -> applyMergeToWorkingDir remote oh
                        Nothing -> syncRemoteFilesToLocal  -- fallback
                    maybeRemoteHash <- liftIO $ Git.getHashFromBundle fetchedBundle
                    case maybeRemoteHash of
                        Just rHash -> liftIO $ void $ Git.updateRemoteTrackingBranchToHash rHash
                        Nothing    -> return ()) mRemote

-- ============================================================================
-- Internal helpers (not exported, moved from Commands.hs)
-- ============================================================================

-- | Determine the remote target type from a remote name.
-- Returns the RemoteTarget if the remote is configured, Nothing otherwise.
getRemoteTargetType :: FilePath -> String -> IO (Maybe Device.RemoteTarget)
getRemoteTargetType cwd remoteName = Device.readRemoteFile cwd remoteName

-- Git helpers via effect layer
getLocalHeadE :: IO (Maybe String)
getLocalHeadE = do
    (code, out, _) <- Git.runGitWithOutput ["rev-parse", "HEAD"]
    return $ if code == ExitSuccess then Just (filter (not . isSpace) out) else Nothing

checkIsAheadE :: String -> String -> IO Bool
checkIsAheadE rHash lHash = do
    (code, _, _) <- Git.runGitWithOutput ["merge-base", "--is-ancestor", rHash, lHash]
    return (code == ExitSuccess)

hasStagedChangesE :: IO Bool
hasStagedChangesE = do
    (code, _, _) <- Git.runGitWithOutput ["diff", "--cached", "--quiet"]
    return (code == ExitFailure 1)

-- | True if the path is a text file in the index (content stored in metadata, not hash/size).
-- Used during pull to avoid re-downloading from rclone when content is already in the bundle.
isTextFileInIndex :: FilePath -> FilePath -> IO Bool
isTextFileInIndex localRoot path = do
    let metaPath = localRoot </> bitIndexPath </> path
    exists <- Dir.doesFileExist metaPath
    if not exists then return False
    else do
        mcontent <- readFileMaybe metaPath
        return $ case mcontent of
            Nothing -> False
            Just content -> not (any ("hash: " `isPrefixOf`) (lines content))

-- | Copy a file from the index to the working tree. Call only when the path
-- is a text file (content in index). Creates parent dirs as needed.
copyFromIndexToWorkTree :: FilePath -> FilePath -> IO ()
copyFromIndexToWorkTree localRoot path = do
    let metaPath = localRoot </> bitIndexPath </> path
        workPath = localRoot </> path
    createDirectoryIfMissing True (takeDirectory workPath)
    copyFile metaPath workPath

-- | Helper to read a file safely (returns Nothing on error)
readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path = do
    exists <- Dir.doesFileExist path
    if exists
        then Just <$> readFile path
        else return Nothing

-- ============================================================================
-- Compatibility helpers (effect system removal, Step 2)
-- ============================================================================

-- These are dumb IO wrappers to keep the refactor mechanical. They replace the
-- former Free-monad effect constructors.

tell :: String -> IO ()
tell = putStrLn

tellErr :: String -> IO ()
tellErr = hPutStrLn stderr

askUser :: String -> IO String
askUser prompt = do
    putStr prompt
    hFlush stdout
    getLine

gitRaw :: [String] -> IO ExitCode
gitRaw = Git.runGitRaw

gitQuery :: [String] -> IO (ExitCode, String, String)
gitQuery = Git.runGitWithOutput


readFileE :: FilePath -> IO (Maybe String)
readFileE = readFileMaybe

writeFileAtomicE :: FilePath -> String -> IO ()
writeFileAtomicE = writeFile

copyFileE :: FilePath -> FilePath -> IO ()
copyFileE = copyFile

fileExistsE :: FilePath -> IO Bool
fileExistsE = Dir.doesFileExist

dirExistsE :: FilePath -> IO Bool
dirExistsE = Dir.doesDirectoryExist

createDirE :: FilePath -> IO ()
createDirE = createDirectoryIfMissing True

removeFileE :: FilePath -> IO ()
removeFileE = Dir.removeFile

removeDirRecursiveE :: FilePath -> IO ()
removeDirRecursiveE = Dir.removeDirectoryRecursive

getCurrentDirE :: IO FilePath
getCurrentDirE = Dir.getCurrentDirectory

exitWithE :: ExitCode -> IO ()
exitWithE = exitWith

safeRemoveE :: FilePath -> IO ()
safeRemoveE = safeRemove

-- | Run an action with the remote, or print error if not configured.
withRemote :: (Remote -> BitM ()) -> BitM ()
withRemote action = do
  mRemote <- asks envRemote
  case mRemote of
    Nothing -> liftIO $ hPutStrLn stderr "Error: No remote configured."
    Just remote -> action remote

-- ============================================================================
-- Filesystem remote operations (new for filesystem remote feature)
-- ============================================================================

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
    (fetchCode, fetchOut, fetchErr) <- Git.runGitAt remoteIndex 
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
        Just oldHead -> do
            (checkCode, _, _) <- Git.runGitAt remoteIndex 
                ["merge-base", "--is-ancestor", "HEAD", "refs/remotes/origin/main"]
            when (checkCode /= ExitSuccess) $ do
                hPutStrLn stderr "error: Remote has local commits that you don't have."
                hPutStrLn stderr "hint: Run 'bit pull' to merge remote changes first, then push again."
                exitWith (ExitFailure 1)
        Nothing -> return ()  -- First push, no check needed
    
    -- 5. Merge at remote (ff-only)
    putStrLn "Merging at remote (fast-forward only)..."
    (mergeCode, mergeOut, mergeErr) <- Git.runGitAt remoteIndex 
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
            case mOldHead of
                Nothing -> do
                    -- First push: sync all files from new HEAD
                    putStrLn "First push: syncing all files to remote..."
                    filesystemSyncAllFiles cwd remotePath newHead
                Just oldHead -> do
                    -- Subsequent push: sync only changed files
                    putStrLn "Syncing changed files to remote..."
                    filesystemSyncChangedFiles cwd remotePath oldHead newHead
            
            -- 8. Update local tracking ref
            putStrLn "Updating local tracking ref..."
            void $ Git.updateRemoteTrackingBranchToHead
            
            putStrLn "Push complete."

-- | Sync all files from a commit to the filesystem remote (first push).
filesystemSyncAllFiles :: FilePath -> FilePath -> String -> IO ()
filesystemSyncAllFiles localRoot remotePath commitHash = do
    let remoteIndex = remotePath </> ".bit" </> "index"
    files <- Git.runGitAt remoteIndex ["ls-tree", "-r", "--name-only", commitHash]
    case files of
        (ExitSuccess, out, _) -> do
            let paths = filter (not . null) (lines out)
            forM_ paths $ \path -> do
                -- Check if it's a text file (content in index) or binary (hash/size in index)
                -- After git merge, ALL metadata files exist - we need to check content
                let metaPath = remoteIndex </> path
                isText <- isTextMetadataFile metaPath
                if isText
                    then do
                        -- Text file: metadata IS the content, copy from remote index to working tree
                        let workPath = remotePath </> path
                        createDirectoryIfMissing True (takeDirectory workPath)
                        copyFile metaPath workPath
                    else do
                        -- Binary file: metadata is hash/size, copy actual file from local working tree
                        let srcPath = localRoot </> path
                        let destPath = remotePath </> path
                        srcExists <- Dir.doesFileExist srcPath
                        when srcExists $ do
                            createDirectoryIfMissing True (takeDirectory destPath)
                            copyFile srcPath destPath
        _ -> return ()

-- | Check if a metadata file is a text file (content stored directly) or binary (hash/size stored).
-- Text files don't have "hash:" lines, binary files do.
isTextMetadataFile :: FilePath -> IO Bool
isTextMetadataFile metaPath = do
    exists <- Dir.doesFileExist metaPath
    if not exists then return False
    else do
        content <- readFile metaPath
        return $ not (any ("hash: " `isPrefixOf`) (lines content))

-- | Sync only changed files between two commits.
filesystemSyncChangedFiles :: FilePath -> FilePath -> String -> String -> IO ()
filesystemSyncChangedFiles localRoot remotePath oldHead newHead = do
    let remoteIndex = remotePath </> ".bit" </> "index"
    changes <- Git.runGitAt remoteIndex ["diff", "--name-status", oldHead, newHead]
    case changes of
        (ExitSuccess, out, _) -> do
            let parsedChanges = parseFilesystemDiffOutput out
            forM_ parsedChanges $ \(status, path, mNewPath) -> case status of
                'A' -> filesystemCopyFileToRemote localRoot remotePath remoteIndex path
                'M' -> filesystemCopyFileToRemote localRoot remotePath remoteIndex path
                'D' -> filesystemDeleteFileAtRemote remotePath path
                'R' -> case mNewPath of
                    Just newPath -> do
                        filesystemDeleteFileAtRemote remotePath path
                        filesystemCopyFileToRemote localRoot remotePath remoteIndex newPath
                    Nothing -> return ()
                _ -> return ()
        _ -> return ()

-- | Copy a file from local to remote (handles both text and binary).
filesystemCopyFileToRemote :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
filesystemCopyFileToRemote localRoot remotePath remoteIndex path = do
    -- Check if it's a text file (content in index) or binary (hash/size in index)
    let metaPath = remoteIndex </> path
    isText <- isTextMetadataFile metaPath
    if isText
        then do
            -- Text file: metadata IS the content, copy from remote index to working tree
            let workPath = remotePath </> path
            createDirectoryIfMissing True (takeDirectory workPath)
            copyFile metaPath workPath
        else do
            -- Binary file: metadata is hash/size, copy actual file from local working tree
            let srcPath = localRoot </> path
            let destPath = remotePath </> path
            srcExists <- Dir.doesFileExist srcPath
            when srcExists $ do
                createDirectoryIfMissing True (takeDirectory destPath)
                copyFile srcPath destPath

-- | Delete a file at the remote working tree.
filesystemDeleteFileAtRemote :: FilePath -> FilePath -> IO ()
filesystemDeleteFileAtRemote remotePath path = do
    let fullPath = remotePath </> path
    exists <- Dir.doesFileExist fullPath
    when exists $ Dir.removeFile fullPath

-- | Parse git diff --name-status output for filesystem operations.
parseFilesystemDiffOutput :: String -> [(Char, FilePath, Maybe FilePath)]
parseFilesystemDiffOutput = mapMaybe parseLine . lines
  where
    parseLine line = case line of
        (status:rest)
            | status == 'R' || status == 'C' ->
                case words (dropWhile (\c -> c /= '\t' && c /= ' ') rest) of
                    (old:new:_) -> Just (status, old, Just new)
                    _ -> Nothing
            | status `elem` ("ADM" :: String) ->
                case words rest of
                    (path:_) -> Just (status, path, Nothing)
                    _ -> Nothing
            | otherwise -> Nothing
        _ -> Nothing

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
    let localIndex = cwd </> ".bit" </> "index"
    
    putStrLn "Fetching remote commits..."
    (fetchCode, fetchOut, fetchErr) <- Git.runGitWithOutput 
        ["fetch", remoteIndexGit, "main:refs/remotes/origin/main"]
    
    when (fetchCode /= ExitSuccess) $ do
        hPutStrLn stderr $ "Error fetching from remote: " ++ fetchErr
        exitWith fetchCode
    
    -- Output fetch results similar to cloud pull
    hPutStrLn stderr $ "From " ++ remoteName remote
    hPutStrLn stderr $ " * [new branch]      main       -> origin/main"
    
    -- 2. Set up tracking (if not already)
    void $ Git.runGitWithOutput ["config", "branch.main.remote", "origin"]
    void $ Git.runGitWithOutput ["config", "branch.main.merge", "refs/heads/main"]
    
    -- 3. Get remote HEAD hash
    (remoteHeadCode, remoteHeadOut, _) <- Git.runGitWithOutput ["rev-parse", "refs/remotes/origin/main"]
    when (remoteHeadCode /= ExitSuccess) $ do
        hPutStrLn stderr "Error: Could not get remote HEAD"
        exitWith (ExitFailure 1)
    
    let remoteHash = filter (not . isSpace) remoteHeadOut
    
    -- 4. Merge locally using existing logic
    if pullAcceptRemote opts
        then filesystemPullAcceptRemote cwd remotePath remoteHash
        else filesystemPullNormal cwd remotePath remoteHash

-- | Pull with --accept-remote for filesystem remotes.
filesystemPullAcceptRemote :: FilePath -> FilePath -> String -> IO ()
filesystemPullAcceptRemote cwd remotePath remoteHash = do
    putStrLn "Accepting remote file state as truth..."
    
    -- Record current HEAD before checkout
    oldHead <- getLocalHeadE
    
    -- Force-checkout the remote branch
    checkoutCode <- Git.checkoutRemoteAsMain
    if checkoutCode /= ExitSuccess
        then do
            hPutStrLn stderr "Error: Failed to checkout remote state."
            exitWith (ExitFailure 1)
        else do
            -- Sync actual files based on what changed
            case oldHead of
                Just oh -> filesystemApplyMergeToWorkingDir cwd remotePath oh remoteHash
                Nothing -> filesystemSyncRemoteFilesToLocal cwd remotePath remoteHash
            
            -- Update tracking ref
            void $ Git.updateRemoteTrackingBranchToHash remoteHash
            putStrLn "Pull with --accept-remote completed."

-- | Normal pull for filesystem remotes (with merge).
filesystemPullNormal :: FilePath -> FilePath -> String -> IO ()
filesystemPullNormal cwd remotePath remoteHash = do
    oldHash <- getLocalHeadE
    
    case oldHash of
        Nothing -> do
            -- First pull: just checkout remote
            putStrLn $ "Checking out " ++ take 7 remoteHash ++ " (first pull)"
            checkoutCode <- Git.checkoutRemoteAsMain
            if checkoutCode == ExitSuccess
                then do
                    filesystemSyncRemoteFilesToLocal cwd remotePath remoteHash
                    putStrLn "Syncing binaries... done."
                    -- Update tracking ref (Tracking Ref Invariant from spec)
                    void $ Git.updateRemoteTrackingBranchToHash remoteHash
                else do
                    hPutStrLn stderr "Error: Failed to checkout remote branch."
                    exitWith (ExitFailure 1)
        
        Just localHash -> do
            -- Subsequent pull: merge
            (mergeCode, mergeOut, mergeErr) <- Git.runGitWithOutput 
                ["merge", "--no-commit", "--no-ff", "refs/remotes/origin/main"]
            
            (finalMergeCode, finalMergeOut, finalMergeErr) <-
                if mergeCode /= ExitSuccess && "refusing to merge unrelated histories" `List.isInfixOf` (mergeOut ++ mergeErr)
                then do
                    putStrLn "Merging unrelated histories..."
                    Git.runGitWithOutput ["merge", "--no-commit", "--no-ff", "--allow-unrelated-histories", "refs/remotes/origin/main"]
                else return (mergeCode, mergeOut, mergeErr)
            
            if finalMergeCode == ExitSuccess
                then do
                    putStrLn $ "Updating " ++ take 7 localHash ++ ".." ++ take 7 remoteHash
                    putStrLn "Merge made by the 'recursive' strategy."
                    hasChanges <- hasStagedChangesE
                    when hasChanges $ void $ Git.runGitRaw ["commit", "-m", "Merge remote"]
                    filesystemApplyMergeToWorkingDir cwd remotePath localHash remoteHash
                    putStrLn "Syncing binaries... done."
                    void $ Git.updateRemoteTrackingBranchToHash remoteHash
                else do
                    putStrLn finalMergeOut
                    hPutStrLn stderr finalMergeErr
                    putStrLn "Automatic merge failed."
                    putStrLn "bit requires you to pick a version for each conflict."
                    putStrLn ""
                    putStrLn "Resolving conflicts..."
                    
                    conflicts <- Conflict.getConflictedFilesE
                    resolutions <- Conflict.resolveAll conflicts
                    let total = length resolutions
                    
                    invalid <- Metadata.validateMetadataDir (cwd </> bitIndexPath)
                    unless (null invalid) $ do
                        void $ Git.runGitRaw ["merge", "--abort"]
                        hPutStrLn stderr "fatal: Metadata files contain conflict markers. Merge aborted."
                        throwIO (userError "Invalid metadata")
                    
                    conflictsNow <- Conflict.getConflictedFilesE
                    if null conflictsNow
                        then do
                            void $ Git.runGitRaw ["commit", "-m", "Merge remote (resolved " ++ show total ++ " conflict(s))"]
                            putStrLn $ "Merge complete. " ++ show total ++ " conflict(s) resolved."
                            filesystemApplyMergeToWorkingDir cwd remotePath localHash remoteHash
                            putStrLn "Syncing binaries... done."
                            void $ Git.updateRemoteTrackingBranchToHash remoteHash
                        else return ()

-- | Sync all files from remote to local based on commit hash (first pull).
filesystemSyncRemoteFilesToLocal :: FilePath -> FilePath -> String -> IO ()
filesystemSyncRemoteFilesToLocal localRoot remotePath commitHash = do
    let localIndex = localRoot </> ".bit" </> "index"
    (code, out, _) <- Git.runGitWithOutput ["ls-tree", "-r", "--name-only", commitHash]
    when (code == ExitSuccess) $ do
        let paths = filter (not . null) (lines out)
        forM_ paths $ \path -> do
            -- Check if it's a text file (content in index) or binary (hash/size in index)
            let metaPath = localIndex </> path
            isText <- isTextMetadataFile metaPath
            if isText
                then do
                    -- Text file: metadata IS the content, copy from local index to working tree
                    let workPath = localRoot </> path
                    createDirectoryIfMissing True (takeDirectory workPath)
                    copyFile metaPath workPath
                else do
                    -- Binary file: metadata is hash/size, copy actual file from remote working tree
                    let srcPath = remotePath </> path
                    let destPath = localRoot </> path
                    srcExists <- Dir.doesFileExist srcPath
                    when srcExists $ do
                        createDirectoryIfMissing True (takeDirectory destPath)
                        copyFile srcPath destPath

-- | Apply merge changes to working directory for filesystem pull.
filesystemApplyMergeToWorkingDir :: FilePath -> FilePath -> String -> String -> IO ()
filesystemApplyMergeToWorkingDir localRoot remotePath oldHead newHead = do
    changes <- Git.getDiffNameStatus oldHead newHead
    putStrLn "--- Pulling changes from remote ---"
    if null changes
        then putStrLn "Working tree already up to date with remote."
        else do
            forM_ changes $ \(status, path, mNewPath) -> case status of
                'A' -> filesystemDownloadOrCopyFromIndex localRoot remotePath path
                'M' -> filesystemDownloadOrCopyFromIndex localRoot remotePath path
                'D' -> safeDeleteWorkFile localRoot path
                'R' -> case mNewPath of
                    Just newPath -> do
                        safeDeleteWorkFile localRoot path
                        filesystemDownloadOrCopyFromIndex localRoot remotePath newPath
                    Nothing -> return ()
                _ -> return ()

-- | Download a file from remote or copy from index for filesystem pull.
filesystemDownloadOrCopyFromIndex :: FilePath -> FilePath -> FilePath -> IO ()
filesystemDownloadOrCopyFromIndex localRoot remotePath path = do
    fromIndex <- isTextFileInIndex localRoot path
    if fromIndex
        then copyFromIndexToWorkTree localRoot path
        else do
            -- Binary file: copy from remote working tree
            let srcPath = remotePath </> path
            let destPath = localRoot </> path
            srcExists <- Dir.doesFileExist srcPath
            when srcExists $ do
                createDirectoryIfMissing True (takeDirectory destPath)
                copyFile srcPath destPath

-- | Executes/Prints the command to be run in the shell (push: local -> remote).
executeCommand :: FilePath -> Remote -> RcloneAction -> IO ()
executeCommand localRoot remote action = case action of
        Copy src dest -> do
            let localPath = toPosix (localRoot </> src)
            void $ Transport.copyToRemote localPath remote (toPosix dest)

        Move src dest ->
            void $ Transport.moveRemote remote (toPosix src) (toPosix dest)

        Delete path ->
            void $ Transport.deleteRemote remote (toPosix path)

        Swap _ _ _ -> return ()  -- not produced by planAction; future-proofing

-- | Execute a single pull action: copy from remote to local or delete local file.
-- Text files are already in the git bundle (index); copy from index to work dir instead of rclone.
executePullCommand :: FilePath -> Remote -> RcloneAction -> IO ()
executePullCommand localRoot remote action = case action of
        Copy src dest -> do
            fromIndex <- isTextFileInIndex localRoot dest
            if fromIndex
            then copyFromIndexToWorkTree localRoot dest
            else do
                let localPath = toPosix (localRoot </> dest)
                createDirectoryIfMissing True (takeDirectory (localRoot </> dest))
                void $ Transport.copyFromRemote remote (toPosix dest) localPath
        Move src dest -> do
            fromIndex <- isTextFileInIndex localRoot src
            if fromIndex
            then copyFromIndexToWorkTree localRoot src
            else do
                let localSrcPath = localRoot </> src
                createDirectoryIfMissing True (takeDirectory localSrcPath)
                void $ Transport.copyFromRemote remote (toPosix src) (toPosix localSrcPath)
            let localDestPath = localRoot </> dest
            exists <- Dir.doesFileExist localDestPath
            when exists $ Dir.removeFile localDestPath
        Delete path -> do
            let localPath = localRoot </> path
            exists <- Dir.doesFileExist localPath
            when exists $ Dir.removeFile localPath
        Swap _ _ _ -> return ()

-- Helper to ensure we don't crash if cleanup fails (IO version for use outside BitM)
safeRemove :: FilePath -> IO ()
safeRemove path = do
    exists <- Dir.doesFileExist path
    when exists (Dir.removeFile path)


-- | Show up to 10 paths; if more than 20, show first 10 then "... and N more".
formatPathList :: [FilePath] -> [String]
formatPathList paths
  | length paths <= 20 = map (\p -> "        " ++ toPosix p) paths
  | otherwise         = map (\p -> "        " ++ toPosix p) (take 10 paths)
                        ++ ["        ... and " ++ show (length paths - 10) ++ " more"]

-- ============================================================================
-- Domain logic functions (moved from Transport in Step 4)
-- ============================================================================

-- | Classify remote state (empty, valid bit, non-bit, corrupted, network error)
-- This is domain logic: it knows what .bit/ means and interprets remote contents
classifyRemoteState :: Remote -> IO RemoteState
classifyRemoteState remote = do
    result <- Transport.listRemoteItems remote 1
    case result of
        Left err -> return (StateNetworkError err)
        Right items -> return (interpretRemoteItems items)

-- | Pure interpretation of remote items into domain state
interpretRemoteItems :: [Transport.TransportItem] -> RemoteState
interpretRemoteItems items
    | null items = StateEmpty
    | ".bit" `elem` map Transport.tiName items = StateValidRgit
    | otherwise = StateNonRgitOccupied (take 3 (map Transport.tiName items))

-- | Download the remote bundle for comparison. Returns temp bundle path or error.
-- This is domain logic: it knows about .bit/ layout and bundle files
fetchBundle :: Remote -> IO FetchResult
fetchBundle remote = do
    let localDest = ".bit/temp_remote.bundle"
    
    result <- Transport.copyFromRemoteDetailed remote ".bit/bit.bundle" localDest
    case result of
        Transport.CopySuccess -> return (BundleFound localDest)
        Transport.CopyNotFound -> return RemoteEmpty
        Transport.CopyNetworkError _ -> 
            return (NetworkError "Network unreachable: Check your internet connection or remote name.")
        Transport.CopyOtherError err -> return (NetworkError err)

-- ============================================================================
-- Core operations implementations
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
cleanupTemp path = do
    exists <- Dir.doesFileExist path
    when exists (Dir.removeFile path)

-- fetchBundle moved to Internal.Transport


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
                else mapM_ (\a -> liftIO $ executeCommand cwd remote a) actions)
        remoteResult


processExistingRemote :: BitM ()
processExistingRemote = do
    force <- asks envForce
    forceWithLease <- asks envForceWithLease
    mRemote <- asks envRemote
    -- Handle --force: skip all checks and push anyway
    if force
        then do
            lift $ tellErr "Warning: --force used. Overwriting remote history..."
            maybe (lift $ tellErr "Error: No remote configured.") pushToRemote mRemote
        else do
            -- Handle --force-with-lease: compare remote bundle hash against fetched_remote.bundle
            if forceWithLease
                then do
                    maybeRemoteHash <- liftIO $ Git.getHashFromBundle fetchedBundle
                    let fetchedPath = fromCwdPath (bundleCwdPath fetchedBundle)
                    hasFetchedBundle <- lift $ fileExistsE fetchedPath

                    case (maybeRemoteHash, hasFetchedBundle) of
                        (Just rHash, True) -> do
                            maybeFetchedHash <- liftIO $ Git.getHashFromBundle fetchedBundle
                            case maybeFetchedHash of
                                Just fHash | rHash == fHash -> do
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
                else do
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

-- | Sync files from remote to local (make local match remote). Used after pull.
syncRemoteFilesToLocal :: BitM ()
syncRemoteFilesToLocal = withRemote $ \remote -> do
    cwd <- asks envCwd
    localFiles <- asks envLocalFiles
    remoteResult <- lift $ Remote.Scan.fetchRemoteFiles remote
    either
        (\_ -> lift $ tellErr "Error: Failed to fetch remote file list.")
        (\remoteFiles -> do
            let actions = Pipeline.pullSyncFiles localFiles remoteFiles
            lift $ tell "--- Pulling changes from remote ---"
            if null actions
                then lift $ tell "Working tree already up to date with remote."
                else mapM_ (\a -> lift $ executePullCommand cwd remote a) actions)
        remoteResult

-- | After a merge, mirror git's metadata changes onto the actual working directory.
-- Uses `git diff --name-status oldHead newHead` to determine what changed,
-- then downloads/deletes/moves actual files accordingly.
-- This replaces syncRemoteFilesToLocal for merge pulls.
applyMergeToWorkingDir :: Remote -> String -> BitM ()
applyMergeToWorkingDir remote oldHead = do
    cwd <- asks envCwd
    newHead <- liftIO getLocalHeadE
    case newHead of
        Nothing -> return ()  -- shouldn't happen after merge commit
        Just newH -> do
            changes <- liftIO $ Git.getDiffNameStatus oldHead newH
            liftIO $ putStrLn "--- Pulling changes from remote ---"
            if null changes
                then liftIO $ putStrLn "Working tree already up to date with remote."
                else do
                    forM_ changes $ \(status, path, mNewPath) -> case status of
                        'A' -> liftIO $ downloadOrCopyFromIndex cwd remote path
                        'M' -> liftIO $ downloadOrCopyFromIndex cwd remote path
                        'D' -> liftIO $ safeDeleteWorkFile cwd path
                        'R' -> case mNewPath of
                            Just newPath -> do
                                liftIO $ safeDeleteWorkFile cwd path
                                liftIO $ downloadOrCopyFromIndex cwd remote newPath
                            Nothing -> return ()
                        _ -> return ()  -- ignore unknown statuses

-- | Download a file from remote, or copy from index if it's a text file.
downloadOrCopyFromIndex :: FilePath -> Remote -> FilePath -> IO ()
downloadOrCopyFromIndex cwd remote path = do
    fromIndex <- isTextFileInIndex cwd path
    if fromIndex
        then copyFromIndexToWorkTree cwd path
        else do
            let localPath = cwd </> path
            createDirectoryIfMissing True (takeDirectory localPath)
            void $ Transport.copyFromRemote remote (toPosix path) (toPosix localPath)

-- | Safely delete a file from the working directory.
safeDeleteWorkFile :: FilePath -> FilePath -> IO ()
safeDeleteWorkFile cwd path = do
    let fullPath = cwd </> path
    exists <- Dir.doesFileExist fullPath
    when exists $ Dir.removeFile fullPath

-- | Old showRemoteStatus logic: classify remote, fetch bundle if valid rgit.
-- Returns the temp bundle path on success, Nothing otherwise.
fetchRemoteBundle :: Remote -> IO (Maybe FilePath)
fetchRemoteBundle remote = do
    remoteState <- classifyRemoteState remote

    case remoteState of
        StateNetworkError _err -> do
            hPutStrLn stderr $ unlines
                [ _err
                , "fatal: Could not read from remote repository."
                , ""
                , "Please make sure you have the correct access rights"
                , "and the repository exists."
                ]
            return Nothing

        StateEmpty -> do
            -- Git fetch silently succeeds when remote is empty
            return Nothing

        StateNonRgitOccupied samples -> do
            hPutStrLn stderr $ "StateNonRgitOccupied: " ++ show samples
            hPutStrLn stderr $ unlines
                [ "fatal: Could not read from remote repository."
                , ""
                , "Please make sure you have the correct access rights"
                , "and the repository exists."
                ]
            return Nothing

        StateCorruptedRgit _msg -> do
            hPutStrLn stderr $ "StateCorruptedRgit: " ++ _msg
            hPutStrLn stderr $ unlines
                [ "fatal: Could not read from remote repository."
                , ""
                , "Please make sure you have the correct access rights"
                , "and the repository exists."
                ]
            return Nothing

        StateValidRgit -> do
            fetchResult <- fetchBundle remote
            case fetchResult of
                BundleFound bPath -> return $ Just bPath
                _ -> do
                    hPutStrLn stderr $ unlines
                        [ "fatal: Could not read from remote repository."
                        , ""
                        , "Please make sure you have the correct access rights"
                        , "and the repository exists."
                        ]
                    return Nothing

saveFetchedBundle :: Remote -> Maybe FilePath -> IO ()
saveFetchedBundle remote Nothing = pure ()
saveFetchedBundle remote (Just bPath) = do
    let fetchedPath = fromCwdPath (bundleCwdPath fetchedBundle)
    hadPrevious <- Dir.doesFileExist fetchedPath
    maybeOldHash <- if hadPrevious
        then Git.getHashFromBundle fetchedBundle
        else return Nothing

    -- Copy FIRST, then read hash from the correct location
    copyFile bPath fetchedPath
    safeRemove bPath
    maybeNewHash <- Git.getHashFromBundle fetchedBundle

    _ <- Git.setupRemote (remoteUrl remote)
    _ <- Git.setupBranchTracking

    case maybeNewHash of
        Just _ -> void $ Git.fetchFromBundle fetchedBundle
        Nothing -> return ()

    -- Output fetch results in git format
    case (maybeOldHash, maybeNewHash) of
        (Nothing, Just newHash) -> do
            -- First time fetching - show as new branch
            hPutStrLn stderr $ "From " ++ remoteName remote
            hPutStrLn stderr $ " * [new branch]      main       -> origin/main"
        (Just oldHash, Just newHash) ->
            if oldHash == newHash
                then return () -- Up to date, no output
                else do
                    -- Check if this is a normal update (old hash is ancestor of new hash)
                    isNormal <- Git.checkIsAhead oldHash newHash
                    hPutStrLn stderr $ "From " ++ remoteName remote
                    if isNormal
                        then hPutStrLn stderr $ "   " ++ take 7 oldHash ++ ".." ++ take 7 newHash ++ "  main       -> origin/main"
                        else hPutStrLn stderr $ " + " ++ take 7 oldHash ++ "..." ++ take 7 newHash ++ " main       -> origin/main  (forced update)"
        _ -> return () -- Error case, already handled

-- | Full pull with conflict resolution. Uses bracket_ to abort merge on exception (e.g. Ctrl+C).

-- | Pull with --accept-remote: accept remote file state as truth.
-- Scans actual remote files, updates local metadata to match, syncs files, and commits.
-- | Pull with --accept-remote: force-checkout the remote branch, then sync files.
-- Git manages .bit/index/ (the metadata); we only sync actual files to the working tree.
pullAcceptRemoteImpl :: Remote -> BitM ()
pullAcceptRemoteImpl remote = do
    cwd <- asks envCwd
    lift $ tell "Accepting remote file state as truth..."

    -- 1. Fetch the remote bundle so git has the remote's history
    maybeBundlePath <- lift $ fetchRemoteBundle remote
    case maybeBundlePath of
        Nothing -> lift $ tellErr "Error: Could not fetch remote bundle."
        Just bPath -> do
            lift $ saveFetchedBundle remote (Just bPath)

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
                    case oldHead of
                        Just oh -> applyMergeToWorkingDir remote oh
                        Nothing -> syncRemoteFilesToLocal  -- First time, no diff available

                    -- 5. Update tracking ref
                    maybeRemoteHash <- lift $ Git.getHashFromBundle fetchedBundle
                    case maybeRemoteHash of
                        Just rHash -> lift $ void $ Git.updateRemoteTrackingBranchToHash rHash
                        Nothing    -> return ()

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
            lift $ saveFetchedBundle remote (Just bPath)

            remoteMeta <- lift $ Verify.loadMetadataFromBundle fetchedBundle
            lift $ tell "Scanning remote files... done."
            result <- lift $ Remote.Scan.fetchRemoteFiles remote
            case result of
                Left _ -> lift $ tellErr "Error: Could not fetch remote file list."
                Right remoteFiles -> do
                    let filteredRemoteFiles = filterOutBitPaths remoteFiles
                    localMeta <- lift $ Verify.loadMetadataIndex (cwd </> bitIndexPath)

                    let remoteFileMap = Map.fromList
                          [ (normalise e.path, (h, e.kind))
                          | e <- filteredRemoteFiles
                          , h <- maybeToList (syncHash e.kind)
                          ]
                        remoteMetaMap = Map.fromList [(normalise p, (h, sz)) | (p, h, sz) <- remoteMeta]
                        localMetaMap = Map.fromList [(normalise p, (h, sz)) | (p, h, sz) <- localMeta]

                    lift $ tell "Comparing..."
                    let divergentFiles = findDivergentFiles remoteFileMap remoteMetaMap localMetaMap

                    if null divergentFiles
                        then do
                            lift $ tell "No remote divergence detected. Proceeding with normal pull..."
                            pullWithCleanup remote
                        else do
                            oldHash <- lift getLocalHeadE
                            (remoteCode, remoteOut, _) <- lift $ gitQuery ["rev-parse", "refs/remotes/origin/main"]
                            let newHash = takeWhile (/= '\n') remoteOut

                            (mergeCode, mergeOut, mergeErr) <- lift $ gitQuery ["merge", "--no-commit", "--no-ff", "refs/remotes/origin/main"]
                            (_finalMergeCode, _, _) <- lift $ if mergeCode /= ExitSuccess && "refusing to merge unrelated histories" `List.isInfixOf` (mergeOut ++ mergeErr)
                                then do tell "Merging unrelated histories (e.g. first pull)..."; gitQuery ["merge", "--no-commit", "--no-ff", "--allow-unrelated-histories", "refs/remotes/origin/main"]
                                else return (mergeCode, mergeOut, mergeErr)

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

-- | Find files where remote actual files don't match remote metadata.
findDivergentFiles :: Map.Map FilePath (Hash 'MD5, EntryKind) -> Map.Map FilePath (Hash 'MD5, Integer) -> Map.Map FilePath (Hash 'MD5, Integer) -> [(FilePath, Hash 'MD5, Hash 'MD5, Integer, Integer)]
findDivergentFiles remoteFileMap remoteMetaMap localMetaMap =
    Map.foldlWithKey (\acc path (metaHash, metaSize) ->
        case Map.lookup path remoteFileMap of
            Nothing -> acc  -- File missing on remote, skip
            Just (actualHash, kind) ->
                case kind of
                    File _ actualSize _ ->
                        if actualHash == metaHash && actualSize == metaSize
                            then acc  -- Matches, no divergence
                            else (path, metaHash, actualHash, metaSize, actualSize) : acc  -- Divergence!
                    _ -> acc
        ) [] remoteMetaMap

-- | Create conflict directories for divergent files.
createConflictDirectories :: Remote -> [(FilePath, Hash 'MD5, Hash 'MD5, Integer, Integer)] -> Map.Map FilePath (Hash 'MD5, EntryKind) -> Map.Map FilePath (Hash 'MD5, Integer) -> Map.Map FilePath (Hash 'MD5, Integer) -> BitM ()
createConflictDirectories remote divergentFiles remoteFileMap remoteMetaMap localMetaMap = do
    cwd <- asks envCwd
    let conflictsDir = cwd </> ".bit" </> "conflicts"
    lift $ createDirE conflictsDir

    forM_ divergentFiles $ \(path, metaHash, actualHash, metaSize, actualSize) -> do
        let conflictDir = conflictsDir </> path
        lift $ createDirE (takeDirectory conflictDir)

        let localPath = cwd </> path
        localExists <- lift $ fileExistsE localPath
        when localExists $ lift $ copyFileE localPath (conflictDir </> "LOCAL")

        code <- liftIO $ Transport.copyFromRemote remote (toPosix path) (conflictDir </> "REMOTE")
        when (code /= ExitSuccess) $ lift $ tellErr $ "Warning: Could not download remote file: " ++ path

        lift $ case Map.lookup (normalise path) localMetaMap of
            Just (localHash, localSize) ->
                writeFileAtomicE (conflictDir </> "METADATA_LOCAL") $
                    serializeMetadata (MetaContent localHash localSize)
            Nothing -> writeFileAtomicE (conflictDir </> "METADATA_LOCAL") "hash: (not tracked)\nsize: 0\n"

        lift $ writeFileAtomicE (conflictDir </> "METADATA_REMOTE") $
            serializeMetadata (MetaContent metaHash metaSize)

-- | Print conflict list in spec format.
printConflictList :: [(FilePath, Hash 'MD5, Hash 'MD5, Integer, Integer)] -> Map.Map FilePath (Hash 'MD5, EntryKind) -> Map.Map FilePath (Hash 'MD5, Integer) -> Map.Map FilePath (Hash 'MD5, Integer) -> IO ()
printConflictList divergentFiles remoteFileMap remoteMetaMap localMetaMap = do
    putStrLn ""
    putStrLn "✗ Remote divergence detected:"
    putStrLn ""
    
    forM_ divergentFiles $ \(path, metaHash, actualHash, metaSize, actualSize) -> do
        putStrLn $ "  " ++ toPosix path ++ ":"
        
        -- Get local metadata (use displayHash for Hash 'MD5 values)
        let localInfo = case Map.lookup (normalise path) localMetaMap of
                Just (localHash, localSize) -> (displayHash localHash, show localSize)
                Nothing -> ("(not tracked)", "0")
        
        putStrLn $ "    Local:           " ++ fst localInfo ++ " (" ++ snd localInfo ++ " bytes)"
        putStrLn $ "    Remote actual:   " ++ displayHash actualHash ++ " (" ++ show actualSize ++ " bytes)"
        putStrLn $ "    Remote metadata: " ++ displayHash metaHash ++ " (" ++ show metaSize ++ " bytes)"
        putStrLn $ ""
        putStrLn $ "    Files saved to: .bit/conflicts/" ++ toPosix path ++ "/"
        putStrLn ""
    
    putStrLn "This can happen when:"
    putStrLn "  - Files were modified directly on the remote (not via bit)"
    putStrLn "  - A partial push from another client"
    putStrLn "  - Remote storage corruption"

pullWithCleanup :: Remote -> BitM ()
pullWithCleanup remote = do
    env <- asks id
    result <- liftIO $ try @SomeException (runBitM env (pullLogic remote))
    case result of
        Left ex -> do
            inProgress <- lift $ Git.isMergeInProgress
            if inProgress
                then do
                    lift $ void $ gitRaw ["merge", "--abort"]
                    lift $ tell "Merge aborted. Your working tree is unchanged."
                else lift $ throwIO ex
        Right _ -> return ()

pullLogic :: Remote -> BitM ()
pullLogic remote = do
    cwd <- asks envCwd
    maybeBundlePath <- lift $ fetchRemoteBundle remote
    case maybeBundlePath of
        Nothing -> return ()
        Just bPath -> do
            lift $ saveFetchedBundle remote (Just bPath)
            (_, countOut, _) <- lift $ gitQuery ["rev-list", "--count", "refs/remotes/origin/main"]
            let n = takeWhile (`elem` ['0'..'9']) (filter (/= '\n') countOut)
            lift $ tell $ "remote: Counting objects: " ++ (if null n then "0" else n) ++ ", done."

            oldHash <- lift getLocalHeadE
            (remoteCode, remoteOut, _) <- lift $ gitQuery ["rev-parse", "refs/remotes/origin/main"]
            let newHash = takeWhile (/= '\n') remoteOut

            case oldHash of
                Nothing -> do
                    lift $ tell $ "Checking out " ++ take 7 newHash ++ " (first pull)"
                    checkoutCode <- lift $ Git.checkoutRemoteAsMain
                    if checkoutCode == ExitSuccess
                        then do
                            syncRemoteFilesToLocal
                            lift $ tell "Syncing binaries... done."
                        else lift $ tellErr "Error: Failed to checkout remote branch."

                Just localHead -> do
                    (mergeCode, mergeOut, mergeErr) <- lift $ gitQuery ["merge", "--no-commit", "--no-ff", "refs/remotes/origin/main"]

                    (finalMergeCode, finalMergeOut, finalMergeErr) <-
                        lift $ if mergeCode /= ExitSuccess && "refusing to merge unrelated histories" `List.isInfixOf` (mergeOut ++ mergeErr)
                        then do tell "Merging unrelated histories..."; gitQuery ["merge", "--no-commit", "--no-ff", "--allow-unrelated-histories", "refs/remotes/origin/main"]
                        else return (mergeCode, mergeOut, mergeErr)

                    if finalMergeCode == ExitSuccess
                    then do
                        lift $ tell $ "Updating " ++ take 7 localHead ++ ".." ++ take 7 newHash
                        lift $ tell "Merge made by the 'recursive' strategy."
                        hasChanges <- lift hasStagedChangesE
                        when hasChanges $ lift $ void $ gitRaw ["commit", "-m", "Merge remote"]
                        applyMergeToWorkingDir remote localHead
                        lift $ tell "Syncing binaries... done."
                        maybeRemoteHash <- lift $ Git.getHashFromBundle fetchedBundle
                        case maybeRemoteHash of
                            Just rHash -> lift $ void $ Git.updateRemoteTrackingBranchToHash rHash
                            Nothing    -> return ()
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

                        invalid <- lift $ Metadata.validateMetadataDir (cwd </> bitIndexPath)
                        unless (null invalid) $ do
                            lift $ void $ gitRaw ["merge", "--abort"]
                            lift $ tellErr "fatal: Metadata files contain conflict markers. Merge aborted."
                            lift $ throwIO (userError "Invalid metadata")

                        conflictsNow <- lift Conflict.getConflictedFilesE
                        if null conflictsNow
                        then do
                            -- Always commit: MERGE_HEAD exists so git commit succeeds
                            -- even when the index matches HEAD (e.g. user chose "keep local"
                            -- and the resolved version is identical to HEAD's tree).
                            -- Skipping this leaves MERGE_HEAD dangling and makes the next
                            -- push fail its ancestry check.
                            lift $ do
                                void $ gitRaw ["commit", "-m", "Merge remote (resolved " ++ show total ++ " conflict(s))"]
                                tell $ "Merge complete. " ++ show total ++ " conflict(s) resolved."
                            -- After auto-resolution, files are already in correct state (we chose local/remote versions)
                            -- Still need to sync any files that changed on remote but weren't in conflict
                            applyMergeToWorkingDir remote localHead
                            lift $ tell "Syncing binaries... done."
                            maybeRemoteHash <- lift $ Git.getHashFromBundle fetchedBundle
                            case maybeRemoteHash of
                                Just rHash -> lift $ void $ Git.updateRemoteTrackingBranchToHash rHash
                                Nothing    -> return ()
                        else return ()  -- Still have unresolved conflicts, don't sync files yet

printVerifyIssue :: (String -> String) -> Verify.VerifyIssue -> IO ()
printVerifyIssue fmtHash = \case
  Verify.HashMismatch path expectedHash actualHash _expectedSize _actualSize -> do
    hPutStrLn stderr $ "[ERROR] Hash mismatch: " ++ toPosix path
    hPutStrLn stderr $ "  Expected: " ++ fmtHash expectedHash
    hPutStrLn stderr $ "  Actual:   " ++ fmtHash actualHash
  Verify.Missing path ->
    hPutStrLn stderr $ "[ERROR] Missing: " ++ toPosix path

-- | Format remote display line (e.g. "origin → black_usb:Backup (physical, connected at E:\)")
formatRemoteDisplay :: FilePath -> String -> Maybe Device.RemoteTarget -> IO String
formatRemoteDisplay cwd name mTarget = case mTarget of
    Just (Device.TargetLocalPath p) -> return (name ++ " → " ++ p ++ " (local path)")
    Just (Device.TargetDevice dev path) -> do
        res <- Device.resolveRemoteTarget cwd (Device.TargetDevice dev path)
        mInfo <- Device.readDeviceFile cwd dev
        let typ = maybe "unknown" (\i -> case Device.deviceType i of Device.Physical -> "physical"; Device.Network -> "network") mInfo
        case res of
            Device.Resolved mount -> return (name ++ " → " ++ dev ++ ":" ++ path ++ " (" ++ typ ++ ", connected at " ++ mount ++ ")")
            Device.NotConnected _ -> return (name ++ " → " ++ dev ++ ":" ++ path ++ " (" ++ typ ++ ", NOT CONNECTED)")
    Just (Device.TargetCloud u) -> return (name ++ " → " ++ u ++ " (cloud)")
    Nothing -> return (name ++ " → (no target)")

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
        _ -> return ()

-- | Extract paths from restore/checkout args, skipping flags and options.
-- Git restore: [options] [--] <pathspec>...
-- Git checkout: [options] [--] <pathspec>...
restoreCheckoutPaths :: [String] -> [String]
restoreCheckoutPaths args =
    let restoreFlags = ["--staged", "-S", "--worktree", "-W",
                        "--patch", "-p", "--quiet", "-q",
                        "--ours", "--theirs", "--merge", "-m",
                        "--pathspec-file-nul", "--overlay", "--no-overlay",
                        "--ignore-unmerged", "--recurse-submodules", "--no-recurse-submodules"]
        isFlag arg = arg `elem` restoreFlags ||
                     arg == "--" ||
                     "--source=" `isPrefixOf` arg ||
                     "-s" `isPrefixOf` arg ||
                     "--pathspec-from-file=" `isPrefixOf` arg ||
                     "--conflict=" `isPrefixOf` arg ||
                     "--inter-hunk-context=" `isPrefixOf` arg ||
                     "--unified=" `isPrefixOf` arg ||
                     "-U" `isPrefixOf` arg
        -- Collect paths (after -- if present, or non-flag args)
        (_, paths) = foldl (\(afterDash, acc) arg ->
            if arg == "--" then (True, acc)
            else if afterDash then (True, arg:acc)
            else if isFlag arg then (False, acc)
            else (False, arg:acc)
            ) (False, []) args
    in reverse paths

-- | Expand pathspecs (e.g. ".", "dir/") to concrete file paths in the index.
expandPathsToFiles :: FilePath -> [String] -> IO [FilePath]
expandPathsToFiles cwd paths = do
    let indexRoot = cwd </> bitIndexPath
    allFiles <- Scan.listMetadataPaths indexRoot
    return $ concatMap (\p ->
        if p == "." || p == "./"
        then allFiles
        else let p' = normalise p
                 pPrefix = p' ++ "/"
                 matches = filter (\f -> let f' = normalise f
                                         in f' == p' || pPrefix `isPrefixOf` (f' ++ "/")) allFiles
             in if null matches then [p] else matches
        ) paths

-- | Restore files from git. For text files, also copies the restored metadata
-- file (which contains the actual content) back to the working directory.
-- Supports full git restore syntax: restore [options] [--] <pathspec>...
doRestore :: [String] -> BitM ExitCode
doRestore args = do
    cwd <- asks envCwd
    code <- lift $ gitRaw ("restore" : args)
    when (code == ExitSuccess) $ do
        let stagedOnly = ("--staged" `elem` args || "-S" `elem` args) &&
                         not ("--worktree" `elem` args || "-W" `elem` args)
        unless stagedOnly $ do
            let rawPaths = restoreCheckoutPaths args
            paths <- lift $ expandPathsToFiles cwd rawPaths
            forM_ paths $ \path -> do
                let metaPath = cwd </> bitIndexPath </> path
                let workPath = cwd </> path
                metaExists <- lift $ fileExistsE metaPath
                when metaExists $ do
                    mcontent <- lift $ readFileE metaPath
                    let isBinaryMetadata = maybe True (\content -> any ("hash: " `isPrefixOf`) (lines content)) mcontent
                    unless isBinaryMetadata $ do
                        lift $ createDirE (takeDirectory workPath)
                        lift $ copyFileE metaPath workPath
    return code

-- | Checkout paths from index/HEAD (git checkout [options] -- <path>).
-- Same as restore for path form: restores metadata, copies text files to working dir.
doCheckout :: [String] -> BitM ExitCode
doCheckout args = do
    let args' = case List.elemIndex "--" args of
          Just _ -> args
          Nothing -> let (opts, paths) = span (\a -> a == "--" || "-" `isPrefixOf` a) args
                     in opts ++ ["--"] ++ paths
    code <- lift $ gitRaw ("checkout" : args')
    when (code == ExitSuccess) $ do
        cwd <- asks envCwd
        let rawPaths = restoreCheckoutPaths args'
        paths <- lift $ expandPathsToFiles cwd rawPaths
        forM_ paths $ \path -> do
            let metaPath = cwd </> bitIndexPath </> path
            let workPath = cwd </> path
            metaExists <- lift $ fileExistsE metaPath
            when metaExists $ do
                mcontent <- lift $ readFileE metaPath
                let isBinaryMetadata = maybe True (\content -> any ("hash: " `isPrefixOf`) (lines content)) mcontent
                unless isBinaryMetadata $ do
                    lift $ createDirE (takeDirectory workPath)
                    lift $ copyFileE metaPath workPath
    return code

