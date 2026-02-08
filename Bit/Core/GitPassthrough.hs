{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Core.GitPassthrough
    ( -- Git passthrough
      add
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
      -- Merge/branch management  
    , mergeContinue
    , mergeAbort
    , doMergeAbort
    , unsetUpstream
      -- Restore/checkout helpers
    , doRestore
    , doCheckout
    ) where

import Prelude hiding (log)
import qualified System.Directory as Dir
import System.FilePath ((</>), takeDirectory)
import Control.Monad (when, unless, void, forM_)
import Control.Monad.Trans.Class (lift)
import System.Exit (ExitCode(..), exitWith)
import Control.Exception (throwIO)
import qualified Internal.Git as Git
import Internal.Config (bitIndexPath)
import System.IO (stderr, hPutStrLn)
import Bit.Types (BitM, BitEnv(..))
import Control.Monad.Trans.Reader (asks)
import qualified Data.List
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import qualified Bit.Conflict as Conflict
import qualified Bit.Device as Device
import qualified Bit.Internal.Metadata as Metadata
import Bit.Remote (Remote, remoteName, remoteUrl)
import qualified Bit.Core.Transport as Transport
import Bit.Core.Helpers
    ( fileExistsE
    , createDirE
    , copyFileE
    , readFileE
    , removeDirectoryRecursive
    , restoreCheckoutPaths
    , expandPathsToFiles
    , getLocalHeadE
    , getRemoteTargetType
    )

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
-- Merge/branch management
-- ============================================================================

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
                        case mRemote of
                            Nothing -> return ()
                            Just remote -> do
                                mTarget <- liftIO $ getRemoteTargetType cwd (remoteName remote)
                                let transport = case mTarget of
                                      Just (Device.TargetDevice _ _) -> Transport.mkFilesystemTransport (remoteUrl remote)
                                      Just (Device.TargetLocalPath _) -> Transport.mkFilesystemTransport (remoteUrl remote)
                                      _ -> Transport.mkCloudTransport remote
                                Transport.syncBinariesAfterMerge transport remote oldHead
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

                case mRemote of
                    Nothing -> return ()
                    Just remote -> do
                        mTarget <- liftIO $ getRemoteTargetType cwd (remoteName remote)
                        let transport = case mTarget of
                              Just (Device.TargetDevice _ _) -> Transport.mkFilesystemTransport (remoteUrl remote)
                              Just (Device.TargetLocalPath _) -> Transport.mkFilesystemTransport (remoteUrl remote)
                              _ -> Transport.mkCloudTransport remote
                        Transport.syncBinariesAfterMerge transport remote oldHead

mergeAbort :: IO ()
mergeAbort = doMergeAbort

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

unsetUpstream :: IO ()
unsetUpstream = void Git.unsetBranchUpstream

-- ============================================================================
-- Restore and Checkout implementations
-- ============================================================================

doRestore :: [String] -> BitM ExitCode
doRestore args = do
    cwd <- asks envCwd
    code <- lift $ Git.runGitRaw ("restore" : args)
    when (code == ExitSuccess) $ do
        let stagedOnly = ("--staged" `elem` args || "-S" `elem` args) &&
                         not ("--worktree" `elem` args || "-W" `elem` args)
        unless stagedOnly $ do
            let rawPaths = restoreCheckoutPaths args
            paths <- lift $ expandPathsToFiles cwd rawPaths
            forM_ paths $ \filePath -> do
                let metaPath = cwd </> bitIndexPath </> filePath
                let workPath = cwd </> filePath
                metaExists <- lift $ fileExistsE metaPath
                when metaExists $ do
                    mcontent <- lift $ readFileE metaPath
                    let isBinaryMetadata = maybe True (\content -> any ("hash: " `isPrefixOf`) (lines content)) mcontent
                    unless isBinaryMetadata $ do
                        lift $ createDirE (takeDirectory workPath)
                        lift $ copyFileE metaPath workPath
    return code

doCheckout :: [String] -> BitM ExitCode
doCheckout args = do
    let args' = case Data.List.elemIndex "--" args of
          Just _ -> args
          Nothing -> let (opts, paths) = Data.List.span (\a -> a == "--" || "-" `isPrefixOf` a) args
                     in opts ++ ["--"] ++ paths
    code <- lift $ Git.runGitRaw ("checkout" : args')
    when (code == ExitSuccess) $ do
        cwd <- asks envCwd
        let rawPaths = restoreCheckoutPaths args'
        paths <- lift $ expandPathsToFiles cwd rawPaths
        forM_ paths $ \filePath -> do
            let metaPath = cwd </> bitIndexPath </> filePath
            let workPath = cwd </> filePath
            metaExists <- lift $ fileExistsE metaPath
            when metaExists $ do
                mcontent <- lift $ readFileE metaPath
                let isBinaryMetadata = maybe True (\content -> any ("hash: " `isPrefixOf`) (lines content)) mcontent
                unless isBinaryMetadata $ do
                    lift $ createDirE (takeDirectory workPath)
                    lift $ copyFileE metaPath workPath
    return code
