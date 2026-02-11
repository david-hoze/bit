{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

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
import System.FilePath ((</>), takeDirectory, equalFilePath)
import Control.Monad (when, unless, void, forM_)
import Data.Foldable (traverse_)
import Control.Monad.Trans.Class (lift)
import System.Exit (ExitCode(..), exitWith)
import Control.Exception (throwIO, IOException, catch)
import qualified Internal.Git as Git
import Internal.Config (bitIndexPath)
import System.IO (stderr, hPutStrLn, hPutStr)
import Bit.Types (BitM, BitEnv(..))
import Control.Monad.Trans.Reader (asks)
import qualified Data.List
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, foldl')
import qualified Bit.Conflict as Conflict
import qualified Bit.Device as Device
import qualified Bit.Internal.Metadata as Metadata
import Bit.Remote (remoteName, remoteUrl)
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
    , getRemoteType
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

rm :: [String] -> BitM ExitCode
rm args = do
    cwd <- asks envCwd
    let flags = parseRmFlags args
        -- Strip quiet flags so git always outputs the file list for parsing
        gitArgs = stripQuiet args
    (code, out, err) <- liftIO $ Git.runGitWithOutput ("rm" : gitArgs)

    -- Remove actual files from working directory
    when (code == ExitSuccess && not (rmCached flags) && not (rmDryRun flags)) $ liftIO $ do
        let paths = parseRmOutput out
        forM_ paths $ \path -> do
            let actualPath = cwd </> path
            exists <- Dir.doesFileExist actualPath
            when exists $
                Dir.removeFile actualPath `catch` \(e :: IOException) ->
                    hPutStrLn stderr $ "warning: failed to remove " ++ path ++ ": " ++ show e
        -- Clean up empty parent directories
        forM_ paths $ \path ->
            removeEmptyParents cwd (takeDirectory (cwd </> path))

    -- Print output
    liftIO $ do
        unless (rmQuiet flags) $
            putStr (Git.rewriteGitHints out)
        hPutStr stderr (Git.rewriteGitHints err)
        case code of
            ExitSuccess -> pure ()
            ExitFailure n -> hPutStrLn stderr ("bit: git exited with code " ++ show n)

    pure code

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

    if | not (null gitConflicts) ->
            liftIO $ hPutStrLn stderr "error: you have not resolved your conflicts yet."
       | not conflictsExist -> do
            (code, _, _) <- liftIO $ Git.runGitWithOutput ["rev-parse", "--verify", "MERGE_HEAD"]
            case code of
                ExitSuccess -> do
                    oldHead <- liftIO getLocalHeadE
                    liftIO $ do
                        void $ Git.runGitRaw ["commit", "-m", "Merge remote"]
                        putStrLn "Merge complete."
                    traverse_ (\remote -> do
                            mType <- liftIO $ getRemoteType cwd (remoteName remote)
                            let transport = case mType of
                                  Just t | Device.isFilesystemType t -> Transport.mkFilesystemTransport (remoteUrl remote)
                                  _ -> Transport.mkCloudTransport remote
                            Transport.syncBinariesAfterMerge transport remote oldHead) mRemote
                _ -> liftIO $ do
                    hPutStrLn stderr "error: no merge in progress."
                    exitWith (ExitFailure 1)
       | otherwise -> do
                invalid <- liftIO $ Metadata.validateMetadataDir (cwd </> bitIndexPath)
                unless (null invalid) $ liftIO $ do
                    hPutStrLn stderr "fatal: Metadata files contain conflict markers. Merge aborted."
                    throwIO (userError "Invalid metadata")

                oldHead <- liftIO getLocalHeadE
                (code, _, _) <- liftIO $ Git.runGitWithOutput ["rev-parse", "--verify", "MERGE_HEAD"]
                when (code /= ExitSuccess) $ do
                    -- Use actual remote name if available, fall back to "origin"
                    let remName = maybe "origin" remoteName mRemote
                    (mergeCode, _, _) <- liftIO $ Git.runGitWithOutput ["merge", "--no-commit", "--no-ff", Git.remoteTrackingRef remName]
                    when (mergeCode /= ExitSuccess) $
                        liftIO $ hPutStrLn stderr "warning: Could not start merge. Proceeding anyway."

                liftIO $ do
                    void $ Git.runGitRaw ["commit", "-m", "Merge remote (manual merge resolved)"]
                    putStrLn "Merge complete."
                    removeDirectoryRecursive conflictsDir
                    putStrLn "Conflict directories cleaned up."

                traverse_ (\remote -> do
                        mType <- liftIO $ getRemoteType cwd (remoteName remote)
                        let transport = case mType of
                              Just t | Device.isFilesystemType t -> Transport.mkFilesystemTransport (remoteUrl remote)
                              _ -> Transport.mkCloudTransport remote
                        Transport.syncBinariesAfterMerge transport remote oldHead) mRemote

mergeAbort :: IO ()
mergeAbort = doMergeAbort

doMergeAbort :: IO ()
doMergeAbort = do
    cwd <- Dir.getCurrentDirectory
    let conflictsDir = cwd </> ".bit" </> "conflicts"
    
    -- Abort git merge
    code <- Git.mergeAbort
    case code of
        ExitSuccess -> do
            putStrLn "Merge aborted. Your working tree is unchanged."
            
            -- Clean up conflict directories
            conflictsExist <- Dir.doesDirectoryExist conflictsDir
            when conflictsExist $ do
                removeDirectoryRecursive conflictsDir
                putStrLn "Conflict directories cleaned up."
        _ -> do
            hPutStrLn stderr "error: no merge in progress."
            exitWith (ExitFailure 1)

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
        unless stagedOnly $
            syncTextFilesFromIndex cwd (restoreCheckoutPaths args)
    pure code

doCheckout :: [String] -> BitM ExitCode
doCheckout args = do
    let args' = case Data.List.elemIndex "--" args of
          Just _ -> args
          Nothing -> let (opts, paths) = Data.List.span (\a -> a == "--" || "-" `isPrefixOf` a) args
                     in opts ++ ["--"] ++ paths
    code <- lift $ Git.runGitRaw ("checkout" : args')
    when (code == ExitSuccess) $ do
        cwd <- asks envCwd
        syncTextFilesFromIndex cwd (restoreCheckoutPaths args')
    pure code

-- | After a successful restore/checkout, copy text files from .bit/index/ back
-- to the working directory. Binary metadata files are left alone.
syncTextFilesFromIndex :: FilePath -> [FilePath] -> BitM ()
syncTextFilesFromIndex cwd rawPaths = do
    paths <- lift $ expandPathsToFiles cwd rawPaths
    forM_ paths $ \filePath -> do
        let metaPath = cwd </> bitIndexPath </> filePath
        let workPath = cwd </> filePath
        metaExists <- lift $ fileExistsE metaPath
        when metaExists $ do
            mcontent <- lift $ readFileE metaPath
            let isBinaryMetadata = maybe True (\content -> any ("hash: " `isPrefixOf`) (lines content)) mcontent
            unless isBinaryMetadata $ lift $ do
                createDirE (takeDirectory workPath)
                copyFileE metaPath workPath

-- ============================================================================
-- rm helpers
-- ============================================================================

data RmFlags = RmFlags
    { rmCached :: Bool
    , rmDryRun :: Bool
    , rmQuiet  :: Bool
    }

-- | Parse rm-relevant flags from args, respecting @--@ separator.
parseRmFlags :: [String] -> RmFlags
parseRmFlags args =
    let (flagPart, _) = break (== "--") args
    in foldl' checkArg (RmFlags False False False) flagPart
  where
    checkArg flags "--cached"  = flags { rmCached = True }
    checkArg flags "--dry-run" = flags { rmDryRun = True }
    checkArg flags "--quiet"   = flags { rmQuiet = True }
    checkArg flags arg
        | "-" `isPrefixOf` arg && not ("--" `isPrefixOf` arg) =
            flags { rmDryRun = rmDryRun flags || 'n' `elem` drop 1 arg
                  , rmQuiet  = rmQuiet flags || 'q' `elem` drop 1 arg
                  }
    checkArg flags _ = flags

-- | Strip @-q@/@--quiet@ from args (before @--@) so git always outputs file list.
stripQuiet :: [String] -> [String]
stripQuiet args =
    let (before, after) = break (== "--") args
    in concatMap stripArg before ++ after
  where
    stripArg "--quiet" = []
    stripArg arg
        | "-" `isPrefixOf` arg && not ("--" `isPrefixOf` arg) =
            let stripped = filter (/= 'q') (drop 1 arg)
            in if null stripped then [] else ['-' : stripped]
    stripArg arg = [arg]

-- | Parse @rm 'path'@ lines from git rm output.
parseRmOutput :: String -> [FilePath]
parseRmOutput out =
    [ take (length path - 1) path
    | line <- lines out
    , "rm '" `isPrefixOf` line
    , let path = drop 4 line
    , not (null path)
    ]

-- | Remove empty parent directories up to (but not including) @stopAt@.
removeEmptyParents :: FilePath -> FilePath -> IO ()
removeEmptyParents stopAt dir = go dir `catch` \(_ :: IOException) -> pure ()
  where
    go d | equalFilePath d stopAt = pure ()
         | equalFilePath d (takeDirectory d) = pure ()
         | otherwise = do
              contents <- Dir.listDirectory d
              when (null contents) $ do
                  Dir.removeDirectory d
                  go (takeDirectory d)
