{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Bit.Git.Passthrough
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
import qualified Bit.Git.Run as Git
import System.IO (stderr, hPutStrLn, hPutStr)
import Bit.Types (BitM, BitEnv(..))
import Control.Monad.Trans.Reader (asks)
import qualified Data.List
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, foldl')
import System.Environment (lookupEnv)
import qualified Bit.Core.Conflict as Conflict
import Bit.Config.Metadata (parseMetadata, MetaContent(..), validateMetadataDir)
import Bit.CAS (copyBlobFromCasTo)
import Bit.Remote (remoteName)
import qualified Bit.Rclone.Sync as Transport
import Bit.Core.Helpers
    ( fileExistsE
    , createDirE
    , copyFileE
    , readFileE
    , removeDirectoryRecursive
    , restoreCheckoutPaths
    , expandPathsToFiles
    , getLocalHeadE
    )

-- ============================================================================
-- Git passthrough (thin wrappers)
-- ============================================================================

add :: FilePath -> [String] -> IO ExitCode
add prefix args = Git.runGitRawIn prefix ("add" : args)

commit :: FilePath -> [String] -> IO ExitCode
commit prefix args = Git.runGitRawIn prefix ("commit" : args)

diff :: FilePath -> [String] -> IO ExitCode
diff prefix args = Git.runGitRawIn prefix ("diff" : args)

log :: FilePath -> [String] -> IO ExitCode
log prefix args = Git.runGitRawIn prefix ("log" : args)

lsFiles :: FilePath -> [String] -> IO ExitCode
lsFiles prefix args = do
    -- When BIT_GIT_JUNCTION is set (git test suite) and -o/--others is used,
    -- git needs the actual working tree to enumerate untracked files. The junction
    -- handles repo discovery, so use runGitHere to let pathspecs and -X paths
    -- resolve from the real CWD. For all other modes, use the normal -C .bit/index
    -- path with -X paths resolved to absolute.
    junction <- lookupEnv "BIT_GIT_JUNCTION"
    let hasOthers = any (`elem` ["-o", "--others"]) args
    case junction of
        Just "1" | hasOthers -> Git.runGitHere ("ls-files" : args)
        _ -> do
            resolved <- resolveExcludeFilePaths args
            Git.runGitRawIn prefix ("ls-files" : resolved)

reset :: FilePath -> [String] -> IO ExitCode
reset prefix args = Git.runGitRawIn prefix ("reset" : args)

rm :: [String] -> BitM ExitCode
rm args = do
    cwd <- asks envCwd
    prefix <- asks envPrefix
    let flags = parseRmFlags args
        -- Strip quiet flags so git always outputs the file list for parsing
        gitArgs = stripQuiet args
    (code, out, err) <- liftIO $ Git.runGitWithOutputIn prefix ("rm" : gitArgs)

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

mv :: FilePath -> [String] -> IO ExitCode
mv prefix args = Git.runGitRawIn prefix ("mv" : args)

branch :: FilePath -> [String] -> IO ExitCode
branch prefix args = Git.runGitRawIn prefix ("branch" : args)

merge :: FilePath -> [String] -> IO ExitCode
merge prefix args = Git.runGitRawIn prefix ("merge" : args)

-- ============================================================================
-- Stateful passthrough (needs BitEnv)
-- ============================================================================

status :: [String] -> BitM ExitCode
status args = do
    prefix <- asks envPrefix
    liftIO $ Git.runGitRawIn prefix ("status" : args)

restore :: [String] -> BitM ExitCode
restore = doRestore

checkout :: [String] -> BitM ExitCode
checkout = doCheckout

-- ============================================================================
-- Merge/branch management
-- ============================================================================

mergeContinue :: BitM ()
mergeContinue = do
    bitDir <- asks envBitDir
    mRemote <- asks envRemote
    let conflictsDir = bitDir </> "conflicts"
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
                    traverse_ (\remote ->
                            Transport.syncBinariesAfterMerge remote oldHead) mRemote
                _ -> liftIO dieNoMergeInProgress
       | otherwise -> do
                invalid <- liftIO $ validateMetadataDir (bitDir </> "index")
                unless (null invalid) $ liftIO $ do
                    hPutStrLn stderr Conflict.conflictMarkersFatalMessage
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

                traverse_ (\remote ->
                        Transport.syncBinariesAfterMerge remote oldHead) mRemote

mergeAbort :: IO ()
mergeAbort = doMergeAbort

-- | Print "error: no merge in progress." and exit with failure.
dieNoMergeInProgress :: IO ()
dieNoMergeInProgress = do
    hPutStrLn stderr "error: no merge in progress."
    exitWith (ExitFailure 1)

doMergeAbort :: IO ()
doMergeAbort = do
    cwd <- Dir.getCurrentDirectory
    -- Check for bitlink (separated repos) or normal .bit directory
    let dotBit = cwd </> ".bit"
    isDir <- Dir.doesDirectoryExist dotBit
    bitDir <- if isDir then pure dotBit
              else do
                isFile <- Dir.doesFileExist dotBit
                if isFile
                    then do
                        content <- readFile dotBit
                        case lines content of
                            (firstLine:_) -> pure (drop 8 (filter (/= '\r') firstLine))
                            [] -> pure dotBit
                    else pure dotBit
    let conflictsDir = bitDir </> "conflicts"
    
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
        _ -> dieNoMergeInProgress

unsetUpstream :: IO ()
unsetUpstream = void Git.unsetBranchUpstream

-- ============================================================================
-- Restore and Checkout implementations
-- ============================================================================

doRestore :: [String] -> BitM ExitCode
doRestore args = do
    cwd <- asks envCwd
    prefix <- asks envPrefix
    code <- lift $ Git.runGitRawIn prefix ("restore" : args)
    when (code == ExitSuccess) $ do
        let stagedOnly = ("--staged" `elem` args || "-S" `elem` args) &&
                         not ("--worktree" `elem` args || "-W" `elem` args)
        unless stagedOnly $ do
            let rawPaths = restoreCheckoutPaths args
                adjustedPaths = if null prefix then rawPaths
                                else map (prefix </>) rawPaths
            syncTextFilesFromIndex cwd adjustedPaths
    pure code

doCheckout :: [String] -> BitM ExitCode
doCheckout args = do
    prefix <- asks envPrefix
    -- Pass args through to git as-is. Git handles branch vs path
    -- disambiguation naturally; inserting -- would force path interpretation
    -- and break branch switching (e.g. "checkout <branch>").
    let args' = args
    code <- lift $ Git.runGitRawIn prefix ("checkout" : args')
    when (code == ExitSuccess) $ do
        cwd <- asks envCwd
        let rawPaths = restoreCheckoutPaths args'
            adjustedPaths = if null prefix then rawPaths
                            else map (prefix </>) rawPaths
        syncTextFilesFromIndex cwd adjustedPaths
    pure code

-- | After a successful restore/checkout, sync .bit/index/ to working directory:
-- text files: copy content from index. Binary metadata: copy blob from CAS to work path.
syncTextFilesFromIndex :: FilePath -> [FilePath] -> BitM ()
syncTextFilesFromIndex cwd rawPaths = do
    bitDir <- asks envBitDir
    let casDir = bitDir </> "cas"
    paths <- lift $ expandPathsToFiles cwd rawPaths
    forM_ paths $ \filePath -> do
        let metaPath = bitDir </> "index" </> filePath
        let workPath = cwd </> filePath
        metaExists <- lift $ fileExistsE metaPath
        when metaExists $ do
            mcontent <- lift $ readFileE metaPath
            let isBinaryMetadata = maybe True (\content -> any ("hash: " `isPrefixOf`) (lines content)) mcontent
            if isBinaryMetadata
                then do
                    -- Restore binary from CAS when blob exists; in lite mode CAS is often empty — warn and skip, do not hard-fail
                    case mcontent of
                        Just content -> do
                            case parseMetadata content of
                                Just mc -> do
                                    ok <- lift $ copyBlobFromCasTo casDir (metaHash mc) workPath
                                    unless ok $ lift $ do
                                        hPutStrLn stderr ("warning: " ++ filePath ++ " — no content in CAS (e.g. lite mode); restore skipped. Use solid mode or pull from remote to get content.")
                                Nothing -> lift $ do
                                    hPutStrLn stderr ("warning: invalid metadata for " ++ filePath ++ ", skipping restore")
                        Nothing -> pure ()
                else lift $ do
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

-- | Resolve file path arguments for flags that take a file path (-X, --exclude-from).
-- When git runs with -C .bit/index, relative paths resolve from the wrong directory.
-- This makes them absolute so they resolve correctly regardless of -C.
resolveExcludeFilePaths :: [String] -> IO [String]
resolveExcludeFilePaths [] = pure []
resolveExcludeFilePaths [x] = pure [x]
resolveExcludeFilePaths (flag:path:rest)
    | flag `elem` ["-X", "--exclude-from"] = do
        absPath <- Dir.makeAbsolute path
        resolved <- resolveExcludeFilePaths rest
        pure (flag : absPath : resolved)
    | otherwise = do
        resolved <- resolveExcludeFilePaths (path:rest)
        pure (flag : resolved)

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
