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
    , revert
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
      -- Working tree sync
    , syncWorkingTreeFromDiff
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
import Bit.CDC.Manifest (readManifestFromCas)
import Bit.CDC.Reassemble (reassembleFile)
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

reset :: [String] -> BitM ExitCode
reset args = do
    prefix <- asks envPrefix
    junction <- liftIO $ lookupEnv "BIT_GIT_JUNCTION"
    let hasHard = "--hard" `elem` args
    case junction of
        Just "1" | hasHard -> do
            -- Junction mode --hard: run via junction so git uses CWD as work tree
            liftIO $ Git.runGitHere ("reset" : args)
        _ | hasHard -> do
            -- Normal mode --hard: need to sync working tree after reset
            oldHead <- liftIO getLocalHeadE
            code <- liftIO $ Git.runGitRawIn prefix ("reset" : args)
            when (code == ExitSuccess) $
                syncWorkingTreeFromDiff oldHead
            pure code
        _ ->
            -- --soft or --mixed (default): no working tree sync needed
            liftIO $ Git.runGitRawIn prefix ("reset" : args)

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

mv :: [String] -> BitM ExitCode
mv args = do
    cwd <- asks envCwd
    prefix <- asks envPrefix
    -- Parse move pairs from args before running (git mv produces no parseable output)
    moves <- liftIO $ parseMvArgs cwd args
    code <- liftIO $ Git.runGitRawIn prefix ("mv" : args)
    when (code == ExitSuccess) $ liftIO $ do
        forM_ moves $ \(old, new) -> do
            let oldWork = cwd </> old
                newWork = cwd </> new
            oldExists <- Dir.doesFileExist oldWork
            when oldExists $ do
                Dir.createDirectoryIfMissing True (takeDirectory newWork)
                Dir.renameFile oldWork newWork
                removeEmptyParents cwd (takeDirectory oldWork)
    pure code

branch :: FilePath -> [String] -> IO ExitCode
branch prefix args = Git.runGitRawIn prefix ("branch" : args)

merge :: FilePath -> [String] -> IO ExitCode
merge prefix args = do
    junction <- lookupEnv "BIT_GIT_JUNCTION"
    case junction of
        Just "1" -> Git.runGitHere ("merge" : args)
        _ -> Git.runGitRawIn prefix ("merge" : args)

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

-- | Revert a commit, then sync changed binary files from CAS to working tree.
revert :: [String] -> BitM ExitCode
revert args = do
    prefix <- asks envPrefix
    code <- lift $ Git.runGitRawIn prefix ("revert" : args)
    when (code == ExitSuccess) $ do
        cwd <- asks envCwd
        -- After revert creates a new commit, get the files it changed
        changes <- lift $ Git.getDiffNameStatus "HEAD~1" "HEAD"
        let changedPaths = concatMap changedFilePaths changes
        syncTextFilesFromIndex cwd changedPaths
    pure code
  where
    changedFilePaths (Git.Added p)      = [p]
    changedFilePaths (Git.Modified p)   = [p]
    changedFilePaths (Git.Deleted _)    = []
    changedFilePaths (Git.Renamed _ p)  = [p]
    changedFilePaths (Git.Copied _ p)   = [p]

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
    junction <- liftIO $ lookupEnv "BIT_GIT_JUNCTION"
    let stagedOnly = ("--staged" `elem` args || "-S" `elem` args) &&
                     not ("--worktree" `elem` args || "-W" `elem` args)
    case junction of
        Just "1" | not stagedOnly -> do
            -- Junction mode with worktree changes: run through junction
            liftIO $ Git.runGitHere ("restore" : args)
        _ -> do
            code <- lift $ Git.runGitRawIn prefix ("restore" : args)
            when (code == ExitSuccess) $
                unless stagedOnly $ do
                    let rawPaths = restoreCheckoutPaths args
                        adjustedPaths = if null prefix then rawPaths
                                        else map (prefix </>) rawPaths
                    syncTextFilesFromIndex cwd adjustedPaths
            pure code

doCheckout :: [String] -> BitM ExitCode
doCheckout args = do
    prefix <- asks envPrefix
    junction <- liftIO $ lookupEnv "BIT_GIT_JUNCTION"
    let isBranchSwitch = not (hasDashDash args) && not (hasPathCheckoutFlags args)
    case junction of
        Just "1" | isBranchSwitch -> do
            -- Junction mode branch switch: run through junction so git uses CWD as work tree
            liftIO $ Git.runGitHere ("checkout" : args)
        _ | isBranchSwitch -> do
            -- Normal mode branch switch: use diff-based sync
            oldHead <- liftIO getLocalHeadE
            code <- lift $ Git.runGitRawIn prefix ("checkout" : args)
            when (code == ExitSuccess) $
                syncWorkingTreeFromDiff oldHead
            pure code
        _ -> do
            -- Path checkout: use path-based sync (existing behavior)
            code <- lift $ Git.runGitRawIn prefix ("checkout" : args)
            when (code == ExitSuccess) $ do
                cwd <- asks envCwd
                let rawPaths = restoreCheckoutPaths args
                    adjustedPaths = if null prefix then rawPaths
                                    else map (prefix </>) rawPaths
                syncTextFilesFromIndex cwd adjustedPaths
            pure code
  where
    hasDashDash = elem "--"
    -- Flags that indicate path checkout (not branch switch)
    hasPathCheckoutFlags as = any (`elem` as) ["--ours", "--theirs", "--merge", "-p", "--patch"]

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
                                    -- Try CDC manifest first (chunked files), fall back to whole blob
                                    mManifest <- lift $ readManifestFromCas casDir (metaHash mc)
                                    case mManifest of
                                        Just manifest -> void $ lift $ reassembleFile casDir manifest workPath
                                        Nothing -> do
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

-- | Parse mv source→dest pairs from the args (git mv doesn't output pairs, so we parse args).
-- Supports: git mv <source> <dest>, git mv <source>... <dest-dir>
parseMvOutput :: String -> [(FilePath, FilePath)]
parseMvOutput _ = []
-- Note: git mv produces no parseable output. Instead, we parse the args directly.

-- | Extract mv source→dest pairs from command args.
-- git mv <source> <dest> → [(source, dest)]
-- git mv <source>... <dir> → [(source, dir/basename source)]
-- We skip flags (starting with -)
parseMvArgs :: FilePath -> [String] -> IO [(FilePath, FilePath)]
parseMvArgs _ [] = pure []
parseMvArgs _ [_] = pure []
parseMvArgs cwd args = do
    let nonFlags = filter (\a -> not ("-" `isPrefixOf` a) && a /= "--") args
    case nonFlags of
        [] -> pure []
        [_] -> pure []
        paths -> do
            let dest = last paths
                sources = Prelude.init paths
                destPath = cwd </> dest
            isDir <- Dir.doesDirectoryExist destPath
            if isDir
                then pure [(s, dest </> takeBaseName' s) | s <- sources]
                else case sources of
                    [src] -> pure [(src, dest)]
                    _ -> pure []  -- multiple sources to non-dir is an error
  where
    takeBaseName' p = case reverse (splitPath' p) of
        (x:_) -> x
        [] -> p
    splitPath' = filter (not . null) . go
      where go [] = []
            go s = let (w, rest) = break (== '/') s
                       rest' = drop 1 rest
                   in w : go rest'

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

-- ============================================================================
-- Diff-based working tree sync
-- ============================================================================

-- | After a git operation that changes HEAD in .bit/index/, diff old vs new HEAD
-- and sync the working tree accordingly. Handles add/modify/delete/rename.
-- When oldHead is Nothing (e.g. unborn branch), treats all files at new HEAD as added.
syncWorkingTreeFromDiff :: Maybe String -> BitM ()
syncWorkingTreeFromDiff oldHead = do
    cwd <- asks envCwd
    bitDir <- asks envBitDir
    newHead <- liftIO getLocalHeadE
    case newHead of
        Nothing -> pure ()  -- no HEAD after operation (shouldn't happen)
        Just new -> do
            changes <- case oldHead of
                Just old | old /= new ->
                    liftIO $ Git.getDiffNameStatus old new
                Just _ -> pure []  -- same commit, no changes
                Nothing -> do
                    -- No old HEAD (unborn branch) — treat all files as Added
                    files <- liftIO $ Git.getFilesAtCommit new
                    pure $ map Git.Added files
            let indexDir = bitDir </> "index"
            liftIO $ forM_ changes $ \change -> case change of
                Git.Added p -> syncFileFromIndex indexDir cwd p
                Git.Modified p -> syncFileFromIndex indexDir cwd p
                Git.Renamed old new' -> do
                    let oldWork = cwd </> old
                    oldExists <- Dir.doesFileExist oldWork
                    when oldExists $ do
                        Dir.removeFile oldWork
                        removeEmptyParents cwd (takeDirectory oldWork)
                    syncFileFromIndex indexDir cwd new'
                Git.Copied _ new' -> syncFileFromIndex indexDir cwd new'
                Git.Deleted p -> do
                    let workPath = cwd </> p
                    exists <- Dir.doesFileExist workPath
                    when exists $ do
                        Dir.removeFile workPath
                        removeEmptyParents cwd (takeDirectory workPath)
  where
    syncFileFromIndex :: FilePath -> FilePath -> FilePath -> IO ()
    syncFileFromIndex indexDir cwd path = do
        let src = indexDir </> path
            dst = cwd </> path
        srcExists <- Dir.doesFileExist src
        when srcExists $ do
            Dir.createDirectoryIfMissing True (takeDirectory dst)
            Dir.copyFile src dst

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
