{-# LANGUAGE MultiWayIf #-}

module Bit.Git.Run
    ( add
    , commit
    , commitFile
    , diff
    , init
    , reset
    , rm
    , mv
    , branch
    , merge
    , createBundle
    , config
    , getLocalHead
    , AncestorQuery(..)
    , checkIsAhead
    , getHashFromBundle
    , restore
    , checkout
    , status
    , addRemote
    , getRemoteUrl
    , getTrackedRemoteName
    , getConfiguredRemoteName
    , updateRemoteTrackingBranch
    , updateRemoteTrackingBranchToHead
    , updateRemoteTrackingBranchToHash
    , setupBranchTracking
    , setupBranchTrackingFor
    , unsetBranchUpstream
    , mergeAbort
    , isMergeInProgress
    , checkoutRemoteAsMain
    , getConflictedFiles
    , getConflictType
    , checkoutOurs
    , checkoutTheirs
    , runGitRaw
    , runGitRawAt
    , runGitWithOutput
    , runGitAt
    , rewriteGitHints
    , DeletedSide(..)
    , ConflictType(..)
    , readFileFromRef
    , listFilesInRef
    , fsck
    , hasStagedChanges
    , getDiffNameStatus
    , getFilesAtCommit
    , remoteTrackingRef
    , NameStatusChange(..)
    , parseNameStatusOutput
    , getRemoteTrackingHash
    , runGitGlobal
    ) where

import Data.Maybe (mapMaybe, listToMaybe)

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Bit.Config.Paths
import Data.Char (isSpace)
import Control.Monad (when, guard)
import Prelude hiding (init)
import Data.List (isPrefixOf)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Environment (lookupEnv)

baseFlags :: [String]
baseFlags = ["-C", bitIndexPath]

-- | Build the tracking ref for a named remote: @refs/remotes/\<name\>/main@
remoteTrackingRef :: String -> String
remoteTrackingRef name = "refs/remotes/" ++ name ++ "/main"

-- | Read the hash from the tracking ref for a named remote (refs/remotes/<name>/main).
-- Returns Nothing if the ref doesn't exist (e.g. first push before any fetch).
getRemoteTrackingHash :: String -> IO (Maybe String)
getRemoteTrackingHash name = do
    (code, out, _) <- runGitWithOutput ["rev-parse", remoteTrackingRef name]
    pure $ case code of
        ExitSuccess -> Just (filter (not . isSpace) out)
        _ -> Nothing

-- | Query: is aqAncestor an ancestor of aqDescendant? Record avoids transposing the two String hashes.
data AncestorQuery = AncestorQuery { aqAncestor, aqDescendant :: String }
  deriving (Show, Eq)

-- | Represents the subset of Git functionality rgit uses
data GitCommand
    = Init { _separateGitDir :: FilePath }
    | Config { _configName :: String, _configValue :: String }
    | RevParse { _revParseRef :: String }
    | CommitFile { _commitMessage :: String, _commitFile :: FilePath }
    | RevList { _revListLeft :: String, _revListRight :: String }
    | CreateBundle { _createBundlePath :: FilePath }
    | GetBundleHead { _getBundleHeadPath :: FilePath }
    | IsAncestor AncestorQuery
    | GetHead

-- | Run a Git command and return (ExitCode, StdOut, StdErr)
runGit :: GitCommand -> IO (ExitCode, String, String)
runGit cmd = do
    let subArgs = translateCommand cmd
    let fullArgs = baseFlags ++ subArgs
    -- We use readProcessWithExitCode so we can handle errors without crashing
    readProcessWithExitCode "git" fullArgs ""
  where
    translateCommand :: GitCommand -> [String]
    translateCommand c = case c of
        Init _dir ->
            -- dir is the full path to .git directory (e.g., .rgit/index/.git)
            -- We need to change to the parent directory and run git init there
            -- Git will automatically create .git in the current directory
            ["init"]

        Config k v ->
            ["config", k, v]

        RevParse r ->
            ["rev-parse", r]

        CommitFile msg f ->
            -- Using "-- f" at the end tells Git to only look at that path
            ["commit", "-m", msg, "--", f]

        RevList l r ->
            ["rev-list", "--left-right", "--count", l ++ "..." ++ r]

        CreateBundle path ->
            ["bundle", "create", path, "--all"]

        GetBundleHead path ->
            ["bundle", "list-heads", path]

        IsAncestor (AncestorQuery a d) ->
            ["merge-base", "--is-ancestor", a, d]

        GetHead ->
            ["rev-parse", "HEAD"]

getLocalHead :: IO (Maybe String)
getLocalHead = do
    (code, out, _) <- runGit GetHead
    pure (guard (code == ExitSuccess) >> Just (filter (not . isSpace) out))

getHashFromBundle :: BundleName -> IO (Maybe String)
getHashFromBundle bundleName = do
    let (GitRelPath relPath) = bundleGitRelPath bundleName
    (code, out, _) <- runGit (GetBundleHead relPath)
    pure (guard (code == ExitSuccess && not (null out)) >> listToMaybe (words out))

runGitCommand :: GitCommand -> IO ExitCode
runGitCommand cmd = do
    (c, o, e) <- runGit cmd
    -- Don't print error messages for IsAncestor since non-zero exit codes are expected
    -- (they indicate "no, not an ancestor" which is a valid answer, not an error)
    when (c /= ExitSuccess && not (isAncestorCommand cmd)) $ 
        hPutStrLn stderr ("bit: git command failed: " ++ e)
    putStr o
    hPutStr stderr e
    pure c
  where
    isAncestorCommand (IsAncestor _) = True
    isAncestorCommand _ = False

commitFile :: String -> FilePath -> IO ExitCode
commitFile msg filePath = runGitCommand (CommitFile msg filePath)

init :: FilePath -> IO ExitCode
init dir = runGitCommand (Init dir)

createBundle :: BundleName -> IO ExitCode
createBundle bundleName =
    let (GitRelPath relPath) = bundleGitRelPath bundleName
    in runGitCommand (CreateBundle relPath)

config :: String -> String -> IO ExitCode
config configName configValue = runGitCommand (Config configName configValue)

-- | Check if aqDescendant is ahead of aqAncestor (i.e., aqAncestor is an ancestor of aqDescendant).
checkIsAhead :: AncestorQuery -> IO Bool
checkIsAhead q =
    (== ExitSuccess) <$> runGitCommand (IsAncestor q)

replace :: String -> String -> String -> String
replace _ _ [] = []
replace old new str@(c:cs)
    | old `isPrefixOf` str = new ++ replace old new (drop (length old) str)
    | otherwise            = c : replace old new cs

rewriteGitHints :: String -> String
rewriteGitHints =
    replace "(use \"git " "(use \"bit "



runGitRaw :: [String] -> IO ExitCode
runGitRaw args = do
  noColor <- lookupEnv "BIT_NO_COLOR"
  let colorFlag = case noColor of
        Just "1" -> "never"
        Just "true" -> "never"
        _ -> "auto"
  let fullArgs =
        baseFlags
        ++ ["-c", "color.ui=" ++ colorFlag]
        ++ args

  (code, out, err) <- readProcessWithExitCode "git" fullArgs ""

  putStr (rewriteGitHints out)
  hPutStr stderr (rewriteGitHints err)

  case code of
    ExitSuccess   -> pure ()
    ExitFailure n ->
      hPutStrLn stderr ("bit: git exited with code " ++ show n)

  pure code

-- | Like runGitRaw but targets an arbitrary directory instead of .bit/index.
runGitRawAt :: FilePath -> [String] -> IO ExitCode
runGitRawAt dir args = do
  noColor <- lookupEnv "BIT_NO_COLOR"
  let colorFlag = case noColor of
        Just "1" -> "never"
        Just "true" -> "never"
        _ -> "auto"
  let fullArgs =
        ["-C", dir]
        ++ ["-c", "color.ui=" ++ colorFlag]
        ++ args

  (code, out, err) <- readProcessWithExitCode "git" fullArgs ""

  putStr (rewriteGitHints out)
  hPutStr stderr (rewriteGitHints err)

  case code of
    ExitSuccess   -> pure ()
    ExitFailure n ->
      hPutStrLn stderr ("bit: git exited with code " ++ show n)

  pure code

add :: [String] -> IO ExitCode
add     = runGitRaw . ("add" :)
commit :: [String] -> IO ExitCode
commit  = runGitRaw . ("commit" :)
diff :: [String] -> IO ExitCode
diff    = runGitRaw . ("diff" :)
restore :: [String] -> IO ExitCode
restore  = runGitRaw . ("restore" :)
checkout :: [String] -> IO ExitCode
checkout = runGitRaw . ("checkout" :)
status :: [String] -> IO ExitCode
status   = runGitRaw . ("status" :)
reset :: [String] -> IO ExitCode
reset   = runGitRaw . ("reset" :)
rm :: [String] -> IO ExitCode
rm      = runGitRaw . ("rm" :)
mv :: [String] -> IO ExitCode
mv      = runGitRaw . ("mv" :)
branch :: [String] -> IO ExitCode
branch  = runGitRaw . ("branch" :)
merge :: [String] -> IO ExitCode
merge   = runGitRaw . ("merge" :)

-- | Add or update a remote (Git-style: git remote add <name> <url> / set-url if exists)
addRemote :: String -> String -> IO ExitCode
addRemote remoteName url = do
    (code, _, _) <- readProcessWithExitCode "git" (baseFlags ++ ["remote", "get-url", remoteName]) ""
    case code of
        ExitSuccess -> do
            readProcessWithExitCode "git" (baseFlags ++ ["remote", "set-url", remoteName, url]) "" >>= \(c, _, _) -> pure c
        ExitFailure _ -> do
            readProcessWithExitCode "git" (baseFlags ++ ["remote", "add", remoteName, url]) "" >>= \(c, _, _) -> pure c

-- | Get the URL for a remote by name (git remote get-url <name>). Returns Nothing if remote missing.
getRemoteUrl :: String -> IO (Maybe String)
getRemoteUrl remoteName = do
    (code, out, _) <- readProcessWithExitCode "git" (baseFlags ++ ["remote", "get-url", remoteName]) ""
    pure $ case code of
        ExitSuccess -> Just (filter (/= '\n') out)
        _ -> Nothing

-- | Get the remote name that the current branch tracks (branch.main.remote).
-- Falls back to "origin" if not configured — this means commands work with
-- a reasonable default even without explicit -u, but callers should be aware
-- that "origin" fallback doesn't mean tracking IS configured.
getTrackedRemoteName :: IO String
getTrackedRemoteName = do
    (code, out, _) <- readProcessWithExitCode "git" (baseFlags ++ ["config", "--get", "branch.main.remote"]) ""
    pure $ case code of
        ExitSuccess -> filter (/= '\n') out
        _ -> "origin"

-- | Get the explicitly configured upstream remote name (branch.main.remote).
-- Returns Nothing if no upstream is configured. Does NOT fall back to "origin".
-- Used by pull which requires explicit upstream (spec: "bit pull requires
-- explicit remote (no fallback)"). Fetch uses getTrackedRemoteName instead
-- (falls back to "origin", git-standard behavior).
getConfiguredRemoteName :: IO (Maybe String)
getConfiguredRemoteName = do
    (code, out, _) <- readProcessWithExitCode "git" (baseFlags ++ ["config", "--get", "branch.main.remote"]) ""
    pure $ case code of
        ExitSuccess -> let name = filter (/= '\n') out
                       in if null name then Nothing else Just name
        _ -> Nothing

-- | Update the remote tracking branch refs/remotes/<name>/main to point to the hash from the bundle.
-- Use when the objects are already in the repo (e.g. after push).
updateRemoteTrackingBranch :: String -> BundleName -> IO ExitCode
updateRemoteTrackingBranch name bundleName =
    getHashFromBundle bundleName >>= maybe (pure (ExitFailure 1)) (updateRemoteTrackingBranchToHash name)

-- | Set refs/remotes/<name>/main to a specific hash. Use after a successful pull so status shows
-- "up to date with '<name>/main'" instead of "ahead by N commits".
updateRemoteTrackingBranchToHash :: String -> String -> IO ExitCode
updateRemoteTrackingBranchToHash name hash =
    readProcessWithExitCode "git" (baseFlags ++ ["update-ref", remoteTrackingRef name, hash]) "" >>= \(c, _, _) -> pure c

-- | Set refs/remotes/<name>/main to current HEAD.
-- WARNING: Only correct after PUSH (where remote now matches local HEAD).
-- After PULL/MERGE, use updateRemoteTrackingBranchToHash with the remote hash instead,
-- because HEAD includes local merge commits the remote doesn't have.
-- See: "Tracking Ref Invariant" in docs/spec.md.
updateRemoteTrackingBranchToHead :: String -> IO ExitCode
updateRemoteTrackingBranchToHead name = do
    (code, out, _) <- readProcessWithExitCode "git" (baseFlags ++ ["rev-parse", "HEAD"]) ""
    case filter (/= '\n') out of
        hash | code == ExitSuccess && not (null hash) ->
            updateRemoteTrackingBranchToHash name hash
        _ -> pure (ExitFailure 1)

-- | Set up the local branch to track a specific remote
-- Configures branch.main.remote and branch.main.merge
setupBranchTrackingFor :: String -> IO ExitCode
setupBranchTrackingFor remoteName = do
    (code1, _, _) <- readProcessWithExitCode "git"
        (baseFlags ++ ["config", "branch.main.remote", remoteName]) ""
    (code2, _, _) <- readProcessWithExitCode "git"
        (baseFlags ++ ["config", "branch.main.merge", "refs/heads/main"]) ""
    case (code1, code2) of
        (ExitSuccess, ExitSuccess) -> pure ExitSuccess
        _ -> pure (ExitFailure 1)

-- | Set up the local branch to track origin/main
-- This configures branch.main.remote and branch.main.merge so git status knows what to compare
setupBranchTracking :: IO ExitCode
setupBranchTracking = setupBranchTrackingFor "origin"

-- | Unset the upstream for the current branch (clears "upstream is gone" when remote refs are missing)
unsetBranchUpstream :: IO ExitCode
unsetBranchUpstream = do
    (code, _, _) <- readProcessWithExitCode "git" (baseFlags ++ ["branch", "--unset-upstream"]) ""
    pure code

-- | Run git with baseFlags; returns (exitCode, stdout, stderr). Does not rewrite hints.
runGitWithOutput :: [String] -> IO (ExitCode, String, String)
runGitWithOutput args = do
  let fullArgs = baseFlags ++ ["-c", "color.ui=never"] ++ args
  readProcessWithExitCode "git" fullArgs ""

-- | Abort an in-progress merge.
mergeAbort :: IO ExitCode
mergeAbort = do
  (code, out, err) <- runGitWithOutput ["merge", "--abort"]
  putStr (rewriteGitHints out)
  hPutStr stderr (rewriteGitHints err)
  pure code

-- | True if a merge is in progress (MERGE_HEAD exists).
isMergeInProgress :: IO Bool
isMergeInProgress = do
  (code, _, _) <- runGitWithOutput ["rev-parse", "--verify", "MERGE_HEAD"]
  pure (code == ExitSuccess)

-- | Checkout refs/remotes/<name>/main as the local main branch.
-- Used on first pull when there are no local commits (unborn branch).
-- This avoids the need for merge and gives us the remote's history directly.
--
-- TRACKING CONTRACT: This function NEVER modifies branch tracking config.
-- Uses --no-track to prevent git from auto-setting branch.main.remote.
-- Callers who need tracking must call setupBranchTrackingFor explicitly.
--
-- Uses -f (force) to overwrite any local files created during init.
checkoutRemoteAsMain :: String -> IO ExitCode
checkoutRemoteAsMain name = do
  -- Use checkout -B to create/reset branch and checkout in one step
  -- Use -f to force overwrite of any local files (like .gitattributes from init)
  -- Use --no-track to prevent auto-setting branch.main.remote (git-standard: require explicit -u)
  (code, _, _) <- runGitWithOutput ["checkout", "-f", "-B", "main", "--no-track", remoteTrackingRef name]
  pure code

-- | Paths relative to work tree (index/...) that are unmerged.
getConflictedFiles :: IO [FilePath]
getConflictedFiles = do
  (code, out, _) <- runGitWithOutput ["diff", "--name-only", "--diff-filter=U"]
  pure $ case code of
    ExitSuccess -> filter (not . null) (lines out)
    _ -> []

-- | Which side deleted the file in a modify/delete conflict.
data DeletedSide
  = DeletedInOurs   -- ^ Deleted in HEAD (ours); modified in theirs
  | DeletedInTheirs -- ^ Deleted in theirs; modified in HEAD (ours)
  deriving (Show, Eq)

-- | Conflict type for Git-like messages. Path is work-tree relative (e.g. index/src/model.bin).
data ConflictType
  = ContentConflict FilePath
  | ModifyDelete FilePath DeletedSide
  | AddAdd FilePath
  deriving (Show, Eq)

-- | Determine conflict type using git ls-files -u. Path is as in index (e.g. index/foo).
-- Format: "mode SP oid SP stage TAB name"
getConflictType :: FilePath -> IO ConflictType
getConflictType path = do
  (_, out, _) <- runGitWithOutput ["ls-files", "-u", "--", path]
  let beforeTab line = takeWhile (/= '\t') line
  let stageNums :: [Int]
      stageNums = mapMaybe stageNum (lines out)
      stageNum line = case reverse (words (beforeTab line)) of
        ("1":_) -> Just (1 :: Int)
        ("2":_) -> Just 2
        ("3":_) -> Just 3
        _ -> Nothing
  let has1 = 1 `elem` stageNums
  let has2 = 2 `elem` stageNums
  let has3 = 3 `elem` stageNums
  pure $ if | has2 && has3 && has1     -> ContentConflict path
            | has2 && has3 && not has1 -> AddAdd path
            | has2 && not has3         -> ModifyDelete path DeletedInTheirs
            | has3 && not has2         -> ModifyDelete path DeletedInOurs
            | otherwise                -> ContentConflict path

-- | Check out our version for path (work-tree path under .rgit/index).
checkoutOurs :: FilePath -> IO ExitCode
checkoutOurs path = do
  (code, out, err) <- runGitWithOutput ["checkout", "--ours", "--", path]
  putStr (rewriteGitHints out)
  hPutStr stderr (rewriteGitHints err)
  pure code

-- | Check out their version for path.
checkoutTheirs :: FilePath -> IO ExitCode
checkoutTheirs path = do
  (code, out, err) <- runGitWithOutput ["checkout", "--theirs", "--", path]
  putStr (rewriteGitHints out)
  hPutStr stderr (rewriteGitHints err)
  pure code

-- | Read file content from a Git ref (e.g., "refs/remotes/origin/main:path/to/file").
-- Returns Nothing if file doesn't exist in that ref.
readFileFromRef :: String -> FilePath -> IO (Maybe String)
readFileFromRef gitRef path = do
  (code, out, _err) <- runGitWithOutput ["show", gitRef ++ ":" ++ path]
  pure $ case code of
    ExitSuccess | not (null out) -> Just out
    _ -> Nothing

-- | List all files in a Git ref's tree (recursive). Returns paths relative to work tree root.
listFilesInRef :: String -> IO [FilePath]
listFilesInRef gitRef = do
  (code, out, _) <- runGitWithOutput ["ls-tree", "-r", "--name-only", gitRef]
  pure $ case code of
    ExitSuccess -> filter (not . null) (lines out)
    _ -> []

-- | Run git fsck to check metadata history integrity.
-- Returns (exitCode, output, errorOutput).
fsck :: IO (ExitCode, String, String)
fsck = runGitWithOutput ["fsck"]

-- | Check if there are staged changes ready to commit.
-- Returns True if there are staged changes, False otherwise.
hasStagedChanges :: IO Bool
hasStagedChanges = do
  (code, _, _) <- runGitWithOutput ["diff", "--cached", "--quiet"]
  pure (code == ExitFailure 1)  -- git diff --cached --quiet exits with 1 if there are changes

-- | Parsed line from `git diff --name-status` output.
-- Makes invalid states unrepresentable (no bare Char + Maybe tuple).
data NameStatusChange
    = Added FilePath
    | Deleted FilePath
    | Modified FilePath
    | Renamed FilePath FilePath  -- ^ old path, new path
    | Copied FilePath FilePath   -- ^ old path, new path
    deriving (Show, Eq)

-- | Parse raw `git diff --name-status` output into structured changes.
parseNameStatusOutput :: String -> [NameStatusChange]
parseNameStatusOutput = mapMaybe parseLine . lines
  where
    parseLine line = case line of
        (fileStatus:rest)
            | fileStatus == 'R' || fileStatus == 'C' ->
                case words (dropWhile (\c -> c /= '\t' && c /= ' ') rest) of
                    (old:new:_) -> Just $ case fileStatus of
                        'R' -> Renamed old new
                        _  -> Copied old new
                    _ -> Nothing
            | fileStatus == 'A' ->
                case words rest of (path:_) -> Just (Added path); _ -> Nothing
            | fileStatus == 'D' ->
                case words rest of (path:_) -> Just (Deleted path); _ -> Nothing
            | fileStatus == 'M' ->
                case words rest of (path:_) -> Just (Modified path); _ -> Nothing
            | otherwise -> Nothing
        _ -> Nothing

-- | Get the list of file changes between two commits.
getDiffNameStatus :: String -> String -> IO [NameStatusChange]
getDiffNameStatus oldHead newHead = do
    (code, out, _) <- runGitWithOutput ["diff", "-M", "--name-status", oldHead, newHead]
    pure $ case code of
        ExitSuccess -> parseNameStatusOutput out
        _ -> []

-- | Get all file paths at a given commit. Used when there's no old HEAD to diff against.
getFilesAtCommit :: String -> IO [FilePath]
getFilesAtCommit gitRef = do
    (code, out, _) <- runGitWithOutput ["ls-tree", "-r", "--name-only", gitRef]
    pure $ case code of
        ExitSuccess -> filter (not . null) (lines out)
        _ -> []

-- | Run a git command targeting a specific index path (for filesystem remotes).
-- This is used when operating on a remote filesystem repo directly.
-- The indexPath should be the path to the .bit/index directory (NOT the .git subdirectory).
runGitAt :: FilePath -> [String] -> IO (ExitCode, String, String)
runGitAt indexPath args = readProcessWithExitCode "git" (["-C", indexPath] ++ args) ""

-- | Run git without -C .bit/index (no repo context).
-- For commands that don't need a repo: --exec-path, --version, etc.
runGitGlobal :: [String] -> IO ExitCode
runGitGlobal args = do
  (code, out, err) <- readProcessWithExitCode "git" args ""
  putStr out
  hPutStr stderr err
  pure code
