{-# LANGUAGE MultiWayIf #-}

module Internal.Git
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
    , checkIsAhead
    , getHashFromBundle
    , restore
    , checkout
    , status
    , setupRemote
    , addRemote
    , getRemoteUrl
    , getTrackedRemoteName
    , fetchFromBundle
    , updateRemoteTrackingBranch
    , updateRemoteTrackingBranchToHead
    , updateRemoteTrackingBranchToHash
    , setupBranchTracking
    , setupBranchTrackingFor
    , unsetBranchUpstream
    , mergeOriginMain
    , mergeNoCommit
    , mergeNoCommitAllowUnrelated
    , mergeAbort
    , isMergeInProgress
    , checkoutRemoteAsMain
    , getConflictedFiles
    , getConflictType
    , checkoutOurs
    , checkoutTheirs
    , runGitRaw
    , runGitWithOutput
    , runGitAt
    , ConflictType(..)
    , readFileFromRef
    , listFilesInRef
    , fsck
    , hasStagedChanges
    , getDiffNameStatus
    , getFilesAtCommit
    ) where

import Data.Maybe (mapMaybe, listToMaybe)

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Internal.Config
import Data.Char (isSpace)
import Control.Monad (when, guard)
import Prelude hiding (init)
import Data.List (isPrefixOf)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Environment (lookupEnv)

baseFlags :: [String]
baseFlags = ["-C", bitIndexPath]

-- | Represents the subset of Git functionality rgit uses
data GitCommand
    = Init { _separateGitDir :: FilePath }
    | Config { _configName :: String, _configValue :: String }
    | RevParse { _revParseRef :: String }
    | CommitFile { _commitMessage :: String, _commitFile :: FilePath }
    | RevList { _revListLeft :: String, _revListRight :: String }
    | CreateBundle { _createBundlePath :: FilePath }
    | GetBundleHead { _getBundleHeadPath :: FilePath }
    | IsAncestor { _ancestorHash :: String, _descendantHash :: String }
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

        IsAncestor a d ->
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
    isAncestorCommand (IsAncestor _ _) = True
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

-- | Check if @localHash@ is ahead of @remoteHash@ (i.e., remote is an ancestor of local).
-- Parameter order: remote hash first, local hash second — matching @git merge-base --is-ancestor@.
checkIsAhead :: String -> String -> IO Bool
checkIsAhead remoteHash localHash =
    (== ExitSuccess) <$> runGitCommand (IsAncestor remoteHash localHash)

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

-- | Set up a git remote named "origin" pointing to the given URL (legacy / internal use)
setupRemote :: String -> IO ExitCode
setupRemote url = addRemote "origin" url

-- | Pull from a bundle file into the local repo: fetch the bundle's refs so all
-- objects and refs/remotes/origin/main exist in .rgit/index/.git. This is the "real"
-- pull from the fetched bundle; without it, the ref would point to a hash not in the repo.
fetchFromBundle :: BundleName -> IO ExitCode
fetchFromBundle bundleName = do
    let (GitRelPath bundle) = bundleGitRelPath bundleName
    (code, out, err) <- readProcessWithExitCode "git"
        (baseFlags ++ ["fetch", bundle, "+refs/heads/main:refs/remotes/origin/main"]) ""
    putStr out
    hPutStr stderr err
    pure code

-- | Update the remote tracking branch refs/remotes/origin/main to point to the hash from the bundle.
-- Use when the objects are already in the repo (e.g. after push); for fetch/pull use fetchFromBundle.
updateRemoteTrackingBranch :: BundleName -> IO ExitCode
updateRemoteTrackingBranch bundleName =
    getHashFromBundle bundleName >>= maybe (pure (ExitFailure 1)) updateRemoteTrackingBranchToHash

-- | Set refs/remotes/origin/main to a specific hash. Use after a successful pull so status shows
-- "up to date with 'origin/main'" instead of "ahead by N commits".
updateRemoteTrackingBranchToHash :: String -> IO ExitCode
updateRemoteTrackingBranchToHash hash =
    readProcessWithExitCode "git" (baseFlags ++ ["update-ref", "refs/remotes/origin/main", hash]) "" >>= \(c, _, _) -> pure c

-- | Set refs/remotes/origin/main to current HEAD.
-- WARNING: Only correct after PUSH (where remote now matches local HEAD).
-- After PULL/MERGE, use updateRemoteTrackingBranchToHash with the bundle hash instead,
-- because HEAD includes local merge commits the remote doesn't have.
-- See: "Tracking Ref Invariant" in docs/spec.md.
updateRemoteTrackingBranchToHead :: IO ExitCode
updateRemoteTrackingBranchToHead = do
    (code, out, _) <- readProcessWithExitCode "git" (baseFlags ++ ["rev-parse", "HEAD"]) ""
    case filter (/= '\n') out of
        hash | code == ExitSuccess && not (null hash) ->
            updateRemoteTrackingBranchToHash hash
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

-- | Merge refs/remotes/origin/main into the current branch (HEAD).
-- Used by rgit pull after fetching the remote bundle.
mergeOriginMain :: IO ExitCode
mergeOriginMain = runGitRaw ["merge", "refs/remotes/origin/main", "--no-edit"]

-- | Run git with baseFlags; returns (exitCode, stdout, stderr). Does not rewrite hints.
runGitWithOutput :: [String] -> IO (ExitCode, String, String)
runGitWithOutput args = do
  let fullArgs = baseFlags ++ ["-c", "color.ui=never"] ++ args
  readProcessWithExitCode "git" fullArgs ""

-- | Merge without committing (for pull flow). Returns (exitCode, stdout, stderr).
mergeNoCommit :: IO (ExitCode, String, String)
mergeNoCommit = runGitWithOutput ["merge", "--no-commit", "--no-ff", "refs/remotes/origin/main"]

-- | Like mergeNoCommit but allows merging unrelated histories (e.g. first pull into a fresh init).
mergeNoCommitAllowUnrelated :: IO (ExitCode, String, String)
mergeNoCommitAllowUnrelated = runGitWithOutput ["merge", "--no-commit", "--no-ff", "--allow-unrelated-histories", "refs/remotes/origin/main"]

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

-- | Checkout refs/remotes/origin/main as the local main branch.
-- Used on first pull when there are no local commits (unborn branch).
-- This avoids the need for merge and gives us the remote's history directly.
--
-- TRACKING CONTRACT: This function NEVER modifies branch tracking config.
-- Uses --no-track to prevent git from auto-setting branch.main.remote.
-- Callers who need tracking must call setupBranchTrackingFor explicitly.
--
-- Uses -f (force) to overwrite any local files created during init.
checkoutRemoteAsMain :: IO ExitCode
checkoutRemoteAsMain = do
  -- Use checkout -B to create/reset branch and checkout in one step
  -- Use -f to force overwrite of any local files (like .gitattributes from init)
  -- Use --no-track to prevent auto-setting branch.main.remote (git-standard: require explicit -u)
  (code, _, _) <- runGitWithOutput ["checkout", "-f", "-B", "main", "--no-track", "refs/remotes/origin/main"]
  pure code

-- | Paths relative to work tree (index/...) that are unmerged.
getConflictedFiles :: IO [FilePath]
getConflictedFiles = do
  (code, out, _) <- runGitWithOutput ["diff", "--name-only", "--diff-filter=U"]
  pure $ case code of
    ExitSuccess -> filter (not . null) (lines out)
    _ -> []

-- | Conflict type for Git-like messages. Path is work-tree relative (e.g. index/src/model.bin).
data ConflictType
  = ContentConflict FilePath   -- both modified
  | ModifyDelete FilePath Bool -- True = deleted in HEAD (ours)
  | AddAdd FilePath            -- both added different
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
            | has2 && not has3         -> ModifyDelete path False  -- deleted in theirs
            | has3 && not has2         -> ModifyDelete path True   -- deleted in ours (HEAD)
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

-- | Get the list of file changes between two commits.
-- Returns list of (status, path, maybe-new-path-for-renames).
-- Status: 'A' = added, 'D' = deleted, 'M' = modified, 'R' = renamed.
getDiffNameStatus :: String -> String -> IO [(Char, FilePath, Maybe FilePath)]
getDiffNameStatus oldHead newHead = do
    (code, out, _) <- runGitWithOutput ["diff", "--name-status", oldHead, newHead]
    pure $ case code of
        ExitSuccess -> parseNameStatus out
        _ -> []

parseNameStatus :: String -> [(Char, FilePath, Maybe FilePath)]
parseNameStatus = mapMaybe parseLine . lines
  where
    parseLine line = case line of
        (fileStatus:rest)
            | fileStatus == 'R' || fileStatus == 'C' ->
                -- R100\told\tnew or Rnnn old new (tab-separated)
                case words (dropWhile (\c -> c /= '\t' && c /= ' ') rest) of
                    (old:new:_) -> Just (fileStatus, old, Just new)
                    _ -> Nothing
            | fileStatus `elem` "ADM" ->
                case words rest of
                    (path:_) -> Just (fileStatus, path, Nothing)
                    _ -> Nothing
            | otherwise -> Nothing
        _ -> Nothing

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
