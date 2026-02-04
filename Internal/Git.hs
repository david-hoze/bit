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

import Data.List (lines)
import Data.Maybe (mapMaybe, listToMaybe)

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, (</>))
import Internal.Config
import Data.Char (isSpace)
import Control.Monad (when, guard)
import Prelude hiding (init)
import Data.List (isPrefixOf)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Environment (lookupEnv)

baseFlags = ["-C", bitIndexPath]
gitDir = ".git"

-- | Represents the subset of Git functionality rgit uses
data GitCommand
    = Init { separateGitDir :: FilePath }
    | Config { name :: String, value :: String }
    | RevParse { ref :: String }
    | CommitFile { message :: String, file :: FilePath }
    | RevList { left :: String, right :: String }
    | CreateBundle { createBundlePath :: FilePath }
    | GetBundleHead { getBundleHeadPath :: FilePath }
    | IsAncestor { ancestor :: String, descendant :: String }
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
        Init dir ->
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
    return $ (guard (code == ExitSuccess) >> Just (filter (not . isSpace) out))

getHashFromBundle :: BundleName -> IO (Maybe String)
getHashFromBundle name = do
    let (GitRelPath relPath) = bundleGitRelPath name
    (code, out, _) <- runGit (GetBundleHead relPath)
    return $ guard (code == ExitSuccess && not (null out)) >> listToMaybe (words out)

runGitCommand :: GitCommand -> IO ExitCode
runGitCommand cmd = do
    (c, o, e) <- runGit cmd
    -- Don't print error messages for IsAncestor since non-zero exit codes are expected
    -- (they indicate "no, not an ancestor" which is a valid answer, not an error)
    when (c /= ExitSuccess && not (isAncestorCommand cmd)) $ 
        hPutStrLn stderr ("bit: git command failed: " ++ e)
    putStr o
    hPutStr stderr e
    return c
  where
    isAncestorCommand (IsAncestor _ _) = True
    isAncestorCommand _ = False

commitFile message file = runGitCommand (CommitFile message file)

init dir = runGitCommand (Init dir)

createBundle :: BundleName -> IO ExitCode
createBundle name = do
    let (GitRelPath relPath) = bundleGitRelPath name
    runGitCommand (CreateBundle relPath)

config name value = runGitCommand (Config name value)

checkIsAhead :: String -> String -> IO Bool
checkIsAhead rHash lHash = do
    code <- runGitCommand (IsAncestor rHash lHash)
    return (code == ExitSuccess)

replace :: String -> String -> String -> String
replace _ _ [] = []
replace old new str@(c:cs)
    | old `isPrefixOf` str = new ++ replace old new (drop (length old) str)
    | otherwise            = c : replace old new cs

rewriteGitHints :: String -> String
rewriteGitHints =
    replace "(use \"git " "(use \"bit "


guardedArgs :: [String] -> IO [String]
guardedArgs args =
  if any (\a -> "--git-dir" `isPrefixOf` a || "--work-tree" `isPrefixOf` a || a == "-C") args
     then fail "bit: overriding git-dir/work-tree/-C is not allowed"
     else pure args

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

add     = runGitRaw . ("add" :)
commit  = runGitRaw . ("commit" :)
diff    = runGitRaw . ("diff" :)
restore  = runGitRaw . ("restore" :)
checkout = runGitRaw . ("checkout" :)
status   = runGitRaw . ("status" :)
reset   = runGitRaw . ("reset" :)
rm      = runGitRaw . ("rm" :)
mv      = runGitRaw . ("mv" :)
branch  = runGitRaw . ("branch" :)
merge   = runGitRaw . ("merge" :)

-- | Add or update a remote (Git-style: git remote add <name> <url> / set-url if exists)
addRemote :: String -> String -> IO ExitCode
addRemote name url = do
    (code, _, _) <- readProcessWithExitCode "git" (baseFlags ++ ["remote", "get-url", name]) ""
    case code of
        ExitSuccess -> do
            readProcessWithExitCode "git" (baseFlags ++ ["remote", "set-url", name, url]) "" >>= \(c, _, _) -> return c
        ExitFailure _ -> do
            readProcessWithExitCode "git" (baseFlags ++ ["remote", "add", name, url]) "" >>= \(c, _, _) -> return c

-- | Get the URL for a remote by name (git remote get-url <name>). Returns Nothing if remote missing.
getRemoteUrl :: String -> IO (Maybe String)
getRemoteUrl name = do
    (code, out, _) <- readProcessWithExitCode "git" (baseFlags ++ ["remote", "get-url", name]) ""
    if code /= ExitSuccess then return Nothing
    else return (Just (filter (/= '\n') out))

-- | Get the remote name that the current branch tracks (branch.main.remote). Defaults to "origin".
getTrackedRemoteName :: IO String
getTrackedRemoteName = do
    (code, out, _) <- readProcessWithExitCode "git" (baseFlags ++ ["config", "--get", "branch.main.remote"]) ""
    if code /= ExitSuccess then return "origin"
    else return (filter (/= '\n') out)

-- | Set up a git remote named "origin" pointing to the given URL (legacy / internal use)
setupRemote :: String -> IO ExitCode
setupRemote url = addRemote "origin" url

-- | Pull from a bundle file into the local repo: fetch the bundle's refs so all
-- objects and refs/remotes/origin/main exist in .rgit/index/.git. This is the "real"
-- pull from the fetched bundle; without it, the ref would point to a hash not in the repo.
fetchFromBundle :: BundleName -> IO ExitCode
fetchFromBundle name = do
    let (GitRelPath bundle) = bundleGitRelPath name
    (code, out, err) <- readProcessWithExitCode "git"
        (baseFlags ++ ["fetch", bundle, "+refs/heads/main:refs/remotes/origin/main"]) ""
    putStr out
    hPutStr stderr err
    return code

-- | Update the remote tracking branch refs/remotes/origin/main to point to the hash from the bundle.
-- Use when the objects are already in the repo (e.g. after push); for fetch/pull use fetchFromBundle.
updateRemoteTrackingBranch :: BundleName -> IO ExitCode
updateRemoteTrackingBranch name = do
    maybeHash <- getHashFromBundle name
    case maybeHash of
        Just hash -> do
            -- Update the remote tracking branch ref
            -- Use update-ref to create or update refs/remotes/origin/main
            updateRemoteTrackingBranchToHash hash
        Nothing -> return (ExitFailure 1)

-- | Set refs/remotes/origin/main to a specific hash. Use after a successful pull so status shows
-- "up to date with 'origin/main'" instead of "ahead by N commits".
updateRemoteTrackingBranchToHash :: String -> IO ExitCode
updateRemoteTrackingBranchToHash hash =
    readProcessWithExitCode "git" (baseFlags ++ ["update-ref", "refs/remotes/origin/main", hash]) "" >>= \(c, _, _) -> return c

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
        _ -> return (ExitFailure 1)

-- | Set up the local branch to track origin/main
-- This configures branch.main.remote and branch.main.merge so git status knows what to compare
setupBranchTracking :: IO ExitCode
setupBranchTracking = do
    -- Set branch.main.remote = origin and branch.main.merge = refs/heads/main
    (code1, _, _) <- readProcessWithExitCode "git" (baseFlags ++ ["config", "branch.main.remote", "origin"]) ""
    (code2, _, _) <- readProcessWithExitCode "git" (baseFlags ++ ["config", "branch.main.merge", "refs/heads/main"]) ""
    case (code1, code2) of
        (ExitSuccess, ExitSuccess) -> return ExitSuccess
        _ -> return (ExitFailure 1)

-- | Unset the upstream for the current branch (clears "upstream is gone" when remote refs are missing)
unsetBranchUpstream :: IO ExitCode
unsetBranchUpstream = do
    (code, _, _) <- readProcessWithExitCode "git" (baseFlags ++ ["branch", "--unset-upstream"]) ""
    return code

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
  return code

-- | True if a merge is in progress (MERGE_HEAD exists).
isMergeInProgress :: IO Bool
isMergeInProgress = do
  (code, _, _) <- runGitWithOutput ["rev-parse", "--verify", "MERGE_HEAD"]
  return (code == ExitSuccess)

-- | Checkout refs/remotes/origin/main as the local main branch.
-- Used on first pull when there are no local commits (unborn branch).
-- This avoids the need for merge and gives us the remote's history directly.
-- Uses -f (force) to overwrite any local files created during init.
checkoutRemoteAsMain :: IO ExitCode
checkoutRemoteAsMain = do
  -- Use checkout -B to create/reset branch and checkout in one step
  -- Use -f to force overwrite of any local files (like .gitattributes from init)
  (code, _, _) <- runGitWithOutput ["checkout", "-f", "-B", "main", "refs/remotes/origin/main"]
  return code

-- | Paths relative to work tree (index/...) that are unmerged.
getConflictedFiles :: IO [FilePath]
getConflictedFiles = do
  (code, out, _) <- runGitWithOutput ["diff", "--name-only", "--diff-filter=U"]
  if code /= ExitSuccess then return [] else return (filter (not . null) (lines out))

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
  let stageNum line = case reverse (words (beforeTab line)) of
        (s:_) | s `elem` ["1","2","3"] -> Just (read s :: Int)
        _ -> Nothing
  let stageNums = mapMaybe stageNum (lines out)
  let has1 = 1 `elem` stageNums
  let has2 = 2 `elem` stageNums
  let has3 = 3 `elem` stageNums
  if has2 && has3 && has1 then return (ContentConflict path)
  else if has2 && has3 && not has1 then return (AddAdd path)
  else if has2 && not has3 then return (ModifyDelete path False)  -- deleted in theirs
  else if has3 && not has2 then return (ModifyDelete path True)   -- deleted in ours (HEAD)
  else return (ContentConflict path)

-- | Check out our version for path (work-tree path under .rgit/index).
checkoutOurs :: FilePath -> IO ExitCode
checkoutOurs path = do
  (code, out, err) <- runGitWithOutput ["checkout", "--ours", "--", path]
  putStr (rewriteGitHints out)
  hPutStr stderr (rewriteGitHints err)
  return code

-- | Check out their version for path.
checkoutTheirs :: FilePath -> IO ExitCode
checkoutTheirs path = do
  (code, out, err) <- runGitWithOutput ["checkout", "--theirs", "--", path]
  putStr (rewriteGitHints out)
  hPutStr stderr (rewriteGitHints err)
  return code

-- | Read file content from a Git ref (e.g., "refs/remotes/origin/main:path/to/file").
-- Returns Nothing if file doesn't exist in that ref.
readFileFromRef :: String -> FilePath -> IO (Maybe String)
readFileFromRef ref path = do
  (code, out, err) <- runGitWithOutput ["show", ref ++ ":" ++ path]
  if code == ExitSuccess && not (null out)
    then return (Just out)
    else return Nothing

-- | List all files in a Git ref's tree (recursive). Returns paths relative to work tree root.
listFilesInRef :: String -> IO [FilePath]
listFilesInRef ref = do
  (code, out, _) <- runGitWithOutput ["ls-tree", "-r", "--name-only", ref]
  if code == ExitSuccess
    then return (filter (not . null) (lines out))
    else return []

-- | Run git fsck to check metadata history integrity.
-- Returns (exitCode, output, errorOutput).
fsck :: IO (ExitCode, String, String)
fsck = runGitWithOutput ["fsck"]

-- | Check if there are staged changes ready to commit.
-- Returns True if there are staged changes, False otherwise.
hasStagedChanges :: IO Bool
hasStagedChanges = do
  (code, _, _) <- runGitWithOutput ["diff", "--cached", "--quiet"]
  return (code == ExitFailure 1)  -- git diff --cached --quiet exits with 1 if there are changes

-- | Get the list of file changes between two commits.
-- Returns list of (status, path, maybe-new-path-for-renames).
-- Status: 'A' = added, 'D' = deleted, 'M' = modified, 'R' = renamed.
getDiffNameStatus :: String -> String -> IO [(Char, FilePath, Maybe FilePath)]
getDiffNameStatus oldHead newHead = do
    (code, out, _) <- runGitWithOutput ["diff", "--name-status", oldHead, newHead]
    if code /= ExitSuccess then return []
    else return (parseNameStatus out)

parseNameStatus :: String -> [(Char, FilePath, Maybe FilePath)]
parseNameStatus = mapMaybe parseLine . lines
  where
    parseLine line = case line of
        (status:rest)
            | status == 'R' || status == 'C' ->
                -- R100\told\tnew or Rnnn old new (tab-separated)
                case words (dropWhile (\c -> c /= '\t' && c /= ' ') rest) of
                    (old:new:_) -> Just (status, old, Just new)
                    _ -> Nothing
            | status `elem` "ADM" ->
                case words rest of
                    (path:_) -> Just (status, path, Nothing)
                    _ -> Nothing
            | otherwise -> Nothing
        _ -> Nothing

-- | Get all file paths at a given commit. Used when there's no old HEAD to diff against.
getFilesAtCommit :: String -> IO [FilePath]
getFilesAtCommit ref = do
    (code, out, _) <- runGitWithOutput ["ls-tree", "-r", "--name-only", ref]
    if code /= ExitSuccess then return []
    else return (filter (not . null) (lines out))

-- | Run a git command targeting a specific index path (for filesystem remotes).
-- This is used when operating on a remote filesystem repo directly.
-- The indexPath should be the path to the .bit/index directory (NOT the .git subdirectory).
runGitAt :: FilePath -> [String] -> IO (ExitCode, String, String)
runGitAt indexPath args = readProcessWithExitCode "git" (["-C", indexPath] ++ args) ""
