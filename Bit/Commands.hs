{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bit.Commands (run) where

import qualified Bit.Core as Bit
import Bit.Types (BitEnv(..), ForceMode(..), runBitM)
import qualified Bit.Scan.Local as Scan  -- Only for the pre-scan in runCommand
import qualified Bit.Core.Submodule as Submodule
import Bit.Remote (getDefaultRemote, getUpstreamRemote, resolveRemote)
import qualified Bit.Device.Identity as Device
import Bit.Utils (atomicWriteFileStr)
import Bit.IO.Concurrency (Concurrency(..))
import qualified Bit.Device.RemoteWorkspace as RemoteWorkspace
import System.Environment (getArgs, lookupEnv, setEnv)
import Bit.Help (printMainHelp, printTerseHelp, printCommandHelp)
import System.Exit (ExitCode(..), exitWith, exitSuccess)
import System.FilePath ((</>), makeRelative, takeDirectory)
import System.IO (hPutStrLn, stderr)
import Control.Monad (when, unless, void)
import qualified System.Directory as Dir
import qualified Bit.Git.Run as Git
import Data.List (dropWhileEnd, isPrefixOf, stripPrefix)
import qualified System.Info
-- Strict IO imports to avoid Windows file locking issues
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')

run :: IO ()
run = do
    args <- getArgs
    -- When GIT_DIR is set, the user is explicitly controlling git's repo
    -- location. Pass through to git directly without .bit interception.
    -- This handles GIT_DIR=/dev/null (test_cmp), GIT_DIR=<path> (git test
    -- suite tests 10/13), and any other explicit GIT_DIR usage.
    mGitDir <- lookupEnv "GIT_DIR"
    case mGitDir of
        Just d | not (null d) -> Git.runGitGlobal args >>= exitWith
        _ -> pure ()
    -- When --git-dir or --work-tree appears as a leading global flag,
    -- pass through to git directly (same logic as GIT_DIR env var).
    when (hasExplicitGitDir args) $
        Git.runGitGlobal args >>= exitWith
    case args of
        []               -> Git.runGitGlobal [] >>= exitWith
        ["help"]         -> printMainHelp >> exitSuccess
        ["help", cmd]    -> printCommandHelp cmd >> exitSuccess
        ["help", c1, c2] -> printCommandHelp (c1 ++ " " ++ c2) >> exitSuccess
        ["-h"]           -> printMainHelp >> exitSuccess
        ["--help"]       -> printMainHelp >> exitSuccess
        _ | isGitGlobalFlag args -> Git.runGitGlobal args >>= exitWith
        ("-C":dir:rest) -> handleDashC dir rest
        _  -> case extractRemoteTarget args of
            RemoteError msg -> do
                hPutStrLn stderr $ "fatal: " ++ msg
                exitWith (ExitFailure 1)
            NoRemote remaining -> runCommand remaining
            RemoteFound remoteName remaining ->
                runRemoteCommand remoteName remaining

-- | Parse 'bit init [flags] [dir]' into InitOptions + target directory.
-- Flags that take a separate arg: --template, --separate-git-dir, --object-format,
-- --ref-format, -b, --initial-branch. All others are standalone.
parseInitArgs :: [String] -> (Bit.InitOptions, Maybe FilePath)
parseInitArgs = go Bit.defaultInitOptions Nothing
  where
    consumesNext f = f `elem`
        ["--template", "--object-format", "--ref-format"
        , "-b", "--initial-branch"]
    go opts dir [] = (opts, dir)
    go opts dir ("-q":rest) = go opts { Bit.initQuiet = True } dir rest
    go opts dir ("--quiet":rest) = go opts { Bit.initQuiet = True } dir rest
    go opts dir ("--bare":rest) = go opts { Bit.initBare = True, Bit.initGitFlags = Bit.initGitFlags opts ++ ["--bare"] } dir rest
    go opts dir ("--separate-git-dir":v:rest) = go opts { Bit.initSeparateGitDir = Just v } dir rest
    go opts dir (f:rest)
        | Just v <- stripPrefix "--separate-git-dir=" f = go opts { Bit.initSeparateGitDir = Just v } dir rest
    go opts dir (f:v:rest)
        | consumesNext f = go opts { Bit.initGitFlags = Bit.initGitFlags opts ++ [f, v] } dir rest
    go opts dir (f:rest)
        | "-" `isPrefixOf` f = go opts { Bit.initGitFlags = Bit.initGitFlags opts ++ [f] } dir rest
        | otherwise = go opts (Just f) rest

-- | Peel git "global" flags that appear before the subcommand.
-- Currently handles @-c key=val@ and @--bare@.
peelGitGlobalFlags :: [String] -> ([String], [String])
peelGitGlobalFlags ("-c":val:rest) =
    let (more, remaining) = peelGitGlobalFlags rest
    in ("-c" : val : more, remaining)
peelGitGlobalFlags ("--bare":rest) =
    let (more, remaining) = peelGitGlobalFlags rest
    in ("--bare" : more, remaining)
peelGitGlobalFlags args = ([], args)

-- | Split peeled flags into @-c key=val@ pairs vs other flags (like @--bare@).
partitionDashC :: [String] -> ([String], [String])
partitionDashC ("-c":v:rest) = let (cs, os) = partitionDashC rest in ("-c":v:cs, os)
partitionDashC (x:rest) = let (cs, os) = partitionDashC rest in (cs, x:os)
partitionDashC [] = ([], [])

-- | Run 'bit init' with parsed options.
-- Bare repos are passed through to git directly (bit doesn't manage bare repos).
-- dashCFlags are @-c key=val@ pairs peeled from before "init".
runInit :: [String] -> [String] -> IO ()
runInit dashCFlags args = do
    let (opts, mDir) = parseInitArgs args
    let optsWithGlobal = opts { Bit.initGitGlobalFlags = dashCFlags }
    if Bit.initBare optsWithGlobal
        then do
            code <- Git.runGitGlobal (dashCFlags ++ "init" : args)
            when (code == ExitSuccess) $ do
                let bareDir = maybe "." id mDir
                Dir.createDirectoryIfMissing True (bareDir </> "bit" </> "cas")
            exitWith code
        else do
            targetDir <- case mDir of
                Nothing -> Dir.getCurrentDirectory
                Just d  -> Dir.makeAbsolute d
            unless (Bit.initQuiet optsWithGlobal) $
                putStrLn $ "Initializing bit in: " ++ targetDir
            code <- Bit.initializeRepoAt targetDir optsWithGlobal
            when (code == ExitSuccess) $
                unless (Bit.initQuiet optsWithGlobal) $
                    putStrLn "bit initialized successfully!"
            exitWith code

-- | Git flags that don't need a repo (or .bit directory).
isGitGlobalFlag :: [String] -> Bool
isGitGlobalFlag (flag:_) = flag `elem`
    ["--exec-path", "--version", "--html-path", "--man-path", "--info-path"]
isGitGlobalFlag _ = False

-- | Check if --git-dir or --work-tree appears as a leading global flag.
-- Scans only flags before the first subcommand (non-flag argument), skipping
-- -c key=val and -C dir pairs. This avoids false positives on e.g.
-- "git rev-parse --git-dir" where --git-dir appears after the subcommand.
hasExplicitGitDir :: [String] -> Bool
hasExplicitGitDir = go
  where
    go [] = False
    go (x:rest)
        | "--git-dir=" `isPrefixOf` x   = True
        | "--work-tree=" `isPrefixOf` x  = True
        | x == "--git-dir"  = not (null rest)
        | x == "--work-tree" = not (null rest)
        | x == "-c" = go (drop 1 rest)
        | x == "-C" = go (drop 1 rest)
        | x == "--bare" = go rest
        | "-" `isPrefixOf` x = go rest
        | otherwise = False  -- reached subcommand, stop scanning

-- | Handle @git -C \<dir\> ...@ passthrough.
-- If the target directory contains a .bit/ repo, redirect into its index;
-- otherwise pass through to git unchanged.
handleDashC :: FilePath -> [String] -> IO ()
handleDashC dir rest = do
    hasBitDir <- Dir.doesDirectoryExist (dir </> ".bit")
    hasBitLink <- Dir.doesFileExist (dir </> ".bit")
    if hasBitDir || hasBitLink
        then do
            -- cd to the target directory and run through normal bit dispatch.
            -- This handles init (with correct relative path resolution),
            -- working tree commands (add/commit via scan), and passthrough
            -- (which uses the .git gitlink for native repo discovery).
            absDir <- Dir.makeAbsolute dir
            Dir.setCurrentDirectory absDir
            runCommand rest
        else Git.runGitRawAt dir rest >>= exitWith

-- | Commands that bit handles natively (not aliases).
isKnownCommand :: String -> Bool
isKnownCommand name = name `elem`
    [ "init", "add", "commit", "diff", "status", "log", "ls-files"
    , "rm", "mv", "reset", "restore", "checkout", "branch", "merge"
    , "push", "pull", "fetch", "remote", "verify", "repair", "fsck"
    , "cas", "submodule"
    , "help"
    ]

-- | Try to expand a git alias and re-dispatch, or pass through to git.
-- When a .bit/index path is available, queries local+global config;
-- otherwise queries global config only.
-- If no alias is found, passes the command through to git directly.
-- Tracks alias depth via BIT_ALIAS_DEPTH env var to detect loops (max 10).
tryAlias :: Maybe FilePath -> FilePath -> FilePath -> String -> [String] -> IO ()
tryAlias mIndexPath origCwd root name rest = do
    -- Check alias depth to detect loops
    depthStr <- lookupEnv "BIT_ALIAS_DEPTH"
    let depth = maybe 0 (\s -> case reads s of { [(n,"")] -> n; _ -> 0 }) depthStr :: Int
    when (depth >= 10) $ do
        hPutStrLn stderr $ "fatal: alias loop detected: expansion of '" ++ name ++ "' does not terminate"
        exitWith (ExitFailure 128)
    let gitArgs = case mIndexPath of
            Just p  -> ["-C", p, "config", "--get", "alias." ++ name]
            Nothing -> ["config", "--get", "alias." ++ name]
    (code, out, _) <- Git.spawnGit gitArgs
    case code of
        ExitSuccess ->
            let expanded = words (filter (/= '\r') (filter (/= '\n') out))
            in case expanded of
                -- Shell aliases must run from CWD (not .bit/index) because git
                -- resolves paths relative to the working tree root. Using -C .bit/index
                -- would make the working tree root be .bit/index, breaking relative paths.
                (x:_) | "!" `isPrefixOf` x -> Git.runGitGlobal (name : rest) >>= exitWith
                [] -> passthrough (name : rest)
                _ -> do
                    setEnv "BIT_ALIAS_DEPTH" (show (depth + 1))
                    runCommand (expanded ++ rest) >> exitSuccess
        _ -> passthrough (name : rest)
  where
    passthrough args = case mIndexPath of
        Just p  -> do
            -- cd to repo root so .git (junction or gitlink) is found at CWD.
            -- CWD may still be origCwd (user's subdirectory) at this point.
            Dir.setCurrentDirectory root
            hasGitDir <- Dir.doesDirectoryExist ".git"
            hasGitFile <- Dir.doesFileExist ".git"
            -- NOTE: No IO calls between the doesDirectoryExist/doesFileExist
            -- checks above and the if/else below. On Windows, calling
            -- getCurrentDirectory (or any file IO) before the hasGitDir check
            -- causes subtle side effects on junction resolution that break
            -- subsequent git operations. The CWD restore must happen INSIDE
            -- each branch.
            if hasGitDir
                then do
                    -- Restore user's CWD so relative paths in args resolve correctly.
                    -- When CWD changes, also pass --work-tree=<root> because git
                    -- follows the .git junction to .bit/index/.git and thinks the
                    -- work tree is .bit/index/, making relative paths like ../f fail.
                    when (root /= origCwd) $ do
                        Dir.setCurrentDirectory origCwd
                    Git.runGitGlobal
                        (if root /= origCwd
                            then "--work-tree" : root : args
                            else args)
                        >>= exitWith
                else if hasGitFile
                    then do
                        -- Read gitlink target BEFORE restoring CWD (gitlink is at repo root)
                        mTarget <- readGitlink ".git"
                        -- Now restore CWD for relative path resolution
                        when (root /= origCwd) $
                            Dir.setCurrentDirectory origCwd
                        case mTarget of
                            Just target ->
                                Git.runGitGlobal ("--git-dir" : target : args)
                                    >>= exitWith
                            Nothing ->
                                Git.runGitGlobal args >>= exitWith
                    else Git.runGitRawAt p args >>= exitWith
        Nothing -> Git.runGitGlobal args >>= exitWith   -- no bit repo: bare git

-- | Result of extracting a remote target from CLI args.
data RemoteExtract
    = NoRemote [String]            -- ^ No remote specified; remaining args
    | RemoteFound String [String]  -- ^ Remote name + remaining args
    | RemoteError String           -- ^ Error message

-- | Extract @<remote> or --remote <name> from the leading args.
-- Both forms must appear at the start of the arg list, consistent with
-- each other and avoiding conflicts with subcommand flags (e.g. verify --remote).
extractRemoteTarget :: [String] -> RemoteExtract
extractRemoteTarget [] = NoRemote []
extractRemoteTarget (('@':name@(_:_)):rest)
    | "--remote" `elem` rest = RemoteError
        "Cannot use both @<remote> and --remote <name>."
    | otherwise = RemoteFound name rest
extractRemoteTarget ("--remote":name:rest) = RemoteFound name rest
extractRemoteTarget ["--remote"] = RemoteError
    "'--remote' requires a remote name argument."
extractRemoteTarget args = NoRemote args

-- | Execute a command in the context of an ephemeral remote workspace.
-- Each command fetches the bundle from remote, inflates into a temp workspace,
-- operates, re-bundles if needed, and pushes back. No persistent workspace.
runRemoteCommand :: String -> [String] -> IO ()
runRemoteCommand remoteName args = do
    origCwd <- Dir.getCurrentDirectory
    mRoot <- findBitRoot origCwd
    case mRoot of
        Just (NormalRoot _) -> pure ()
        _ -> do
            hPutStrLn stderr "fatal: not a bit repository (or any of the parent directories): .bit"
            exitWith (ExitFailure 1)
    let cwd = case mRoot of
            Just (NormalRoot r) -> r
            _                   -> origCwd
    Dir.setCurrentDirectory cwd
    -- Resolve bitDir and set index path for git commands
    remoteBitDir' <- resolveBitDir cwd
    Git.setIndexPath (remoteBitDir' </> "index")

    mRemote <- resolveRemote cwd remoteName
    case mRemote of
        Nothing -> do
            hPutStrLn stderr $ "fatal: remote '" ++ remoteName ++ "' not found."
            exitWith (ExitFailure 1)
        Just remote -> do
            let isSequential = "--sequential" `elem` args
                concurrency = if isSequential then Sequential else Parallel 0
                cmd = filter (/= "--sequential") args
                runVerifyCmd repairMode = do
                    remoteBitDir <- resolveBitDir cwd
                    let env = BitEnv cwd remoteBitDir "" (Just remote) NoForce
                    runBitM env $ Bit.verify (Bit.VerifyRemotePath remote) repairMode concurrency
                rejectFilesystem = do
                    mType <- Device.readRemoteType cwd remoteName
                    let isFs = maybe False Device.isFilesystemType mType
                    when isFs $ do
                        hPutStrLn stderr "Remote workspaces are only supported for cloud remotes."
                        hPutStrLn stderr "For filesystem remotes, navigate to the directory and run bit commands there."
                        exitWith (ExitFailure 1)
            case cmd of
                ["init"] ->
                    rejectFilesystem >> RemoteWorkspace.initRemote remote remoteName
                ("add":paths) ->
                    rejectFilesystem >> RemoteWorkspace.addRemote remote paths >>= exitWith
                ("commit":commitArgs) ->
                    rejectFilesystem >> RemoteWorkspace.commitRemote remote commitArgs >>= exitWith
                ("status":rest) ->
                    rejectFilesystem >> RemoteWorkspace.statusRemote remote rest >>= exitWith
                ("log":rest) ->
                    rejectFilesystem >> RemoteWorkspace.logRemote remote rest >>= exitWith
                ("ls-files":rest) ->
                    rejectFilesystem >> RemoteWorkspace.lsFilesRemote remote rest >>= exitWith
                ["verify"] ->
                    runVerifyCmd Bit.PromptRepair
                ["repair"] ->
                    runVerifyCmd Bit.AutoRepair
                _ -> do
                    hPutStrLn stderr $ "error: command not supported in remote context: " ++ unwords cmd
                    hPutStrLn stderr "Supported: init, add, commit, status, log, ls-files, verify, repair"
                    exitWith (ExitFailure 1)

-- | Helper function to push with upstream tracking
pushWithUpstream :: BitEnv -> FilePath -> String -> IO ()
pushWithUpstream env cwd name = do
    mNamedRemote <- resolveRemote cwd name
    let envWithRemote = env { envRemote = mNamedRemote }
    runBitM envWithRemote Bit.push
    -- After successful push, set upstream tracking
    void $ Git.setupBranchTrackingFor name
    putStrLn $ "branch 'main' set up to track '" ++ name ++ "/main'."

-- | Sync .bitignore to .bit/index/.gitignore with normalization
syncBitignoreToIndex :: FilePath -> FilePath -> IO ()
syncBitignoreToIndex cwd bitDir = do
    let bitignoreSrc = cwd </> ".bitignore"
        bitignoreDest = bitDir </> "index" </> ".gitignore"
    bitignoreExists <- Dir.doesFileExist bitignoreSrc
    if bitignoreExists
        then writeBitignore bitignoreSrc bitignoreDest
        else removeStaleGitignore bitignoreDest
  where
    writeBitignore :: FilePath -> FilePath -> IO ()
    writeBitignore src dest = do
        bs <- BS.readFile src
        let content = either (const "") T.unpack (decodeUtf8' bs)
            normalizedLines = filter (not . null) $
              map (trim . filter (/= '\r')) (lines content)
        atomicWriteFileStr dest (unlines normalizedLines)
    
    removeStaleGitignore :: FilePath -> IO ()
    removeStaleGitignore dest = do
        destExists <- Dir.doesFileExist dest
        when destExists $ Dir.removeFile dest
    
    trim :: String -> String
    trim = dropWhile (== ' ') . dropWhileEnd (== ' ')

-- | Sync user's .gitattributes to .bit/index/.gitattributes.
-- If the user has no .gitattributes, remove the stale copy from .bit/index/.
-- This runs AFTER writeMetadataFiles (which copies text files from the
-- working tree) to handle the cleanup case where the user deletes their
-- .gitattributes — the scan wouldn't include it and listMetadataPaths
-- skips .gitattributes, so without this the stale copy would persist.
syncGitattributesToIndex :: FilePath -> FilePath -> IO ()
syncGitattributesToIndex cwd bitDir = do
    let userSrc = cwd </> ".gitattributes"
        dest    = bitDir </> "index" </> ".gitattributes"
    userExists <- Dir.doesFileExist userSrc
    if userExists
        then do
            bs <- BS.readFile userSrc
            let content = either (const "") T.unpack (decodeUtf8' bs)
            atomicWriteFileStr dest content
        else do
            destExists <- Dir.doesFileExist dest
            when destExists $ Dir.removeFile dest

-- | Resolve the .bit directory from a repo root.
-- For normal repos, .bit is a directory. For separated repos, .bit is a file
-- (bitlink) containing "bitdir: <path>" pointing to the real bit directory.
resolveBitDir :: FilePath -> IO FilePath
resolveBitDir root = do
    let dotBit = root </> ".bit"
    isDir <- Dir.doesDirectoryExist dotBit
    if isDir
        then pure dotBit
        else do
            isFile <- Dir.doesFileExist dotBit
            if isFile
                then do
                    bs <- BS.readFile dotBit
                    let content = either (const "") T.unpack (decodeUtf8' bs)
                    case lines content of
                        (firstLine:_) -> pure (drop 8 (filter (/= '\r') firstLine))
                        [] -> error "not a bit repository: empty .bit file"
                else error "not a bit repository"

-- | Read a gitlink file and extract the gitdir target path.
-- Returns Nothing if the file is not a valid gitlink (missing "gitdir: " prefix).
readGitlink :: FilePath -> IO (Maybe String)
readGitlink path = do
    raw <- BS.readFile path
    case decodeUtf8' raw of
        Left _ -> pure Nothing
        Right t ->
            let firstLine = takeWhile (\c -> c /= '\n' && c /= '\r')
                              (T.unpack t)
            in pure $ stripPrefix "gitdir: " firstLine

-- | Extract the command key from args for help lookup.
-- Handles multi-word commands like "remote add", "merge --continue", etc.
commandKey :: [String] -> String
commandKey ("remote":sub:_)
    | sub `elem` ["add", "show"] = "remote " ++ sub
commandKey ("merge":sub:_)
    | sub `elem` ["--continue", "--abort"] = "merge " ++ sub
commandKey ("branch":sub:_)
    | sub `elem` ["--unset-upstream"] = "branch " ++ sub
commandKey ("cas":sub:_)
    | sub == "backfill" = "cas backfill"
commandKey (cmd:_) = cmd
commandKey [] = ""

-- | Result of walking up to find a bit repository.
data BitRoot
    = NormalRoot FilePath   -- ^ Standard repo with .bit/ directory
    | BareRoot FilePath     -- ^ Git bare repo with bit/ subdirectory

-- | Walk up from the given directory to find the closest bit repository.
-- Checks for both .bit/ (normal repo) and bare repo markers (HEAD + bit/) at
-- each level, returning whichever is closest. This ensures a bare repo nested
-- inside a normal repo's tree is found before the parent normal repo.
-- Respects BIT_CEILING_DIRECTORIES to stop the walk.
findBitRoot :: FilePath -> IO (Maybe BitRoot)
findBitRoot start = do
    ceilings <- getCeilingDirs
    canonStart <- Dir.canonicalizePath start
    go ceilings canonStart
  where
    go ceilings dir = do
        hasDotBit <- Dir.doesDirectoryExist (dir </> ".bit")
        hasBitLink <- Dir.doesFileExist (dir </> ".bit")
        if hasDotBit || hasBitLink
            then pure (Just (NormalRoot dir))
            else do
                hasBitDir <- Dir.doesDirectoryExist (dir </> "bit")
                hasHead <- Dir.doesFileExist (dir </> "HEAD")
                if hasBitDir && hasHead
                    then pure (Just (BareRoot dir))
                    else let parent = takeDirectory dir
                         in if parent == dir || dir `elem` ceilings
                            then pure Nothing
                            else go ceilings parent
    getCeilingDirs = do
        mBit <- lookupEnv "BIT_CEILING_DIRECTORIES"
        mGit <- lookupEnv "GIT_CEILING_DIRECTORIES"
        let sep = if System.Info.os `elem` ["mingw32", "win32"] then ';' else ':'
            parse Nothing = []
            parse (Just val) = filter (not . null) (splitOn sep val)
            raw = parse mBit ++ parse mGit
        mapM Dir.canonicalizePath raw
    splitOn _ [] = []
    splitOn c s  = let (w, rest) = break (== c) s
                   in w : case rest of { [] -> []; (_:t) -> splitOn c t }

runCommand :: [String] -> IO ()
runCommand args = do
    let hasHelp = "--help" `elem` args
    let hasTerseHelp = "-h" `elem` args
    let hasForce = "--force" `elem` args || "-f" `elem` args
    let hasForceWithLease = "--force-with-lease" `elem` args
    let isSequential = "--sequential" `elem` args
    when (hasForce && hasForceWithLease) $ do
        hPutStrLn stderr "fatal: Cannot use both --force and --force-with-lease"
        exitWith (ExitFailure 1)
    let forceMode
          | hasForce          = Force
          | hasForceWithLease = ForceWithLease
          | otherwise         = NoForce
    let concurrency = if isSequential then Sequential else Parallel 0
    -- Note: -f is NOT stripped here because it means different things per command
    -- (e.g. -f = --file for config, -f = --force for checkout). The hasForce
    -- check above detects it for push/pull force mode; passthrough commands
    -- keep -f for git to interpret per-command.
    let cmd = filter (`notElem` ["--force", "--force-with-lease", "--sequential", "-h", "--help"]) args

    -- Help intercept (before repo check — help works without a repo)
    when (hasHelp || hasTerseHelp) $ do
        let key = commandKey cmd
        if null key
            then printMainHelp >> exitSuccess
            else if hasTerseHelp
                then printTerseHelp key >> exitSuccess
                else printCommandHelp key >> exitSuccess

    -- init runs before repo discovery — it creates repos, not uses them.
    -- Peel global flags (-c key=val, --bare) that appear before "init".
    let (peeledFlags, coreCmd) = peelGitGlobalFlags cmd
    let (dashCFlags, otherPeeled) = partitionDashC peeledFlags
    case coreCmd of
        ("init":rest) -> runInit dashCFlags (otherPeeled ++ rest)
        _ -> pure ()

    origCwd <- Dir.getCurrentDirectory

    -- If CWD is itself a git directory (bare repo, .git dir) but NOT a bit
    -- repo, pass through to git. This prevents bit from walking up to a
    -- parent .bit/ repo when the user (or test) cd'd into a git directory.
    hasHead <- Dir.doesFileExist (origCwd </> "HEAD")
    hasRefs <- Dir.doesDirectoryExist (origCwd </> "refs")
    hasDotBit <- Dir.doesDirectoryExist (origCwd </> ".bit")
    hasDotBitLink <- Dir.doesFileExist (origCwd </> ".bit")
    when (hasHead && hasRefs && not hasDotBit && not hasDotBitLink) $
        Git.runGitHere cmd >>= exitWith

    mRoot <- findBitRoot origCwd

    -- Handle bare repo case: pass through to git for unknown commands,
    -- reject known bit commands that require a work tree
    case mRoot of
        Just (BareRoot _) -> case cmd of
            (name:rest) | not (isKnownCommand name) -> tryAlias Nothing origCwd origCwd name rest
            _ -> do
                hPutStrLn stderr "fatal: this operation must be run in a work tree"
                exitWith (ExitFailure 128)
        _ -> pure ()

    let bitExists = case mRoot of { Just (NormalRoot _) -> True; _ -> False }
        root = case mRoot of
            Just (NormalRoot r) -> r
            _                   -> origCwd
        prefix = case mRoot of
            Just (NormalRoot r) -> let rel = makeRelative r origCwd
                                   in if rel == "." then "" else rel
            _                   -> ""

    -- Preliminary .bit/index for setIndexPath and tryAlias (relative; git -C uses it)
    bitDirPrelim <- if bitExists then resolveBitDir root else pure (root </> ".bit")
    let indexPath = bitDirPrelim </> "index"
    when bitExists $ Git.setIndexPath indexPath

    -- Bit-specific config: intercept known bit config keys before alias/passthrough.
    -- All other config forms (--global, --unset-all, -f, alias.*, etc.) fall through
    -- to the unknown-command path and reach git via passthrough.
    case cmd of
        ["config", "--list"] | bitExists -> do
            Dir.setCurrentDirectory root
            bd <- resolveBitDir =<< Dir.getCurrentDirectory
            Bit.configListWithRoot bd >> exitWith ExitSuccess
        ["config", key] | bitExists && key `elem` Bit.knownConfigKeys -> do
            Dir.setCurrentDirectory root
            bd <- resolveBitDir =<< Dir.getCurrentDirectory
            Bit.configGetWithRoot bd key >> exitWith ExitSuccess
        ["config", key, value] | bitExists && key `elem` Bit.knownConfigKeys -> do
            Dir.setCurrentDirectory root
            bd <- resolveBitDir =<< Dir.getCurrentDirectory
            Bit.configSetWithRoot bd key value >> exitWith ExitSuccess
        _ -> pure ()

    -- For unknown commands, try alias expansion before repo check
    let mIndexPath = if bitExists then Just indexPath else Nothing
    case cmd of
        (name:rest) | not (isKnownCommand name) ->
            tryAlias mIndexPath origCwd root name rest
        _ -> pure ()

    -- Repo existence check (init and unknown commands already handled above)
    when (not bitExists) $ do
        hPutStrLn stderr "fatal: not a bit repository (or any of the parent directories): .bit"
        exitWith (ExitFailure 1)

    -- Change to repo root so .bit/ paths resolve correctly and bitDir can be absolute
    when bitExists $
        Dir.setCurrentDirectory root

    -- Re-resolve .bit so it is absolute (needed for CAS/metadata paths when root was relative)
    bitDir <- if bitExists then (resolveBitDir =<< Dir.getCurrentDirectory) else pure (root </> ".bit")
    when bitExists $ Git.setIndexPath (bitDir </> "index")

    -- Ensure index/<prefix> subdirectory exists for git -C
    when (bitExists && not (null prefix)) $
        Dir.createDirectoryIfMissing True (indexPath </> prefix)

    let cwd = root

    -- Lightweight env (no scan) — for fetch, verify, and explicit-remote commands (falls back to "origin")
    let baseEnv = do
            mRemote <- getDefaultRemote cwd
            pure $ BitEnv cwd bitDir prefix mRemote forceMode

    -- Strict upstream env — requires branch.main.remote (for push and pull without explicit remote)
    let upstreamEnv = do
            mRemote <- getUpstreamRemote cwd
            pure $ BitEnv cwd bitDir prefix mRemote forceMode

    -- Scan + bitignore sync + metadata write — for write commands (add, commit, etc.)
    let scanAndWrite = do
            syncBitignoreToIndex cwd bitDir
            localFiles <- Scan.scanWorkingDir cwd concurrency
            Scan.writeMetadataFiles cwd localFiles
            syncGitattributesToIndex cwd bitDir

    -- Helper functions for running commands
    let runScanned action = do { scanAndWrite; env <- baseEnv; runBitM env action }
    let runBase action = baseEnv >>= \env -> runBitM env action
    -- push/pull without explicit remote: use upstreamEnv (no "origin" fallback)
    let runUpstream action = upstreamEnv >>= \env -> runBitM env action
    let runScannedUpstream action = do { scanAndWrite; env <- upstreamEnv; runBitM env action }
    let runScannedWithRemote name action = do
            scanAndWrite
            env <- baseEnv
            mNamedRemote <- resolveRemote cwd name
            runBitM env { envRemote = mNamedRemote } action
    let runBaseWithRemote name action = do
            env <- baseEnv
            mNamedRemote <- resolveRemote cwd name
            runBitM env { envRemote = mNamedRemote } action
    let runPushWithUpstream name = baseEnv >>= \env -> pushWithUpstream env cwd name
    let optsAcceptRemote = Bit.PullOptions Bit.PullAcceptRemote
    let optsManualMerge = Bit.PullOptions Bit.PullManualMerge
    let isPush ("push":_) = True
        isPush _ = False

    case cmd of
        -- ── No env needed ────────────────────────────────────
        -- (init is dispatched earlier, before repo discovery)
        ("remote":"add":rest)           -> case filter (/= "--bare") rest of
            [name, url] -> Bit.remoteAdd name url ("--bare" `elem` rest)
            _ -> do hPutStrLn stderr "usage: bit remote add <name> <url> [--bare]"; exitWith (ExitFailure 1)
        ["fsck"]                        -> Bit.fsck cwd
        ["merge", "--abort"]            -> Bit.mergeAbort
        ["branch", "--unset-upstream"]  -> Bit.unsetUpstream
        -- config: bit-specific keys handled above; everything else passes through to git
        ("config":rest)                 -> Git.runGitRawAt (bitDir </> "index") ("config" : rest) >>= exitWith
        ["cas", "backfill"]             -> Bit.casBackfill cwd >> exitWith ExitSuccess

        -- ── Lightweight env (no scan) ────────────────────────
        ("log":rest)                    -> Bit.log prefix rest >>= exitWith
        ("ls-files":rest)               -> Bit.lsFiles prefix rest >>= exitWith
        ["remote", "show"]              -> runBase $ Bit.remoteShow Nothing
        ["remote", "show", name]        -> runBaseWithRemote name $ Bit.remoteShow (Just name)
        ["verify"]                      -> runBase $ Bit.verify Bit.VerifyLocal Bit.PromptRepair concurrency
        ["repair"]                      -> runBase $ Bit.verify Bit.VerifyLocal Bit.AutoRepair concurrency

        ("rm":rest)                     -> runBase (Bit.rm rest) >>= exitWith
        ("branch":rest)                 -> Bit.branch prefix rest >>= exitWith

        -- ── Full scanned env (needs working directory state) ─
        ("add":rest)                    -> do
            scanAndWrite
            Submodule.detectAndHandleSubrepo cwd bitDir prefix rest
            code <- Bit.add prefix rest
            when (code == ExitSuccess) $
                Submodule.syncGitlinksBack cwd bitDir rest
            exitWith code
        ("commit":rest)                 -> do
            scanAndWrite
            Bit.commit prefix rest >>= exitWith
        ("diff":rest)                   -> do
            scanAndWrite
            Bit.diff prefix rest >>= exitWith
        ("mv":rest)                     -> do
            scanAndWrite
            Bit.mv prefix rest >>= exitWith
        ("reset":rest)                  -> do
            scanAndWrite
            Bit.reset prefix rest >>= exitWith
        ["merge", "--continue"]         -> runScanned Bit.mergeContinue
        ("merge":rest)                  -> do
            scanAndWrite
            Bit.merge prefix rest >>= exitWith
        ("status":rest)                 -> runScanned (Bit.status rest) >>= exitWith
        ("restore":rest)                -> runScanned (Bit.restore rest) >>= exitWith
        ("checkout":rest)               -> runScanned (Bit.checkout rest) >>= exitWith
        
        -- push (no scan needed — uses git metadata diff; requires upstream)
        -- Filter -f from push args (it means --force for push, detected via forceMode)
        _ | isPush cmd -> case filter (/= "-f") cmd of
            ["push"]                        -> runUpstream Bit.push
            ["push", "-u", name]            -> runPushWithUpstream name
            ["push", "--set-upstream", name] -> runPushWithUpstream name
            ["push", name]                  -> runBaseWithRemote name Bit.push
            _ -> Git.runGitRaw cmd >>= exitWith
        
        -- pull (no explicit remote: requires upstream, no "origin" fallback)
        ["pull"]                        -> runScannedUpstream $ Bit.pull Bit.defaultPullOptions
        ["pull", name]                  -> runScannedWithRemote name $ Bit.pull Bit.defaultPullOptions
        ["pull", "--accept-remote"]     -> runScannedUpstream $ Bit.pull optsAcceptRemote
        ["pull", "--manual-merge"]      -> runScannedUpstream $ Bit.pull optsManualMerge
        ["pull", name, "--accept-remote"] -> runScannedWithRemote name $ Bit.pull optsAcceptRemote
        ["pull", "--accept-remote", name] -> runScannedWithRemote name $ Bit.pull optsAcceptRemote
        ["pull", name, "--manual-merge"] -> runScannedWithRemote name $ Bit.pull optsManualMerge
        ["pull", "--manual-merge", name] -> runScannedWithRemote name $ Bit.pull optsManualMerge
        
        -- submodule (delegates to git -C .bit/index submodule, syncs working dir)
        ("submodule":rest)              -> Submodule.submodule cwd bitDir rest >>= exitWith

        -- fetch (falls back to "origin" like git fetch)
        ["fetch"]                       -> runScanned Bit.fetch
        ["fetch", name]                 -> runScannedWithRemote name Bit.fetch
        
        _                               -> do
            hPutStrLn stderr $ "bit: '" ++ unwords cmd ++ "' is not a bit command. See 'bit help'."
            exitWith (ExitFailure 1)
