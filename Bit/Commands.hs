{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bit.Commands (run) where

import qualified Bit.Core as Bit
import Bit.Types (BitEnv(..), ForceMode(..), runBitM)
import qualified Bit.Scan.Local as Scan  -- Only for the pre-scan in runCommand
import Bit.Remote (getDefaultRemote, getUpstreamRemote, resolveRemote)
import qualified Bit.Device.Identity as Device
import Bit.Utils (atomicWriteFileStr)
import Bit.IO.Concurrency (Concurrency(..))
import qualified Bit.Device.RemoteWorkspace as RemoteWorkspace
import System.Environment (getArgs, lookupEnv)
import Bit.Help (printMainHelp, printTerseHelp, printCommandHelp)
import System.Exit (ExitCode(..), exitWith, exitSuccess)
import System.FilePath ((</>), makeRelative, takeDirectory)
import System.IO (hPutStrLn, stderr)
import Control.Monad (when, unless, void)
import qualified System.Directory as Dir
import qualified Bit.Git.Run as Git
import Data.List (dropWhileEnd, isPrefixOf)
import qualified System.Info
-- Strict IO imports to avoid Windows file locking issues
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')

run :: IO ()
run = do
    args <- getArgs
    case args of
        []               -> Git.runGitGlobal [] >>= exitWith
        ["help"]         -> printMainHelp >> exitSuccess
        ["help", cmd]    -> printCommandHelp cmd >> exitSuccess
        ["help", c1, c2] -> printCommandHelp (c1 ++ " " ++ c2) >> exitSuccess
        ["-h"]           -> printMainHelp >> exitSuccess
        ["--help"]       -> printMainHelp >> exitSuccess
        _ | isGitGlobalFlag args -> Git.runGitGlobal args >>= exitWith
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
        ["--template", "--separate-git-dir", "--object-format", "--ref-format"
        , "-b", "--initial-branch"]
    go opts dir [] = (opts, dir)
    go opts dir ("-q":rest) = go opts { Bit.initQuiet = True } dir rest
    go opts dir ("--quiet":rest) = go opts { Bit.initQuiet = True } dir rest
    go opts dir ("--bare":rest) = go opts { Bit.initBare = True, Bit.initGitFlags = Bit.initGitFlags opts ++ ["--bare"] } dir rest
    go opts dir (f:v:rest)
        | consumesNext f = go opts { Bit.initGitFlags = Bit.initGitFlags opts ++ [f, v] } dir rest
    go opts dir (f:rest)
        | "-" `isPrefixOf` f = go opts { Bit.initGitFlags = Bit.initGitFlags opts ++ [f] } dir rest
        | otherwise = go opts (Just f) rest

-- | Run 'bit init' with parsed options.
-- Bare repos are passed through to git directly (bit doesn't manage bare repos).
runInit :: [String] -> IO ()
runInit args = do
    let (opts, mDir) = parseInitArgs args
    if Bit.initBare opts
        then do
            code <- Git.runGitGlobal ("init" : args)
            let bareDir = maybe "." id mDir
            Dir.createDirectoryIfMissing True (bareDir </> "bit" </> "cas")
            exitWith code
        else case mDir of
            Nothing -> do
                cwd <- Dir.getCurrentDirectory
                unless (Bit.initQuiet opts) $
                    putStrLn $ "Initializing bit in: " ++ cwd
                Bit.initializeRepoAt cwd opts
                unless (Bit.initQuiet opts) $
                    putStrLn "bit initialized successfully!"
            Just d -> do
                unless (Bit.initQuiet opts) $
                    putStrLn $ "Initializing bit in: " ++ d
                Bit.initializeRepoAt d opts
                unless (Bit.initQuiet opts) $
                    putStrLn "bit initialized successfully!"

-- | Git flags that don't need a repo (or .bit directory).
isGitGlobalFlag :: [String] -> Bool
isGitGlobalFlag (flag:_) = flag `elem`
    ["--exec-path", "--version", "--html-path", "--man-path", "--info-path"]
isGitGlobalFlag _ = False

-- | Commands that bit handles natively (not aliases).
isKnownCommand :: String -> Bool
isKnownCommand name = name `elem`
    [ "init", "add", "commit", "diff", "status", "log", "ls-files"
    , "rm", "mv", "reset", "restore", "checkout", "branch", "merge"
    , "push", "pull", "fetch", "remote", "verify", "repair", "fsck"
    , "help"
    ]

-- | Try to expand a git alias and re-dispatch, or pass through to git.
-- When a .bit/index path is available, queries local+global config;
-- otherwise queries global config only.
-- If no alias is found, passes the command through to git directly.
tryAlias :: Maybe FilePath -> String -> [String] -> IO ()
tryAlias mIndexPath name rest = do
    let gitArgs = case mIndexPath of
            Just p  -> ["-C", p, "config", "--get", "alias." ++ name]
            Nothing -> ["config", "--get", "alias." ++ name]
    (code, out, _) <- Git.spawnGit gitArgs
    case code of
        ExitSuccess ->
            let expanded = words (filter (/= '\r') (filter (/= '\n') out))
            in case expanded of
                ("!":_) -> Git.runGitGlobal (name : rest) >>= exitWith  -- shell alias
                [] -> passthrough (name : rest)
                _ -> runCommand (expanded ++ rest) >> exitSuccess
        _ -> passthrough (name : rest)
  where
    passthrough args = case mIndexPath of
        Just p  -> do
            -- If CWD is itself a git dir (e.g. user cd'd into .git),
            -- don't override with -C — let git's own discovery work.
            insideGitDir <- Dir.doesFileExist "HEAD"
            if insideGitDir
                then Git.runGitHere args >>= exitWith
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
                    let env = BitEnv cwd "" (Just remote) NoForce
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
syncBitignoreToIndex :: FilePath -> IO ()
syncBitignoreToIndex cwd = do
    let bitignoreSrc = cwd </> ".bitignore"
        bitignoreDest = cwd </> ".bit" </> "index" </> ".gitignore"
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

-- | Extract the command key from args for help lookup.
-- Handles multi-word commands like "remote add", "merge --continue", etc.
commandKey :: [String] -> String
commandKey ("remote":sub:_)
    | sub `elem` ["add", "show"] = "remote " ++ sub
commandKey ("merge":sub:_)
    | sub `elem` ["--continue", "--abort"] = "merge " ++ sub
commandKey ("branch":sub:_)
    | sub `elem` ["--unset-upstream"] = "branch " ++ sub
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
        if hasDotBit
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
        mVal <- lookupEnv "BIT_CEILING_DIRECTORIES"
        case mVal of
            Nothing -> pure []
            Just val -> do
                let sep = if System.Info.os `elem` ["mingw32", "win32"] then ';' else ':'
                let raw = splitOn sep val
                mapM Dir.canonicalizePath (filter (not . null) raw)
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
    let cmd = filter (`notElem` ["--force", "-f", "--force-with-lease", "--sequential", "-h", "--help"]) args

    -- Help intercept (before repo check — help works without a repo)
    when (hasHelp || hasTerseHelp) $ do
        let key = commandKey cmd
        if null key
            then printMainHelp >> exitSuccess
            else if hasTerseHelp
                then printTerseHelp key >> exitSuccess
                else printCommandHelp key >> exitSuccess

    -- init runs before repo discovery — it creates repos, not uses them.
    case cmd of
        ("init":rest) -> runInit rest >> exitSuccess
        _ -> pure ()

    origCwd <- Dir.getCurrentDirectory
    mRoot <- findBitRoot origCwd

    -- Handle bare repo case: pass through to git for unknown commands,
    -- reject known bit commands that require a work tree
    case mRoot of
        Just (BareRoot _) -> case cmd of
            (name:rest) | not (isKnownCommand name) -> tryAlias Nothing name rest
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

    -- For unknown commands, try alias expansion before the repo check.
    -- This allows global aliases to work even without a .bit directory.
    let indexPath = root </> ".bit" </> "index"
    let mIndexPath = if bitExists then Just indexPath else Nothing
    case cmd of
        (name:rest) | not (isKnownCommand name) -> tryAlias mIndexPath name rest
        _ -> pure ()

    -- Repo existence check (init and unknown commands already handled above)
    when (not bitExists) $ do
        hPutStrLn stderr "fatal: not a bit repository (or any of the parent directories): .bit"
        exitWith (ExitFailure 1)

    -- Change to repo root so all .bit/ paths resolve correctly
    when bitExists $
        Dir.setCurrentDirectory root

    -- Ensure .bit/index/<prefix> subdirectory exists for git -C
    when (bitExists && not (null prefix)) $
        Dir.createDirectoryIfMissing True (root </> ".bit" </> "index" </> prefix)

    let cwd = root

    -- Lightweight env (no scan) — for fetch, verify, and explicit-remote commands (falls back to "origin")
    let baseEnv = do
            mRemote <- getDefaultRemote cwd
            pure $ BitEnv cwd prefix mRemote forceMode

    -- Strict upstream env — requires branch.main.remote (for push and pull without explicit remote)
    let upstreamEnv = do
            mRemote <- getUpstreamRemote cwd
            pure $ BitEnv cwd prefix mRemote forceMode

    -- Scan + bitignore sync + metadata write — for write commands (add, commit, etc.)
    let scanAndWrite = do
            syncBitignoreToIndex cwd
            localFiles <- Scan.scanWorkingDir cwd concurrency
            Scan.writeMetadataFiles cwd localFiles

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

    case cmd of
        -- ── No env needed ────────────────────────────────────
        -- (init is dispatched earlier, before repo discovery)
        ["remote", "add", name, url]    -> Bit.remoteAdd name url
        ["fsck"]                        -> Bit.fsck cwd
        ["merge", "--abort"]            -> Bit.mergeAbort
        ["branch", "--unset-upstream"]  -> Bit.unsetUpstream

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
            Bit.add prefix rest >>= exitWith
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
        ["push"]                        -> runUpstream Bit.push
        ["push", "-u", name]            -> runPushWithUpstream name
        ["push", "--set-upstream", name] -> runPushWithUpstream name
        ["push", name]                  -> runBaseWithRemote name Bit.push
        
        -- pull (no explicit remote: requires upstream, no "origin" fallback)
        ["pull"]                        -> runScannedUpstream $ Bit.pull Bit.defaultPullOptions
        ["pull", name]                  -> runScannedWithRemote name $ Bit.pull Bit.defaultPullOptions
        ["pull", "--accept-remote"]     -> runScannedUpstream $ Bit.pull optsAcceptRemote
        ["pull", "--manual-merge"]      -> runScannedUpstream $ Bit.pull optsManualMerge
        ["pull", name, "--accept-remote"] -> runScannedWithRemote name $ Bit.pull optsAcceptRemote
        ["pull", "--accept-remote", name] -> runScannedWithRemote name $ Bit.pull optsAcceptRemote
        ["pull", name, "--manual-merge"] -> runScannedWithRemote name $ Bit.pull optsManualMerge
        ["pull", "--manual-merge", name] -> runScannedWithRemote name $ Bit.pull optsManualMerge
        
        -- fetch (falls back to "origin" like git fetch)
        ["fetch"]                       -> runScanned Bit.fetch
        ["fetch", name]                 -> runScannedWithRemote name Bit.fetch
        
        _                               -> do
            hPutStrLn stderr $ "bit: '" ++ unwords cmd ++ "' is not a bit command. See 'bit help'."
            exitWith (ExitFailure 1)
