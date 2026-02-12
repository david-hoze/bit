{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bit.Commands (run) where

import qualified Bit.Core as Bit
import Bit.Types (BitEnv(..), ForceMode(..), runBitM)
import qualified Bit.Scan as Scan  -- Only for the pre-scan in runCommand
import Bit.Remote (getDefaultRemote, resolveRemote)
import Bit.Utils (atomicWriteFileStr)
import Bit.Concurrency (Concurrency(..))
import qualified Bit.RemoteWorkspace as RemoteWorkspace
import System.Environment (getArgs)
import Bit.Help (printMainHelp, printTerseHelp, printCommandHelp)
import System.Exit (ExitCode(..), exitWith, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Control.Monad (when, unless, void)
import qualified System.Directory as Dir
import qualified Internal.Git as Git
import Data.List (dropWhileEnd)
-- Strict IO imports to avoid Windows file locking issues
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')

run :: IO ()
run = do
    args <- getArgs
    case args of
        []               -> printMainHelp >> exitSuccess
        ["help"]         -> printMainHelp >> exitSuccess
        ["help", cmd]    -> printCommandHelp cmd >> exitSuccess
        ["help", c1, c2] -> printCommandHelp (c1 ++ " " ++ c2) >> exitSuccess
        ["-h"]           -> printMainHelp >> exitSuccess
        ["--help"]       -> printMainHelp >> exitSuccess
        _  -> case extractRemoteTarget args of
            RemoteError msg -> do
                hPutStrLn stderr $ "fatal: " ++ msg
                exitWith (ExitFailure 1)
            NoRemote remaining -> runCommand remaining
            RemoteFound remoteName remaining ->
                runRemoteCommand remoteName remaining

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
    cwd <- Dir.getCurrentDirectory
    bitExists <- Dir.doesDirectoryExist (cwd </> ".bit")
    unless bitExists $ do
        hPutStrLn stderr "fatal: not a bit repository (or any of the parent directories): .bit"
        exitWith (ExitFailure 1)

    mRemote <- resolveRemote cwd remoteName
    case mRemote of
        Nothing -> do
            hPutStrLn stderr $ "fatal: remote '" ++ remoteName ++ "' not found."
            exitWith (ExitFailure 1)
        Just remote ->
            let isSequential = "--sequential" `elem` args
                concurrency = if isSequential then Sequential else Parallel 0
                cmd = filter (/= "--sequential") args
                runVerifyCmd repairMode = do
                    let env = BitEnv cwd (Just remote) NoForce
                    runBitM env $ Bit.verify (Bit.VerifyRemotePath remote) repairMode concurrency
            in case cmd of
            ["init"] ->
                RemoteWorkspace.initRemote remote remoteName
            ("add":paths) ->
                RemoteWorkspace.addRemote remote paths >>= exitWith
            ("commit":commitArgs) ->
                RemoteWorkspace.commitRemote remote commitArgs >>= exitWith
            ("status":rest) ->
                RemoteWorkspace.statusRemote remote rest >>= exitWith
            ("log":rest) ->
                RemoteWorkspace.logRemote remote rest >>= exitWith
            ("ls-files":rest) ->
                RemoteWorkspace.lsFilesRemote remote rest >>= exitWith
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

    cwd <- Dir.getCurrentDirectory
    bitExists <- Dir.doesDirectoryExist (cwd </> ".bit")

    -- Lightweight env (no scan) — for read-only commands
    let baseEnv = do
            mRemote <- getDefaultRemote cwd
            pure $ BitEnv cwd mRemote forceMode

    -- Scan + bitignore sync + metadata write — for write commands (add, commit, etc.)
    let scanAndWrite = do
            syncBitignoreToIndex cwd
            localFiles <- Scan.scanWorkingDir cwd
            Scan.writeMetadataFiles cwd localFiles

    -- Repo existence check (skip for init)
    let needsRepo = cmd /= ["init"]
    when (needsRepo && not bitExists) $ do
        hPutStrLn stderr "fatal: not a bit repository (or any of the parent directories): .bit"
        exitWith (ExitFailure 1)

    -- Helper functions for running commands
    let runScanned action = do { scanAndWrite; env <- baseEnv; runBitM env action }
    let runBase action = baseEnv >>= \env -> runBitM env action
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
        ["init"]                        -> Bit.init
        ["remote", "add", name, url]    -> Bit.remoteAdd name url
        ["fsck"]                        -> Bit.fsck cwd
        ["merge", "--abort"]            -> Bit.mergeAbort
        ["branch", "--unset-upstream"]  -> Bit.unsetUpstream

        -- ── Lightweight env (no scan) ────────────────────────
        ("log":rest)                    -> Bit.log rest >>= exitWith
        ("ls-files":rest)               -> Bit.lsFiles rest >>= exitWith
        ["remote", "show"]              -> runBase $ Bit.remoteShow Nothing
        ["remote", "show", name]        -> runBaseWithRemote name $ Bit.remoteShow (Just name)
        ["verify"]                      -> runBase $ Bit.verify Bit.VerifyLocal Bit.PromptRepair concurrency
        ["repair"]                      -> runBase $ Bit.verify Bit.VerifyLocal Bit.AutoRepair concurrency

        ("rm":rest)                     -> runBase (Bit.rm rest) >>= exitWith

        -- ── Full scanned env (needs working directory state) ─
        ("add":rest)                    -> do
            scanAndWrite
            Bit.add rest >>= exitWith
        ("commit":rest)                 -> do
            scanAndWrite
            Bit.commit rest >>= exitWith
        ("diff":rest)                   -> do
            scanAndWrite
            Bit.diff rest >>= exitWith
        ("status":rest)                 -> runScanned (Bit.status rest) >>= exitWith
        ("restore":rest)                -> runScanned (Bit.restore rest) >>= exitWith
        ("checkout":rest)               -> runScanned (Bit.checkout rest) >>= exitWith
        
        -- push (no scan needed — uses git metadata diff)
        ["push"]                        -> runBase Bit.push
        ["push", "-u", name]            -> runPushWithUpstream name
        ["push", "--set-upstream", name] -> runPushWithUpstream name
        ["push", name]                  -> runBaseWithRemote name Bit.push
        
        -- pull
        ["pull"]                        -> runScanned $ Bit.pull Bit.defaultPullOptions
        ["pull", name]                  -> runScannedWithRemote name $ Bit.pull Bit.defaultPullOptions
        ["pull", "--accept-remote"]     -> runScanned $ Bit.pull optsAcceptRemote
        ["pull", "--manual-merge"]      -> runScanned $ Bit.pull optsManualMerge
        ["pull", name, "--accept-remote"] -> runScannedWithRemote name $ Bit.pull optsAcceptRemote
        ["pull", "--accept-remote", name] -> runScannedWithRemote name $ Bit.pull optsAcceptRemote
        ["pull", name, "--manual-merge"] -> runScannedWithRemote name $ Bit.pull optsManualMerge
        ["pull", "--manual-merge", name] -> runScannedWithRemote name $ Bit.pull optsManualMerge
        
        -- fetch
        ["fetch"]                       -> runScanned Bit.fetch
        ["fetch", name]                 -> runScannedWithRemote name Bit.fetch
        
        ["merge", "--continue"]         -> runScanned Bit.mergeContinue
        _                               -> do
            hPutStrLn stderr $ "bit: '" ++ unwords cmd ++ "' is not a bit command. See 'bit help'."
            exitWith (ExitFailure 1)
