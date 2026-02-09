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
import System.Exit (ExitCode(..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (rawSystem)
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
        [] -> hPutStrLn stderr $ unlines
            [ "Usage: bit <command> [options]"
            , ""
            , "Commands:"
            , "  init                           Initialize a new bit repository"
            , "  status                         Show working tree status"
            , "  add <path>                     Add file contents to metadata"
            , "  commit -m <msg>                Record changes to the repository"
            , "  log                            Show commit history"
            , "  diff                           Show changes"
            , "  restore [options] [--] <path>  Restore working tree files"
            , "  checkout [options] -- <path>   Checkout files from index"
            , ""
            , "  push [-u|--set-upstream] [<remote>] [--skip-verify]"
            , "                                 Push to remote"
            , "  pull [<remote>] [options]      Pull from remote"
            , "      --accept-remote            Accept remote state as truth"
            , "      --manual-merge             Manual conflict resolution"
            , "      --skip-verify              Skip proof of possession check"
            , "  fetch [<remote>]               Fetch metadata from remote"
            , ""
            , "  remote add <name> <url>        Add a remote"
            , "  remote show [<name>]           Show remote information"
            , "  remote check [<name>]          Check remote connectivity"
            , ""
            , "  verify [--remote]              Verify file integrity"
            , "  fsck                           Full integrity check"
            , "  merge --continue|--abort       Continue or abort merge"
            , "  branch --unset-upstream        Unset upstream tracking"
            , ""
            , "Remote-targeted commands:"
            , "  --remote <name> <cmd>          Target a remote workspace (portable)"
            , "  @<remote> <cmd>                Shorthand (needs quoting in PowerShell)"
            , ""
            , "  Supported: init, add <path>, commit -m <msg>, status, log"
            ]
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

-- | Execute a command in the context of a remote workspace
runRemoteCommand :: String -> [String] -> IO ()
runRemoteCommand remoteName args = do
    cwd <- Dir.getCurrentDirectory
    bitExists <- Dir.doesDirectoryExist (cwd </> ".bit")
    unless bitExists $ do
        hPutStrLn stderr "fatal: not a bit repository (or any of the parent directories): .bit"
        exitWith (ExitFailure 1)

    -- Resolve the remote
    mRemote <- resolveRemote cwd remoteName
    case mRemote of
        Nothing -> do
            hPutStrLn stderr $ "fatal: remote '" ++ remoteName ++ "' not found."
            exitWith (ExitFailure 1)
        Just remote -> do
            let wsPath = RemoteWorkspace.remoteWorkspacePath cwd remoteName

            case args of
                ["init"] ->
                    RemoteWorkspace.initRemoteWorkspace cwd remote remoteName

                ("add":paths) -> do
                    -- Stage files in the remote workspace git repo
                    wsExists <- Dir.doesDirectoryExist (wsPath </> ".git")
                    unless wsExists $ do
                        hPutStrLn stderr $ "fatal: remote workspace not initialized. Run 'bit --remote " ++ remoteName ++ " init' first."
                        exitWith (ExitFailure 1)
                    -- git add in the workspace
                    code <- rawSystem "git" (["-C", wsPath, "add"] ++ paths)
                    exitWith code

                ("commit":commitArgs) -> do
                    wsExists <- Dir.doesDirectoryExist (wsPath </> ".git")
                    unless wsExists $ do
                        hPutStrLn stderr $ "fatal: remote workspace not initialized. Run 'bit --remote " ++ remoteName ++ " init' first."
                        exitWith (ExitFailure 1)
                    -- git commit in the workspace
                    code <- rawSystem "git" (["-C", wsPath, "commit"] ++ commitArgs)
                    when (code == ExitSuccess) $ do
                        -- Create bundle and push to remote using shared function
                        void $ RemoteWorkspace.createAndPushBundle wsPath remote
                    exitWith code

                ("status":rest) -> do
                    wsExists <- Dir.doesDirectoryExist (wsPath </> ".git")
                    unless wsExists $ do
                        hPutStrLn stderr $ "fatal: remote workspace not initialized. Run 'bit --remote " ++ remoteName ++ " init' first."
                        exitWith (ExitFailure 1)
                    void $ rawSystem "git" (["-C", wsPath, "status"] ++ rest)

                ("log":rest) -> do
                    wsExists <- Dir.doesDirectoryExist (wsPath </> ".git")
                    unless wsExists $ do
                        hPutStrLn stderr $ "fatal: remote workspace not initialized. Run 'bit --remote " ++ remoteName ++ " init' first."
                        exitWith (ExitFailure 1)
                    void $ rawSystem "git" (["-C", wsPath, "log"] ++ rest)

                _ -> do
                    hPutStrLn stderr $ "error: command not supported in remote context: " ++ unwords args
                    hPutStrLn stderr "Supported: init, add, commit, status, log"
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

runCommand :: [String] -> IO ()
runCommand args = do
    let hasForce = "--force" `elem` args || "-f" `elem` args
    let hasForceWithLease = "--force-with-lease" `elem` args
    let isSequential = "--sequential" `elem` args
    let isSkipVerify = "--skip-verify" `elem` args
    when (hasForce && hasForceWithLease) $ do
        hPutStrLn stderr "fatal: Cannot use both --force and --force-with-lease"
        exitWith (ExitFailure 1)
    let forceMode
          | hasForce          = Force
          | hasForceWithLease = ForceWithLease
          | otherwise         = NoForce
    let cmd = filter (`notElem` ["--force", "-f", "--force-with-lease", "--sequential", "--skip-verify"]) args

    cwd <- Dir.getCurrentDirectory
    bitExists <- Dir.doesDirectoryExist (cwd </> ".bit")

    -- Lightweight env (no scan) — for read-only commands
    let baseEnv = do
            mRemote <- getDefaultRemote cwd
            pure $ BitEnv cwd [] mRemote forceMode isSkipVerify

    -- Full env (scan + bitignore sync + metadata write) — for write commands
    let scannedEnv = do
            syncBitignoreToIndex cwd
            localFiles <- Scan.scanWorkingDir cwd
            Scan.writeMetadataFiles cwd localFiles
            mRemote <- getDefaultRemote cwd
            pure $ BitEnv cwd localFiles mRemote forceMode isSkipVerify

    -- Repo existence check (skip for init)
    let needsRepo = cmd /= ["init"]
    when (needsRepo && not bitExists) $ do
        hPutStrLn stderr "fatal: not a bit repository (or any of the parent directories): .bit"
        exitWith (ExitFailure 1)

    -- Helper functions for running commands
    let runScanned action = scannedEnv >>= \env -> runBitM env action
    let runBase action = baseEnv >>= \env -> runBitM env action
    let runScannedWithRemote name action = do
            env <- scannedEnv
            mNamedRemote <- resolveRemote cwd name
            runBitM env { envRemote = mNamedRemote } action
    let runBaseWithRemote name action = do
            env <- baseEnv
            mNamedRemote <- resolveRemote cwd name
            runBitM env { envRemote = mNamedRemote } action

    case cmd of
        -- ── No env needed ────────────────────────────────────
        ["init"]                        -> Bit.init
        ["remote", "add", name, url]    -> Bit.remoteAdd name url
        ["fsck"]                        -> Bit.fsck cwd (if isSequential then Sequential else Parallel 0)
        ["merge", "--abort"]            -> Bit.mergeAbort
        ["branch", "--unset-upstream"]  -> Bit.unsetUpstream

        -- ── Lightweight env (no scan) ────────────────────────
        ("log":rest)                    -> Bit.log rest >>= exitWith
        ("ls-files":rest)               -> Bit.lsFiles rest >>= exitWith
        ["remote", "show"]              -> runBase $ Bit.remoteShow Nothing
        ["remote", "show", name]        -> runBaseWithRemote name $ Bit.remoteShow (Just name)
        ["remote", "check"]             -> runBase $ Bit.remoteCheck Nothing
        ["remote", "check", name]       -> runBaseWithRemote name $ Bit.remoteCheck (Just name)
        ["verify"]                      -> runBase $ Bit.verify False (if isSequential then Sequential else Parallel 0)
        ["verify", "--remote"]          -> runBase $ Bit.verify True (if isSequential then Sequential else Parallel 0)

        -- ── Full scanned env (needs working directory state) ─
        ("add":rest)                    -> do
            void scannedEnv
            Bit.add rest >>= exitWith
        ("commit":rest)                 -> do
            void scannedEnv
            Bit.commit rest >>= exitWith
        ("diff":rest)                   -> do
            void scannedEnv
            Bit.diff rest >>= exitWith
        ("status":rest)                 -> runScanned (Bit.status rest) >>= exitWith
        ("restore":rest)                -> runScanned (Bit.restore rest) >>= exitWith
        ("checkout":rest)               -> runScanned (Bit.checkout rest) >>= exitWith
        
        -- push
        ["push"]                        -> runScanned Bit.push
        ["push", "-u", name]            -> scannedEnv >>= \env -> pushWithUpstream env cwd name
        ["push", "--set-upstream", name] -> scannedEnv >>= \env -> pushWithUpstream env cwd name
        ["push", name]                  -> runScannedWithRemote name Bit.push
        
        -- pull
        ["pull"]                        -> runScanned $ Bit.pull Bit.defaultPullOptions { Bit.pullSkipVerify = isSkipVerify }
        ["pull", name]                  -> runScannedWithRemote name $ Bit.pull Bit.defaultPullOptions { Bit.pullSkipVerify = isSkipVerify }
        ["pull", "--accept-remote"]     -> runScanned $ Bit.pull (Bit.PullOptions Bit.PullAcceptRemote isSkipVerify)
        ["pull", "--manual-merge"]      -> runScanned $ Bit.pull (Bit.PullOptions Bit.PullManualMerge isSkipVerify)
        ["pull", name, "--accept-remote"] -> runScannedWithRemote name $ Bit.pull (Bit.PullOptions Bit.PullAcceptRemote isSkipVerify)
        ["pull", "--accept-remote", name] -> runScannedWithRemote name $ Bit.pull (Bit.PullOptions Bit.PullAcceptRemote isSkipVerify)
        ["pull", name, "--manual-merge"] -> runScannedWithRemote name $ Bit.pull (Bit.PullOptions Bit.PullManualMerge isSkipVerify)
        ["pull", "--manual-merge", name] -> runScannedWithRemote name $ Bit.pull (Bit.PullOptions Bit.PullManualMerge isSkipVerify)
        
        -- fetch
        ["fetch"]                       -> runScanned Bit.fetch
        ["fetch", name]                 -> runScannedWithRemote name Bit.fetch
        
        ["merge", "--continue"]         -> runScanned Bit.mergeContinue
        _                               -> hPutStrLn stderr "Unknown command."
