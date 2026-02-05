{-# LANGUAGE LambdaCase #-}

module Bit.Commands (run) where

import qualified Bit.Core as Bit
import Bit.Types (BitEnv(..), runBitM)
import qualified Bit.Scan as Scan  -- Only for the pre-scan in runCommand
import Bit.Remote (getDefaultRemote, resolveRemote)
import Bit.Utils (atomicWriteFileStr)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Control.Monad (when, unless, void)
import qualified System.Directory as Dir
import qualified Internal.Git as Git
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
            , "  push [-u|--set-upstream] [<remote>]"
            , "                                 Push to remote"
            , "  pull [<remote>] [options]      Pull from remote"
            , "      --accept-remote            Accept remote state as truth"
            , "      --manual-merge             Manual conflict resolution"
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
            ]
        _  -> runCommand args

-- | Helper function to push with upstream tracking
pushWithUpstream :: BitEnv -> FilePath -> String -> IO ()
pushWithUpstream env cwd name = do
    mNamedRemote <- resolveRemote cwd name
    let envWithRemote = env { envRemote = mNamedRemote }
    runBitM envWithRemote Bit.push
    -- After successful push, set upstream tracking
    void $ Git.setupBranchTrackingFor name
    putStrLn $ "branch 'main' set up to track '" ++ name ++ "/main'."

runCommand :: [String] -> IO ()
runCommand args = do
    let isForce = "--force" `elem` args || "-f" `elem` args
    let isForceWithLease = "--force-with-lease" `elem` args
    when (isForce && isForceWithLease) $ do
        hPutStrLn stderr "fatal: Cannot use both --force and --force-with-lease"
        exitWith (ExitFailure 1)
    let cmd = filter (`notElem` ["--force", "-f", "--force-with-lease"]) args

    cwd <- Dir.getCurrentDirectory

    -- Pre-scan (skip for commands that don't need it)
    -- TODO: Move this scan into Bit.hs later - it builds the env, so keeping it here for now
    -- Skip scanning for read-only commands that only read git history, index, or config
    let skipScan = cmd == ["init"]
                || cmd `elem` [["verify"], ["verify", "--remote"], ["fsck"]]
                || (not (null cmd) && head cmd == "log")
                || (not (null cmd) && head cmd == "ls-files")
                || (length cmd >= 2 && take 2 cmd == ["remote", "check"])
                || (length cmd >= 2 && take 2 cmd == ["remote", "show"])
    
    -- Only scan and write metadata if .bit directory exists (repo is initialized)
    bitExists <- Dir.doesDirectoryExist (cwd </> ".bit")
    
    -- Copy .bitignore to .bit/index/.gitignore before scan (if repo exists)
    -- Using .gitignore directly since git check-ignore reads it automatically
    -- Read and re-write to normalize line endings and remove trailing spaces
    when (bitExists && not skipScan) $ do
        let bitignoreSrc = cwd </> ".bitignore"
        let bitignoreDest = cwd </> ".bit" </> "index" </> ".gitignore"
        bitignoreExists <- Dir.doesFileExist bitignoreSrc
        if bitignoreExists
            then do
                -- Use strict ByteString reading to avoid Windows file locking issues
                bs <- BS.readFile bitignoreSrc
                let content = case decodeUtf8' bs of
                      Left _ -> ""
                      Right txt -> T.unpack txt
                -- Normalize: trim each line, remove empty lines, use LF endings
                let normalizedLines = filter (not . null) $ map (dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse . filter (/= '\r')) (lines content)
                atomicWriteFileStr bitignoreDest (unlines normalizedLines)
            else do
                -- Remove stale .gitignore if root .bitignore doesn't exist
                destExists <- Dir.doesFileExist bitignoreDest
                when destExists $ Dir.removeFile bitignoreDest
    
    localFiles <- if skipScan || not bitExists then return [] else Scan.scanWorkingDir cwd
    unless (skipScan || not bitExists) $ Scan.writeMetadataFiles cwd localFiles
    mRemote <- getDefaultRemote cwd

    let env = BitEnv cwd localFiles mRemote isForce isForceWithLease

    -- Check if repository is initialized for commands that need it
    let needsRepo = cmd /= ["init"]
    when (needsRepo && not bitExists) $ do
        hPutStrLn stderr "fatal: not a bit repository (or any of the parent directories): .bit"
        exitWith (ExitFailure 1)

    case cmd of
        ["init"]                        -> Bit.init
        ["remote", "add", name, url]    -> Bit.remoteAdd name url
        ["remote", "show"]              -> runBitM env $ Bit.remoteShow Nothing
        ["remote", "show", name]        -> do
            mNamedRemote <- resolveRemote cwd name
            let envWithRemote = env { envRemote = mNamedRemote }
            runBitM envWithRemote $ Bit.remoteShow (Just name)
        ["remote", "check"]             -> runBitM env $ Bit.remoteCheck Nothing
        ["remote", "check", name]       -> do
            mNamedRemote <- resolveRemote cwd name
            let envWithRemote = env { envRemote = mNamedRemote }
            runBitM envWithRemote $ Bit.remoteCheck (Just name)
        ["verify"]                      -> runBitM env $ Bit.verify False
        ["verify", "--remote"]          -> runBitM env $ Bit.verify True
        ["fsck"]                        -> Bit.fsck cwd
        ("add":rest)                    -> Bit.add rest >>= exitWith
        ("commit":rest)                 -> Bit.commit rest >>= exitWith
        ("diff":rest)                   -> Bit.diff rest >>= exitWith
        ("log":rest)                    -> Bit.log rest >>= exitWith
        ("ls-files":rest)               -> Bit.lsFiles rest >>= exitWith
        ("restore":rest)                -> runBitM env (Bit.restore rest) >>= exitWith
        ("checkout":rest)               -> runBitM env (Bit.checkout rest) >>= exitWith
        ("status":rest)                 -> runBitM env (Bit.status rest) >>= exitWith
        
        -- push
        ["push"]                        -> runBitM env Bit.push
        ["push", "-u", name]            -> pushWithUpstream env cwd name
        ["push", "--set-upstream", name] -> pushWithUpstream env cwd name
        ["push", name]                  -> do
            mNamedRemote <- resolveRemote cwd name
            let envWithRemote = env { envRemote = mNamedRemote }
            runBitM envWithRemote Bit.push
        
        -- pull
        ["pull"]                        -> runBitM env $ Bit.pull Bit.defaultPullOptions
        ["pull", name]                  -> do
            mNamedRemote <- resolveRemote cwd name
            let envWithRemote = env { envRemote = mNamedRemote }
            runBitM envWithRemote $ Bit.pull Bit.defaultPullOptions
        ["pull", "--accept-remote"]     -> runBitM env $ Bit.pull Bit.defaultPullOptions { Bit.pullAcceptRemote = True }
        ["pull", "--manual-merge"]      -> runBitM env $ Bit.pull Bit.defaultPullOptions { Bit.pullManualMerge = True }
        ["pull", name, "--accept-remote"] -> do
            mNamedRemote <- resolveRemote cwd name
            let envWithRemote = env { envRemote = mNamedRemote }
            runBitM envWithRemote $ Bit.pull Bit.defaultPullOptions { Bit.pullAcceptRemote = True }
        ["pull", "--accept-remote", name] -> do
            mNamedRemote <- resolveRemote cwd name
            let envWithRemote = env { envRemote = mNamedRemote }
            runBitM envWithRemote $ Bit.pull Bit.defaultPullOptions { Bit.pullAcceptRemote = True }
        ["pull", name, "--manual-merge"] -> do
            mNamedRemote <- resolveRemote cwd name
            let envWithRemote = env { envRemote = mNamedRemote }
            runBitM envWithRemote $ Bit.pull Bit.defaultPullOptions { Bit.pullManualMerge = True }
        ["pull", "--manual-merge", name] -> do
            mNamedRemote <- resolveRemote cwd name
            let envWithRemote = env { envRemote = mNamedRemote }
            runBitM envWithRemote $ Bit.pull Bit.defaultPullOptions { Bit.pullManualMerge = True }
        
        -- fetch
        ["fetch"]                       -> runBitM env Bit.fetch
        ["fetch", name]                 -> do
            mNamedRemote <- resolveRemote cwd name
            let envWithRemote = env { envRemote = mNamedRemote }
            runBitM envWithRemote Bit.fetch
        
        ["merge", "--continue"]         -> runBitM env Bit.mergeContinue
        ["merge", "--abort"]            -> Bit.mergeAbort
        ["branch", "--unset-upstream"]  -> Bit.unsetUpstream
        _                               -> hPutStrLn stderr "Unknown command."
