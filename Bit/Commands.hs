{-# LANGUAGE LambdaCase #-}

module Bit.Commands (run) where

import qualified Bit.Core as Bit
import Bit.Types (BitEnv(..), BitM, runBitM)
import qualified Bit.Scan as Scan  -- Only for the pre-scan in runCommand
import Bit.Remote (getDefaultRemote, resolveRemote)
import Bit.Utils (atomicWriteFileStr)
import Bit.Concurrency (Concurrency(..))
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
        let content = case decodeUtf8' bs of
              Left _ -> ""
              Right txt -> T.unpack txt
            normalizedLines = filter (not . null) $
              map (trim . filter (/= '\r')) (lines content)
        atomicWriteFileStr dest (unlines normalizedLines)
    
    removeStaleGitignore :: FilePath -> IO ()
    removeStaleGitignore dest = do
        destExists <- Dir.doesFileExist dest
        when destExists $ Dir.removeFile dest
    
    trim :: String -> String
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | Run a BitM action with a resolved remote
runWithRemote :: BitEnv -> FilePath -> String -> BitM a -> IO a
runWithRemote env cwd name action = do
    mNamedRemote <- resolveRemote cwd name
    let envWithRemote = env { envRemote = mNamedRemote }
    runBitM envWithRemote action

runCommand :: [String] -> IO ()
runCommand args = do
    let isForce = "--force" `elem` args || "-f" `elem` args
    let isForceWithLease = "--force-with-lease" `elem` args
    let isSequential = "--sequential" `elem` args
    when (isForce && isForceWithLease) $ do
        hPutStrLn stderr "fatal: Cannot use both --force and --force-with-lease"
        exitWith (ExitFailure 1)
    let cmd = filter (`notElem` ["--force", "-f", "--force-with-lease", "--sequential"]) args

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
    when (bitExists && not skipScan) $
        syncBitignoreToIndex cwd
    
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
        ["remote", "show", name]        -> runWithRemote env cwd name $ Bit.remoteShow (Just name)
        ["remote", "check"]             -> runBitM env $ Bit.remoteCheck Nothing
        ["remote", "check", name]       -> runWithRemote env cwd name $ Bit.remoteCheck (Just name)
        ["verify"]                      -> runBitM env $ Bit.verify False (if isSequential then Sequential else Parallel 0)
        ["verify", "--remote"]          -> runBitM env $ Bit.verify True (if isSequential then Sequential else Parallel 0)
        ["fsck"]                        -> Bit.fsck cwd (if isSequential then Sequential else Parallel 0)
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
        ["push", name]                  -> runWithRemote env cwd name Bit.push
        
        -- pull
        ["pull"]                        -> runBitM env $ Bit.pull Bit.defaultPullOptions
        ["pull", name]                  -> runWithRemote env cwd name $ Bit.pull Bit.defaultPullOptions
        ["pull", "--accept-remote"]     -> runBitM env $ Bit.pull Bit.defaultPullOptions { Bit.pullAcceptRemote = True }
        ["pull", "--manual-merge"]      -> runBitM env $ Bit.pull Bit.defaultPullOptions { Bit.pullManualMerge = True }
        ["pull", name, "--accept-remote"] -> runWithRemote env cwd name $ Bit.pull Bit.defaultPullOptions { Bit.pullAcceptRemote = True }
        ["pull", "--accept-remote", name] -> runWithRemote env cwd name $ Bit.pull Bit.defaultPullOptions { Bit.pullAcceptRemote = True }
        ["pull", name, "--manual-merge"] -> runWithRemote env cwd name $ Bit.pull Bit.defaultPullOptions { Bit.pullManualMerge = True }
        ["pull", "--manual-merge", name] -> runWithRemote env cwd name $ Bit.pull Bit.defaultPullOptions { Bit.pullManualMerge = True }
        
        -- fetch
        ["fetch"]                       -> runBitM env Bit.fetch
        ["fetch", name]                 -> runWithRemote env cwd name Bit.fetch
        
        ["merge", "--continue"]         -> runBitM env Bit.mergeContinue
        ["merge", "--abort"]            -> Bit.mergeAbort
        ["branch", "--unset-upstream"]  -> Bit.unsetUpstream
        _                               -> hPutStrLn stderr "Unknown command."
