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

runCommand :: [String] -> IO ()
runCommand args = do
    let isForce = "--force" `elem` args || "-f" `elem` args
    let isForceWithLease = "--force-with-lease" `elem` args
    let isSequential = "--sequential" `elem` args
    let isSkipVerify = "--skip-verify" `elem` args
    when (isForce && isForceWithLease) $ do
        hPutStrLn stderr "fatal: Cannot use both --force and --force-with-lease"
        exitWith (ExitFailure 1)
    let cmd = filter (`notElem` ["--force", "-f", "--force-with-lease", "--sequential", "--skip-verify"]) args

    cwd <- Dir.getCurrentDirectory
    bitExists <- Dir.doesDirectoryExist (cwd </> ".bit")

    -- Lightweight env (no scan) — for read-only commands
    let baseEnv = do
            mRemote <- getDefaultRemote cwd
            return $ BitEnv cwd [] mRemote isForce isForceWithLease isSkipVerify

    -- Full env (scan + bitignore sync + metadata write) — for write commands
    let scannedEnv = do
            syncBitignoreToIndex cwd
            localFiles <- Scan.scanWorkingDir cwd
            Scan.writeMetadataFiles cwd localFiles
            mRemote <- getDefaultRemote cwd
            return $ BitEnv cwd localFiles mRemote isForce isForceWithLease isSkipVerify

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
            _ <- scannedEnv
            Bit.add rest >>= exitWith
        ("commit":rest)                 -> do
            _ <- scannedEnv
            Bit.commit rest >>= exitWith
        ("diff":rest)                   -> do
            _ <- scannedEnv
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
        ["pull", "--accept-remote"]     -> runScanned $ Bit.pull Bit.defaultPullOptions { Bit.pullAcceptRemote = True, Bit.pullSkipVerify = isSkipVerify }
        ["pull", "--manual-merge"]      -> runScanned $ Bit.pull Bit.defaultPullOptions { Bit.pullManualMerge = True, Bit.pullSkipVerify = isSkipVerify }
        ["pull", name, "--accept-remote"] -> runScannedWithRemote name $ Bit.pull Bit.defaultPullOptions { Bit.pullAcceptRemote = True, Bit.pullSkipVerify = isSkipVerify }
        ["pull", "--accept-remote", name] -> runScannedWithRemote name $ Bit.pull Bit.defaultPullOptions { Bit.pullAcceptRemote = True, Bit.pullSkipVerify = isSkipVerify }
        ["pull", name, "--manual-merge"] -> runScannedWithRemote name $ Bit.pull Bit.defaultPullOptions { Bit.pullManualMerge = True, Bit.pullSkipVerify = isSkipVerify }
        ["pull", "--manual-merge", name] -> runScannedWithRemote name $ Bit.pull Bit.defaultPullOptions { Bit.pullManualMerge = True, Bit.pullSkipVerify = isSkipVerify }
        
        -- fetch
        ["fetch"]                       -> runScanned Bit.fetch
        ["fetch", name]                 -> runScannedWithRemote name Bit.fetch
        
        ["merge", "--continue"]         -> runScanned Bit.mergeContinue
        _                               -> hPutStrLn stderr "Unknown command."
