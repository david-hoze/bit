{-# LANGUAGE LambdaCase #-}

module Bit.Commands (run) where

import qualified Bit.Core as Bit
import Bit.Types (BitEnv(..), runBitM)
import qualified Bit.Scan as Scan  -- Only for the pre-scan in runCommand
import Bit.Remote (getDefaultRemote, resolveRemote)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import Control.Monad (when, unless, void)
import qualified System.Directory as Dir

run :: IO ()
run = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "Usage: bit [init|status|add|commit|restore|checkout|fetch|pull|push|verify|verify --remote|fsck|branch --unset-upstream|remote add <name> <url>|remote show [<name>]|remote check [<name>]]"
        _  -> runCommand args

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
    let skipScan = cmd == ["init"]
                || cmd `elem` [["verify"], ["verify", "--remote"], ["fsck"], ["remote", "check"]]
                || (length cmd == 3 && take 2 cmd == ["remote", "check"])
    localFiles <- if skipScan then return [] else Scan.scanWorkingDir cwd
    unless skipScan $ Scan.writeMetadataFiles cwd localFiles
    mRemote <- getDefaultRemote cwd

    let env = BitEnv cwd localFiles mRemote isForce isForceWithLease

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
        ("add":rest)                    -> void $ Bit.add rest
        ("commit":rest)                 -> void $ Bit.commit rest
        ("diff":rest)                   -> void $ Bit.diff rest
        ("restore":rest)                -> runBitM env $ Bit.restore rest
        ("checkout":rest)               -> runBitM env $ Bit.checkout rest
        ("status":rest)                 -> runBitM env $ Bit.status rest
        ["fetch"]                       -> runBitM env Bit.fetch
        ["pull"]                        -> runBitM env $ Bit.pull Bit.defaultPullOptions
        ["pull", "--accept-remote"]     -> runBitM env $ Bit.pull Bit.defaultPullOptions { Bit.pullAcceptRemote = True }
        ["pull", "--manual-merge"]      -> runBitM env $ Bit.pull Bit.defaultPullOptions { Bit.pullManualMerge = True }
        ["push"]                        -> runBitM env Bit.push
        ["merge", "--continue"]         -> runBitM env Bit.mergeContinue
        ["merge", "--abort"]            -> Bit.mergeAbort
        ["branch", "--unset-upstream"]  -> Bit.unsetUpstream
        _                               -> hPutStrLn stderr "Unknown command."
