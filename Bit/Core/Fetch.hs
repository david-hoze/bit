{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Core.Fetch
    ( fetch
    , cloudFetch
    , filesystemFetch
    , fetchRemoteBundle
    , fetchBundle
    , saveFetchedBundle
    , classifyRemoteState
    , interpretRemoteItems
    ) where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import Control.Monad (when, unless)
import System.Exit (ExitCode(..), exitWith)
import qualified Internal.Git as Git
import qualified Internal.Transport as Transport
import System.IO (stderr, hPutStrLn)
import Bit.Remote (Remote, remoteName, remoteUrl, RemoteState(..), FetchResult(..))
import Bit.Types (BitM, BitEnv(..))
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Internal.Config (fromCwdPath, bundleCwdPath, BundleName(..), fetchedBundle)
import Bit.Core.Helpers (getRemoteTargetType, getLocalHeadE, withRemote, safeRemove, tell, tellErr)
import qualified Bit.Device as Device
import System.Directory (copyFile)
import Control.Monad (void)

-- ============================================================================
-- Fetch operations
-- ============================================================================

fetch :: BitM ()
fetch = withRemote $ \remote -> do
    cwd <- asks envCwd
    
    -- Determine if this is a filesystem or cloud remote
    mTarget <- liftIO $ getRemoteTargetType cwd (remoteName remote)
    case mTarget of
        Just (Device.TargetDevice _ _) -> liftIO $ filesystemFetch cwd remote
        Just (Device.TargetLocalPath _) -> liftIO $ filesystemFetch cwd remote
        _ -> cloudFetch remote  -- Cloud remote or no target info (use cloud flow)

-- | Fetch from a cloud remote (original flow, unchanged).
cloudFetch :: Remote -> BitM ()
cloudFetch remote = do
    mb <- liftIO $ fetchRemoteBundle remote
    liftIO $ saveFetchedBundle remote mb

-- | Fetch from a filesystem remote. Fetches commits without merging or syncing files.
filesystemFetch :: FilePath -> Remote -> IO ()
filesystemFetch _cwd remote = do
    let remotePath = remoteUrl remote
    putStrLn $ "Fetching from filesystem remote: " ++ remotePath
    
    -- Check if remote has .bit/ directory
    let remoteBitDir = remotePath </> ".bit"
    remoteHasBit <- Dir.doesDirectoryExist remoteBitDir
    unless remoteHasBit $ do
        hPutStrLn stderr "error: Remote is not a bit repository."
        exitWith (ExitFailure 1)
    
    -- Fetch remote into local
    let remoteIndexGit = remotePath </> ".bit" </> "index" </> ".git"
    
    putStrLn "Fetching remote commits..."
    (fetchCode, _fetchOut, fetchErr) <- Git.runGitWithOutput 
        ["fetch", remoteIndexGit, "main:refs/remotes/origin/main"]
    
    when (fetchCode /= ExitSuccess) $ do
        hPutStrLn stderr $ "Error fetching from remote: " ++ fetchErr
        exitWith fetchCode
    
    -- Output fetch results similar to cloud fetch
    hPutStrLn stderr $ "From " ++ remoteName remote
    hPutStrLn stderr $ " * [new branch]      main       -> origin/main"
    
    putStrLn "Fetch complete."

-- ============================================================================
-- Helper functions for fetch operations
-- ============================================================================

-- | Classify remote state (empty, valid bit, non-bit, corrupted, network error)
-- This is domain logic: it knows what .bit/ means and interprets remote contents
classifyRemoteState :: Remote -> IO RemoteState
classifyRemoteState remote = do
    result <- Transport.listRemoteItems remote 1
    case result of
        Left err -> pure (StateNetworkError err)
        Right items -> pure (interpretRemoteItems items)

-- | Pure interpretation of remote items into domain state
interpretRemoteItems :: [Transport.TransportItem] -> RemoteState
interpretRemoteItems items
    | null items = StateEmpty
    | ".bit" `elem` map Transport.tiName items = StateValidRgit
    | otherwise = StateNonRgitOccupied (take 3 (map Transport.tiName items))

-- | Download the remote bundle for comparison. Returns temp bundle path or error.
-- This is domain logic: it knows about .bit/ layout and bundle files
fetchBundle :: Remote -> IO FetchResult
fetchBundle remote = do
    let localDest = ".bit/temp_remote.bundle"
    
    result <- Transport.copyFromRemoteDetailed remote ".bit/bit.bundle" localDest
    case result of
        Transport.CopySuccess -> pure (BundleFound localDest)
        Transport.CopyNotFound -> pure RemoteEmpty
        Transport.CopyNetworkError _ -> 
            pure (NetworkError "Network unreachable: Check your internet connection or remote name.")
        Transport.CopyOtherError err -> pure (NetworkError err)

-- | Fetch the remote bundle, classify its state, and return path or Nothing on error.
fetchRemoteBundle :: Remote -> IO (Maybe FilePath)
fetchRemoteBundle remote = do
    -- First check remote state (this also determines if remote exists/is valid)
    remoteState <- classifyRemoteState remote
    
    case remoteState of
        StateEmpty -> do
            hPutStrLn stderr "Aborting: Remote is empty. Run 'bit push' first."
            pure Nothing
        
        StateNonRgitOccupied items -> do
            let itemList = unlines $ map ("    " ++) items
            hPutStrLn stderr $ unlines
                [ "fatal: The remote path is not empty and not a bit repository."
                , ""
                , "Found files/directories:"
                , itemList
                , ""
                , "To use a new bit remote, either choose an empty location or push"
                , "to initialize a bit repository at the remote location first."
                ]
            pure Nothing

        StateValidRgit -> do
            fetchResult <- fetchBundle remote
            case fetchResult of
                BundleFound bPath -> pure $ Just bPath
                _ -> do
                    hPutStrLn stderr $ unlines
                        [ "fatal: Could not read from remote repository."
                        , ""
                        , "Please make sure you have the correct access rights"
                        , "and the repository exists."
                        ]
                    pure Nothing

        StateNetworkError err ->
            do
                hPutStrLn stderr $ "Aborting: Network error -> " ++ err
                pure Nothing

        StateCorruptedRgit msg ->
            do
                hPutStrLn stderr $ "Aborting: [X] Corrupted remote -> " ++ msg
                pure Nothing

saveFetchedBundle :: Remote -> Maybe FilePath -> IO ()
saveFetchedBundle _remote Nothing = pure ()
saveFetchedBundle remote (Just bPath) = do
    let fetchedPath = fromCwdPath (bundleCwdPath fetchedBundle)
    hadPrevious <- Dir.doesFileExist fetchedPath
    maybeOldHash <- if hadPrevious
        then Git.getHashFromBundle fetchedBundle
        else pure Nothing

    -- Copy FIRST, then read hash from the correct location
    copyFile bPath fetchedPath
    safeRemove bPath
    maybeNewHash <- Git.getHashFromBundle fetchedBundle

    -- Ensure the internal git remote "origin" points to the right URL.
    -- This is the git-internal remote used for fetching refs from bundles,
    -- NOT the user's configured bit remotes (those are in .bit/remotes/).
    -- This ensures "git fetch origin/main" works correctly for pull/merge.
    void $ Git.setupRemote (remoteUrl remote)

    -- Compare hashes
    case (maybeOldHash, maybeNewHash) of
        -- If we already had a fetched bundle and the hash is unchanged, be silent.
        -- Tests expect `bit fetch` to produce no output in this case.
        (Just oldHash, Just newHash) | oldHash == newHash -> pure ()
        (Just oldHash, Just newHash) -> do
            putStrLn "Scanning remote..."
            void $ Git.updateRemoteTrackingBranch fetchedBundle
            putStrLn $ "Updated: " ++ oldHash ++ " -> " ++ newHash
        (Nothing, Just newHash) -> do
            putStrLn "Scanning remote..."
            void $ Git.updateRemoteTrackingBranch fetchedBundle
            hPutStrLn stderr $ "From " ++ remoteName remote
            hPutStrLn stderr $ " * [new branch]      main       -> origin/main"
            putStrLn $ "Fetched: " ++ newHash
        _ -> hPutStrLn stderr "Warning: Could not extract hash from bundle."

    -- Only print completion if we actually printed something above.
    case (maybeOldHash, maybeNewHash) of
        (Just oldHash, Just newHash) | oldHash == newHash -> pure ()
        _ -> putStrLn "Fetch complete."
