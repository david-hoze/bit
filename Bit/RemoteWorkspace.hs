{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bit.RemoteWorkspace
  ( initRemote
  , addRemote
  , commitRemote
  , statusRemote
  , logRemote
  , lsFilesRemote
  ) where

import Bit.Types (FileEntry(..), EntryKind(..), ContentType(..), Path(..))
import Bit.Remote (Remote)
import qualified Bit.Remote.Scan as Remote.Scan
import Bit.Scan (hashAndClassifyFile, binaryExtensions)
import qualified Internal.ConfigFile as ConfigFile
import Internal.ConfigFile (TextConfig)
import qualified Internal.Transport as Transport
import Bit.Utils (isBitPath, atomicWriteFileStr, trimGitOutput)
import Bit.Internal.Metadata (MetaContent(..), serializeMetadata)
import System.FilePath ((</>), takeExtension, takeDirectory)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , getTemporaryDirectory
    , listDirectory
    , removeDirectoryRecursive
    , removeFile
    )
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr, stdout, hFlush)
import qualified Internal.Git as Git
import Control.Monad (when, unless, forM, forM_, void)
import Data.Char (toLower)
import Data.List (partition)
import Control.Exception (SomeException, bracket, catch)

----------------------------------------------------------------------
-- Temp directory management
----------------------------------------------------------------------

-- | Create a temp directory and run an action in it.
-- Cleans up any leftover from a previous run, then cleans up on exit.
-- Exception-safe via bracket.
withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name action = bracket setup cleanup action
  where
    setup = do
        sysTemp <- getTemporaryDirectory
        let dir = sysTemp </> name
        -- Remove leftover from a previous run (e.g. if cleanup failed)
        removeDirectoryRecursive dir `catch` \(_ :: SomeException) -> pure ()
        createDirectoryIfMissing True dir
        pure dir
    cleanup dir =
        removeDirectoryRecursive dir `catch` \(_ :: SomeException) -> pure ()

----------------------------------------------------------------------
-- Git helpers
----------------------------------------------------------------------

-- | Run a git command at a given path; exit with error on failure.
runOrDie :: FilePath -> [String] -> String -> IO ()
runOrDie dir args desc = do
    (code, _, err) <- Git.runGitAt dir args
    when (code /= ExitSuccess) $ do
        hPutStrLn stderr $ "fatal: failed to " ++ desc ++ "."
        unless (null err) $ hPutStrLn stderr err
        exitWith (ExitFailure 1)

----------------------------------------------------------------------
-- Bundle operations
----------------------------------------------------------------------

-- | Inflate a git bundle into a workspace directory.
-- Uses init + fetch into tracking refs + checkout to avoid:
--   1. "refusing to fetch into checked out branch" (init+fetch into heads)
--   2. "directory already exists" on Windows (git clone)
inflateBundle :: FilePath -> FilePath -> IO ()
inflateBundle bundlePath wsPath = do
    createDirectoryIfMissing True wsPath
    runOrDie wsPath ["init", "--initial-branch=main"] "initialize workspace"
    void $ Git.runGitAt wsPath ["config", "core.quotePath", "false"]
    -- Fetch into remote tracking refs (not refs/heads/*) to avoid conflict
    -- with the checked-out main branch
    runOrDie wsPath ["fetch", bundlePath, "+refs/heads/*:refs/remotes/bundle/*"] "fetch from bundle"
    -- Use reset --hard to update the branch pointer, index, AND working tree.
    -- (checkout -B can skip the working tree update when already on the same branch)
    runOrDie wsPath ["reset", "--hard", "refs/remotes/bundle/main"] "checkout main branch"

-- | Create a bundle from workspace and push to remote.
bundleAndPush :: FilePath -> Remote -> FilePath -> IO ()
bundleAndPush wsPath remote tmpBase = do
    let newBundle = tmpBase </> "new.bundle"
    putStrLn "Creating metadata bundle..."
    (bCode, _, _) <- Git.runGitAt wsPath ["bundle", "create", newBundle, "--all"]
    when (bCode /= ExitSuccess) $ do
        hPutStrLn stderr "fatal: failed to create bundle."
        exitWith (ExitFailure 1)
    putStrLn "Pushing bundle to remote..."
    pCode <- Transport.copyToRemote newBundle remote ".bit/bit.bundle"
    when (pCode /= ExitSuccess) $ do
        hPutStrLn stderr "fatal: failed to push bundle to remote."
        exitWith (ExitFailure 1)
    putStrLn "Changes pushed to remote."

----------------------------------------------------------------------
-- Ephemeral workspace patterns
----------------------------------------------------------------------

-- | Run an action in an ephemeral workspace inflated from the remote's bundle.
-- After the action succeeds and HEAD has changed, re-bundles and pushes.
-- Workspace is cleaned up on exit (exception-safe).
withRemoteWorkspace :: Remote -> (FilePath -> IO ExitCode) -> IO ExitCode
withRemoteWorkspace remote action = withTempDir "bit-remote-ws" $ \tmpBase -> do
    let bundlePath = tmpBase </> "bit.bundle"
        wsPath = tmpBase </> "workspace"
    result <- Transport.copyFromRemoteDetailed remote ".bit/bit.bundle" bundlePath
    case result of
        Transport.CopyNotFound -> do
            hPutStrLn stderr "fatal: no bit repository on remote. Run 'bit @remote init' first."
            pure (ExitFailure 1)
        Transport.CopyNetworkError err -> do
            hPutStrLn stderr $ "fatal: network error: " ++ err
            pure (ExitFailure 1)
        Transport.CopyOtherError err -> do
            hPutStrLn stderr $ "fatal: " ++ err
            pure (ExitFailure 1)
        Transport.CopySuccess -> do
            inflateBundle bundlePath wsPath
            -- Capture HEAD before action
            (_, oldHeadRaw, _) <- Git.runGitAt wsPath ["rev-parse", "HEAD"]
            let oldHead = trimGitOutput oldHeadRaw
            -- Run action
            code <- action wsPath
            case code of
                ExitSuccess -> do
                    (_, newHeadRaw, _) <- Git.runGitAt wsPath ["rev-parse", "HEAD"]
                    let newHead = trimGitOutput newHeadRaw
                    if oldHead == newHead
                        then pure ExitSuccess
                        else do
                            bundleAndPush wsPath remote tmpBase
                            pure ExitSuccess
                _ -> pure code

-- | Read-only: fetches and inflates but never pushes back.
-- Workspace is cleaned up on exit (exception-safe).
withRemoteWorkspaceReadOnly :: Remote -> (FilePath -> IO ExitCode) -> IO ExitCode
withRemoteWorkspaceReadOnly remote action = withTempDir "bit-remote-ro" $ \tmpBase -> do
    let bundlePath = tmpBase </> "bit.bundle"
        wsPath = tmpBase </> "workspace"
    result <- Transport.copyFromRemoteDetailed remote ".bit/bit.bundle" bundlePath
    case result of
        Transport.CopyNotFound -> do
            hPutStrLn stderr "fatal: no bit repository on remote. Run 'bit @remote init' first."
            pure (ExitFailure 1)
        Transport.CopyNetworkError err -> do
            hPutStrLn stderr $ "fatal: network error: " ++ err
            pure (ExitFailure 1)
        Transport.CopyOtherError err -> do
            hPutStrLn stderr $ "fatal: " ++ err
            pure (ExitFailure 1)
        Transport.CopySuccess -> do
            inflateBundle bundlePath wsPath
            action wsPath

----------------------------------------------------------------------
-- File classification (preserved from old implementation)
----------------------------------------------------------------------

-- | Partition remote files into definitely-binary and text-candidates.
-- A file is definitely binary if:
--   1. Size >= textSizeLimit, OR
--   2. Extension is in binaryExtensions
-- Everything else is a text candidate (needs download + classification).
partitionFiles :: TextConfig -> [FileEntry] -> ([FileEntry], [FileEntry])
partitionFiles config = partition isBinary
  where
    isBinary fe = case kind fe of
        File{fSize} ->
            fSize >= ConfigFile.textSizeLimit config
            || map toLower (takeExtension (unPath (path fe))) `elem` binaryExtensions
        _ -> True

-- | Download text candidate files from remote, classify them.
classifyTextCandidates :: Remote -> TextConfig -> [FileEntry] -> IO [FileEntry]
classifyTextCandidates remote config candidates = do
    tempDir <- getTemporaryDirectory >>= \t -> do
        let d = t </> "bit-classify-remote"
        createDirectoryIfMissing True d
        pure d
    classifiedEntries <- forM candidates $ \fe -> do
        let remotePath = path fe
        let localPath = tempDir </> unPath (path fe)
        createDirectoryIfMissing True (takeDirectory localPath)
        code <- Transport.copyFromRemote remote (unPath remotePath) localPath
        case code of
            ExitSuccess ->
                case kind fe of
                    File{fSize} -> do
                        (h, contentType) <- hashAndClassifyFile localPath fSize config
                        pure fe { kind = File { fHash = h, fSize = fSize, fContentType = contentType } }
                    _ -> pure fe
            _ -> do
                hPutStrLn stderr $ "Warning: Could not download " ++ unPath (path fe) ++ " for classification, treating as binary."
                pure fe
    removeDirectoryRecursive tempDir `catch` (\(_ :: SomeException) -> pure ())
    pure classifiedEntries

----------------------------------------------------------------------
-- Workspace operations
----------------------------------------------------------------------

-- | Remove all files from workspace (keeping .git directory).
-- This ensures the workspace reflects the current remote state exactly,
-- so git can detect additions, modifications, AND deletions correctly.
clearWorkspace :: FilePath -> IO ()
clearWorkspace wsPath = do
    contents <- listDirectory wsPath
    forM_ contents $ \item ->
        when (item /= ".git") $ do
            let itemPath = wsPath </> item
            isDir <- doesDirectoryExist itemPath
            if isDir
                then removeDirectoryRecursive itemPath
                else removeFile itemPath

-- | Write metadata files into workspace for all classified remote files.
-- Text files: download actual content from remote.
-- Binary files: write hash+size metadata.
writeFilesToWorkspace :: FilePath -> Remote -> [FileEntry] -> IO ()
writeFilesToWorkspace wsPath remote allFiles = do
    let (textFiles, binaryFiles) = partition isTextFile allFiles
    -- Download text file content from remote
    unless (null textFiles) $ do
        putStrLn $ "Downloading " ++ show (length textFiles) ++ " text files..."
        forM_ textFiles $ \fe -> do
            let localPath = wsPath </> unPath (path fe)
            createDirectoryIfMissing True (takeDirectory localPath)
            code <- Transport.copyFromRemote remote (unPath (path fe)) localPath
            when (code /= ExitSuccess) $
                hPutStrLn stderr $ "Warning: Failed to download text file " ++ unPath (path fe)
    -- Write hash+size metadata for binary files
    forM_ binaryFiles $ \fe -> do
        let metaPath = wsPath </> unPath (path fe)
        createDirectoryIfMissing True (takeDirectory metaPath)
        case kind fe of
            File{fHash, fSize} ->
                atomicWriteFileStr metaPath (serializeMetadata (MetaContent fHash fSize))
            _ -> pure ()
  where
    isTextFile fe = case kind fe of
        File{fContentType = TextContent} -> True
        _ -> False

-- | Scan the remote, classify files, and write metadata into the workspace.
-- Returns True on success, False on failure (errors printed to stderr).
scanAndWriteMetadata :: Remote -> FilePath -> IO Bool
scanAndWriteMetadata remote wsPath = do
    putStrLn "Scanning remote..."
    scanResult <- Remote.Scan.fetchRemoteFiles remote
    case scanResult of
        Left err -> do
            hPutStrLn stderr $ "fatal: error scanning remote: " ++ show err
            pure False
        Right remoteFiles -> do
            let files = filter (not . isBitPath . unPath . path) remoteFiles
            putStrLn $ "Found " ++ show (length files) ++ " files on remote."
            config <- ConfigFile.readTextConfig
            let (binary, textCands) = partitionFiles config files
            putStrLn $ "  " ++ show (length binary) ++ " binary files (by size/extension)"
            putStrLn $ "  " ++ show (length textCands) ++ " small files to classify..."
            classifiedFiles <- if null textCands
                then pure []
                else classifyTextCandidates remote config textCands
            let allFiles = binary ++ classifiedFiles
            clearWorkspace wsPath
            writeFilesToWorkspace wsPath remote allFiles
            pure True

-- | Stage and auto-commit changes in the workspace.
stageAndCommit :: FilePath -> [String] -> IO ExitCode
stageAndCommit wsPath paths = do
    let addPaths = if null paths then ["."] else paths
    (addCode, _, _) <- Git.runGitAt wsPath ("add" : addPaths)
    case addCode of
        ExitSuccess -> do
            (diffCode, _, _) <- Git.runGitAt wsPath ["diff", "--cached", "--quiet"]
            case diffCode of
                ExitFailure 1 -> do
                    (commitCode, _, _) <- Git.runGitAt wsPath ["commit", "-m", "Update remote metadata"]
                    case commitCode of
                        ExitSuccess -> do
                            putStrLn "Remote metadata updated."
                            pure ExitSuccess
                        _ -> do
                            hPutStrLn stderr "error: failed to commit metadata."
                            pure commitCode
                _ -> do
                    putStrLn "Nothing to add — remote metadata is up to date."
                    pure ExitSuccess
        _ -> do
            hPutStrLn stderr "error: failed to stage files."
            pure addCode

----------------------------------------------------------------------
-- Public API: Remote commands
----------------------------------------------------------------------

-- | Initialize a remote repository (create empty bundle and push).
-- Does NOT use withRemoteWorkspace — there's no bundle to fetch yet.
initRemote :: Remote -> String -> IO ()
initRemote remote remoteName = withTempDir "bit-remote-init" $ \tmpBase -> do
    let checkPath = tmpBase </> "check.bundle"
        wsPath = tmpBase </> "workspace"
    -- Check if already initialized
    result <- Transport.copyFromRemoteDetailed remote ".bit/bit.bundle" checkPath
    case result of
        Transport.CopySuccess -> do
            hPutStrLn stderr "fatal: remote already has a bit repository."
            exitWith (ExitFailure 1)
        Transport.CopyNetworkError err -> do
            hPutStrLn stderr $ "fatal: network error: " ++ err
            exitWith (ExitFailure 1)
        Transport.CopyOtherError err -> do
            hPutStrLn stderr $ "fatal: " ++ err
            exitWith (ExitFailure 1)
        Transport.CopyNotFound -> do
            -- Create fresh repo
            createDirectoryIfMissing True wsPath
            runOrDie wsPath ["init", "--initial-branch=main"] "initialize repository"
            void $ Git.runGitAt wsPath ["config", "core.quotePath", "false"]
            runOrDie wsPath ["commit", "--allow-empty", "-m", "Initial remote repository"] "create initial commit"
            -- Bundle and push
            let bundlePath = tmpBase </> "init.bundle"
            (bCode, _, _) <- Git.runGitAt wsPath ["bundle", "create", bundlePath, "--all"]
            when (bCode /= ExitSuccess) $ do
                hPutStrLn stderr "fatal: failed to create bundle."
                exitWith (ExitFailure 1)
            pCode <- Transport.copyToRemote bundlePath remote ".bit/bit.bundle"
            case pCode of
                ExitSuccess ->
                    putStrLn $ "Initialized bit repository on remote '" ++ remoteName ++ "'."
                _ -> do
                    hPutStrLn stderr "fatal: failed to push bundle to remote."
                    exitWith (ExitFailure 1)

-- | Add files in remote workspace (scan + stage + auto-commit).
-- Scans the full remote to reconstruct metadata, writes it into the
-- ephemeral workspace, stages the specified paths, and auto-commits.
addRemote :: Remote -> [String] -> IO ExitCode
addRemote remote paths =
    withRemoteWorkspace remote $ \wsPath -> do
        ok <- scanAndWriteMetadata remote wsPath
        if ok
            then stageAndCommit wsPath paths
            else pure (ExitFailure 1)

-- | Commit in remote workspace (interactive, passes through stdio).
-- Useful for amending the last commit message or creating additional commits.
commitRemote :: Remote -> [String] -> IO ExitCode
commitRemote remote commitArgs =
    withRemoteWorkspace remote $ \wsPath ->
        Git.runGitRawAt wsPath ("commit" : commitArgs)

-- | Show status of remote workspace (read-only).
-- Scans the remote to detect untracked files, then runs git status.
statusRemote :: Remote -> [String] -> IO ExitCode
statusRemote remote rest =
    withRemoteWorkspaceReadOnly remote $ \wsPath -> do
        -- Scan remote and write metadata so git can detect untracked files
        ok <- scanAndWriteMetadata remote wsPath
        hFlush stdout  -- Ensure scan output appears before git status
        if ok
            then do
                -- Update index to match working tree after writing files
                -- This ensures stat info matches and content-identical files show as clean
                void $ Git.runGitAt wsPath ["add", "-u"]
                void $ Git.runGitAt wsPath ["reset", "HEAD"]
                Git.runGitRawAt wsPath ("status" : rest)
            else pure (ExitFailure 1)

-- | Show log of remote workspace (read-only).
logRemote :: Remote -> [String] -> IO ExitCode
logRemote remote rest =
    withRemoteWorkspaceReadOnly remote $ \wsPath ->
        Git.runGitRawAt wsPath ("log" : "--decorate-refs=refs/heads/" : rest)

-- | List tracked files in remote workspace (read-only).
-- Scans the remote to reconstruct metadata, then runs git ls-files.
lsFilesRemote :: Remote -> [String] -> IO ExitCode
lsFilesRemote remote rest =
    withRemoteWorkspaceReadOnly remote $ \wsPath -> do
        ok <- scanAndWriteMetadata remote wsPath
        hFlush stdout
        if ok
            then do
                void $ Git.runGitAt wsPath ["add", "-u"]
                void $ Git.runGitAt wsPath ["reset", "HEAD"]
                Git.runGitRawAt wsPath ("ls-files" : rest)
            else pure (ExitFailure 1)
