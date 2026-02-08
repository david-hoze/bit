{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bit.RemoteWorkspace
  ( RemoteWorkspace(..)
  , initRemoteWorkspace
  , remoteWorkspacePath
  ) where

import Bit.Types (FileEntry(..), EntryKind(..), ContentType(..))
import Bit.Remote (Remote, remoteUrl)
import qualified Bit.Remote.Scan as Remote.Scan
import Bit.Scan (hashAndClassifyFile, binaryExtensions)
import qualified Internal.ConfigFile as ConfigFile
import Internal.ConfigFile (TextConfig)
import qualified Internal.Transport as Transport
import Bit.Utils (isBitPath, atomicWriteFileStr)
import Bit.Internal.Metadata (MetaContent(..), serializeMetadata)
import System.FilePath ((</>), takeExtension, takeDirectory)
import System.Directory 
    ( doesDirectoryExist
    , createDirectoryIfMissing
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)
import Control.Monad (when, forM, forM_)
import Data.Char (toLower)
import Data.List (partition)
import Control.Exception (catch, SomeException)

-- | A remote workspace contains metadata and git repo for a remote
data RemoteWorkspace = RemoteWorkspace
  { wsPath :: FilePath
  , wsRemote :: Remote
  } deriving (Show)

-- | Path to the remote workspace for a given remote name
remoteWorkspacePath :: FilePath -> String -> FilePath
remoteWorkspacePath cwd remName = cwd </> ".bit" </> "remote-workspaces" </> remName

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
            || map toLower (takeExtension (path fe)) `elem` binaryExtensions
        _ -> True  -- directories are not text candidates

-- | Download text candidate files from remote, classify them, return updated FileEntries.
classifyRemoteTextCandidates :: Remote -> TextConfig -> [FileEntry] -> IO [FileEntry]
classifyRemoteTextCandidates remote config candidates = do
    -- Create temp directory for downloads
    tempDir <- getTemporaryDirectory >>= \t -> do
        let d = t </> "bit-classify-remote"
        createDirectoryIfMissing True d
        pure d

    -- Download and classify each candidate file
    classifiedEntries <- forM candidates $ \fe -> do
        let remotePath = path fe  -- Already normalized by rclone
        let localPath = tempDir </> path fe
        createDirectoryIfMissing True (takeDirectory localPath)

        -- Download via rclone
        code <- Transport.copyFromRemote remote remotePath localPath
        case code of
            ExitSuccess -> do
                -- Classify using existing function
                case kind fe of
                    File{fSize} -> do
                        (h, isText) <- hashAndClassifyFile localPath fSize config
                        pure fe { kind = File { fHash = h, fSize = fSize, fContentType = if isText then TextContent else BinaryContent } }
                    _ -> pure fe
            _ -> do
                -- Download failed — treat as binary, keep rclone hash
                hPutStrLn stderr $ "Warning: Could not download " ++ path fe ++ " for classification, treating as binary."
                pure fe

    -- Cleanup temp dir
    removeDirectoryRecursive tempDir `catch` (\(_ :: SomeException) -> pure ())

    pure classifiedEntries

-- | Initialize a remote workspace by scanning the remote and building metadata
initRemoteWorkspace :: FilePath -> Remote -> String -> IO ()
initRemoteWorkspace cwd remote remName = do
    let wsPath = remoteWorkspacePath cwd remName
    let wsGit = wsPath </> ".git"

    -- Check if workspace already exists
    exists <- doesDirectoryExist wsGit
    when exists $ do
        hPutStrLn stderr $ "Remote workspace '" ++ remName ++ "' already exists."
        hPutStrLn stderr $ "Use 'bit @" ++ remName ++ " status' to see its state, or delete .bit/remote-workspaces/" ++ remName ++ "/ to start over."
        exitWith (ExitFailure 1)

    putStrLn $ "Scanning remote '" ++ remName ++ "' (" ++ remoteUrl remote ++ ")..."

    -- Step 1: Fetch file list from remote
    result <- Remote.Scan.fetchRemoteFiles remote
    case result of
        Left err -> do
            hPutStrLn stderr $ "Error scanning remote: " ++ show err
            exitWith (ExitFailure 1)
        Right remoteFiles -> do
            let files = filter (not . isBitPath . path) remoteFiles
            putStrLn $ "Found " ++ show (length files) ++ " files on remote."

            -- Step 2: Partition into binary (certain) and text candidates
            config <- ConfigFile.readTextConfig
            let (definitelyBinary, textCandidates) = partitionFiles config files

            putStrLn $ "  " ++ show (length definitelyBinary) ++ " binary files (by size/extension)"
            putStrLn $ "  " ++ show (length textCandidates) ++ " small files to classify..."

            -- Step 3: Download text candidates to temp dir and classify
            classifiedFiles <- if null textCandidates
                then pure []
                else classifyRemoteTextCandidates remote config textCandidates

            -- Step 4: Merge results
            let allFiles = definitelyBinary ++ classifiedFiles

            -- Step 5: Create workspace and write metadata
            putStrLn "Building metadata workspace..."
            createDirectoryIfMissing True wsPath
            
            -- For text files: we need the actual content, but we just classified from remote
            -- For binary files: we just need metadata (hash+size)
            -- Split the files accordingly
            let (textFiles, binaryFiles) = partition isTextFile allFiles
                  where
                    isTextFile fe = case kind fe of
                        File{fContentType = TextContent} -> True
                        _ -> False
            
            -- For text files from the classified set: download them again to the workspace
            -- (we could optimize by keeping the temp files, but this is simpler and more robust)
            putStrLn $ "Downloading " ++ show (length textFiles) ++ " text files..."
            forM_ textFiles $ \fe -> do
                let localPath = wsPath </> path fe
                createDirectoryIfMissing True (takeDirectory localPath)
                code <- Transport.copyFromRemote remote (path fe) localPath
                when (code /= ExitSuccess) $
                    hPutStrLn stderr $ "Warning: Failed to download text file " ++ path fe

            -- For binary files: write metadata
            forM_ binaryFiles $ \fe -> do
                let metaPath = wsPath </> path fe
                createDirectoryIfMissing True (takeDirectory metaPath)
                case kind fe of
                    File{fHash, fSize} ->
                        atomicWriteFileStr metaPath (serializeMetadata (MetaContent fHash fSize))
                    _ -> pure ()

            -- Step 6: Initialize git repo in workspace
            putStrLn "Initializing git repository..."
            callProcess "git" ["-C", wsPath, "init", "--initial-branch=main"]
            callProcess "git" ["-C", wsPath, "config", "core.quotePath", "false"]

            let textCount = length textFiles
            let binCount = length binaryFiles
            putStrLn $ "Remote workspace initialized: " ++ show textCount ++ " text, " ++ show binCount ++ " binary files."
            putStrLn $ "Next steps:"
            putStrLn $ "  bit @" ++ remName ++ " add ."
            putStrLn $ "  bit @" ++ remName ++ " commit -m \"Initial commit\""
