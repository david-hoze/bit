{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Bit.Core.Helpers
    ( -- Types
      PullMode(..)
    , PullOptions(..)
    , defaultPullOptions
      -- Git helpers
    , AncestorQuery(..)
    , getLocalHeadE
    , checkIsAheadE
    , hasStagedChangesE
    , getRemoteType
    , getRemoteTargetType
    , isFilesystemRemote
    , checkFilesystemRemoteIsRepo
      -- Monadic helpers
    , withRemote
    , gitQuery
    , gitRaw
    , tell
    , tellErr
    , fileExistsE
    , createDirE
    , readFileE
    , writeFileAtomicE
    , copyFileE
      -- Utility functions
    , safeRemove
    , formatPathList
    , formatVerifyCounts
    , formatVerifiedRemoteFiles
    , printVerifyIssue
    , readFileMaybe
    , removeDirectoryRecursive
    , restoreCheckoutPaths
    , expandPathsToFiles
    ) where

import System.Directory (removeDirectory)
import qualified Bit.IO.Platform as Platform
import Bit.IO.Platform (copyFile, removeFile, createDirectoryIfMissing, listDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), normalise)
import Control.Monad (when, unless, forM_)
import System.Exit (ExitCode(..), exitWith)
import Bit.Git.Run (AncestorQuery(..))
import qualified Bit.Git.Run as Git
import qualified Bit.Device.Identity as Device
import Bit.Remote (Remote, remoteName, RemotePath(..))
import Data.List (isPrefixOf, foldl')
import System.IO (stderr, hPutStrLn)
import Bit.Types (BitM, BitEnv(..), unPath)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Bit.Utils (toPosix, atomicWriteFileStr, trimGitOutput, formatBytes)
import qualified Bit.Scan.Verify as Verify
import qualified Bit.Scan.Local as Scan
-- Strict IO imports to avoid Windows file locking issues
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')

-- ============================================================================
-- Types
-- ============================================================================

-- | How to handle the merge during pull.
data PullMode
    = PullNormal        -- ^ Normal merge (fast-forward or three-way)
    | PullAcceptRemote  -- ^ Force-checkout remote branch
    | PullManualMerge   -- ^ Interactive per-file conflict resolution
    deriving (Show, Eq)

newtype PullOptions = PullOptions
    { pullMode       :: PullMode
    } deriving (Show)

defaultPullOptions :: PullOptions
defaultPullOptions = PullOptions PullNormal

-- ============================================================================
-- Git helpers via effect layer
-- ============================================================================

getLocalHeadE :: IO (Maybe String)
getLocalHeadE = do
    (code, out, _) <- Git.runGitWithOutput ["rev-parse", "HEAD"]
    pure $ case code of
        ExitSuccess -> Just (trimGitOutput out)
        _ -> Nothing

-- | Check if aqDescendant is ahead of aqAncestor (i.e., aqAncestor is an ancestor of aqDescendant).
checkIsAheadE :: AncestorQuery -> IO Bool
checkIsAheadE (AncestorQuery ancestor descendant) =
    (\(code, _, _) -> code == ExitSuccess) <$>
    Git.runGitWithOutput ["merge-base", "--is-ancestor", ancestor, descendant]

hasStagedChangesE :: IO Bool
hasStagedChangesE =
    (\(code, _, _) -> code == ExitFailure 1) <$>
    Git.runGitWithOutput ["diff", "--cached", "--quiet"]

-- | Determine the remote type from a remote name.
-- Returns the RemoteType if the remote is configured, Nothing otherwise.
getRemoteType :: FilePath -> String -> IO (Maybe Device.RemoteType)
getRemoteType cwd remName = Device.readRemoteType cwd remName

-- | Determine the remote target type from a remote name (legacy, for backward compat).
-- Returns the RemoteTarget if the remote is configured, Nothing otherwise.
getRemoteTargetType :: FilePath -> String -> IO (Maybe Device.RemoteTarget)
getRemoteTargetType cwd remName = Device.readRemoteFile cwd remName

-- | True if the resolved remote is a filesystem (or device) remote; False for cloud or unconfigured.
isFilesystemRemote :: Remote -> BitM Bool
isFilesystemRemote remote = do
    cwd <- asks envCwd
    liftIO $ fmap (maybe False Device.isFilesystemType) (getRemoteType cwd (remoteName remote))

-- | Check if a filesystem path is a bit repository. Exits with error if not.
-- Used by filesystem push, pull, and fetch operations to validate the remote.
checkFilesystemRemoteIsRepo :: RemotePath -> IO ()
checkFilesystemRemoteIsRepo (RemotePath remotePath) = do
    let remoteBitDir = remotePath </> ".bit"
    remoteHasBit <- Platform.doesDirectoryExist remoteBitDir
    unless remoteHasBit $ do
        hPutStrLn stderr "error: Remote is not a bit repository."
        exitWith (ExitFailure 1)

-- ============================================================================
-- Compatibility helpers (effect system removal)
-- ============================================================================

tell :: String -> IO ()
tell = putStrLn

tellErr :: String -> IO ()
tellErr = hPutStrLn stderr

gitRaw :: [String] -> IO ExitCode
gitRaw = Git.runGitRaw

gitQuery :: [String] -> IO (ExitCode, String, String)
gitQuery = Git.runGitWithOutput

readFileE :: FilePath -> IO (Maybe String)
readFileE = readFileMaybe

writeFileAtomicE :: FilePath -> String -> IO ()
writeFileAtomicE = atomicWriteFileStr

copyFileE :: FilePath -> FilePath -> IO ()
copyFileE = copyFile

fileExistsE :: FilePath -> IO Bool
fileExistsE = doesFileExist

createDirE :: FilePath -> IO ()
createDirE = createDirectoryIfMissing True

-- | Run an action with the remote, or print error if not configured.
withRemote :: (Remote -> BitM ()) -> BitM ()
withRemote action = do
  mRemote <- asks envRemote
  case mRemote of
    Nothing -> liftIO $ do
        hPutStrLn stderr "fatal: No upstream configured and no remote specified."
        hPutStrLn stderr "hint: bit push <remote>"
        hPutStrLn stderr "hint: bit push -u <remote>    (to set default upstream)"
        exitWith (ExitFailure 1)
    Just remote -> action remote

-- ============================================================================
-- Utility functions
-- ============================================================================

safeRemove :: FilePath -> IO ()
safeRemove filePath = do
    exists <- doesFileExist filePath
    when exists (removeFile filePath)

formatPathList :: [FilePath] -> [String]
formatPathList paths
  | length paths <= 20 = map (\p -> "        " ++ toPosix p) paths
  | otherwise         = map (\p -> "        " ++ toPosix p) (take 10 paths)
                        ++ ["        ... and " ++ show (length paths - 10) ++ " more"]

-- | Format "N files checked, M issues" with leading spaces (for verify/repair output).
formatVerifyCounts :: Int -> Int -> String
formatVerifyCounts count numIssues = "  " ++ show count ++ " files checked, " ++ show numIssues ++ " issues"

-- | Format "Verified N remote files." for pull verify success message.
formatVerifiedRemoteFiles :: Int -> String
formatVerifiedRemoteFiles n = "Verified " ++ show n ++ " remote files."

printVerifyIssue :: (String -> String) -> Verify.VerifyIssue -> IO ()
printVerifyIssue fmtHash = \case
  Verify.HashMismatch filePath expectedHash actualHash expectedSize actualSize
    | expectedHash == "(committed)" ->
        hPutStrLn stderr $ "[ERROR] Text file modified: " ++ toPosix (unPath filePath)
    | expectedHash == actualHash -> do
        hPutStrLn stderr $ "[ERROR] Size mismatch: " ++ toPosix (unPath filePath)
        hPutStrLn stderr $ "  Expected size: " ++ formatBytes expectedSize
        hPutStrLn stderr $ "  Actual size:   " ++ formatBytes actualSize
    | otherwise -> do
        hPutStrLn stderr $ "[ERROR] Metadata mismatch: " ++ toPosix (unPath filePath)
        hPutStrLn stderr $ "  Expected: " ++ fmtHash expectedHash
        hPutStrLn stderr $ "  Actual:   " ++ fmtHash actualHash
  Verify.Missing filePath ->
    hPutStrLn stderr $ "[ERROR] Missing: " ++ toPosix (unPath filePath)

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe filePath = do
    exists <- doesFileExist filePath
    if exists
        then do
            bs <- BS.readFile filePath
            pure $ either (const Nothing) (Just . T.unpack) (decodeUtf8' bs)
        else pure Nothing

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive dir = do
    exists <- doesDirectoryExist dir
    when exists $ do
        contents <- listDirectory dir
        forM_ contents $ \item -> do
            let itemPath = dir </> item
            isDir <- doesDirectoryExist itemPath
            if isDir
                then removeDirectoryRecursive itemPath
                else removeFile itemPath
        removeDirectory dir

restoreCheckoutPaths :: [String] -> [String]
restoreCheckoutPaths args =
    let restoreFlags = ["--staged", "-S", "--worktree", "-W",
                        "--patch", "-p", "--quiet", "-q",
                        "--ours", "--theirs", "--merge", "-m",
                        "--pathspec-file-nul", "--overlay", "--no-overlay",
                        "--ignore-unmerged", "--recurse-submodules", "--no-recurse-submodules"]
        isFlag arg = arg `elem` restoreFlags ||
                     arg == "--" ||
                     "--source=" `isPrefixOf` arg ||
                     "-s" `isPrefixOf` arg ||
                     "--pathspec-from-file=" `isPrefixOf` arg ||
                     "--conflict=" `isPrefixOf` arg ||
                     "--inter-hunk-context=" `isPrefixOf` arg ||
                     "--unified=" `isPrefixOf` arg ||
                     "-U" `isPrefixOf` arg
        (_, paths) = foldl' (\(afterDash, acc) arg ->
            if | arg == "--" -> (True, acc)
               | afterDash   -> (True, arg:acc)
               | isFlag arg  -> (False, acc)
               | otherwise   -> (False, arg:acc)
            ) (False, []) args
    in reverse paths

expandPathsToFiles :: FilePath -> [String] -> IO [FilePath]
expandPathsToFiles cwd paths = do
    -- Resolve bitDir to get correct index path (supports bitlinks)
    let dotBit = cwd </> ".bit"
    bitDir <- do
        isDir <- doesDirectoryExist dotBit
        if isDir then pure dotBit
        else do
            isFile <- doesFileExist dotBit
            if isFile then do
                bs <- BS.readFile dotBit
                let content = either (const "") T.unpack (decodeUtf8' bs)
                case lines content of
                    (firstLine:_) -> pure (drop 8 (filter (/= '\r') firstLine))
                    [] -> pure dotBit
            else pure dotBit
    let indexRoot = bitDir </> "index"
    allFiles <- Scan.listMetadataPaths indexRoot
    pure $ concatMap (\p ->
        if p == "." || p == "./"
        then allFiles
        else let p' = normalise p
                 pPrefix = p' ++ "/"
                 matches = filter (\f -> let f' = normalise f
                                         in f' == p' || pPrefix `isPrefixOf` (f' ++ "/")) allFiles
             in if null matches then [p] else matches
        ) paths
