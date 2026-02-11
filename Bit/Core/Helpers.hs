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
    , getLocalHeadE
    , checkIsAheadE
    , hasStagedChangesE
    , getRemoteType
    , getRemoteTargetType
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
    , printVerifyIssue
    , readFileMaybe
    , removeDirectoryRecursive
    , restoreCheckoutPaths
    , expandPathsToFiles
    ) where

import qualified System.Directory as Dir
import System.Directory (copyFile, removeFile, createDirectoryIfMissing, removeDirectory, listDirectory, doesDirectoryExist)
import System.FilePath ((</>), normalise)
import Control.Monad (when, unless, forM_)
import System.Exit (ExitCode(..), exitWith)
import qualified Internal.Git as Git
import qualified Bit.Device as Device
import Bit.Remote (Remote)
import Data.List (isPrefixOf, foldl')
import System.IO (stderr, hPutStrLn)
import Bit.Types (BitM, BitEnv(..), unPath)
import Control.Monad.Trans.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Bit.Utils (toPosix, atomicWriteFileStr, trimGitOutput)
import qualified Bit.Verify as Verify
import qualified Bit.Scan as Scan
import Internal.Config (bitIndexPath)
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

-- | Check if @localHash@ is ahead of @remoteHash@ (i.e., remote is an ancestor of local).
-- Parameter order: remote hash first, local hash second — matching @git merge-base --is-ancestor@.
checkIsAheadE :: String -> String -> IO Bool
checkIsAheadE remoteHash localHash =
    (\(code, _, _) -> code == ExitSuccess) <$>
    Git.runGitWithOutput ["merge-base", "--is-ancestor", remoteHash, localHash]

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

-- | Check if a filesystem path is a bit repository. Exits with error if not.
-- Used by filesystem push, pull, and fetch operations to validate the remote.
checkFilesystemRemoteIsRepo :: FilePath -> IO ()
checkFilesystemRemoteIsRepo remotePath = do
    let remoteBitDir = remotePath </> ".bit"
    remoteHasBit <- Dir.doesDirectoryExist remoteBitDir
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
fileExistsE = Dir.doesFileExist

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
    exists <- Dir.doesFileExist filePath
    when exists (Dir.removeFile filePath)

formatPathList :: [FilePath] -> [String]
formatPathList paths
  | length paths <= 20 = map (\p -> "        " ++ toPosix p) paths
  | otherwise         = map (\p -> "        " ++ toPosix p) (take 10 paths)
                        ++ ["        ... and " ++ show (length paths - 10) ++ " more"]

printVerifyIssue :: (String -> String) -> Verify.VerifyIssue -> IO ()
printVerifyIssue fmtHash = \case
  Verify.HashMismatch filePath expectedHash actualHash _expectedSize _actualSize -> do
    hPutStrLn stderr $ "[ERROR] Hash mismatch: " ++ toPosix (unPath filePath)
    hPutStrLn stderr $ "  Expected: " ++ fmtHash expectedHash
    hPutStrLn stderr $ "  Actual:   " ++ fmtHash actualHash
  Verify.Missing filePath ->
    hPutStrLn stderr $ "[ERROR] Missing: " ++ toPosix (unPath filePath)

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe filePath = do
    exists <- Dir.doesFileExist filePath
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
    let indexRoot = cwd </> bitIndexPath
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
