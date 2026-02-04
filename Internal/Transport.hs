{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Internal.Transport
    ( copyToRemote
    , copyFromRemote
    , copyFromRemoteDetailed
    , CopyResult(..)
    , moveRemote
    , deleteRemote
    , purgeRemote
    , mkdirRemote
    , listRemoteJson
    , listRemoteJsonWithHash
    , listRemoteItems
    , TransportItem(..)
    , checkRemote
    , CheckResult(..)
    ) where

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (removeFile, doesFileExist)
import System.IO (hPutStrLn, stderr)
import System.FilePath (normalise)
import Control.Monad (when)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Bit.Remote (Remote, remoteUrl)

-- | Build a full remote path from Remote + relative path.
-- Handles trailing-slash normalization internally.
remoteFilePath :: Remote -> FilePath -> String
remoteFilePath remote relPath =
    let base = remoteUrl remote
        -- Ensure exactly one separator between base and relative path
        base' = if not (null base) && last base == '/' then init base else base
    in base' ++ "/" ++ relPath

-- Dumb transport-level data types
data TransportItem = TransportItem
    { tiName  :: String
    , tiIsDir :: Bool
    } deriving (Show, Eq)

-- Internal type for JSON parsing
data RcloneItem = RcloneItem 
    { name   :: String
    , isDir  :: Bool 
    } deriving (Show, Generic)

-- We manually map lowercase Haskell fields to Capitalized JSON keys
instance Aeson.FromJSON RcloneItem where
    parseJSON = Aeson.withObject "RcloneItem" $ \v -> RcloneItem
        <$> v Aeson..: fromString "Name"
        <*> v Aeson..: fromString "IsDir"

-- Detailed copy result for domain-level error handling
data CopyResult 
    = CopySuccess 
    | CopyNotFound 
    | CopyNetworkError String 
    | CopyOtherError String
    deriving (Show, Eq)

-- | Result of an rclone check operation (--combined output).
data CheckResult = CheckResult
    { checkMatches     :: [FilePath]  -- '=' lines: identical on both sides
    , checkDiffers     :: [FilePath]  -- '*' lines: present on both but different
    , checkMissingDest :: [FilePath]  -- '+' lines: local only, not on remote
    , checkMissingSrc  :: [FilePath]  -- '-' lines: remote only, not on local
    , checkErrors      :: [FilePath]  -- '!' lines: error reading/hashing
    , checkExitCode    :: ExitCode
    , checkRawOutput   :: String      -- raw --combined output for .rgit/last-check.txt
    , checkStderr      :: String      -- stderr (for network/auth error messages)
    } deriving (Show)

-- Low-level rclone operations

-- | Copy local file to remote at relative path
copyToRemote :: FilePath -> Remote -> FilePath -> IO ExitCode
copyToRemote localPath remote relPath = do
    let fullRemote = remoteFilePath remote relPath
    (code, _, _) <- readProcessWithExitCode "rclone" ["copyto", localPath, fullRemote] ""
    return code

-- | Copy file from remote (relative path) to local
copyFromRemote :: Remote -> FilePath -> FilePath -> IO ExitCode
copyFromRemote remote relPath localPath = do
    let fullRemote = remoteFilePath remote relPath
    (code, _, _) <- readProcessWithExitCode "rclone" ["copyto", fullRemote, localPath] ""
    return code

-- | Copy from remote with detailed error classification
copyFromRemoteDetailed :: Remote -> FilePath -> FilePath -> IO CopyResult
copyFromRemoteDetailed remote relPath localPath = do
    let fullRemote = remoteFilePath remote relPath
    (code, _, err) <- readProcessWithExitCode "rclone" ["copyto", fullRemote, localPath] ""
    case code of
        ExitSuccess -> return CopySuccess
        ExitFailure _ 
            | "directory not found" `isInfixOf` err || "object not found" `isInfixOf` err -> return CopyNotFound
            | "no such host" `isInfixOf` err || "dial tcp" `isInfixOf` err -> return (CopyNetworkError err)
            | otherwise -> return (CopyOtherError err)

-- | Move a file on remote (both paths relative to remote root)
moveRemote :: Remote -> FilePath -> FilePath -> IO ExitCode
moveRemote remote srcRel destRel = do
    let src = remoteFilePath remote srcRel
        dest = remoteFilePath remote destRel
    (code, _, _) <- readProcessWithExitCode "rclone" ["moveto", src, dest] ""
    return code

-- | Delete a file on remote (relative path)
deleteRemote :: Remote -> FilePath -> IO ExitCode
deleteRemote remote relPath = do
    let fullRemote = remoteFilePath remote relPath
    (code, _, _) <- readProcessWithExitCode "rclone" ["deletefile", fullRemote] ""
    return code

-- | Purge entire remote (no relative path — purges the remote root)
purgeRemote :: Remote -> IO ExitCode
purgeRemote remote = do
    let fullRemote = remoteUrl remote
    (code, _, _) <- readProcessWithExitCode "rclone" ["purge", fullRemote] ""
    return code

-- | Create directory on remote (relative path)
mkdirRemote :: Remote -> FilePath -> IO ExitCode
mkdirRemote remote relPath = do
    let fullRemote = remoteFilePath remote relPath
    (code, _, _) <- readProcessWithExitCode "rclone" ["mkdir", fullRemote] ""
    return code

-- | List remote directory as JSON (at remote root)
listRemoteJson :: Remote -> Int -> IO (ExitCode, String, String)
listRemoteJson remote maxDepth =
    readProcessWithExitCode "rclone" ["lsjson", "--max-depth", show maxDepth, remoteUrl remote] ""

-- | List remote directory items (at remote root, parsed)
listRemoteItems :: Remote -> Int -> IO (Either String [TransportItem])
listRemoteItems remote maxDepth = do
    (code, out, err) <- listRemoteJson remote maxDepth
    case code of
        ExitFailure _ -> 
            if "directory not found" `isInfixOf` err 
            then return (Right [])  -- Empty directory
            else return (Left err)  -- Network or other error
        ExitSuccess -> do
            case Aeson.decode (LBS.pack out) :: Maybe [RcloneItem] of
                Nothing -> return (Left "Failed to parse rclone JSON output")
                Just items -> return (Right [TransportItem (name item) (isDir item) | item <- items])

-- | List remote recursively with hashes
listRemoteJsonWithHash :: Remote -> IO (ExitCode, String, String)
listRemoteJsonWithHash remote =
    readProcessWithExitCode "rclone" ["lsjson", remoteUrl remote, "--hash", "--recursive"] ""

-- | Check local against remote
checkRemote :: FilePath -> Remote -> IO CheckResult
checkRemote localPath remote = do
    let args = [ "check"
               , localPath
               , remoteUrl remote
               , "--combined", "-"
               , "--exclude", ".bit/**"
               ]
    (code, out, err) <- readProcessWithExitCode "rclone" args ""
    let parsed = parseCombinedOutput out
    return CheckResult
        { checkMatches     = parsed '='
        , checkDiffers     = parsed '*'
        , checkMissingDest = parsed '+'
        , checkMissingSrc  = parsed '-'
        , checkErrors      = parsed '!'
        , checkExitCode    = code
        , checkRawOutput   = out
        , checkStderr      = err
        }
  where
    -- Parse "<symbol> <path>" lines; path is everything after first space.
    parseCombinedOutput :: String -> (Char -> [FilePath])
    parseCombinedOutput raw =
        let lines' = lines raw
            go sym = [ normalise path
                     | line <- lines'
                     , not (null line)
                     , let (symChar, path) = case span (/= ' ') line of
                               (s, ' ' : r) -> (s, r)
                               (s, _)       -> (s, "")
                     , not (null symChar)
                     , head symChar == sym
                     ]
        in go

