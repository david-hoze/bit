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

import System.Process (readProcessWithExitCode, CreateProcess(..), StdStream(..), proc, waitForProcess, createProcess)
import System.Exit (ExitCode(..))
import System.IO (hGetLine, hIsEOF, hClose, Handle)
import System.FilePath (normalise)
import Control.Monad (unless, void)
import Control.Concurrent.Async (async, wait)
import Data.List (isInfixOf)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)
import Data.String (fromString)
import Bit.Remote (Remote, remoteUrl)
import Data.IORef (IORef, modifyIORef')
import Control.Exception (bracket, try, SomeException)

-- | Run a process and capture stdout as raw bytes (avoiding locale encoding issues).
-- Returns (ExitCode, ByteString, String) where stdout is raw bytes and stderr is String.
-- Uses bracket for exception-safe resource cleanup.
-- Reads stdout and stderr concurrently to avoid pipe deadlocks.
readProcessBytes :: FilePath -> [String] -> IO (ExitCode, LBS.ByteString, String)
readProcessBytes cmd args = do
    let cp = (proc cmd args)
            { std_out = CreatePipe
            , std_err = CreatePipe
            , std_in = Inherit
            }
    bracket (createProcess cp) cleanupProcess $ \(_, mStdout, mStderr, ph) -> do
        case (mStdout, mStderr) of
            (Just hOut, Just hErr) -> do
                -- Read stdout and stderr concurrently to avoid deadlocks
                -- BS.hGetContents is strict and closes handle when done
                asyncOut <- async (BS.hGetContents hOut)
                asyncErr <- async (hGetContents' hErr)
                -- Wait for both reads to complete
                outBytes <- wait asyncOut
                errStr   <- wait asyncErr
                code     <- waitForProcess ph
                return (code, LBS.fromStrict outBytes, errStr)
            _ -> error "readProcessBytes: failed to create pipes"
  where
    -- Cleanup: close any handles that might still be open and wait for process
    cleanupProcess (mStdin, mStdout, mStderr, ph) = do
        -- Try to close handles (may already be closed by hGetContents)
        maybe (return ()) (const $ return ()) mStdin
        maybe (return ()) (\h -> void (try (hClose h) :: IO (Either SomeException ()))) mStdout
        maybe (return ()) (\h -> void (try (hClose h) :: IO (Either SomeException ()))) mStderr
        -- Ensure process is cleaned up
        void (try (waitForProcess ph) :: IO (Either SomeException ExitCode))
    
    -- Strict reading of handle contents
    hGetContents' :: Handle -> IO String
    hGetContents' h = go []
      where
        go acc = do
            eof <- hIsEOF h
            if eof
                then return (concat (reverse acc))
                else do
                    line <- hGetLine h
                    go ((line ++ "\n") : acc)

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
-- Returns (ExitCode, ByteString, String) where stdout is raw bytes for proper UTF-8 handling
listRemoteJson :: Remote -> Int -> IO (ExitCode, LBS.ByteString, String)
listRemoteJson remote maxDepth =
    readProcessBytes "rclone" ["lsjson", "--max-depth", show maxDepth, remoteUrl remote]

-- | List remote directory items (at remote root, parsed)
listRemoteItems :: Remote -> Int -> IO (Either String [TransportItem])
listRemoteItems remote maxDepth = do
    (code, outBytes, err) <- listRemoteJson remote maxDepth
    case code of
        ExitFailure _ -> 
            if "directory not found" `isInfixOf` err 
            then return (Right [])  -- Empty directory
            else return (Left err)  -- Network or other error
        ExitSuccess -> do
            case Aeson.decode outBytes :: Maybe [RcloneItem] of
                Nothing -> return (Left "Failed to parse rclone JSON output")
                Just items -> return (Right [TransportItem (name item) (isDir item) | item <- items])

-- | List remote recursively with hashes
-- Returns (ExitCode, ByteString, String) where stdout is raw bytes for proper UTF-8 handling
listRemoteJsonWithHash :: Remote -> IO (ExitCode, LBS.ByteString, String)
listRemoteJsonWithHash remote =
    readProcessBytes "rclone" ["lsjson", remoteUrl remote, "--hash", "--recursive"]

-- | Check local against remote with optional progress tracking
checkRemote :: FilePath -> Remote -> Maybe (IORef Int) -> IO CheckResult
checkRemote localPath remote mCounter = do
    let args = [ "check"
               , localPath
               , remoteUrl remote
               , "--combined", "-"
               , "--exclude", ".bit/**"
               ]
    case mCounter of
        Nothing -> do
            -- No progress tracking - use simple blocking version
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
        Just counter -> do
            -- Stream output and track progress
            let cp = (proc "rclone" args)
                    { std_out = CreatePipe
                    , std_err = CreatePipe
                    , std_in = Inherit
                    }
            bracket (createProcess cp) cleanup $ \(_, mStdout, mStderr, ph) -> do
                case (mStdout, mStderr) of
                    (Just hOut, Just hErr) -> do
                        -- CRITICAL: drain stderr concurrently to avoid pipe deadlock
                        -- When rclone checks large repos (especially cloud remotes like Google Drive),
                        -- it produces stderr output (rate limits, retries, transfer stats).
                        -- If we read stdout first, stderr pipe buffer can fill (~64KB on Windows)
                        -- causing rclone to block on stderr write while we block on stdout read.
                        asyncErr <- async (hGetContents' hErr)
                        -- Read stdout line by line, accumulating and counting
                        outLines <- readLinesWithProgress hOut counter
                        -- Now join stderr read
                        errOutput <- wait asyncErr
                        -- Wait for process to finish
                        code <- waitForProcess ph
                        let out = unlines outLines
                            parsed = parseCombinedOutput out
                        return CheckResult
                            { checkMatches     = parsed '='
                            , checkDiffers     = parsed '*'
                            , checkMissingDest = parsed '+'
                            , checkMissingSrc  = parsed '-'
                            , checkErrors      = parsed '!'
                            , checkExitCode    = code
                            , checkRawOutput   = out
                            , checkStderr      = errOutput
                            }
                    _ -> error "checkRemote: failed to create pipes"
  where
    cleanup (_, mOut, mErr, ph) = do
        -- Try to close any handles that are still open
        maybe (return ()) (\h -> void (try (hClose h) :: IO (Either SomeException ()))) mOut
        maybe (return ()) (\h -> void (try (hClose h) :: IO (Either SomeException ()))) mErr
        -- Ensure process is cleaned up
        void (try (waitForProcess ph) :: IO (Either SomeException ExitCode))
    
    -- Read lines from handle, incrementing counter for each line
    readLinesWithProgress :: Handle -> IORef Int -> IO [String]
    readLinesWithProgress h counter = go []
      where
        go acc = do
            eof <- hIsEOF h
            if eof
                then return (reverse acc)
                else do
                    line <- hGetLine h
                    unless (null line) $ modifyIORef' counter (+1)
                    go (line : acc)
    
    -- Strict reading of handle contents to avoid lazy IO issues
    hGetContents' :: Handle -> IO String
    hGetContents' h = go []
      where
        go acc = do
            eof <- hIsEOF h
            if eof
                then return (concat (reverse acc))
                else do
                    line <- hGetLine h
                    go ((line ++ "\n") : acc)
    
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


