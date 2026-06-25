{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | @bit lock@ / @bit unlock@ / @bit locks@ — advisory locks for binary assets.
--
-- Binary files cannot be three-way merged, so concurrent edits silently clobber
-- each other. These commands provide Perforce/Git-LFS-style advisory locks,
-- coordinated through the same rclone remote bit already uses for CAS — no
-- server required. A lock is a small record under the remote's @locks/@ prefix,
-- one file per locked path, keyed by the MD5 of the normalized path:
--
-- > locks/<md5(path)>.lock
--
-- The owner identity is git's @user.email@ (with @user.name@ + hostname for
-- display) — bit reuses git's identity rather than inventing one.
--
-- Limitation: dumb storage has no atomic compare-and-swap, so two simultaneous
-- @bit lock@ calls can race. We read-back-after-write to surface contention, but
-- a fully race-free design would ride git's atomic ref updates. Locks are
-- advisory: enforcement at push time is a planned next step.
module Bit.Core.Locks
  ( lockPaths
  , unlockPaths
  , listLocks
  ) where

import Control.Exception (catch, IOException)
import Control.Monad (forM, forM_)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isSuffixOf, sortOn)
import Data.Maybe (mapMaybe)
import System.Exit (ExitCode(..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.IO (openTempFile, hClose)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Bit.Types (hashToText)
import Bit.CAS (stripHashPrefix)
import Bit.Config.Metadata (hashFileBytes)
import Bit.Remote (Remote, resolveRemote)
import qualified Bit.Rclone.Run as Transport
import Bit.Rclone.Run (CopyResult(..))
import qualified Bit.Git.Run as Git
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

-- | Who holds a lock.
data Owner = Owner
  { ownerEmail :: String
  , ownerName  :: String
  , ownerHost  :: String
  }

-- | A parsed lock record.
data LockRecord = LockRecord
  { lrPath  :: String
  , lrEmail :: String
  , lrName  :: String
  , lrHost  :: String
  , lrTime  :: String
  }

-- ---------------------------------------------------------------------------
-- Commands
-- ---------------------------------------------------------------------------

-- | Claim a lock on each path. Refuses (per path) a path already locked by a
-- different owner. Returns failure if any path could not be locked.
lockPaths :: FilePath -> String -> [FilePath] -> IO ExitCode
lockPaths cwd remoteName paths = withRemote cwd remoteName $ \remote -> do
  owner <- getOwner
  oks <- forM paths $ \p -> do
    existing <- fetchLock remote (lockRelPath p)
    case existing of
      Just lr | lrEmail lr /= ownerEmail owner -> do
        putStrLn $ unlockableMsg p lr
        pure False
      Just _ -> do
        putStrLn $ "Already locked by you: " ++ normPath p
        pure True
      Nothing -> do
        code <- writeLock remote owner p
        case code of
          ExitSuccess -> putStrLn ("Locked " ++ normPath p) >> pure True
          _           -> putStrLn ("Failed to lock " ++ normPath p ++ " (remote write error)") >> pure False
  pure (if and oks then ExitSuccess else ExitFailure 1)

-- | Release a lock on each path. Only the owner may release, unless @force@.
unlockPaths :: FilePath -> String -> Bool -> [FilePath] -> IO ExitCode
unlockPaths cwd remoteName force paths = withRemote cwd remoteName $ \remote -> do
  owner <- getOwner
  oks <- forM paths $ \p -> do
    existing <- fetchLock remote (lockRelPath p)
    case existing of
      Nothing -> putStrLn ("Not locked: " ++ normPath p) >> pure True
      Just lr
        | force || lrEmail lr == ownerEmail owner -> do
            code <- Transport.deleteRemote remote (lockRelPath p)
            case code of
              ExitSuccess -> putStrLn ("Unlocked " ++ normPath p) >> pure True
              _           -> putStrLn ("Failed to unlock " ++ normPath p) >> pure False
        | otherwise -> do
            putStrLn $ unlockableMsg p lr ++ " (use --force to override)"
            pure False
  pure (if and oks then ExitSuccess else ExitFailure 1)

-- | List all locks held on the remote.
listLocks :: FilePath -> String -> IO ExitCode
listLocks cwd remoteName = withRemote cwd remoteName $ \remote -> do
  (code, out, _) <- Transport.lsfRemote remote "locks"
  case code of
    ExitFailure _ -> putStrLn "No locks." >> pure ExitSuccess
    ExitSuccess -> do
      let names = [ n | rawLine <- lines out
                      , let n = trim rawLine
                      , ".lock" `isSuffixOf` n ]
      recs <- fmap (mapMaybe id) $ forM names $ \n -> fetchLock remote ("locks/" ++ n)
      if null recs
        then putStrLn "No locks."
        else forM_ (sortOn lrPath recs) $ \lr ->
               putStrLn $ lrPath lr ++ "\t" ++ displayOwner lr
      pure ExitSuccess

-- ---------------------------------------------------------------------------
-- Lock records & transport
-- ---------------------------------------------------------------------------

-- | Remote-relative path of a lock record for a working-tree path.
lockRelPath :: FilePath -> FilePath
lockRelPath p = "locks/" ++ pathKey p ++ ".lock"

-- | Stable filename key: MD5 hex of the normalized path.
pathKey :: FilePath -> String
pathKey = stripHashPrefix . T.unpack . hashToText . hashFileBytes . BC.pack . normPath

-- | Normalize a path for keying: forward slashes, no leading @./@, no trailing
-- slash. Keeps the same lock identity across platforms and minor path spellings.
normPath :: FilePath -> String
normPath = dropWhileEnd (== '/') . dropDotSlash . map fwd
  where
    fwd '\\' = '/'
    fwd c    = c
    dropDotSlash ('.' : '/' : rest) = rest
    dropDotSlash s = s

-- | Serialize a lock record.
renderLock :: Owner -> String -> String -> String
renderLock owner path nowEpoch = unlines
  [ "path: "  ++ path
  , "owner: " ++ ownerEmail owner
  , "name: "  ++ ownerName owner
  , "host: "  ++ ownerHost owner
  , "time: "  ++ nowEpoch
  ]

parseLock :: String -> Maybe LockRecord
parseLock s =
  let kv = [ (trim k, trim (drop 1 v)) | ln <- lines s, let (k, v) = break (== ':') ln, not (null v) ]
      get k = maybe "" id (lookup k kv)
      path = get "path"
  in if null path then Nothing
     else Just LockRecord
       { lrPath  = path
       , lrEmail = get "owner"
       , lrName  = get "name"
       , lrHost  = get "host"
       , lrTime  = get "time"
       }

-- | Fetch and parse a lock record by its remote-relative path. Nothing if it
-- does not exist (or could not be read).
fetchLock :: Remote -> FilePath -> IO (Maybe LockRecord)
fetchLock remote relPath = withTempFile $ \tmp -> do
  result <- Transport.copyFromRemoteDetailed remote relPath tmp
  case result of
    CopySuccess -> parseLock <$> readFile tmp
    _           -> pure Nothing

-- | Write a fresh lock record for a path to the remote.
writeLock :: Remote -> Owner -> FilePath -> IO ExitCode
writeLock remote owner p = do
  now <- getPOSIXTime
  let nowEpoch = show (round now :: Integer)
  withTempFile $ \tmp -> do
    writeFile tmp (renderLock owner (normPath p) nowEpoch)
    Transport.copyToRemote tmp remote (lockRelPath p)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

withRemote :: FilePath -> String -> (Remote -> IO ExitCode) -> IO ExitCode
withRemote cwd remoteName action = do
  mRemote <- resolveRemote cwd remoteName
  case mRemote of
    Nothing -> putStrLn ("Unknown remote: " ++ remoteName) >> pure (ExitFailure 1)
    Just remote -> action remote

-- | Run an action with a fresh temp file path, cleaning it up afterwards.
withTempFile :: (FilePath -> IO a) -> IO a
withTempFile action = do
  tmpDir <- getTemporaryDirectory
  (path, h) <- openTempFile tmpDir "bit-lock.tmp"
  hClose h
  r <- action path
  removeFile path
  pure r

-- | Owner = git user.email / user.name (read from bit's own index repo, so it
-- matches commit identity) + best-effort hostname.
getOwner :: IO Owner
getOwner = do
  indexDir <- Git.getIndexPath
  email <- gitConfig indexDir "user.email"
  name  <- gitConfig indexDir "user.name"
  host  <- getHost
  pure Owner
    { ownerEmail = orElse email "unknown"
    , ownerName  = name
    , ownerHost  = host
    }
  where orElse s d = if null s then d else s

gitConfig :: FilePath -> String -> IO String
gitConfig indexDir key = do
  (code, out, _) <- Git.runGitAt indexDir ["config", "--get", key]
  pure $ case code of
    ExitSuccess -> trim out
    _           -> ""

getHost :: IO String
getHost = do
  envHost <- firstNonEmpty <$> mapM lookupEnv ["HOSTNAME", "COMPUTERNAME"]
  case envHost of
    Just h  -> pure h
    Nothing -> do
      etc <- (trim <$> readFile "/etc/hostname") `catch` \(_ :: IOException) -> pure ""
      pure (if null etc then "unknown" else etc)
  where
    firstNonEmpty xs = case [v | Just v <- xs, not (null v)] of
      (v:_) -> Just v
      []    -> Nothing

displayOwner :: LockRecord -> String
displayOwner lr =
  let who = if null (lrName lr) then lrEmail lr else lrName lr ++ " <" ++ lrEmail lr ++ ">"
  in who ++ (if null (lrHost lr) then "" else " on " ++ lrHost lr)

unlockableMsg :: FilePath -> LockRecord -> String
unlockableMsg p lr = normPath p ++ ": locked by " ++ displayOwner lr

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
