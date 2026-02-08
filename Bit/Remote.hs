module Bit.Remote
  ( Remote          -- export type but NOT constructor/fields
  , mkRemote        -- smart constructor
  , remoteName      -- only the name is public
  , remoteUrl       -- for Transport only (should import from Internal)
  , displayRemote   -- for user-facing messages
  , resolveRemote
  , getDefaultRemote
  , RemoteState(..) -- remote state classification
  , FetchResult(..) -- bundle fetch result
  ) where

import qualified Internal.Git as Git
import qualified Bit.Device as Device

-- | A resolved remote. Bit.hs works with this; only Transport sees the url.
data Remote = Remote
  { _remoteName :: String    -- "origin", "backup", "nas", etc.
  , _remoteUrl  :: String    -- Resolved URL/path for Transport (e.g. "gdrive:Projects/foo", "/mnt/usb/backup")
  } deriving (Show, Eq)

-- | Get the remote name
remoteName :: Remote -> String
remoteName = _remoteName

-- | Get the remote URL (for Transport only)
remoteUrl :: Remote -> String
remoteUrl = _remoteUrl

-- | For user-facing display only. Never use this to construct paths.
displayRemote :: Remote -> String
displayRemote r = _remoteName r ++ " (" ++ _remoteUrl r ++ ")"

-- | Smart constructor
mkRemote :: String -> String -> Remote
mkRemote = Remote

-- | Resolve a remote name to a Remote. Checks:
--   1. .rgit/remotes/<name> (device resolution via Device.hs)
--   2. Git config (git remote get-url <name>)
-- Returns Nothing if remote doesn't exist or device is not connected.
resolveRemote :: FilePath -> String -> IO (Maybe Remote)
resolveRemote cwd name = do
    -- Try .rgit/remotes/<name> first (device-aware resolution)
    mTarget <- Device.readRemoteFile cwd name
    case mTarget of
        Just target -> do
            res <- Device.resolveRemoteTarget cwd target
            case res of
                Device.Resolved url -> pure (Just (mkRemote name url))
                Device.NotConnected _ -> pure Nothing
        Nothing -> do
            -- Fall back to git remote URL
            mUrl <- Git.getRemoteUrl name
            case mUrl of
                Just url | not (null url) -> pure (Just (mkRemote name url))
                _ -> pure Nothing

-- | Get the default remote for push/pull/fetch.
-- Checks branch tracking config, falls back to "origin".
getDefaultRemote :: FilePath -> IO (Maybe Remote)
getDefaultRemote cwd = do
    name <- Git.getTrackedRemoteName  -- defaults to "origin" if not configured
    resolveRemote cwd name

-- | Classification of remote state (used by Bit.hs to determine what action to take)
data RemoteState 
    = StateEmpty                        -- Case A: No files at all
    | StateValidRgit                    -- Case B: .rgit/ exists and looks okay
    | StateNonRgitOccupied [String]     -- Case C: Files exist, but no .rgit/ (stores sample filenames)
    | StateCorruptedRgit String         -- Case D: .rgit/ exists but something is wrong
    | StateNetworkError String          -- Network/Auth failure
    deriving (Show, Eq)

-- | Result of attempting to fetch a bundle from remote
data FetchResult 
    = BundleFound FilePath 
    | RemoteEmpty 
    | NetworkError String
    deriving (Show, Eq)
