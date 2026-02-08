{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Bit.Remote.Scan
  ( fetchRemoteFiles
  , RemoteError(..)
  ) where

import GHC.Generics (Generic)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Aeson (FromJSON(..), decode, withObject, (.:), (.:?))
import System.Exit (ExitCode(..))
import System.FilePath (normalise)
import Data.Maybe
import qualified Data.Text as T
import Bit.Types
import qualified Internal.Transport as Transport
import Bit.Remote (Remote)

----------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------

data RemoteError
  = RcloneFailed
  | DecodeFailed String
  deriving (Show, Eq)

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

fetchRemoteFiles :: Remote -> IO (Either RemoteError [FileEntry])
fetchRemoteFiles remote = do
    (code, rawBytes, _err) <- Transport.listRemoteJsonWithHash remote
    case code of
        ExitSuccess -> pure $ maybe
            (Left (DecodeFailed "Invalid rclone JSON output"))
            (Right . map rcloneFileToFileEntry . filter (not . rfIsDir))
            (decode rawBytes :: Maybe [RcloneFile])
        _ -> pure (Left RcloneFailed)

----------------------------------------------------------------------
-- Conversion
----------------------------------------------------------------------

rcloneFileToFileEntry :: RcloneFile -> FileEntry
rcloneFileToFileEntry rf =
  FileEntry
    { path = normalise rf.rfPath
    , kind = File { fHash = Hash (T.pack ("md5:" ++ md5)), fSize = rf.rfSize, fContentType = BinaryContent }
    }
    where
      md5 =
        Map.findWithDefault "" "md5" (fromMaybe Map.empty rf.rfHashes)
----------------------------------------------------------------------
-- rclone JSON model
----------------------------------------------------------------------

data RcloneFile = RcloneFile
  { rfPath   :: FilePath
  , rfSize   :: Integer
  , rfHashes :: Maybe (Map String String)
  , rfIsDir  :: Bool
  } deriving (Show, Generic)

instance FromJSON RcloneFile where
    parseJSON = withObject "RcloneFile" $ \v ->
        RcloneFile
          <$> v .:  "Path"
          <*> v .:  "Size"
          <*> v .:? "Hashes"
          <*> v .:  "IsDir"
