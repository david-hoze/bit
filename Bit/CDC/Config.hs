-- | CDC configuration: read cdc.enabled and size parameters from .bit/config.
module Bit.CDC.Config
  ( getCdcConfig
  ) where

import Bit.Core.Config (getConfigKey)
import Bit.CDC.Types (ChunkConfig(..), defaultChunkConfig)

-- | Read CDC configuration from .bit/config.
-- Returns Just ChunkConfig if cdc.enabled is "true", Nothing otherwise.
-- Size parameters fall back to defaults if not set.
getCdcConfig :: FilePath -> IO (Maybe ChunkConfig)
getCdcConfig bitRoot = do
  mEnabled <- getConfigKey bitRoot "cdc.enabled"
  case mEnabled of
    Just "true" -> do
      mMin <- readIntKey "cdc.min-size"
      mAvg <- readIntKey "cdc.avg-size"
      mMax <- readIntKey "cdc.max-size"
      let def = defaultChunkConfig
      pure $ Just ChunkConfig
        { ccMinSize = maybe (ccMinSize def) id mMin
        , ccAvgSize = maybe (ccAvgSize def) id mAvg
        , ccMaxSize = maybe (ccMaxSize def) id mMax
        }
    _ -> pure Nothing
  where
    readIntKey key = do
      mVal <- getConfigKey bitRoot key
      pure $ case mVal of
        Just s -> case reads s of
          [(n, "")] | n > 0 -> Just n
          _ -> Nothing
        Nothing -> Nothing
