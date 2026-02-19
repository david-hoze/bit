{-# LANGUAGE DataKinds #-}

-- | Query which CAS blobs already exist on a remote, for dedup during push.
module Bit.Remote.ChunkIndex (queryRemoteBlobs) where

import qualified Data.Set as Set
import Data.Set (Set)
import System.Exit (ExitCode(..))

import Bit.Types (Hash(..), HashAlgo(..))
import Bit.Remote (Remote)
import qualified Bit.Rclone.Run as Transport
import qualified Data.Text as T
import Data.List (isSuffixOf)

-- | List all CAS blob hashes present on a remote.
-- Runs @rclone lsf --recursive <remote>/cas/@ and parses output lines
-- like @a1/a1b2c3d4e5f67890abcdef12345678@ into @Hash 'MD5@ values.
-- Skips manifest files (ending in @.manifest@).
-- On failure, returns empty set (graceful degradation — uploads everything).
queryRemoteBlobs :: Remote -> IO (Set (Hash 'MD5))
queryRemoteBlobs remote = do
    (code, out, _err) <- Transport.lsfRemote remote "cas"
    case code of
        ExitSuccess -> pure $ parseLsfOutput out
        _           -> pure Set.empty

-- | Parse rclone lsf output into a set of MD5 hashes.
-- Each line is @<2-char-prefix>/<32-char-hex>@.
-- We extract the hex part and construct @Hash "md5:<hex>"@.
parseLsfOutput :: String -> Set (Hash 'MD5)
parseLsfOutput = Set.fromList . concatMap parseLine . lines
  where
    parseLine line
        | ".manifest" `isSuffixOf` trimmed = []
        | '/' `elem` trimmed =
            let hex = reverse . takeWhile (/= '/') . reverse $ trimmed
            in if length hex == 32
               then [Hash (T.pack ("md5:" ++ hex))]
               else []
        | otherwise = []
      where
        trimmed = strip line
    strip = reverse . dropWhile (== '\r') . reverse
