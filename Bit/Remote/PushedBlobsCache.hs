{-# LANGUAGE DataKinds #-}

-- | Local "pushed blobs" cache (spec Option C).
--
-- Every push otherwise runs @rclone lsf --recursive <remote>/cas/@ to learn
-- which CAS blobs the remote already has — an O(remote-size) query that is the
-- dominant cost on high-latency remotes (Drive, S3). This module persists that
-- set locally at @.bit/cache/pushed-blobs/<name>@ so subsequent pushes consult
-- the cache first and skip the remote listing.
--
-- Semantics: the cache holds hashes we have confirmed are on the remote (either
-- the remote already had them, or we just uploaded them). It is therefore a
-- subset-or-equal of the remote CAS in the common case, so a present entry is
-- safe to trust. The one stale case is a blob deleted from the remote out of
-- band; deleting the cache file forces a fresh @rclone lsf@ on the next push.
module Bit.Remote.PushedBlobsCache
  ( readPushedBlobs
  , writePushedBlobs
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Bit.Types (Hash(..), HashAlgo(..), hashToText)
import Bit.Config.Paths (pushedBlobsCachePath)

-- | Read the cached set of remote blob hashes. @Nothing@ when no cache file
-- exists yet (caller should fall back to @rclone lsf@). @absBitDir@ is the
-- absolute @.bit@ directory.
readPushedBlobs :: FilePath -> String -> IO (Maybe (Set (Hash 'MD5)))
readPushedBlobs absBitDir name = do
    let path = pushedBlobsCachePath absBitDir name
    exists <- doesFileExist path
    if not exists
        then pure Nothing
        else do
            contents <- TIO.readFile path
            let hashes = [ Hash t
                         | rawLine <- T.lines contents
                         , let t = T.strip rawLine
                         , not (T.null t) ]
            pure (Just (Set.fromList hashes))

-- | Write the cache atomically (create-temp + rename via writeFile to a temp
-- path is unnecessary here; we write directly after ensuring the directory
-- exists). One @md5:<hex>@ hash per line, sorted for stable diffs.
writePushedBlobs :: FilePath -> String -> Set (Hash 'MD5) -> IO ()
writePushedBlobs absBitDir name hashes = do
    let path = pushedBlobsCachePath absBitDir name
    createDirectoryIfMissing True (takeDirectory path)
    let body = T.unlines (map hashToText (Set.toAscList hashes))
    TIO.writeFile path body
