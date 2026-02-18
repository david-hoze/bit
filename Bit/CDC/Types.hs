{-# LANGUAGE DataKinds #-}

-- | CDC types: chunk configuration, chunk descriptors, and manifests.
module Bit.CDC.Types
  ( ChunkConfig(..)
  , defaultChunkConfig
  , Chunk(..)
  , ChunkRef(..)
  , ChunkManifest(..)
  ) where

import Data.Int (Int64)
import Bit.Types (Hash, HashAlgo(..))

-- | Configuration for content-defined chunking.
-- ccMinSize: minimum chunk size (bytes) before boundary search begins.
-- ccAvgSize: target average chunk size (controls mask width).
-- ccMaxSize: maximum chunk size (hard cutoff).
data ChunkConfig = ChunkConfig
  { ccMinSize :: !Int
  , ccAvgSize :: !Int
  , ccMaxSize :: !Int
  } deriving (Show, Eq)

-- | Default chunk config: 32KB min, 128KB avg, 512KB max.
defaultChunkConfig :: ChunkConfig
defaultChunkConfig = ChunkConfig
  { ccMinSize = 32768
  , ccAvgSize = 131072
  , ccMaxSize = 524288
  }

-- | A chunk produced by the chunking algorithm.
data Chunk = Chunk
  { chunkOffset :: !Int64
  , chunkLength :: !Int
  , chunkHash   :: !(Hash 'MD5)
  } deriving (Show, Eq)

-- | A chunk reference stored in a manifest (hash + length only).
data ChunkRef = ChunkRef
  { crHash   :: !(Hash 'MD5)
  , crLength :: !Int
  } deriving (Show, Eq)

-- | Manifest describing how a file is split into chunks.
data ChunkManifest = ChunkManifest
  { cmFileHash   :: !(Hash 'MD5)   -- ^ Hash of the whole file
  , cmFileSize   :: !Int64          -- ^ Size of the whole file
  , cmChunkCount :: !Int            -- ^ Number of chunks
  , cmChunks     :: ![ChunkRef]     -- ^ Ordered list of chunk references
  } deriving (Show, Eq)
