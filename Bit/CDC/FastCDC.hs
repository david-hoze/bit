{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | FastCDC content-defined chunking algorithm.
-- Uses gear-based rolling hash with two masks (small and large) to find
-- chunk boundaries. Pre-normal region uses a larger mask (fewer boundaries),
-- post-normal region uses a smaller mask (more boundaries), biasing toward
-- the target average size.
module Bit.CDC.FastCDC
  ( findBoundary
  , chunkByteString
  , chunkFile
  ) where

import Data.Array ((!))
import Data.Bits (shiftL, (.&.), xor)
import Data.Word (Word64)
import qualified Data.ByteString as BS
import System.IO (withFile, IOMode(ReadMode), hFileSize)

import Bit.CDC.Types (ChunkConfig(..), Chunk(..))
import Bit.CDC.Gear (gearTable)
import Bit.Config.Metadata (hashFileBytes)

-- | Compute floor(log2(n)) for positive n.
logBase2 :: Int -> Int
logBase2 n = go 0 1
  where
    go !k !p
      | p * 2 > n = k
      | otherwise  = go (k + 1) (p * 2)

-- | Small mask (used after normalPoint): more bits set, easier to match → more boundaries.
maskS :: Int -> Word64
maskS avg = (1 `shiftL` (logBase2 avg + 1)) - 1

-- | Large mask (used before normalPoint): fewer bits set, harder to match → fewer boundaries.
maskL :: Int -> Word64
maskL avg = (1 `shiftL` (logBase2 avg - 1)) - 1

-- | Find chunk boundary in a ByteString starting from offset.
-- Returns (boundary position relative to start of data, final fingerprint).
-- The search region is [minSize, maxSize] within the data.
-- Pre-normalPoint uses maskL (hard to match), post-normalPoint uses maskS (easy to match).
findBoundary :: ChunkConfig -> BS.ByteString -> Int -> (Int, Word64)
findBoundary cfg bs startOff
  | BS.length bs - startOff <= ccMinSize cfg = (BS.length bs, 0)
  | otherwise = scanLarge (startOff + ccMinSize cfg) 0
  where
    normPoint = startOff + ccAvgSize cfg
    endPos    = min (BS.length bs) (startOff + ccMaxSize cfg)
    mS        = maskS (ccAvgSize cfg)
    mL        = maskL (ccAvgSize cfg)

    -- Phase 1: scan with large mask (hard to match) up to normalPoint
    scanLarge !pos !fp
      | pos >= endPos = (endPos, fp)
      | pos >= normPoint = scanSmall pos fp
      | otherwise =
          let !b   = BS.index bs pos
              !fp' = (fp `shiftL` 1) `xor` (gearTable ! b)
          in if fp' .&. mL == 0
             then (pos, fp')
             else scanLarge (pos + 1) fp'

    -- Phase 2: scan with small mask (easy to match) from normalPoint to maxSize
    scanSmall !pos !fp
      | pos >= endPos = (endPos, fp)
      | otherwise =
          let !b   = BS.index bs pos
              !fp' = (fp `shiftL` 1) `xor` (gearTable ! b)
          in if fp' .&. mS == 0
             then (pos, fp')
             else scanSmall (pos + 1) fp'

-- | Chunk an in-memory ByteString. Pure, for testing.
chunkByteString :: ChunkConfig -> BS.ByteString -> [Chunk]
chunkByteString cfg bs = go 0
  where
    go !off
      | off >= BS.length bs = []
      | otherwise =
          let (boundary, _) = findBoundary cfg bs off
              !len  = boundary - off
              !cbs  = BS.take len (BS.drop off bs)
              !h    = hashFileBytes cbs
              chunk = Chunk { chunkOffset = fromIntegral off
                            , chunkLength = len
                            , chunkHash   = h
                            }
          in chunk : go boundary

-- | Chunk a file on disk using streaming IO.
-- Reads the file in maxSize-sized blocks, finding boundaries within each block.
-- For boundaries near the end of a block, seeks back to continue from the boundary.
chunkFile :: ChunkConfig -> FilePath -> IO [Chunk]
chunkFile cfg filePath = withFile filePath ReadMode $ \h -> do
  fileSize <- hFileSize h
  if fileSize == 0
    then pure []
    else do
      -- For simplicity and correctness, read the entire file if it fits in memory.
      -- Files that reach CDC are typically large binaries (>= minSize), but not
      -- so large that they can't be read into memory for chunking.
      -- A streaming approach with hGet + hSeek can be added later if needed.
      content <- BS.hGet h (fromIntegral fileSize)
      pure (chunkByteString cfg content)
