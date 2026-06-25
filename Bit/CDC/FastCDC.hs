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
import System.IO (withFile, IOMode(ReadMode), Handle)

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

-- | Chunk a file on disk using streaming IO with a bounded buffer.
--
-- Peak memory is ~2*maxSize regardless of file size, so files larger than RAM
-- chunk correctly. The output is byte-identical to @chunkByteString@ on the same
-- bytes: before every boundary search we guarantee the buffer holds at least
-- maxSize bytes (or all remaining bytes at EOF), so 'findBoundary' sees exactly
-- the same window it would see scanning the whole file in memory.
chunkFile :: ChunkConfig -> FilePath -> IO [Chunk]
chunkFile cfg filePath = withFile filePath ReadMode $ \h ->
    go h 0 BS.empty False []
  where
    maxSz = ccMaxSize cfg

    -- Refill the buffer until it holds at least maxSize bytes or EOF is hit.
    -- Returns the (possibly grown) buffer and whether EOF was reached.
    fill :: Handle -> BS.ByteString -> IO (BS.ByteString, Bool)
    fill h buf
      | BS.length buf >= maxSz = pure (buf, False)
      | otherwise = do
          more <- BS.hGet h maxSz
          if BS.null more
            then pure (buf, True)
            else fill h (buf <> more)

    go :: Handle -> Int -> BS.ByteString -> Bool -> [Chunk] -> IO [Chunk]
    go h absOff buf eof acc = do
      (buf', eof') <- if eof then pure (buf, True) else fill h buf
      if BS.null buf'
        then pure (reverse acc)
        else do
          let (boundary, _) = findBoundary cfg buf' 0
              !len   = boundary
              !cbs   = BS.take len buf'
              !h'    = hashFileBytes cbs
              !chunk = Chunk { chunkOffset = fromIntegral absOff
                             , chunkLength = len
                             , chunkHash   = h'
                             }
              rest   = BS.drop len buf'
          go h (absOff + len) rest eof' (chunk : acc)
