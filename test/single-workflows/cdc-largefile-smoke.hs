{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | Large-file smoke test for the streaming chunker.
--
-- Chunks a file that is much larger than the desired memory headroom and
-- checks two things:
--
--   1. The chunks tile the file exactly — first offset 0, each chunk abuts the
--      next (off+len == next off), last chunk ends at the file size. Combined
--      with the byte-identical property proven in test/CdcSpec.hs, this
--      guarantees reassembly reproduces the original byte-for-byte.
--   2. A whole-file MD5 streamed from disk matches an MD5 accumulated by reading
--      back each chunk's [offset,length) slice in order — i.e. concatenating the
--      chunks reproduces the file's hash.
--
-- Run with +RTS -s and confirm "maximum residency" stays ~2*maxSize regardless
-- of file size (default maxSize 512 KB → a few MB, not multiple GB).
--
-- Build & run (from repo root):
--   cabal exec -- ghc --make -O -o /tmp/cdc-smoke test/single-workflows/cdc-largefile-smoke.hs
--   /tmp/cdc-smoke <path-to-large-file> +RTS -s
module Main where

import qualified Data.ByteString as BS
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import Data.Int (Int64)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

import Bit.CDC.Types (ChunkConfig(..), defaultChunkConfig, Chunk(..))
import Bit.CDC.FastCDC (chunkFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> run path
    _      -> hPutStrLn stderr "usage: cdc-largefile-smoke <file>" >> exitFailure

run :: FilePath -> IO ()
run path = do
  fileSize <- withFile path ReadMode hFileSize
  putStrLn $ "file size: " ++ show fileSize ++ " bytes"

  chunks <- chunkFile defaultChunkConfig path
  let n = length chunks
  putStrLn $ "chunks produced: " ++ show n
  putStrLn $ "max chunk size cfg: " ++ show (ccMaxSize defaultChunkConfig)

  -- (1) chunks must tile the file with no gaps or overlaps
  let tiling = checkTiling (fromIntegral fileSize) chunks
  case tiling of
    Left err -> putStrLn ("TILING FAIL: " ++ err) >> exitFailure
    Right () -> putStrLn "tiling OK: chunks cover [0, fileSize) contiguously"

  -- (2) whole-file hash == hash of chunks read back in order
  wholeHash    <- streamHashWhole path
  fromChunks   <- streamHashChunks path chunks
  putStrLn $ "whole-file md5:   " ++ wholeHash
  putStrLn $ "chunk-stream md5: " ++ fromChunks
  if wholeHash == fromChunks
    then putStrLn "HASH MATCH: reassembled stream matches whole file"
    else putStrLn "HASH FAIL" >> exitFailure

-- | Verify chunks form a contiguous partition of [0, fileSize).
checkTiling :: Int64 -> [Chunk] -> Either String ()
checkTiling fileSize = go 0
  where
    go pos [] | pos == fileSize = Right ()
              | otherwise = Left ("last offset " ++ show pos ++ " /= file size " ++ show fileSize)
    go pos (c:cs)
      | chunkOffset c /= pos =
          Left ("gap/overlap: expected offset " ++ show pos ++ " got " ++ show (chunkOffset c))
      | chunkLength c <= 0 =
          Left ("non-positive chunk length at offset " ++ show pos)
      | otherwise = go (pos + fromIntegral (chunkLength c)) cs

-- | Stream the whole file through MD5 in bounded buffers.
-- The MD5 context is forced each step (@!ctx'@) so the accumulator does not
-- build a thunk chain that retains every buffer read.
streamHashWhole :: FilePath -> IO String
streamHashWhole path = withFile path ReadMode $ \h -> go h MD5.init
  where
    go h !ctx = do
      bs <- BS.hGet h (1024 * 1024)
      if BS.null bs
        then pure (showHash (MD5.finalize ctx))
        else let !ctx' = MD5.update ctx bs in go h ctx'

-- | Read back each chunk's [offset,length) slice in order and stream into MD5.
-- Re-opens and seeks per chunk so peak memory is one chunk at a time.
streamHashChunks :: FilePath -> [Chunk] -> IO String
streamHashChunks path chunks = withFile path ReadMode $ \h -> go h MD5.init chunks
  where
    go _ !ctx [] = pure (showHash (MD5.finalize ctx))
    go h !ctx (c:cs) = do
      hSeek h AbsoluteSeek (fromIntegral (chunkOffset c))
      bs <- BS.hGet h (chunkLength c)
      let !ctx' = MD5.update ctx bs
      go h ctx' cs

showHash :: BS.ByteString -> String
showHash = map (toEnum . fromIntegral) . BS.unpack . encode
