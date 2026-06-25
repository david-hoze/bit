{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Streaming-chunker correctness tests.
--
-- The core property of the streaming 'chunkFile' is that its output is
-- byte-identical to the pure in-memory 'chunkByteString' on the same bytes.
-- If boundaries ever shift, existing CAS blobs stop matching freshly-produced
-- chunks and dedup silently breaks (every file re-uploads in full) with no
-- error. This suite is the blocking gate that proves boundaries did not move.
--
-- Run: cabal test cdc
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.ByteString as BS
import Data.Word (Word8)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import Control.Exception (bracket)

import Bit.CDC.Types (ChunkConfig(..))
import Bit.CDC.FastCDC (chunkByteString, chunkFile)

main :: IO ()
main = defaultMain tests

-- | Small config so modest test inputs still span many chunks.
-- min 64 / avg 256 / max 1024 mirrors the sizes the CLI CDC tests use.
testCfg :: ChunkConfig
testCfg = ChunkConfig { ccMinSize = 64, ccAvgSize = 256, ccMaxSize = 1024 }

tests :: TestTree
tests = testGroup "CDC streaming chunker"
  [ byteIdenticalCases
  , byteIdenticalProperty
  ]

-- | Deterministic byte content of length n: a non-trivial, non-constant
-- pattern so boundary search actually exercises the gear hash.
patternBytes :: Int -> BS.ByteString
patternBytes n = BS.pack (take n (map fromIntegral (cycle pat)))
  where
    pat :: [Int]
    pat = [0, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 7, 71, 199, 251]

-- | Compare streaming chunkFile output to in-memory chunkByteString.
-- Compares offsets, lengths and hashes (the full Chunk derives Eq).
assertSameChunking :: String -> BS.ByteString -> Assertion
assertSameChunking lbl bs = do
  let inMemory = chunkByteString testCfg bs
  onDisk <- withTempBytes bs (chunkFile testCfg)
  assertEqual (lbl ++ ": chunkFile must match chunkByteString") inMemory onDisk

-- | Write bytes to a temp file, run an action on the path, always clean up.
withTempBytes :: BS.ByteString -> (FilePath -> IO a) -> IO a
withTempBytes bs action = do
  tmp <- getTemporaryDirectory
  bracket
    (do (path, h) <- openBinaryTempFile tmp "cdc-stream.bin"
        BS.hPut h bs
        hClose h
        pure path)
    removeFile
    action

byteIdenticalCases :: TestTree
byteIdenticalCases = testGroup "byte-identical to chunkByteString (boundary cases)"
  [ testCase "empty file" $
      assertSameChunking "empty" BS.empty
  , testCase "single byte" $
      assertSameChunking "single" (patternBytes 1)
  , testCase "below minSize" $
      assertSameChunking "below-min" (patternBytes (ccMinSize testCfg - 1))
  , testCase "exactly minSize" $
      assertSameChunking "eq-min" (patternBytes (ccMinSize testCfg))
  , testCase "between minSize and maxSize" $
      assertSameChunking "between" (patternBytes ((ccMinSize testCfg + ccMaxSize testCfg) `div` 2))
  , testCase "exactly maxSize" $
      assertSameChunking "eq-max" (patternBytes (ccMaxSize testCfg))
  , testCase "exact multiple of maxSize" $
      assertSameChunking "mult-max" (patternBytes (ccMaxSize testCfg * 4))
  , testCase "many chunks" $
      assertSameChunking "many" (patternBytes (ccMaxSize testCfg * 37 + 123))
  , testCase "all-zero many chunks (forces max-size cutoffs)" $
      assertSameChunking "zeros" (BS.replicate (ccMaxSize testCfg * 10 + 7) 0)
  ]

byteIdenticalProperty :: TestTree
byteIdenticalProperty = testGroup "byte-identical to chunkByteString (random)"
  [ testProperty "chunkFile == chunkByteString for arbitrary bytes" $
      forAll (chooseInt (0, 8000)) $ \n ->
      forAll (vectorOf n arbitraryByte) $ \ws ->
        ioProperty $ do
          let bs = BS.pack ws
          onDisk <- withTempBytes bs (chunkFile testCfg)
          pure (chunkByteString testCfg bs == onDisk)
  ]

arbitraryByte :: Gen Word8
arbitraryByte = fromIntegral <$> chooseInt (0, 255)
