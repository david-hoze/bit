{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Comprehensive merge tests for bit.
--
-- Inspired by Git's t7600-merge.sh and t6422-merge-rename-corner-cases.sh.
-- Tests the pure conflict resolution, conflict detection, and metadata parsing
-- logic that underpins bit's two-repo merge flow.
--
-- Run: cabal test merge
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List (isInfixOf)

import Bit.Conflict
import Bit.Internal.Metadata (MetaContent(..), parseMetadata, serializeMetadata, displayHash, hasConflictMarkers)
import Bit.Types (Hash(..), HashAlgo(..))
import qualified Data.Text as T

main :: IO ()
main = defaultMain tests

-- ---------------------------------------------------------------------------
-- All tests
-- ---------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Merge"
  [ conflictInfoParsingTests
  , metadataRoundtripTests
  , conflictMarkerTests
  ]

-- ---------------------------------------------------------------------------
-- 3. Conflict info parsing (parseConflictInfo)
--    Mirrors the three conflict types from Git's ls-files -u output:
--    - ContentConflict (both modified, stages 1+2+3)
--    - AddAdd (both added, stages 2+3 only)
--    - ModifyDelete (stage 2 only or stage 3 only)
-- ---------------------------------------------------------------------------

conflictInfoParsingTests :: TestTree
conflictInfoParsingTests = testGroup "parseConflictInfo"
  [ -- Content conflict: base (stage 1) + ours (stage 2) + theirs (stage 3)
    testCase "stages 1,2,3 -> ContentConflict" $ do
      let output = unlines
            [ "100644 abc123 1\tindex/foo.bin"
            , "100644 def456 2\tindex/foo.bin"
            , "100644 ghi789 3\tindex/foo.bin"
            ]
      parseConflictInfo "index/foo.bin" output @?= ContentConflict "index/foo.bin"

  -- Add/add conflict: no base (no stage 1), ours (stage 2) + theirs (stage 3)
  , testCase "stages 2,3 only -> AddAdd" $ do
      let output = unlines
            [ "100644 def456 2\tindex/new.txt"
            , "100644 ghi789 3\tindex/new.txt"
            ]
      parseConflictInfo "index/new.txt" output @?= AddAdd "index/new.txt"

  -- Modify/delete: ours modified (stage 2) but theirs deleted (no stage 3)
  , testCase "stage 2 only -> ModifyDelete (deleted in theirs)" $ do
      let output = unlines
            [ "100644 abc123 1\tindex/gone.bin"
            , "100644 def456 2\tindex/gone.bin"
            ]
      parseConflictInfo "index/gone.bin" output @?= ModifyDelete "index/gone.bin" False

  -- Modify/delete: theirs modified (stage 3) but ours deleted (no stage 2)
  , testCase "stage 3 only -> ModifyDelete (deleted in ours)" $ do
      let output = unlines
            [ "100644 abc123 1\tindex/gone.bin"
            , "100644 ghi789 3\tindex/gone.bin"
            ]
      parseConflictInfo "index/gone.bin" output @?= ModifyDelete "index/gone.bin" True

  -- Empty output (fallback)
  , testCase "empty output -> ContentConflict (fallback)" $ do
      parseConflictInfo "index/x" "" @?= ContentConflict "index/x"

  -- Malformed output (fallback)
  , testCase "garbage output -> ContentConflict (fallback)" $ do
      parseConflictInfo "index/x" "this is not valid ls-files output" @?= ContentConflict "index/x"
  ]

-- ---------------------------------------------------------------------------
-- 4. Metadata round-trip (serialize -> parse)
--    Ensures metadata survives push/pull/merge without corruption.
-- ---------------------------------------------------------------------------

metadataRoundtripTests :: TestTree
metadataRoundtripTests = testGroup "Metadata round-trip"
  [ testCase "serialize then parse is identity" $ do
      let mc = MetaContent (Hash "md5:abc123def456") 1048576
      parseMetadata (serializeMetadata mc) @?= Just mc

  , testCase "serialize then parse with large size" $ do
      let mc = MetaContent (Hash "md5:0000000000000000") 999999999999
      parseMetadata (serializeMetadata mc) @?= Just mc

  , testCase "serialize then parse with zero size" $ do
      let mc = MetaContent (Hash "md5:ffffffffffffffff") 0
      parseMetadata (serializeMetadata mc) @?= Just mc

  , testCase "parse rejects empty string" $ do
      parseMetadata "" @?= Nothing

  , testCase "parse rejects random text (text file content)" $ do
      parseMetadata "Hello, this is a text file.\nWith multiple lines.\n" @?= Nothing

  , testCase "parse handles legacy Hash wrapper format" $ do
      let content = "hash: Hash \"abc123\"\nsize: 100\n"
      case parseMetadata content of
        Just mc -> metaHash mc @?= Hash "abc123"
        Nothing -> assertFailure "Failed to parse legacy format"

  , testCase "parse handles bare quoted format" $ do
      let content = "hash: \"abc123\"\nsize: 100\n"
      case parseMetadata content of
        Just mc -> metaHash mc @?= Hash "abc123"
        Nothing -> assertFailure "Failed to parse bare quoted format"

  , testCase "parse handles raw hash format" $ do
      let content = "hash: md5:abc123def456\nsize: 512\n"
      case parseMetadata content of
        Just mc -> do
          metaHash mc @?= Hash "md5:abc123def456"
          metaSize mc @?= 512
        Nothing -> assertFailure "Failed to parse raw format"

  , testCase "displayHash truncates long hashes" $ do
      let h = Hash "md5:0123456789abcdef0123456789abcdef"
      let displayed = displayHash h
      -- Should be 16 chars + "..."
      length displayed @?= 19

  , testCase "displayHash preserves short hashes" $ do
      let h = Hash "md5:abcdef"
      let displayed = displayHash h
      displayed @?= "md5:abcdef"
  ]

-- ---------------------------------------------------------------------------
-- 5. Conflict marker detection
--    Ensures metadata files with merge conflict markers are caught
--    before they corrupt the metadata index.
-- ---------------------------------------------------------------------------

conflictMarkerTests :: TestTree
conflictMarkerTests = testGroup "Conflict markers"
  [ testCase "serializeMetadata never contains conflict markers" $ do
      let mc = MetaContent (Hash "md5:abc123") 100
      let s = serializeMetadata mc
      assertBool "should not contain <<<<<<<" (not $ "<<<<<<<" `isInfixOf` s)
      assertBool "should not contain =======" (not $ "=======" `isInfixOf` s)
      assertBool "should not contain >>>>>>>" (not $ ">>>>>>>" `isInfixOf` s)
  ]
