{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Data.List (nub)
import Bit.Types
import Bit.Plan (RcloneAction(..))
import Bit.Pipeline (diffAndPlan)
import qualified Data.Text as T

-- Arbitrary instances for property testing

instance Arbitrary (Hash 'MD5) where
  arbitrary = Hash . T.pack . ("md5:" ++) <$> vectorOf 32 (elements "0123456789abcdef")

instance Arbitrary EntryKind where
  arbitrary = do
    h <- arbitrary
    sz <- choose (0, 10000000)
    isText <- arbitrary
    let ct = if isText then TextContent else BinaryContent
    pure $ File h sz ct

instance Arbitrary FileEntry where
  arbitrary = do
    -- Generate simple relative paths
    depth <- choose (1, 3) :: Gen Int
    segments <- vectorOf depth (vectorOf 5 (elements "abcdefghijklmnop"))
    let filePath = foldl1 (\a b -> a ++ "/" ++ b) segments
    k <- arbitrary
    pure $ FileEntry (Path filePath) k

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Pipeline"
  [ testGroup "diffAndPlan properties"
    [ testProperty "identical inputs produce no actions" prop_identicalNoActions
    , testProperty "no duplicate target paths in plan" prop_noDuplicateTargets
    , testProperty "empty source against non-empty target produces only deletes" prop_emptySourceOnlyDeletes
    , testProperty "non-empty source against empty target produces only copies" prop_emptyTargetOnlyCopies
    ]
  , testGroup "diffAndPlan unit tests"
    [ testCase "single added file produces Copy" test_singleAddedFile
    , testCase "single deleted file produces Delete" test_singleDeletedFile
    , testCase "modified file produces Copy (overwrite)" test_modifiedFile
    , testCase "swapped files produce Swap, not two Moves" test_swappedFiles
    , testCase "non-swap rename produces Move" test_nonSwapRename
    , testCase "three-way cycle stays as Moves" test_threeWayCycle
    ]
  , testGroup "resolveSwaps properties"
    [ testProperty "swapped paths produce exactly one Swap" prop_swapDetection
    ]
  ]

-- Properties

prop_identicalNoActions :: [FileEntry] -> Bool
prop_identicalNoActions files =
  null (diffAndPlan files files)

prop_noDuplicateTargets :: [FileEntry] -> [FileEntry] -> Bool
prop_noDuplicateTargets source target =
  let actions = diffAndPlan source target
      targets = concatMap actionTargets actions
  in  length targets == length (nub targets)
  where
    actionTargets (Copy _ d)    = [d]
    actionTargets (Move _ d)    = [d]
    actionTargets (Delete p)    = [p]
    actionTargets (Swap _ s d)  = [s, d]

prop_emptySourceOnlyDeletes :: [FileEntry] -> Bool
prop_emptySourceOnlyDeletes target =
  all isDelete (diffAndPlan [] target)
  where
    isDelete (Delete _) = True
    isDelete _          = False

prop_emptyTargetOnlyCopies :: [FileEntry] -> Bool
prop_emptyTargetOnlyCopies source =
  all isCopyOrMove (diffAndPlan source [])
  where
    isCopyOrMove (Copy _ _) = True
    isCopyOrMove (Move _ _) = True
    isCopyOrMove _          = False

-- Unit tests

test_singleAddedFile :: Assertion
test_singleAddedFile = do
  let h = Hash (T.pack "md5:abc123")
      source = [FileEntry "foo.bin" (File h 100 BinaryContent)]
      target = []
      actions = diffAndPlan source target
  case actions of
    [Copy "foo.bin" "foo.bin"] -> pure ()
    _ -> assertFailure $ "Expected [Copy], got: " ++ show actions

test_singleDeletedFile :: Assertion
test_singleDeletedFile = do
  let h = Hash (T.pack "md5:abc123")
      source = []
      target = [FileEntry "foo.bin" (File h 100 BinaryContent)]
      actions = diffAndPlan source target
  case actions of
    [Delete "foo.bin"] -> pure ()
    _ -> assertFailure $ "Expected [Delete], got: " ++ show actions

test_modifiedFile :: Assertion
test_modifiedFile = do
  let h1 = Hash (T.pack "md5:aaa")
      h2 = Hash (T.pack "md5:bbb")
      source = [FileEntry "foo.bin" (File h1 100 BinaryContent)]
      target = [FileEntry "foo.bin" (File h2 200 BinaryContent)]
      actions = diffAndPlan source target
  case actions of
    [Copy "foo.bin" "foo.bin"] -> pure ()
    _ -> assertFailure $ "Expected [Copy (overwrite)], got: " ++ show actions

-- | Two files swap paths: a.txt gets b.txt's hash, b.txt gets a.txt's hash.
-- Should produce a single Swap, not two Moves.
test_swappedFiles :: Assertion
test_swappedFiles = do
  let hX = Hash (T.pack "md5:xxxx")
      hY = Hash (T.pack "md5:yyyy")
      source = [ FileEntry "a.txt" (File hY 100 BinaryContent)
               , FileEntry "b.txt" (File hX 200 BinaryContent) ]
      target = [ FileEntry "a.txt" (File hX 100 BinaryContent)
               , FileEntry "b.txt" (File hY 200 BinaryContent) ]
      actions = diffAndPlan source target
      swaps = [() | Swap {} <- actions]
      moves = [() | Move {} <- actions]
  assertEqual "expected exactly one Swap" 1 (length swaps)
  assertEqual "expected no Moves" 0 (length moves)

-- | A rename that is NOT a swap: A→B with no B→A.
test_nonSwapRename :: Assertion
test_nonSwapRename = do
  let h = Hash (T.pack "md5:abc123")
      source = [FileEntry "new.bin" (File h 100 BinaryContent)]
      target = [FileEntry "old.bin" (File h 100 BinaryContent)]
      actions = diffAndPlan source target
  case actions of
    [Move "old.bin" "new.bin"] -> pure ()
    _ -> assertFailure $ "Expected [Move old->new], got: " ++ show actions

-- | Three-way cycle (A→B, B→C, C→A) — resolveSwaps leaves these as Moves
-- since Swap only handles pairwise swaps.
test_threeWayCycle :: Assertion
test_threeWayCycle = do
  let hA = Hash (T.pack "md5:aaaa")
      hB = Hash (T.pack "md5:bbbb")
      hC = Hash (T.pack "md5:cccc")
      -- Source has: a→hB, b→hC, c→hA (rotated)
      source = [ FileEntry "a.txt" (File hB 100 BinaryContent)
               , FileEntry "b.txt" (File hC 100 BinaryContent)
               , FileEntry "c.txt" (File hA 100 BinaryContent) ]
      -- Target has: a→hA, b→hB, c→hC (original)
      target = [ FileEntry "a.txt" (File hA 100 BinaryContent)
               , FileEntry "b.txt" (File hB 100 BinaryContent)
               , FileEntry "c.txt" (File hC 100 BinaryContent) ]
      actions = diffAndPlan source target
      swaps = [() | Swap {} <- actions]
  -- Three-way cycles cannot be detected by pairwise swap detection.
  -- They should remain as individual Moves (known limitation).
  assertEqual "expected no Swaps for three-way cycle" 0 (length swaps)

-- | Property: when source and target have the same paths but with two paths'
-- contents swapped, diffAndPlan produces exactly one Swap.
prop_swapDetection :: Property
prop_swapDetection = forAll genSwapPair $ \(source, target) ->
  let actions = diffAndPlan source target
      swaps = [() | Swap {} <- actions]
  in  length swaps === 1
  where
    genSwapPair :: Gen ([FileEntry], [FileEntry])
    genSwapPair = do
      h1 <- arbitrary
      h2 <- arbitrary `suchThat` (/= h1)
      sz1 <- choose (0, 10000000)
      sz2 <- choose (0, 10000000)
      let ct = BinaryContent
          source = [ FileEntry "swap_a" (File h1 sz1 ct)
                   , FileEntry "swap_b" (File h2 sz2 ct) ]
          target = [ FileEntry "swap_a" (File h2 sz2 ct)
                   , FileEntry "swap_b" (File h1 sz1 ct) ]
      pure (source, target)
