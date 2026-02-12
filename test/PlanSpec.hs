{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Bit.Types (Path(..), unPath)
import Bit.Plan (RcloneAction(..), resolveSwaps)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Plan"
  [ testGroup "resolveSwaps unit tests"
    [ testCase "swapped files produce Swap, not two Moves" test_swappedFiles
    , testCase "non-swap rename produces Move" test_nonSwapRename
    , testCase "three-way cycle stays as Moves" test_threeWayCycle
    ]
  , testGroup "resolveSwaps properties"
    [ testProperty "swapped Move pairs produce exactly one Swap" prop_swapDetection
    ]
  ]

-- Unit tests

-- | Two files swap paths: Move a→b and Move b→a.
-- Should produce a single Swap, not two Moves.
test_swappedFiles :: Assertion
test_swappedFiles = do
  let actions = resolveSwaps
        [ Move "a.txt" "b.txt"
        , Move "b.txt" "a.txt"
        ]
      swaps = [() | Swap {} <- actions]
      moves = [() | Move {} <- actions]
  assertEqual "expected exactly one Swap" 1 (length swaps)
  assertEqual "expected no Moves" 0 (length moves)

-- | A rename that is NOT a swap: A→B with no B→A.
test_nonSwapRename :: Assertion
test_nonSwapRename = do
  let actions = resolveSwaps [Move "old.bin" "new.bin"]
  case actions of
    [Move "old.bin" "new.bin"] -> pure ()
    _ -> assertFailure $ "Expected [Move old->new], got: " ++ show actions

-- | Three-way cycle (A→B, B→C, C→A) — resolveSwaps leaves these as Moves
-- since Swap only handles pairwise swaps.
test_threeWayCycle :: Assertion
test_threeWayCycle = do
  let actions = resolveSwaps
        [ Move "a.txt" "b.txt"
        , Move "b.txt" "c.txt"
        , Move "c.txt" "a.txt"
        ]
      swaps = [() | Swap {} <- actions]
  -- Three-way cycles cannot be detected by pairwise swap detection.
  -- They should remain as individual Moves (known limitation).
  assertEqual "expected no Swaps for three-way cycle" 0 (length swaps)

-- Properties

-- | Property: when given two mirrored Move actions (A→B and B→A),
-- resolveSwaps produces exactly one Swap.
prop_swapDetection :: Property
prop_swapDetection = forAll genSwapPair $ \actions ->
  let resolved = resolveSwaps actions
      swaps = [() | Swap {} <- resolved]
  in  length swaps === 1
  where
    genSwapPair :: Gen [RcloneAction]
    genSwapPair = do
      a <- Path <$> genSimplePath
      b <- Path <$> genSimplePath `suchThat` (/= unPath a)
      pure [Move a b, Move b a]

    genSimplePath :: Gen FilePath
    genSimplePath = do
      depth <- choose (1, 3) :: Gen Int
      segments <- vectorOf depth (vectorOf 5 (elements "abcdefghijklmnop"))
      pure $ foldl1 (\x y -> x ++ "/" ++ y) segments
