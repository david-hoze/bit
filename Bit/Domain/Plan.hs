module Bit.Domain.Plan
  ( RcloneAction(..)
  , resolveSwaps
  ) where

import Bit.Types
import Data.List (foldl')
import qualified Data.Map.Strict as Map

-- Specific instructions to be executed via rclone
data RcloneAction
    = Move Path Path
    | Copy Path Path
    | Delete Path
    | Swap Path Path Path  -- Move via a temporary file (TempPath, Source, Dest)
    deriving (Eq, Show)

-- | Detect mirrored Move pairs (A→B and B→A) and replace each pair with a
-- single Swap action that uses a temporary path to avoid overwriting.
-- Non-paired actions pass through unchanged. Only handles pairwise swaps;
-- longer cycles (A→B→C→A) are left as individual Moves (known limitation).
resolveSwaps :: [RcloneAction] -> [RcloneAction]
resolveSwaps actions =
    let moves = [(src, dest) | Move src dest <- actions]
        moveMap = Map.fromList moves
        -- A swap pair exists when Move A B and Move B A both appear
        swapPairs = [ (a, b)
                    | (a, b) <- moves
                    , Map.lookup b moveMap == Just a
                    , a < b  -- canonical ordering to avoid emitting the pair twice
                    ]
        swappedPaths = foldl' (\acc (a, b) -> Map.insert a () (Map.insert b () acc))
                              Map.empty swapPairs
        -- Drop Move pairs that form swaps, AND redundant Copy actions for
        -- those paths.  A content-swap (A↔B) produces both Modified (Copy)
        -- and Renamed (Move) for the same paths; the Swap is a free
        -- remote-side rename, so keep only the Swap.
        isSwapped action = case action of
            Move src dest -> Map.member src swappedPaths && Map.member dest swappedPaths
            Copy src _    -> Map.member src swappedPaths
            _             -> False
        kept = filter (not . isSwapped) actions
        newSwaps = [ Swap (Path (unPath a <> ".bit-swap-tmp")) a b | (a, b) <- swapPairs ]
    in  kept ++ newSwaps
