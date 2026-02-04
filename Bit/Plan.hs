{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bit.Plan
  ( RcloneAction(..)
  , planAction
  ) where

import Bit.Diff (GitDiff(..), LightFileEntry(..))
import Bit.Types

-- Specific instructions to be executed via rclone
data RcloneAction
    = Move Path Path
    | Copy Path Path
    | Delete Path
    | Swap Path Path Path  -- Move via a temporary file (TempPath, Source, Dest)
    deriving (Eq, Show)

-- | The Planner: Translates abstract Diffs into concrete Rclone actions
planAction :: GitDiff -> RcloneAction
planAction (Modified f)      = Copy f.filePath f.filePath -- Upload over existing
planAction (Renamed old new) = Move old.filePath new.filePath
planAction (Added f)         = Copy f.filePath f.filePath
planAction (Deleted f)       = Delete f.filePath

-- | Safety Planner: Logic to handle complex swaps using temporary paths
-- Thanks to ADTs, the system always knows if a temp file is required
makeSwapPlan :: Path -> Path -> [RcloneAction]
makeSwapPlan pathA pathB =
    let tempPath = pathA ++ ".tmp"
    in [ Swap tempPath pathA pathB ]

