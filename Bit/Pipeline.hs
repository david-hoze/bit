{-# LANGUAGE DataKinds #-}

module Bit.Pipeline
  ( -- * Pure core (property-testable)
    diffAndPlan
    -- * Composed pipelines
  , pushSyncFiles
  , pullSyncFiles
  ) where

import Bit.Types
import Bit.Diff (buildIndexFromFileEntries, computeDiff, GitDiff)
import Bit.Plan (RcloneAction(..), planAction)
import Bit.Utils (filterOutBitPaths)

-- | Pure core: given source-of-truth files and current target files,
-- produce the list of actions to make target match source.
-- This is the entire "diff >>> plan" section — no IO, fully property-testable.
-- For push: local is "source of truth", remote is "target to update".
-- For pull: remote is "source of truth", local is "target to update".
diffAndPlan :: [FileEntry] -> [FileEntry] -> [RcloneAction]
diffAndPlan sourceFiles targetFiles =
  let sourceIndex = buildIndexFromFileEntries sourceFiles
      targetIndex = buildIndexFromFileEntries targetFiles
      diffs = computeDiff sourceIndex targetIndex
  in  map planAction diffs

-- | Push pipeline: compute actions to make remote match local.
-- Takes pre-scanned local and remote file lists, returns planned actions.
pushSyncFiles :: [FileEntry] -> [FileEntry] -> [RcloneAction]
pushSyncFiles localFiles remoteFiles =
  diffAndPlan localFiles (filterOutBitPaths remoteFiles)

-- | Pull pipeline: compute actions to make local match remote.
-- Note the reversed argument order: remote is source of truth, local is target.
pullSyncFiles :: [FileEntry] -> [FileEntry] -> [RcloneAction]
pullSyncFiles localFiles remoteFiles =
  diffAndPlan (filterOutBitPaths remoteFiles) localFiles
