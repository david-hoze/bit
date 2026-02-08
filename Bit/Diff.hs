{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bit.Diff
  ( GitDiff(..)
  , LightFileEntry(..)
  , FileIndex(..)
  , buildIndexFromFileEntries
  , computeDiff
  , formatDiff
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)
import Bit.Types

-- Lightweight identity for planning
data LightFileEntry = LightFileEntry
  { filePath :: Path
  , fileHash :: Hash 'MD5
  } deriving (Eq, Show)

data GitDiff
  = Renamed LightFileEntry LightFileEntry
  | Added LightFileEntry
  | Deleted LightFileEntry
  | Modified LightFileEntry
  deriving (Eq, Show)

type FileMap = Map.Map Path FileEntry

data FileIndex = FileIndex
  { byPath :: FileMap
  , byHash :: Map.Map (Hash 'MD5) (Set.Set Path)
  }

buildIndexFromFileEntries :: [FileEntry] -> FileIndex
buildIndexFromFileEntries files =
    FileIndex
      { byPath = Map.fromList [(entry.path, entry) | entry <- files]
      , byHash = Map.fromListWith Set.union
          [ (h, Set.singleton entry.path)
          | entry <- files
          , h <- case entry.kind of
              File h _ _ -> [h]
              _          -> []
          ]
      }

-- Invariant:
-- FileMap must have paths normalized and hashIndex consistent with files
computeDiff :: FileIndex -> FileIndex -> [GitDiff]
computeDiff local remote =
    modified ++ added ++ deleted ++ renamed
  where
    lFiles :: Map.Map Path (Hash 'MD5)
    lFiles = Map.fromList
      [ (entry.path, h)
      | entry <- Map.elems local.byPath
      , h <- case entry.kind of
          File h _ _ -> [h]
          _          -> []
      ]
    rFiles :: Map.Map Path (Hash 'MD5)
    rFiles = Map.fromList
      [ (entry.path, h)
      | entry <- Map.elems remote.byPath
      , h <- case entry.kind of
          File h _ _ -> [h]
          _          -> []
      ]

    lPaths = Map.keysSet local.byPath
    rPaths = Map.keysSet remote.byPath
    lFilePaths = Map.keysSet lFiles
    rFilePaths = Map.keysSet rFiles

    -- 1. Modified: same path, different hash (only for file paths; directories have no hash)
    modified =
      [ Modified (LightFileEntry p lHash)
      | p <- Set.toList (Set.intersection lFilePaths rFilePaths)
      , let lHash = lFiles Map.! p
      , let rHash = rFiles Map.! p
      , lHash /= rHash
      ]

    -- 2. Added: path exists only locally
    added =
      [ Added (LightFileEntry p hsh)
      | (p, hsh) <- Map.toList lFiles
      , p `Set.notMember` rPaths
      , not (Map.member hsh remote.byHash)
      ]

    -- 3. Deleted: path exists only remotely
    deleted =
      [ Deleted (LightFileEntry p hsh)
      | (p, hsh) <- Map.toList rFiles
      , p `Set.notMember` lPaths
      , not (Map.member hsh local.byHash)
      ]

    -- 4. Renamed: same hash, different path (1:1 only; otherwise we'd emit multiple Move for same source)
    renamed =
      [ Renamed (LightFileEntry oldPath hash) (LightFileEntry newPath hash)
      | (hash, oldPathSet) <- Map.toList remote.byHash
      , oldPath <- Set.toList oldPathSet
      , let localPathsWithHash = Set.filter (/= oldPath) (Map.findWithDefault Set.empty hash local.byHash)
      , Set.size localPathsWithHash == 1
      , newPath <- maybe [] (:[]) (listToMaybe (Set.toList localPathsWithHash))
      , Map.member newPath lFiles
      ]

formatDiff :: GitDiff -> String
formatDiff (Added f)      = "[+] Added:    " ++ unPath f.filePath
formatDiff (Deleted f)    = "[-] Deleted:  " ++ unPath f.filePath
formatDiff (Modified f)   = "[*] Modified: " ++ unPath f.filePath
formatDiff (Renamed o n)  = "[M] Moved:    " ++ unPath o.filePath ++ " -> " ++ unPath n.filePath
