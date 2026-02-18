{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Manifest I/O: serialize/parse chunk manifests, read/write them in CAS.
-- A manifest describes how a file is split into content-defined chunks.
-- Manifest path: casBlobPath casDir fileHash ++ ".manifest"
module Bit.CDC.Manifest
  ( writeManifest
  , parseManifest
  , buildManifest
  , casManifestPath
  , isChunkedInCas
  , readManifestFromCas
  , writeManifestToCas
  ) where

import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import qualified Data.Text as T

import Bit.Types (Hash(..), HashAlgo(..), hashToText)
import Bit.CAS (casBlobPath)
import Bit.CDC.Types (Chunk(..), ChunkRef(..), ChunkManifest(..))

-- | Serialize a manifest to text format.
-- Format:
--   file-hash: <hash>
--   file-size: <size>
--   chunk-count: <count>
--   <chunk-hash> <chunk-length>
--   <chunk-hash> <chunk-length>
--   ...
writeManifest :: ChunkManifest -> String
writeManifest cm =
  unlines $
    [ "file-hash: " ++ T.unpack (hashToText (cmFileHash cm))
    , "file-size: " ++ show (cmFileSize cm)
    , "chunk-count: " ++ show (cmChunkCount cm)
    ] ++ map chunkLine (cmChunks cm)
  where
    chunkLine cr = T.unpack (hashToText (crHash cr)) ++ " " ++ show (crLength cr)

-- | Parse a manifest from text format. Returns Nothing on invalid format.
parseManifest :: String -> Maybe ChunkManifest
parseManifest content = do
  let ls = lines content
  fileHashStr <- extractValue "file-hash: " ls
  fileSizeStr <- extractValue "file-size: " ls
  chunkCountStr <- extractValue "chunk-count: " ls
  fileSize <- readMaybe fileSizeStr
  chunkCount <- readMaybe chunkCountStr
  let chunkLines = drop 3 (filter (not . null) ls)
      chunks = map parseChunkLine chunkLines
  if length chunks == chunkCount && all validChunk chunks
    then Just ChunkManifest
      { cmFileHash = Hash (T.pack fileHashStr)
      , cmFileSize = fileSize
      , cmChunkCount = chunkCount
      , cmChunks = map fromJustChunk chunks
      }
    else Nothing
  where
    extractValue prefix ls =
      listToMaybe [drop (length prefix) l | l <- ls, prefix `isPrefixOf` l]
    readMaybe s = case reads s of
      [(n, "")] -> Just n
      [(n, r)] | all (`elem` (" \t\r\n" :: String)) r -> Just n
      _ -> Nothing
    parseChunkLine l =
      case words l of
        [h, sz] -> case readMaybe sz of
          Just s -> Just (ChunkRef (Hash (T.pack h)) s)
          Nothing -> Nothing
        _ -> Nothing
    validChunk = maybe False (const True)
    fromJustChunk (Just c) = c
    fromJustChunk Nothing = error "parseManifest: impossible"

-- | Build a manifest from chunking results.
buildManifest :: Hash 'MD5 -> Int64 -> [Chunk] -> ChunkManifest
buildManifest fileHash fileSize chunks = ChunkManifest
  { cmFileHash = fileHash
  , cmFileSize = fileSize
  , cmChunkCount = length chunks
  , cmChunks = map (\c -> ChunkRef (chunkHash c) (chunkLength c)) chunks
  }

-- | Path to a manifest file in CAS: same as blob path but with ".manifest" suffix.
casManifestPath :: FilePath -> Hash 'MD5 -> FilePath
casManifestPath casDir h = casBlobPath casDir h ++ ".manifest"

-- | Check if a file hash has a chunked representation in CAS.
isChunkedInCas :: FilePath -> Hash 'MD5 -> IO Bool
isChunkedInCas casDir h = doesFileExist (casManifestPath casDir h)

-- | Read a manifest from CAS for the given file hash.
readManifestFromCas :: FilePath -> Hash 'MD5 -> IO (Maybe ChunkManifest)
readManifestFromCas casDir h = do
  let path = casManifestPath casDir h
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      content <- readFile path
      pure (parseManifest content)

-- | Write a manifest to CAS for the given file hash.
writeManifestToCas :: FilePath -> Hash 'MD5 -> ChunkManifest -> IO ()
writeManifestToCas casDir h cm = do
  let path = casManifestPath casDir h
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path (writeManifest cm)
