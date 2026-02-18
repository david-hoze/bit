{-# LANGUAGE DataKinds #-}

-- | CDC chunk store: write chunked blobs to CAS, check chunk availability.
module Bit.CDC.Store
  ( writeChunkedBlobToCas
  , hasAllChunksInCas
  ) where

import qualified Data.ByteString as BS
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Bit.CAS (casBlobPath)
import Bit.CDC.Types (ChunkConfig, Chunk(..), ChunkRef(..), ChunkManifest(..))
import Bit.CDC.FastCDC (chunkFile)
import Bit.CDC.Manifest (buildManifest, writeManifestToCas)
import Bit.Types (Hash, HashAlgo(..))
import Bit.Utils (atomicWriteFile)

-- | Chunk a file and write each chunk + manifest to CAS.
-- Each chunk is stored as a separate blob keyed by its MD5 hash.
-- A manifest file maps the original file hash to its chunks.
writeChunkedBlobToCas :: FilePath -> FilePath -> ChunkConfig -> Hash 'MD5 -> IO ()
writeChunkedBlobToCas sourcePath casDir cfg fileHash = do
  chunks <- chunkFile cfg sourcePath
  -- Read the source file to extract chunk data
  content <- BS.readFile sourcePath
  let fileSize = fromIntegral (BS.length content)
  -- Write each chunk blob to CAS
  mapM_ (writeChunk content) chunks
  -- Write manifest
  let manifest = buildManifest fileHash fileSize chunks
  writeManifestToCas casDir fileHash manifest
  where
    writeChunk content chunk = do
      let off  = fromIntegral (chunkOffset chunk)
          len  = chunkLength chunk
          cbs  = BS.take len (BS.drop off content)
          dest = casBlobPath casDir (chunkHash chunk)
      createDirectoryIfMissing True (takeDirectory dest)
      atomicWriteFile dest cbs

-- | Check if CAS contains all chunks referenced by a manifest.
hasAllChunksInCas :: FilePath -> ChunkManifest -> IO Bool
hasAllChunksInCas casDir manifest =
  allM (\cr -> doesFileExist (casBlobPath casDir (crHash cr))) (cmChunks manifest)
  where
    allM _ [] = pure True
    allM p (x:xs) = do
      ok <- p x
      if ok then allM p xs else pure False
