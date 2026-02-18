{-# LANGUAGE DataKinds #-}

-- | Reassemble a file from its CDC chunks stored in CAS.
module Bit.CDC.Reassemble
  ( reassembleFile
  , verifyReassembledFile
  ) where

import qualified Data.ByteString as BS
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

import Bit.CAS (casBlobPath)
import Bit.CDC.Types (ChunkRef(..), ChunkManifest(..))
import Bit.Config.Metadata (hashFileBytes)
import Bit.Types (hashToText)

-- | Reassemble a file from chunks in CAS. Reads each chunk blob in order
-- and concatenates them to the destination path.
-- Returns False if any chunk is missing from CAS.
reassembleFile :: FilePath -> ChunkManifest -> FilePath -> IO Bool
reassembleFile casDir manifest destPath = do
  createDirectoryIfMissing True (takeDirectory destPath)
  go (cmChunks manifest) []
  where
    go [] acc = do
      BS.writeFile destPath (BS.concat (reverse acc))
      pure True
    go (cr:crs) acc = do
      let chunkPath = casBlobPath casDir (crHash cr)
      exists <- doesFileExist chunkPath
      if not exists
        then pure False
        else do
          content <- BS.readFile chunkPath
          go crs (content : acc)

-- | Reassemble and verify that the result matches the expected file hash.
verifyReassembledFile :: FilePath -> ChunkManifest -> FilePath -> IO Bool
verifyReassembledFile casDir manifest destPath = do
  ok <- reassembleFile casDir manifest destPath
  if not ok
    then pure False
    else do
      content <- BS.readFile destPath
      let actualHash = hashFileBytes content
      pure (hashToText actualHash == hashToText (cmFileHash manifest))
