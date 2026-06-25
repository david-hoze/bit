{-# LANGUAGE DataKinds #-}

-- | Content-addressed store: .bit/cas/<prefix>/<hash> (normal) or bit/cas/ (bare).
-- Blob path uses first 2 chars of hash hex (after "md5:") as prefix.
module Bit.CAS
  ( casBlobPath
  , stripHashPrefix
  , writeBlobToCas
  , hasBlobInCas
  , copyBlobFromCasTo
  ) where

import System.FilePath ((</>), takeDirectory)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Bit.Types (Hash(..), HashAlgo(..), hashToText)
import Bit.Utils (atomicWriteFile)

-- | Path to a blob in CAS: casDir/prefix/hex (spec: blob filename is the content hash, plain hex).
-- Hash format is "<algo>:hex" (e.g. "md5:..." or "blake3:..."); we strip any
-- algorithm prefix so the filename is plain hex regardless of algorithm.
casBlobPath :: FilePath -> Hash 'MD5 -> FilePath
casBlobPath casDir h =
  let hex = stripHashPrefix (T.unpack (hashToText h))
      prefix = take 2 hex
  in casDir </> prefix </> hex

-- | Drop a leading @<algo>:@ tag (e.g. @md5:@, @blake3:@) from a hash's text,
-- leaving the plain hex used as the CAS filename. Hex itself never contains a
-- colon, so splitting on the first colon is unambiguous.
stripHashPrefix :: String -> String
stripHashPrefix s = case break (== ':') s of
  (_, ':' : rest) -> rest
  _               -> s

-- | Write a file's content to CAS keyed by its hash. Idempotent (overwrites if exists).
writeBlobToCas :: FilePath -> FilePath -> Hash 'MD5 -> IO ()
writeBlobToCas sourcePath casDir h = do
  let dest = casBlobPath casDir h
  createDirectoryIfMissing True (takeDirectory dest)
  content <- BS.readFile sourcePath
  atomicWriteFile dest content

-- | Check whether CAS contains a blob for the given hash.
hasBlobInCas :: FilePath -> Hash 'MD5 -> IO Bool
hasBlobInCas casDir h = doesFileExist (casBlobPath casDir h)

-- | Copy blob from CAS to destination path. Returns False if blob not in CAS.
copyBlobFromCasTo :: FilePath -> Hash 'MD5 -> FilePath -> IO Bool
copyBlobFromCasTo casDir h destPath = do
  let src = casBlobPath casDir h
  exists <- doesFileExist src
  if not exists
    then pure False
    else do
      createDirectoryIfMissing True (takeDirectory destPath)
      content <- BS.readFile src
      atomicWriteFile destPath content
      pure True
