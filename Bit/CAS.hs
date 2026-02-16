{-# LANGUAGE DataKinds #-}

-- | Content-addressed store: .bit/cas/<prefix>/<hash> (normal) or bit/cas/ (bare).
-- Blob path uses first 2 chars of hash hex (after "md5:") as prefix.
module Bit.CAS
  ( casBlobPath
  , writeBlobToCas
  , hasBlobInCas
  , copyBlobFromCasTo
  ) where

import Data.List (isPrefixOf)
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Bit.Types (Hash(..), HashAlgo(..), hashToText)
import Bit.Utils (atomicWriteFile)

-- | Path to a blob in CAS: casDir/prefix/hex (spec: blob filename is the content hash, plain hex).
-- Hash format is "md5:hex"; we strip the "md5:" prefix so the filename is plain hex.
casBlobPath :: FilePath -> Hash 'MD5 -> FilePath
casBlobPath casDir h =
  let raw = T.unpack (hashToText h)
      hex = if "md5:" `isPrefixOf` raw then drop 4 raw else raw
      prefix = take 2 hex
  in casDir </> prefix </> hex

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
