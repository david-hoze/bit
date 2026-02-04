{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Scan
  ( scanWorkingDir
  , writeMetadataFiles
  , readMetadataFile
  , listMetadataPaths
  , getFileHashAndSize
  , FileEntry(..)
  , EntryKind(..)
  ) where

import Bit.Types
import System.FilePath
import System.Directory
    ( doesDirectoryExist,
      doesFileExist,
      listDirectory,
      getFileSize,
      createDirectoryIfMissing,
      copyFileWithMetadata )
import Data.List
import qualified Data.ByteString as BS
import Data.Text (unpack)
import Control.Monad
import Data.Text.Encoding (decodeUtf8')
import Data.Char (toLower)
import qualified Internal.ConfigFile as ConfigFile
import Bit.Utils (atomicWriteFileStr)
import Bit.Internal.Metadata (MetaContent(..), readMetadataOrComputeHash, hashFile, serializeMetadata)

-- Binary file extensions that should never be treated as text (hardcoded, not configurable)
binaryExtensions :: [String]
binaryExtensions = [".mp4", ".zip", ".bin", ".exe", ".dll", ".so", ".dylib", ".jpg", ".jpeg", ".png", ".gif", ".pdf", ".gz", ".bz2", ".xz", ".tar", ".rar", ".7z", ".iso", ".img", ".dmg", ".deb", ".rpm", ".msi"]

-- | Classify a file as text or binary based on heuristics:
-- 1. Size < configured limit (from .rgit/config)
-- 2. No NULL bytes in first 8KB
-- 3. Valid UTF-8 (or ASCII subset)
-- 4. Not in binary extension list
-- 5. Extension matches configured text extensions (optional hint)
classifyFile :: FilePath -> Integer -> IO Bool
classifyFile filePath size = do
    config <- ConfigFile.readTextConfig
    -- Check size limit first (fast path)
    if size >= ConfigFile.textSizeLimit config
        then return False
        else do
            -- Check extension
            let ext = map toLower (takeExtension filePath)
            -- Binary extensions always win
            if ext `elem` binaryExtensions
                then return False
                else do
                    -- Read first 8KB and check for NULL bytes and UTF-8 validity
                    content <- BS.readFile filePath
                    let sample = BS.take 8192 content
                    -- Check for NULL bytes
                    if BS.elem 0 sample
                        then return False
                        else do
                            -- Check UTF-8 validity
                            case decodeUtf8' sample of
                                Left _ -> return False  -- Invalid UTF-8
                                Right _ -> return True   -- Valid text file

-- Main scan function
scanWorkingDir :: FilePath -> IO [FileEntry]
scanWorkingDir root = go root
  where
    go :: FilePath -> IO [FileEntry]
    go path = do
      isDir <- doesDirectoryExist path
      let rel = makeRelative root path

      -- ignore .bit folder and .git (git metadata / pointer)
      if rel == ".bit" || (".bit" `isPrefixOf` rel)
          || rel == ".git" || (".git" `isPrefixOf` rel)
        then pure []
        else if isDir
          then do
            names <- listDirectory path
            let children = map (path </>) names
            childEntries <- concat <$> mapM go children
            let dirEntry = FileEntry { path = rel, kind = Directory }
            pure (dirEntry : childEntries)
        else do
          h <- hashFile path
          size <- getFileSize path
          isText <- classifyFile path (fromIntegral size)
          let fEntry = FileEntry
                { path = rel
                , kind = File { fHash = h, fSize = fromIntegral size, fIsText = isText }
                }
          pure [fEntry]

writeMetadataFiles :: FilePath -> [FileEntry] -> IO ()
writeMetadataFiles root entries = do
    let metaRoot = root </> ".bit/index"
    createDirectoryIfMissing True metaRoot

    forM_ entries $ \entry ->
      case kind entry of
        Directory -> do
          let dirPath = metaRoot </> path entry
          createDirectoryIfMissing True dirPath

        File { fHash, fSize, fIsText } -> do
          let metaPath = metaRoot </> path entry
          createDirectoryIfMissing True (takeDirectory metaPath)
          
          if fIsText
            then do
              -- For text files, copy the actual content directly
              let actualPath = root </> path entry
              copyFileWithMetadata actualPath metaPath
            else do
              -- For binary files, write metadata (hash + size). Spec: raw hash value; atomic write.
              atomicWriteFileStr metaPath $
                serializeMetadata (MetaContent fHash fSize)

-- | Parse a metadata file (hash/size lines) or read a text file and compute hash/size.
-- Returns Nothing if file is missing or invalid.
-- Text files in .rgit/index/ contain actual content; binary files contain metadata.
readMetadataFile :: FilePath -> IO (Maybe (Hash 'MD5, Integer))
readMetadataFile fp = fmap (\mc -> (metaHash mc, metaSize mc)) <$> readMetadataOrComputeHash fp

-- | List all metadata file paths under index dir, relative to index root. Excludes .gitattributes.
listMetadataPaths :: FilePath -> IO [FilePath]
listMetadataPaths indexRoot = go indexRoot ""
  where
    go :: FilePath -> FilePath -> IO [FilePath]
    go full rel = do
      isDir <- doesDirectoryExist full
      if isDir
        then do
          names <- listDirectory full
          let skip name = name == "." || name == ".." || name == ".gitattributes" || name == ".git"
          let children = [ (full </> name, if null rel then name else rel </> name) | name <- names, not (skip name) ]
          concat <$> mapM (\(p, r) -> go p r) children
        else do
          isFile <- doesFileExist full
          return (if isFile then [rel] else [])

-- | Get hash and size of a file. Returns Nothing if file is missing or not a regular file.
getFileHashAndSize :: FilePath -> FilePath -> IO (Maybe (Hash 'MD5, Integer))
getFileHashAndSize root relPath = do
  let full = root </> relPath
  exists <- doesFileExist full
  if not exists then return Nothing
  else do
    h <- hashFile full
    sz <- getFileSize full
    return (Just (h, fromIntegral sz))
