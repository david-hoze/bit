{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Bit.Internal.Metadata
  ( MetaContent(..)
  , serializeMetadata
  , parseMetadata
  , parseMetadataFile
  , readMetadataOrComputeHash
  , displayHash
  , hashFileBytes
  , hashFile
  , hasConflictMarkers
  , validateMetadataDir
  , listAllFiles
  ) where

import Bit.Types (Hash(..), HashAlgo(..), hashToText)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO (withFile, IOMode(ReadMode), hIsEOF)
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (listToMaybe)
import Control.Monad (filterM)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)

-- | The single source of truth for what a metadata file contains.
data MetaContent = MetaContent
  { metaHash :: Hash 'MD5
  , metaSize :: Integer
  } deriving (Show, Eq)

-- | Serialize metadata to canonical format. Total function.
-- Format: "hash: <raw_hash>\nsize: <size>\n"
serializeMetadata :: MetaContent -> String
serializeMetadata mc =
  "hash: " ++ T.unpack (hashToText (metaHash mc)) ++ "\n"
  ++ "size: " ++ show (metaSize mc) ++ "\n"

-- | Parse metadata from string content. Pure function.
-- Accepts both raw hash values and legacy Hash "..." wrappers.
-- Returns Nothing if content doesn't look like metadata (e.g. it's a text file's content).
parseMetadata :: String -> Maybe MetaContent
parseMetadata content = do
  let ls = lines content
  hashLine <- listToMaybe [ drop (length ("hash: " :: String)) l
                          | l <- ls, "hash: " `isPrefixOf` l ]
  sizeLine <- listToMaybe [ drop (length ("size: " :: String)) l
                          | l <- ls, "size: " `isPrefixOf` l ]
  let hashVal = cleanHash (trim hashLine)
  size <- readMaybeInt (trim sizeLine)
  if null hashVal
    then Nothing
    else Just MetaContent
      { metaHash = Hash (T.pack hashVal)
      , metaSize = size
      }
  where
    -- Strip legacy wrappers: Hash "...", bare quotes "...", or raw value
    cleanHash s
      | "Hash \"" `isPrefixOf` s =
          let rest = drop (length ("Hash \"" :: String)) s
          in if not (null rest) && last rest == '"' then init rest else rest
      | length s >= 2 && head s == '"' && last s == '"' = init (tail s)
      | otherwise = s
    trim = dropWhile isSpaceChar . reverse . dropWhile isSpaceChar . reverse
    isSpaceChar c = c == ' ' || c == '\t'
    readMaybeInt s = case reads s of
      [(n, "")] -> Just n
      [(n, r)] | all isSpaceChar r -> Just n
      _ -> Nothing

-- | Read + parse a metadata file. Returns Nothing if file doesn't exist or is not valid metadata.
parseMetadataFile :: FilePath -> IO (Maybe MetaContent)
parseMetadataFile fp = do
  exists <- doesFileExist fp
  if not exists
    then pure Nothing
    else do
      bs <- BS.readFile fp
      case decodeUtf8' bs of
        Left _ -> pure Nothing  -- Binary file, not valid metadata
        Right txt -> pure (parseMetadata (T.unpack txt))

-- | Read a metadata file OR (if it's a text file whose content is stored directly)
-- compute hash/size from the file bytes. This is the replacement for the fallback
-- logic in Rgit.Scan.readMetadataFile and Rgit.Verify.loadBinaryMetadata.
readMetadataOrComputeHash :: FilePath -> IO (Maybe MetaContent)
readMetadataOrComputeHash fp = do
  exists <- doesFileExist fp
  if not exists
    then pure Nothing
    else do
      bs <- BS.readFile fp
      case decodeUtf8' bs of
        Left _ -> do
          -- Binary file — compute hash from bytes
          let h = hashFileBytes bs
              sz = fromIntegral (BS.length bs)
          pure (Just (MetaContent h sz))
        Right txt ->
          case parseMetadata (T.unpack txt) of
            Just mc -> pure (Just mc)
            Nothing -> do
              -- Not a metadata file — treat as text file content, compute hash from bytes
              let h = hashFileBytes bs
                  sz = fromIntegral (BS.length bs)
              pure (Just (MetaContent h sz))

-- | Truncate hash for human-readable display.
-- Shows first 16 chars + "..." if longer.
displayHash :: Hash 'MD5 -> String
displayHash h =
  let s = T.unpack (hashToText h)
  in take 16 s ++ if length s > 16 then "..." else ""

-- | Compute MD5 hash of raw bytes. Single source of truth for hashing.
hashFileBytes :: BS.ByteString -> Hash 'MD5
hashFileBytes bs =
  let md5hex = decodeUtf8 (encode (MD5.hash bs))
  in Hash (T.pack "md5:" <> md5hex)

-- | Compute MD5 hash of a file on disk using streaming (constant memory).
-- Reads file in 64KB chunks to avoid loading entire file into RAM.
hashFile :: FilePath -> IO (Hash 'MD5)
hashFile fp = withFile fp ReadMode $ \handle -> do
  let loop !ctx = do
        eof <- hIsEOF handle
        if eof
          then do
            let md5hex = decodeUtf8 (encode (MD5.finalize ctx))
            return (Hash (T.pack "md5:" <> md5hex))
          else do
            chunk <- BS.hGet handle 65536  -- 64KB chunks
            loop (MD5.update ctx chunk)
  loop MD5.init

-- Conflict marker utilities (preserved from old Internal.Metadata) --

conflictMarkers :: [String]
conflictMarkers = ["<<<<<<<", "=======", ">>>>>>>"]

hasConflictMarkers :: FilePath -> IO Bool
hasConflictMarkers path = do
  bs <- BS.readFile path
  case decodeUtf8' bs of
    Left _ -> return False  -- Binary file, no conflict markers possible
    Right txt -> return $ any (\m -> m `isInfixOf` T.unpack txt) conflictMarkers

listAllFiles :: FilePath -> IO [FilePath]
listAllFiles dir = do
  entries <- listDirectory dir
  concat <$> mapM (\name -> do
    let full = dir </> name
    isDir <- doesDirectoryExist full
    if isDir then listAllFiles full else do
      isFile <- doesFileExist full
      return (if isFile then [full] else [])) entries

validateMetadataDir :: FilePath -> IO [FilePath]
validateMetadataDir dir = do
  files <- listAllFiles dir
  filterM hasConflictMarkers files
