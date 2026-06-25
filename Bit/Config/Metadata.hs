{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Bit.Config.Metadata
  ( MetaContent(..)
  , serializeMetadata
  , parseMetadata
  , parseMetadataFile
  , readMetadataOrComputeHash
  , displayHash
  , hashFileBytes
  , hashFile
  , hashBytesWith
  , hashFileWith
  , hashFileLike
  , hashAlgoOf
  , hashAlgoPrefix
  , hasConflictMarkers
  , validateMetadataDir
  , listAllFiles
  ) where

import Bit.Types (Hash(..), HashAlgo(..), hashToText)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO (withFile, IOMode(ReadMode), hIsEOF)
import Data.List (dropWhileEnd, isPrefixOf, isInfixOf)
import Data.Maybe (listToMaybe)
import Control.Monad (filterM)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA256 as SHA256
import qualified BLAKE3
import qualified Data.ByteArray as BA
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
          in dropWhileEnd (== '"') rest
      | ('"':rest) <- s = case reverse rest of
          ('"':middle) -> reverse middle
          _ -> s
      | otherwise = s
    trim = dropWhileEnd isSpaceChar . dropWhile isSpaceChar
    -- Accept \r \n so Windows line endings and trailing newlines don't break parsing.
    isSpaceChar c = c == ' ' || c == '\t' || c == '\r' || c == '\n'
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
      pure $ either (const Nothing) (parseMetadata . T.unpack) (decodeUtf8' bs)

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

-- | The on-disk text prefix that tags a hash with its algorithm.
hashAlgoPrefix :: HashAlgo -> T.Text
hashAlgoPrefix MD5    = T.pack "md5:"
hashAlgoPrefix SHA256 = T.pack "sha256:"
hashAlgoPrefix BLAKE3 = T.pack "blake3:"

-- | Which algorithm produced a stored hash, read from its text prefix.
-- Defaults to MD5 for un-prefixed legacy hashes.
hashAlgoOf :: Hash a -> HashAlgo
hashAlgoOf h =
  let t = hashToText h
  in if T.pack "blake3:" `T.isPrefixOf` t then BLAKE3
     else if T.pack "sha256:" `T.isPrefixOf` t then SHA256
     else MD5

-- | Compute a hash of raw bytes with the given algorithm.
hashBytesWith :: HashAlgo -> BS.ByteString -> Hash a
hashBytesWith algo bs =
  let hex = decodeUtf8 (encode (digestBytes algo bs))
  in Hash (hashAlgoPrefix algo <> hex)
  where
    digestBytes MD5    = MD5.hash
    digestBytes SHA256 = SHA256.hash
    digestBytes BLAKE3 = \b -> BA.convert (BLAKE3.hash Nothing [b] :: BLAKE3.Digest 32)

-- | Compute MD5 hash of raw bytes. Single source of truth for legacy hashing.
hashFileBytes :: BS.ByteString -> Hash 'MD5
hashFileBytes = hashBytesWith MD5

-- | Streaming, constant-memory file hash with the given algorithm.
-- Reads the file in 64KB chunks so files larger than RAM hash correctly.
hashFileWith :: HashAlgo -> FilePath -> IO (Hash a)
hashFileWith MD5    fp = streamDigest fp MD5.init    MD5.update    MD5.finalize    MD5
hashFileWith SHA256 fp = streamDigest fp SHA256.init SHA256.update SHA256.finalize SHA256
hashFileWith BLAKE3 fp =
  streamDigest fp (BLAKE3.init Nothing)
    (\ctx c -> BLAKE3.update ctx [c])
    (\ctx -> BA.convert (BLAKE3.finalize ctx :: BLAKE3.Digest 32)) BLAKE3

-- | Generic streaming hash driver: fold @update@ over 64KB reads, strictly,
-- then @finalize@ to raw bytes and tag with the algorithm prefix.
streamDigest
  :: FilePath
  -> ctx
  -> (ctx -> BS.ByteString -> ctx)
  -> (ctx -> BS.ByteString)
  -> HashAlgo
  -> IO (Hash a)
streamDigest fp ctx0 upd fin algo = withFile fp ReadMode $ \handle -> do
  let loop !ctx = do
        eof <- hIsEOF handle
        if eof
          then pure (Hash (hashAlgoPrefix algo <> decodeUtf8 (encode (fin ctx))))
          else do
            chunk <- BS.hGet handle 65536  -- 64KB chunks
            loop (upd ctx chunk)
  loop ctx0

-- | Compute MD5 hash of a file on disk using streaming (constant memory).
hashFile :: FilePath -> IO (Hash 'MD5)
hashFile = hashFileWith MD5

-- | Recompute a file's hash using the same algorithm as an existing stored
-- hash (read from its prefix). This is what verification uses so a repo
-- migrated to BLAKE3 still compares like-for-like.
hashFileLike :: Hash a -> FilePath -> IO (Hash a)
hashFileLike ref = hashFileWith (hashAlgoOf ref)

-- Conflict marker utilities (preserved from old Internal.Metadata) --

conflictMarkers :: [String]
conflictMarkers = ["<<<<<<<", "=======", ">>>>>>>"]

hasConflictMarkers :: FilePath -> IO Bool
hasConflictMarkers path = do
  bs <- BS.readFile path
  pure $ either (const False) (\txt -> any (`isInfixOf` T.unpack txt) conflictMarkers) (decodeUtf8' bs)

listAllFiles :: FilePath -> IO [FilePath]
listAllFiles dir = do
  entries <- listDirectory dir
  concat <$> mapM (\name -> do
    let full = dir </> name
    isDir <- doesDirectoryExist full
    if isDir then listAllFiles full else do
      isFile <- doesFileExist full
      pure (if isFile then [full] else [])) entries

validateMetadataDir :: FilePath -> IO [FilePath]
validateMetadataDir dir = do
  files <- listAllFiles dir
  filterM hasConflictMarkers files
