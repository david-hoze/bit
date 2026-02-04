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
      copyFileWithMetadata,
      getCurrentDirectory )
import Data.List
import qualified Data.ByteString as BS
import Data.Text (unpack)
import Control.Monad
import Data.Text.Encoding (decodeUtf8')
import Data.Char (toLower)
import qualified Internal.ConfigFile as ConfigFile
import Bit.Utils (atomicWriteFileStr)
import Bit.Internal.Metadata (MetaContent(..), readMetadataOrComputeHash, hashFile, serializeMetadata)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import qualified Data.Set as Set

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

-- | Normalize a file path for consistent comparison (forward slashes, trimmed)
normalizePath :: FilePath -> FilePath
normalizePath = map (\c -> if c == '\\' then '/' else c) . filter (/= '\r')

-- | Check if a filename matches a gitignore-style pattern.
-- Supports: *.ext (extension match), filename (exact match)
matchesPattern :: String -> FilePath -> Bool
matchesPattern pattern path =
    let filename = takeFileName path
        whitespace = ['\r', '\n', ' '] :: [Char]
        normalizedPattern = filter (`notElem` whitespace) pattern
    in if "*." `isPrefixOf` normalizedPattern
       then -- Extension pattern like *.log
            let ext = drop 1 normalizedPattern  -- Remove the *
            in ext `isSuffixOf` filename
       else -- Exact filename match
            normalizedPattern == filename

-- | Check which files should be ignored based on .bitignore patterns.
-- Reads patterns from .bit/index/.gitignore and matches against paths.
checkIgnoredFiles :: FilePath -> [FilePath] -> IO (Set.Set FilePath)
checkIgnoredFiles root paths = do
    let gitignorePath = root </> ".bit" </> "index" </> ".gitignore"
    exists <- doesFileExist gitignorePath
    if not exists
        then return Set.empty
        else do
            content <- readFile gitignorePath
            let whitespace = ['\r', '\n', ' '] :: [Char]
            let patterns = filter (not . null) $ 
                           filter (not . ("#" `isPrefixOf`)) $  -- Skip comments
                           map (filter (`notElem` whitespace)) (lines content)
            let isIgnored p = any (`matchesPattern` p) patterns
            return $ Set.fromList $ filter isIgnored paths

-- Main scan function
scanWorkingDir :: FilePath -> IO [FileEntry]
scanWorkingDir root = do
    -- First pass: collect all paths (without hashing)
    allPaths <- collectPaths root
    
    -- Filter through git check-ignore
    let filePaths = [p | (p, False) <- allPaths]  -- Only check files, not directories
    ignoredSet <- checkIgnoredFiles root filePaths
    
    -- Second pass: hash/classify non-ignored files, skip ignored ones
    entries <- forM allPaths $ \(rel, isDir) -> do
        if isDir
            then return $ Just $ FileEntry { path = rel, kind = Directory }
            else if Set.member (normalizePath rel) ignoredSet
                then return Nothing  -- Skip ignored files
                else do
                    let fullPath = root </> rel
                    h <- hashFile fullPath
                    size <- getFileSize fullPath
                    isText <- classifyFile fullPath (fromIntegral size)
                    return $ Just $ FileEntry
                        { path = rel
                        , kind = File { fHash = h, fSize = fromIntegral size, fIsText = isText }
                        }
    
    return $ concat [[e] | Just e <- entries]
  where
    collectPaths :: FilePath -> IO [(FilePath, Bool)]
    collectPaths path = do
      isDir <- doesDirectoryExist path
      let rel = makeRelative root path

      -- ignore .bit folder, .git, .bitignore, and .gitignore (the latter two are config files)
      if rel == ".bit" || (".bit" `isPrefixOf` rel)
          || rel == ".git" || (".git" `isPrefixOf` rel)
          || rel == ".bitignore"
          || rel == ".gitignore"
        then pure []
        else if isDir
          then do
            names <- listDirectory path
            let children = map (path </>) names
            childPaths <- concat <$> mapM collectPaths children
            pure ((rel, True) : childPaths)
        else pure [(rel, False)]

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
