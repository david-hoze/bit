{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Scan
  ( scanWorkingDir
  , writeMetadataFiles
  , readMetadataFile
  , listMetadataPaths
  , getFileHashAndSize
  , hashAndClassifyFile
  , binaryExtensions
  , FileEntry(..)
  , EntryKind(..)
  ) where

import Bit.Types (Hash(..), HashAlgo(..), FileEntry(..), EntryKind(..), ContentType(..), hashToText, Path(..))
import System.FilePath
import System.Directory
    ( doesDirectoryExist,
      doesFileExist,
      listDirectory,
      getFileSize,
      createDirectoryIfMissing,
      copyFileWithMetadata,
      getModificationTime )
import System.IO (withFile, IOMode(ReadMode), hIsEOF, hPutStr, hPutStrLn, hIsTerminalDevice, stderr)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf)
import Data.Either (isRight)
import Data.Maybe (listToMaybe)
import qualified Data.ByteString as BS
import Control.Monad (void, when, forM_)
import Data.Foldable (traverse_)
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Char (toLower)
import qualified Internal.ConfigFile as ConfigFile
import Bit.Utils (atomicWriteFileStr, toPosix)
import Bit.Internal.Metadata (MetaContent(..), readMetadataOrComputeHash, hashFile, serializeMetadata)
import qualified Data.Set as Set
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import qualified Data.Text as T
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent (getNumCapabilities, forkIO, threadDelay, killThread)
import Control.Concurrent.QSem (newQSem, waitQSem, signalQSem)
import Control.Exception (bracket_, finally)
import Bit.Progress (reportProgress, clearProgress)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

-- Binary file extensions that should never be treated as text (hardcoded, not configurable)
binaryExtensions :: [String]
binaryExtensions = [".mp4", ".zip", ".bin", ".exe", ".dll", ".so", ".dylib", ".jpg", ".jpeg", ".png", ".gif", ".pdf", ".gz", ".bz2", ".xz", ".tar", ".rar", ".7z", ".iso", ".img", ".dmg", ".deb", ".rpm", ".msi"]

-- | Internal: scanned path before hashing. Distinguishes dirs from files without boolean blindness.
data ScannedEntry = ScannedFile FilePath | ScannedDir FilePath
  deriving (Show, Eq)

-- | Single-pass file hash and classification. Returns (hash, contentType).
-- For large files or binary extensions: streams hash only, returns BinaryContent.
-- For others: reads first 8KB for text classification, then streams remaining chunks for hash.
hashAndClassifyFile :: FilePath -> Integer -> ConfigFile.TextConfig -> IO (Hash 'MD5, ContentType)
hashAndClassifyFile filePath size config = do
    let ext = map toLower (takeExtension filePath)
    
    -- Fast path: large files or known binary extensions - just stream hash
    if size >= ConfigFile.textSizeLimit config || ext `elem` binaryExtensions
        then do
            h <- streamHash filePath
            pure (h, BinaryContent)
        else
            -- Single-pass: read first 8KB for classification, continue streaming for hash
            withFile filePath ReadMode $ \handle -> do
                firstChunk <- BS.hGet handle 8192
                let contentType = if not (BS.elem 0 firstChunk) && isRight (decodeUtf8' firstChunk)
                                  then TextContent else BinaryContent
                
                -- Continue streaming hash from where we left off
                let loop !ctx = do
                        eof <- hIsEOF handle
                        if eof
                            then do
                                let md5hex = decodeUtf8 (encode (MD5.finalize ctx))
                                pure (Hash (T.pack "md5:" <> md5hex))
                            else do
                                chunk <- BS.hGet handle 65536
                                loop (MD5.update ctx chunk)
                
                -- Start with first chunk already included
                h <- loop (MD5.update MD5.init firstChunk)
                pure (h, contentType)
  where
    -- Stream hash for files we're not classifying
    streamHash fp = withFile fp ReadMode $ \h -> do
        let loop !ctx = do
                eof <- hIsEOF h
                if eof
                    then do
                        let md5hex = decodeUtf8 (encode (MD5.finalize ctx))
                        pure (Hash (T.pack "md5:" <> md5hex))
                    else do
                        chunk <- BS.hGet h 65536
                        loop (MD5.update ctx chunk)
        loop MD5.init

-- | Cache entry for file metadata to skip re-hashing unchanged files
data CacheEntry = CacheEntry
  { ceMtime :: Integer
  , ceSize :: Integer
  , ceHash :: Hash 'MD5
  , ceContentType :: ContentType
  } deriving (Show, Eq)

-- | Serialize cache entry to string format (isText for backward compatibility)
serializeCacheEntry :: CacheEntry -> String
serializeCacheEntry ce =
  "mtime: " ++ show (ceMtime ce) ++ "\n"
  ++ "size: " ++ show (ceSize ce) ++ "\n"
  ++ "hash: " ++ T.unpack (hashToText (ceHash ce)) ++ "\n"
  ++ "isText: " ++ show (ceContentType ce == TextContent) ++ "\n"

-- | Parse cache entry from string format
parseCacheEntry :: String -> Maybe CacheEntry
parseCacheEntry content = do
  let ls = lines content
  mtimeLine <- listToMaybe [ drop (length ("mtime: " :: String)) l
                           | l <- ls, "mtime: " `isPrefixOf` l ]
  sizeLine <- listToMaybe [ drop (length ("size: " :: String)) l
                          | l <- ls, "size: " `isPrefixOf` l ]
  hashLine <- listToMaybe [ drop (length ("hash: " :: String)) l
                          | l <- ls, "hash: " `isPrefixOf` l ]
  isTextLine <- listToMaybe [ drop (length ("isText: " :: String)) l
                            | l <- ls, "isText: " `isPrefixOf` l ]
  mtime <- readMaybeInt (trim mtimeLine)
  size <- readMaybeInt (trim sizeLine)
  let hashVal = trim hashLine
  isText <- readMaybeBool (trim isTextLine)
  let contentType = if isText then TextContent else BinaryContent
  if null hashVal
    then Nothing
    else Just CacheEntry
      { ceMtime = mtime
      , ceSize = size
      , ceHash = Hash (T.pack hashVal)
      , ceContentType = contentType
      }
  where
    trim = dropWhileEnd isSpaceChar . dropWhile isSpaceChar
    isSpaceChar c = c == ' ' || c == '\t' || c == '\r' || c == '\n'
    readMaybeInt s = case reads s of
      [(n, "")] -> Just n
      [(n, r)] | all isSpaceChar r -> Just n
      _ -> Nothing
    readMaybeBool s = case s of
      "True" -> Just True
      "False" -> Just False
      _ -> Nothing

-- | Load cache entry for a file, returns Nothing if missing or malformed
loadCacheEntry :: FilePath -> FilePath -> IO (Maybe CacheEntry)
loadCacheEntry root relPath = do
  let cachePath = root </> ".bit" </> "cache" </> relPath
  exists <- doesFileExist cachePath
  if not exists
    then pure Nothing
    else do
      -- Use strict bytestring reading to avoid lazy file handle issues on Windows
      bs <- BS.readFile cachePath
      pure $ either (const Nothing) (parseCacheEntry . T.unpack) (decodeUtf8' bs)

-- | Save cache entry for a file (non-atomic write, cache corruption is acceptable)
saveCacheEntry :: FilePath -> FilePath -> CacheEntry -> IO ()
saveCacheEntry root relPath entry = do
  let bitRoot = root </> ".bit"
  bitExists <- doesDirectoryExist bitRoot
  when bitExists $ do
    let cachePath = bitRoot </> "cache" </> relPath
    createDirectoryIfMissing True (takeDirectory cachePath)
    -- Use strict bytestring writing to ensure file handle is closed immediately
    BS.writeFile cachePath (encodeUtf8 (T.pack (serializeCacheEntry entry)))

-- | Normalize a file path for consistent comparison (forward slashes, trimmed)
normalizePath :: FilePath -> FilePath
normalizePath = toPosix . filter (/= '\r')

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
        then pure Set.empty
        else do
            -- Use strict ByteString reading to avoid lazy file handle issues on Windows
            bs <- BS.readFile gitignorePath
            let content = either (const "") T.unpack (decodeUtf8' bs)
            let whitespace = ['\r', '\n', ' '] :: [Char]
            let patterns = filter (not . null) $ 
                           filter (not . ("#" `isPrefixOf`)) $  -- Skip comments
                           map (filter (`notElem` whitespace)) (lines content)
            let isIgnored p = any (`matchesPattern` p) patterns
            pure $ Set.fromList $ filter isIgnored paths

-- | Bounded parallel map: runs up to @bound@ actions concurrently.
mapConcurrentlyBounded :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyBounded bound f xs = do
    sem <- newQSem bound
    mapConcurrently (\x -> bracket_ (waitQSem sem) (signalQSem sem) (f x)) xs

-- | Progress reporter thread that periodically displays scan progress
progressLoop :: IORef Int -> Int -> IO ()
progressLoop counter total = go
  where
    go = do
        n <- readIORef counter
        let pct = (n * 100) `div` max 1 total
        reportProgress $ "Scanning... " ++ show n ++ "/" ++ show total ++ " files (" ++ show pct ++ "%)"
        threadDelay 50000  -- 50ms
        when (n < total) go

-- Main scan function
scanWorkingDir :: FilePath -> IO [FileEntry]
scanWorkingDir root = do
    let bitRoot = root </> ".bit"
    bitExists <- doesDirectoryExist bitRoot
    if not bitExists then pure []
    else do
      -- Read config once for all files
      config <- ConfigFile.readTextConfig
    
      -- First pass: collect all paths (without hashing)
      allPaths <- collectPaths root

      -- Filter through git check-ignore
      let filePaths = [p | ScannedFile p <- allPaths]
      ignoredSet <- checkIgnoredFiles root filePaths

      -- Separate directories from files to hash
      let dirEntries = [FileEntry { path = Path rel, kind = Directory } | ScannedDir rel <- allPaths]
          filesToHash = [(rel, root </> rel) | ScannedFile rel <- allPaths
                                             , not (Set.member (normalizePath rel) ignoredSet)]
    
      -- Setup progress tracking
      let total = length filesToHash
      counter <- newIORef (0 :: Int)
    
      -- Hash/classify files in parallel (bounded by numCapabilities * 4)
      caps <- getNumCapabilities
      let concurrency = max 4 (caps * 4)
    
      let hashWithProgress (rel, fullPath) = do
              size <- getFileSize fullPath
              mtime <- getModificationTime fullPath
              let mtimeInt = floor (utcTimeToPOSIXSeconds mtime) :: Integer
              cached <- loadCacheEntry root rel
              case cached of
                Just ce | ceSize ce == fromIntegral size && ceMtime ce == mtimeInt -> do
                  -- Cache hit: reuse hash and contentType
                  atomicModifyIORef' counter (\n -> (n + 1, ()))
                  pure $ FileEntry
                      { path = Path rel
                      , kind = File { fHash = ceHash ce, fSize = fromIntegral size, fContentType = ceContentType ce }
                      }
                _ -> do
                  -- Cache miss: hash the file, save cache entry
                  (h, contentType) <- hashAndClassifyFile fullPath (fromIntegral size) config
                  saveCacheEntry root rel (CacheEntry mtimeInt (fromIntegral size) h contentType)
                  atomicModifyIORef' counter (\n -> (n + 1, ()))
                  pure $ FileEntry
                      { path = Path rel
                      , kind = File { fHash = h, fSize = fromIntegral size, fContentType = contentType }
                      }
    
      -- Wrap hashing with progress reporter
      let hashingAction = mapConcurrentlyBounded concurrency hashWithProgress filesToHash
    
      fileEntries <- if total > 50
          then do
              -- Show progress for large scans
              reporterThread <- forkIO (progressLoop counter total)
              finally hashingAction $ do
                  killThread reporterThread
                  clearProgress
                  hPutStrLn stderr $ "Scanned " ++ show total ++ " files."
          else
              -- No progress for small scans
              hashingAction
    
      pure $ dirEntries ++ fileEntries
  where
    collectPaths :: FilePath -> IO [ScannedEntry]
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
            pure (ScannedDir rel : childPaths)
        else pure [ScannedFile rel]

writeMetadataFiles :: FilePath -> [FileEntry] -> IO ()
writeMetadataFiles root entries = do
    let bitRoot = root </> ".bit"
    bitExists <- doesDirectoryExist bitRoot
    when bitExists $ do
      let metaRoot = bitRoot </> "index"
      createDirectoryIfMissing True metaRoot

      -- Separate directories from files
      let (dirs, files) = partitionEntries entries
    
      -- First pass: create all directories sequentially (avoid race conditions)
      forM_ dirs $ \dirPath -> do
        let fullPath = metaRoot </> dirPath
        createDirectoryIfMissing True fullPath
    
      -- Second pass: create parent directories for files
      let parentDirs = Set.fromList [takeDirectory (unPath (path e)) | e <- files]
      forM_ parentDirs $ \dirPath -> do
        let fullPath = metaRoot </> dirPath
        createDirectoryIfMissing True fullPath
    
      -- Setup progress tracking
      let total = length files
      isTTY <- hIsTerminalDevice stderr
      counter <- newIORef (0 :: Int)
      skipped <- newIORef (0 :: Int)
    
      -- Start progress reporter thread if we're in a TTY and have enough files
      let shouldShowProgress = isTTY && total > 10
      reporterThread <- if shouldShowProgress
          then Just <$> forkIO (writeProgressLoop counter skipped total)
          else pure Nothing
    
      -- Third pass: write files in parallel (bounded concurrency)
      caps <- getNumCapabilities
      let concurrency = max 4 (caps * 4)
    
      let writeWithProgress entry = do
              let metaPath = metaRoot </> unPath (path entry)
              case kind entry of
                File { fHash, fSize, fContentType } -> do
                  -- Check if file is unchanged before writing
                  needsWrite <- shouldWriteFile root metaPath entry fHash fSize fContentType
                  if needsWrite
                    then do
                      case fContentType of
                        TextContent -> do
                          -- For text files, copy the actual content directly
                          let actualPath = root </> unPath (path entry)
                          copyFileWithMetadata actualPath metaPath
                        BinaryContent -> do
                          -- For binary files, write metadata (hash + size). Spec: raw hash value; atomic write.
                          atomicWriteFileStr metaPath $
                            serializeMetadata (MetaContent fHash fSize)
                      atomicModifyIORef' counter (\n -> (n + 1, ()))
                    else do
                      atomicModifyIORef' skipped (\n -> (n + 1, ()))
                      atomicModifyIORef' counter (\n -> (n + 1, ()))
                Directory -> pure ()  -- Already handled in first pass
                Symlink _ -> pure ()  -- Symlinks handled separately
    
      finally
          (void $ mapConcurrentlyBounded concurrency writeWithProgress files)
          (do
              -- Clean up: kill reporter thread and finalize progress line
              traverse_ killThread reporterThread
              when shouldShowProgress $ do
                  n <- readIORef counter
                  s <- readIORef skipped
                  clearProgress
                  let written = n - s
                  hPutStr stderr $ "Wrote " ++ show written ++ " metadata files"
                  when (s > 0) $ hPutStr stderr $ " (skipped " ++ show s ++ " unchanged)"
                  hPutStrLn stderr "."
          )
  where
    partitionEntries :: [FileEntry] -> ([FilePath], [FileEntry])
    partitionEntries es =
      let dirs = [unPath (path e) | e <- es, case kind e of Directory -> True; _ -> False]
          files = [e | e <- es, case kind e of File{} -> True; _ -> False]
      in (dirs, files)
    
    writeProgressLoop :: IORef Int -> IORef Int -> Int -> IO ()
    writeProgressLoop counter skipped total = go
      where
        go = do
            n <- readIORef counter
            s <- readIORef skipped
            let pct = (n * 100) `div` max 1 total
                written = n - s
                msg = "Writing metadata... " ++ show written ++ "/" ++ show total ++ " files (" ++ show pct ++ "%)"
                      ++ if s > 0 then ", skipped " ++ show s else ""
            reportProgress msg
            threadDelay 50000  -- 50ms
            when (n < total) go

-- | Check if a metadata file needs to be written (returns True if write needed)
shouldWriteFile :: FilePath -> FilePath -> FileEntry -> Hash 'MD5 -> Integer -> ContentType -> IO Bool
shouldWriteFile root metaPath entry fHash fSize fContentType = do
  exists <- doesFileExist metaPath
  if not exists
    then pure True  -- File doesn't exist, must write
    else case fContentType of
      TextContent -> do
        -- For text files: compare mtime and size of source vs destination
        let sourcePath = root </> unPath (path entry)
        sourceMtime <- getModificationTime sourcePath
        sourceSize <- getFileSize sourcePath
        destMtime <- getModificationTime metaPath
        destSize <- getFileSize metaPath
        -- Write if mtime or size differs
        pure (sourceMtime /= destMtime || sourceSize /= destSize)
      BinaryContent -> do
        -- For binary files: read existing metadata and compare hash/size
        existing <- readMetadataOrComputeHash metaPath
        pure $ maybe True  -- Failed to read, must write
          (\(MetaContent existingHash existingSize) ->
            -- Write if hash or size differs
            existingHash /= fHash || existingSize /= fSize) existing

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
          pure (if isFile then [rel] else [])

-- | Get hash and size of a file. Returns Nothing if file is missing or not a regular file.
getFileHashAndSize :: FilePath -> FilePath -> IO (Maybe (Hash 'MD5, Integer))
getFileHashAndSize root relPath = do
  let full = root </> relPath
  exists <- doesFileExist full
  if not exists then pure Nothing
  else do
    h <- hashFile full
    sz <- getFileSize full
    pure (Just (h, fromIntegral sz))
