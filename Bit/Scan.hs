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
import System.IO (withFile, IOMode(ReadMode), hIsEOF, hIsTerminalDevice, hPutStr, hFlush, stderr)
import Data.List
import qualified Data.ByteString as BS
import Data.Text (unpack)
import Control.Monad
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Data.Char (toLower)
import qualified Internal.ConfigFile as ConfigFile
import Bit.Utils (atomicWriteFileStr)
import Bit.Internal.Metadata (MetaContent(..), readMetadataOrComputeHash, hashFile, serializeMetadata)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import qualified Data.Set as Set
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import qualified Data.Text as T
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent (getNumCapabilities, forkIO, threadDelay, killThread)
import Control.Concurrent.QSem (newQSem, waitQSem, signalQSem)
import Control.Exception (bracket_, finally)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')

-- Binary file extensions that should never be treated as text (hardcoded, not configurable)
binaryExtensions :: [String]
binaryExtensions = [".mp4", ".zip", ".bin", ".exe", ".dll", ".so", ".dylib", ".jpg", ".jpeg", ".png", ".gif", ".pdf", ".gz", ".bz2", ".xz", ".tar", ".rar", ".7z", ".iso", ".img", ".dmg", ".deb", ".rpm", ".msi"]

-- | Single-pass file hash and classification. Returns (hash, isText).
-- For large files or binary extensions: streams hash only, returns isText=False.
-- For others: reads first 8KB for text classification, then streams remaining chunks for hash.
hashAndClassifyFile :: FilePath -> Integer -> ConfigFile.TextConfig -> IO (Hash 'MD5, Bool)
hashAndClassifyFile filePath size config = do
    let ext = map toLower (takeExtension filePath)
    
    -- Fast path: large files or known binary extensions - just stream hash
    if size >= ConfigFile.textSizeLimit config || ext `elem` binaryExtensions
        then do
            h <- streamHash filePath
            return (h, False)
        else
            -- Single-pass: read first 8KB for classification, continue streaming for hash
            withFile filePath ReadMode $ \handle -> do
                firstChunk <- BS.hGet handle 8192
                let isText = not (BS.elem 0 firstChunk) &&
                             case decodeUtf8' firstChunk of
                                 Left _ -> False
                                 Right _ -> True
                
                -- Continue streaming hash from where we left off
                let loop !ctx = do
                        eof <- hIsEOF handle
                        if eof
                            then do
                                let md5hex = decodeUtf8 (encode (MD5.finalize ctx))
                                return (Hash (T.pack "md5:" <> md5hex))
                            else do
                                chunk <- BS.hGet handle 65536
                                loop (MD5.update ctx chunk)
                
                -- Start with first chunk already included
                h <- loop (MD5.update MD5.init firstChunk)
                return (h, isText)
  where
    -- Stream hash for files we're not classifying
    streamHash fp = withFile fp ReadMode $ \h -> do
        let loop !ctx = do
                eof <- hIsEOF h
                if eof
                    then do
                        let md5hex = decodeUtf8 (encode (MD5.finalize ctx))
                        return (Hash (T.pack "md5:" <> md5hex))
                    else do
                        chunk <- BS.hGet h 65536
                        loop (MD5.update ctx chunk)
        loop MD5.init

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
        hPutStr stderr $ "\rScanning... " ++ show n ++ "/" ++ show total ++ " files (" ++ show pct ++ "%)"
        hFlush stderr
        threadDelay 50000  -- 50ms
        when (n < total) go

-- Main scan function
scanWorkingDir :: FilePath -> IO [FileEntry]
scanWorkingDir root = do
    -- Read config once for all files
    config <- ConfigFile.readTextConfig
    
    -- First pass: collect all paths (without hashing)
    allPaths <- collectPaths root
    
    -- Filter through git check-ignore
    let filePaths = [p | (p, False) <- allPaths]  -- Only check files, not directories
    ignoredSet <- checkIgnoredFiles root filePaths
    
    -- Separate directories from files to hash
    let (dirs, files) = partition snd allPaths
        dirEntries = [FileEntry { path = rel, kind = Directory } | (rel, _) <- dirs]
        filesToHash = [(rel, root </> rel) | (rel, False) <- allPaths
                                           , not (Set.member (normalizePath rel) ignoredSet)]
    
    -- Setup progress tracking
    let total = length filesToHash
    isTTY <- hIsTerminalDevice stderr
    counter <- newIORef (0 :: Int)
    
    -- Start progress reporter thread if we're in a TTY and have enough files
    let shouldShowProgress = isTTY && total > 50
    reporterThread <- if shouldShowProgress
        then Just <$> forkIO (progressLoop counter total)
        else return Nothing
    
    -- Hash/classify files in parallel (bounded by numCapabilities * 4)
    caps <- getNumCapabilities
    let concurrency = max 4 (caps * 4)
    
    let hashWithProgress (rel, fullPath) = do
            size <- getFileSize fullPath
            (h, isText) <- hashAndClassifyFile fullPath (fromIntegral size) config
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            return $ FileEntry
                { path = rel
                , kind = File { fHash = h, fSize = fromIntegral size, fIsText = isText }
                }
    
    fileEntries <- finally
        (mapConcurrentlyBounded concurrency hashWithProgress filesToHash)
        (do
            -- Clean up: kill reporter thread and clear/finalize progress line
            maybe (return ()) killThread reporterThread
            when shouldShowProgress $ do
                hPutStr stderr "\r\ESC[K"  -- Clear line with ANSI escape
                hPutStr stderr $ "Scanned " ++ show total ++ " files.\n"
                hFlush stderr
        )
    
    return $ dirEntries ++ fileEntries
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
