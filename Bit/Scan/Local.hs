{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Scan.Local
  ( scanWorkingDir
  , scanWorkingDirWithAbort
  , ScanResult(..)
  , ScanPhase(..)
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
import System.IO (withFile, IOMode(ReadMode), hIsEOF, hPutStr, hPutStrLn, hIsTerminalDevice, hFlush, stderr, stdin)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf)
import Data.Either (isRight)
import Data.Maybe (listToMaybe)
import qualified Data.ByteString as BS
import Control.Monad (void, when, forM_)
import Data.Foldable (traverse_)
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Char (toLower)
import qualified Bit.Config.File as ConfigFile
import Bit.Utils (atomicWriteFileStr, toPosix, formatBytes)
import Bit.Config.Metadata (MetaContent(..), readMetadataOrComputeHash, hashFile, serializeMetadata)
import qualified Data.Set as Set
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16 (encode)
import qualified Data.Text as T
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent (getNumCapabilities, forkIO, threadDelay, killThread)
import Control.Concurrent.QSem (newQSem, waitQSem, signalQSem)
import Control.Exception (bracket_, finally)
import Bit.Progress.Report (reportProgress, clearProgress)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Bit.IO.Concurrency (Concurrency(..), ioConcurrency)

-- | Resolve the .bit root directory from a working directory root.
-- Follows bitlink files (e.g. "bitdir: /abs/path") for separated git dirs.
resolveBitRoot :: FilePath -> IO (Maybe FilePath)
resolveBitRoot root = do
    let dotBit = root </> ".bit"
    isDir <- doesDirectoryExist dotBit
    if isDir then pure (Just dotBit)
    else do
        isFile <- doesFileExist dotBit
        if isFile then do
            bs <- BS.readFile dotBit
            let content = either (const "") T.unpack (decodeUtf8' bs)
            case lines content of
                (firstLine:_) -> pure (Just (drop 8 (filter (/= '\r') firstLine)))
                [] -> pure Nothing
        else pure Nothing

-- Binary file extensions that should never be treated as text (hardcoded, not configurable)
binaryExtensions :: [String]
binaryExtensions = [".mp4", ".zip", ".bin", ".exe", ".dll", ".so", ".dylib", ".jpg", ".jpeg", ".png", ".gif", ".pdf", ".gz", ".bz2", ".xz", ".tar", ".rar", ".7z", ".iso", ".img", ".dmg", ".deb", ".rpm", ".msi"]

-- | Internal: scanned path before hashing. Distinguishes dirs from files without boolean blindness.
data ScannedEntry = ScannedFile FilePath | ScannedDir FilePath
  deriving (Show, Eq)

-- | Result of a scan that may skip some files.
data ScanResult = ScanResult
  { srEntries :: [FileEntry]          -- ^ Files with hashes (cache hits + completed hashes)
  , srSkipped :: [(FilePath, Integer)] -- ^ (relPath, fileSize) where hashing was skipped
  }

-- | Internal: a file that needs hashing (cache miss).
data FileToHash = FileToHash
  { fthRel   :: !FilePath   -- ^ Relative path
  , fthFull  :: !FilePath   -- ^ Absolute path
  , fthSize  :: !Integer    -- ^ File size
  , fthMtime :: !Integer    -- ^ Modification time (POSIX seconds)
  }

-- | Phase callbacks for scan progress reporting.
data ScanPhase
    = PhaseCollected Int                    -- ^ N files found
    | PhaseCacheResult Int Int Integer      -- ^ cached, needsHashing, totalBytesNeeded
    | PhaseAllCached Int                    -- ^ all N files cached
    deriving (Show, Eq)

-- | Single-pass file hash and classification. Returns (hash, contentType).
-- For large files or binary extensions: streams hash only, returns BinaryContent.
-- For others: reads first 8KB for text classification, then streams remaining chunks for hash.
-- When mBytesRef is provided, updates it with bytes read per chunk for live progress.
hashAndClassifyFile :: FilePath -> Integer -> ConfigFile.TextConfig -> Maybe (IORef Integer) -> IO (Hash 'MD5, ContentType)
hashAndClassifyFile filePath size config mBytesRef = do
    let ext = map toLower (takeExtension filePath)
        bump chunk = case mBytesRef of
            Just ref -> atomicModifyIORef' ref (\b -> (b + fromIntegral (BS.length chunk), ()))
            Nothing  -> pure ()

    -- Fast path: large files or known binary extensions - just stream hash
    if size >= ConfigFile.textSizeLimit config || ext `elem` binaryExtensions
        then do
            h <- streamHash bump filePath
            pure (h, BinaryContent)
        else
            -- Single-pass: read first 8KB for classification, continue streaming for hash
            withFile filePath ReadMode $ \handle -> do
                firstChunk <- BS.hGet handle 8192
                bump firstChunk
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
                                bump chunk
                                loop (MD5.update ctx chunk)

                -- Start with first chunk already included
                h <- loop (MD5.update MD5.init firstChunk)
                pure (h, contentType)
  where
    -- Stream hash for files we're not classifying
    streamHash :: (BS.ByteString -> IO ()) -> FilePath -> IO (Hash 'MD5)
    streamHash bumpFn fp = withFile fp ReadMode $ \h -> do
        let loop !ctx = do
                eof <- hIsEOF h
                if eof
                    then do
                        let md5hex = decodeUtf8 (encode (MD5.finalize ctx))
                        pure (Hash (T.pack "md5:" <> md5hex))
                    else do
                        chunk <- BS.hGet h 65536
                        bumpFn chunk
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
  mBitRoot <- resolveBitRoot root
  case mBitRoot of
    Nothing -> pure Nothing
    Just bitRoot -> do
      let cachePath = bitRoot </> "cache" </> relPath
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
  mBitRoot <- resolveBitRoot root
  case mBitRoot of
    Nothing -> pure ()
    Just bitRoot -> do
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
    mBitRoot <- resolveBitRoot root
    let gitignorePath = case mBitRoot of
            Just bitRoot -> bitRoot </> "index" </> ".gitignore"
            Nothing      -> root </> ".bit" </> "index" </> ".gitignore"
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

-- | Progress reporter for the hashing phase, showing files + bytes + percentage.
hashProgressLoop :: IORef Int -> IORef Integer -> Int -> Integer -> IO ()
hashProgressLoop fileCounter bytesCounter totalNewFiles totalBytes = go
  where
    go = do
        n <- readIORef fileCounter
        b <- readIORef bytesCounter
        let pct = if totalBytes > 0 then (b * 100) `div` totalBytes else 0
        reportProgress $ "Hashing: " ++ show n ++ "/" ++ show totalNewFiles
            ++ " files, " ++ formatBytes b ++ " / " ++ formatBytes totalBytes
            ++ " (" ++ show pct ++ "%)"
        threadDelay 50000
        when (n < totalNewFiles) go

-- | Format elapsed seconds as a human-readable string.
formatElapsed :: Double -> String
formatElapsed secs
    | secs < 1    = show (round (secs * 1000) :: Int) ++ " ms"
    | secs < 60   = show (round secs :: Int) ++ " s"
    | otherwise   = let mins = floor secs `div` 60 :: Int
                        remainSecs = round secs - mins * 60 :: Int
                    in show mins ++ " min " ++ show remainSecs ++ " s"

-- | Recursively collect all paths under root, excluding .bit, .git, .bitignore, .gitignore.
collectScannedPaths :: FilePath -> IO [ScannedEntry]
collectScannedPaths root = go root
  where
    go path = do
      isDir <- doesDirectoryExist path
      let rel = makeRelative root path
      if rel == ".bit" || (".bit" `isPrefixOf` rel)
          || rel == ".git" || (".git" `isPrefixOf` rel)
          || rel == ".bitignore"
          || rel == ".gitignore"
        then pure []
        else if isDir
          then do
            names <- listDirectory path
            -- Filter .git/.bit at every level, not just root (avoids scanning
            -- nested .git dirs like newdir/.git which the rel-path prefix check misses)
            let filtered = filter (\n -> n /= ".git" && n /= ".bit") names
            childPaths <- concat <$> mapM (go . (path </>)) filtered
            pure (ScannedDir rel : childPaths)
        else pure [ScannedFile rel]

-- | Stat a file and check the hash cache.
-- Returns Right for cache hits, Left for files needing hashing.
statAndCheckCache :: FilePath -> (FilePath, FilePath) -> IO (Either FileToHash FileEntry)
statAndCheckCache root (rel, fullPath) = do
    size <- getFileSize fullPath
    mtime <- getModificationTime fullPath
    let mtimeInt = floor (utcTimeToPOSIXSeconds mtime) :: Integer
    cached <- loadCacheEntry root rel
    case cached of
        Just ce | ceSize ce == fromIntegral size && ceMtime ce == mtimeInt ->
            pure $ Right $ FileEntry
                { path = Path rel
                , kind = File (ceHash ce) (fromIntegral size) (ceContentType ce)
                }
        _ -> pure $ Left $ FileToHash rel fullPath (fromIntegral size) mtimeInt

-- | Hash a single file and save the result to cache.
hashFileToEntry :: FilePath -> ConfigFile.TextConfig -> Maybe (IORef Integer) -> FileToHash -> IO FileEntry
hashFileToEntry root config mBytesRef fth = do
    (h, contentType) <- hashAndClassifyFile (fthFull fth) (fthSize fth) config mBytesRef
    saveCacheEntry root (fthRel fth) (CacheEntry (fthMtime fth) (fthSize fth) h contentType)
    pure $ FileEntry
        { path = Path (fthRel fth)
        , kind = File h (fthSize fth) contentType
        }

-- | Measure storage throughput by reading a sample from a file.
-- Reads up to 10 MB and returns estimated bytes per second.
measureThroughput :: FilePath -> IO Double
measureThroughput filePath = do
    let sampleSize = 10 * 1024 * 1024 :: Int
    start <- getCurrentTime
    bytesRead <- withFile filePath ReadMode $ \h -> do
        let loop !total
                | total >= sampleSize = pure total
                | otherwise = do
                    eof <- hIsEOF h
                    if eof then pure total
                    else do
                        chunk <- BS.hGet h 65536
                        loop (total + BS.length chunk)
        loop 0
    end <- getCurrentTime
    let elapsed = realToFrac (diffUTCTime end start) :: Double
    pure (fromIntegral bytesRead / max 0.001 elapsed)

-- | Main scan function. Always hashes all files (no bandwidth abort).
scanWorkingDir :: FilePath -> Concurrency -> IO [FileEntry]
scanWorkingDir root concurrencyMode = do
    mBitRoot <- resolveBitRoot root
    case mBitRoot of
      Nothing -> pure []
      Just _bitRoot -> do
        config <- ConfigFile.readTextConfig
        allPaths <- collectScannedPaths root

        let filePaths = [p | ScannedFile p <- allPaths]
        ignoredSet <- checkIgnoredFiles root filePaths

        let dirEntries = [FileEntry { path = Path rel, kind = Directory } | ScannedDir rel <- allPaths]
            filesToHash = [(rel, root </> rel) | ScannedFile rel <- allPaths
                                               , not (Set.member (normalizePath rel) ignoredSet)]

        let total = length filesToHash
        counter <- newIORef (0 :: Int)

        concurrency <- case concurrencyMode of
          Sequential  -> pure 1
          Parallel 0  -> ioConcurrency
          Parallel n  -> pure n

        let hashWithProgress (rel, fullPath) = do
                size <- getFileSize fullPath
                mtime <- getModificationTime fullPath
                let mtimeInt = floor (utcTimeToPOSIXSeconds mtime) :: Integer
                cached <- loadCacheEntry root rel
                case cached of
                  Just ce | ceSize ce == fromIntegral size && ceMtime ce == mtimeInt -> do
                    atomicModifyIORef' counter (\n -> (n + 1, ()))
                    pure $ FileEntry
                        { path = Path rel
                        , kind = File { fHash = ceHash ce, fSize = fromIntegral size, fContentType = ceContentType ce }
                        }
                  _ -> do
                    (h, contentType) <- hashAndClassifyFile fullPath (fromIntegral size) config Nothing
                    saveCacheEntry root rel (CacheEntry mtimeInt (fromIntegral size) h contentType)
                    atomicModifyIORef' counter (\n -> (n + 1, ()))
                    pure $ FileEntry
                        { path = Path rel
                        , kind = File { fHash = h, fSize = fromIntegral size, fContentType = contentType }
                        }

        let hashingAction = mapConcurrentlyBounded concurrency hashWithProgress filesToHash

        fileEntries <- if total > 50
            then do
                reporterThread <- forkIO (progressLoop counter total)
                finally hashingAction $ do
                    killThread reporterThread
                    clearProgress
                    hPutStrLn stderr $ "Scanned " ++ show total ++ " files."
            else
                hashingAction

        pure $ dirEntries ++ fileEntries

-- | Scan with bandwidth detection. Measures storage throughput before committing
-- to hash all files. If estimated time exceeds 60 seconds, prompts the user.
-- Returns entries (with hashes) and skipped file paths.
-- The Concurrency parameter controls hashing parallelism (Sequential = 1 thread).
scanWorkingDirWithAbort :: FilePath -> Concurrency -> Maybe (ScanPhase -> IO ()) -> IO ScanResult
scanWorkingDirWithAbort root concurrencyMode mCallback = do
    mBitRoot <- resolveBitRoot root
    case mBitRoot of
      Nothing -> pure (ScanResult [] [])
      Just _bitRoot -> do
        config <- ConfigFile.readTextConfig
        allPaths <- collectScannedPaths root

        let filePaths = [p | ScannedFile p <- allPaths]
        ignoredSet <- checkIgnoredFiles root filePaths

        let dirEntries = [FileEntry { path = Path rel, kind = Directory } | ScannedDir rel <- allPaths]
            filesToProcess = [(rel, root </> rel) | ScannedFile rel <- allPaths
                                                  , not (Set.member (normalizePath rel) ignoredSet)]

        -- Report collection phase
        traverse_ (\cb -> cb (PhaseCollected (length filesToProcess))) mCallback

        -- Phase 1: stat all files and check cache
        statResults <- mapM (statAndCheckCache root) filesToProcess
        let cacheHits    = [e | Right e <- statResults]
            needsHashing = [fth | Left fth <- statResults]
            totalBytesNeeded = sum [fthSize fth | fth <- needsHashing]

        -- Report cache result phase
        if null needsHashing
            then traverse_ (\cb -> cb (PhaseAllCached (length cacheHits))) mCallback
            else traverse_ (\cb -> cb (PhaseCacheResult (length cacheHits) (length needsHashing) totalBytesNeeded)) mCallback

        -- Phase 2: bandwidth check — only if significant work remains
        shouldSkip <- if null needsHashing || totalBytesNeeded < 20 * 1024 * 1024
            then pure False
            else do
                throughput <- measureThroughput (fthFull (head needsHashing))
                let estimatedSecs = fromIntegral totalBytesNeeded / throughput
                if estimatedSecs <= 60
                    then pure False
                    else do
                        let mins = estimatedSecs / 60
                            speedStr = formatBytes (round throughput :: Integer) ++ "/s"
                        hPutStrLn stderr $ "Hashing is slow (" ++ speedStr
                            ++ "). Estimated: " ++ show (ceiling mins :: Int)
                            ++ " min for " ++ show (length needsHashing) ++ " files."
                        hPutStrLn stderr "Size-only verification covers most corruption cases."
                        hPutStr stderr "Continue full hashing? [y/N] "
                        hFlush stderr
                        isTTY <- hIsTerminalDevice stdin
                        if isTTY
                            then do
                                response <- getLine
                                pure (response /= "y" && response /= "Y")
                            else pure True  -- non-interactive: skip

        if shouldSkip
            then pure $ ScanResult
                { srEntries = dirEntries ++ cacheHits
                , srSkipped = [(fthRel fth, fthSize fth) | fth <- needsHashing]
                }
            else do
                -- Phase 3: hash remaining files with progress
                let totalNewFiles = length needsHashing
                counter <- newIORef (0 :: Int)
                bytesHashedRef <- newIORef (0 :: Integer)
                concurrency <- case concurrencyMode of
                    Sequential  -> pure 1
                    Parallel 0  -> ioConcurrency
                    Parallel n  -> pure n

                let hashWithProgress fth = do
                        entry <- hashFileToEntry root config (Just bytesHashedRef) fth
                        atomicModifyIORef' counter (\n -> (n + 1, ()))
                        pure entry

                let hashingAction = mapConcurrentlyBounded concurrency hashWithProgress needsHashing

                hashedEntries <- if totalNewFiles > 0
                    then do
                        isTTY <- hIsTerminalDevice stderr
                        hashStart <- getCurrentTime
                        if isTTY
                          then do
                            reporterThread <- forkIO (hashProgressLoop counter bytesHashedRef totalNewFiles totalBytesNeeded)
                            finally hashingAction $ do
                                killThread reporterThread
                                clearProgress
                                actualCount <- readIORef counter
                                actualBytes <- readIORef bytesHashedRef
                                hashEnd <- getCurrentTime
                                let elapsed = realToFrac (diffUTCTime hashEnd hashStart) :: Double
                                hPutStrLn stderr $ "Hashed " ++ show actualCount
                                    ++ " files (" ++ formatBytes actualBytes
                                    ++ ") in " ++ formatElapsed elapsed ++ "."
                          else do
                            result <- hashingAction
                            actualCount <- readIORef counter
                            actualBytes <- readIORef bytesHashedRef
                            hashEnd <- getCurrentTime
                            let elapsed = realToFrac (diffUTCTime hashEnd hashStart) :: Double
                            hPutStrLn stderr $ "Hashed " ++ show actualCount
                                ++ " files (" ++ formatBytes actualBytes
                                ++ ") in " ++ formatElapsed elapsed ++ "."
                            pure result
                    else hashingAction

                pure $ ScanResult
                    { srEntries = dirEntries ++ cacheHits ++ hashedEntries
                    , srSkipped = []
                    }

writeMetadataFiles :: FilePath -> [FileEntry] -> IO ()
writeMetadataFiles root entries = do
    mBitRoot <- resolveBitRoot root
    case mBitRoot of
      Nothing -> pure ()
      Just bitRoot -> do
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
readMetadataFile :: FilePath -> IO (Maybe MetaContent)
readMetadataFile = readMetadataOrComputeHash

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
getFileHashAndSize :: FilePath -> FilePath -> IO (Maybe MetaContent)
getFileHashAndSize root relPath = do
  let full = root </> relPath
  exists <- doesFileExist full
  if not exists then pure Nothing
  else do
    h <- hashFile full
    sz <- getFileSize full
    pure (Just (MetaContent h (fromIntegral sz)))
