{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}

module Bit.Core.Verify
    ( VerifyTarget(..)
    , RepairMode(..)
    , verify
    , repair
    , fsck
    , casBackfill
    ) where

import System.FilePath ((</>), takeDirectory)
import Control.Monad (when, unless, forM_, forM, filterM)
import Data.Foldable (traverse_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Control.Exception (bracket, finally)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import System.IO (stderr, stdin, hPutStr, hPutStrLn, hFlush, hIsTerminalDevice)
import System.Exit (ExitCode(..), exitWith)
import Data.List (intercalate, partition)
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.Directory as Dir

import Bit.Types (BitM, BitEnv(..), Path(..), Hash(..), HashAlgo(..), hashToText, FileEntry(..), EntryKind(..))
import Bit.IO.Concurrency (Concurrency(..))
import qualified Bit.Scan.Verify as Verify
import qualified Bit.Scan.Local as Scan
import Bit.CAS (hasBlobInCas, writeBlobToCas)
import Bit.CDC.Manifest (readManifestFromCas)
import Data.Maybe (isNothing)
import qualified Bit.Scan.Fsck as Fsck
import Bit.Config.Paths (bundleForRemote, bitIndexPath)
import Bit.Progress.Report (reportProgress, clearProgress)
import Bit.Utils (toPosix, atomicWriteFileStr, formatBytes)
import Bit.Config.Metadata (MetaContent(..), serializeMetadata)
import qualified Bit.Device.Identity as Device
import qualified Bit.Rclone.Progress as CopyProgress
import qualified Bit.Rclone.Run as Transport
import qualified Bit.Git.Run as Git

import Bit.Core.Helpers (printVerifyIssue, isFilesystemRemote)
import Bit.Remote (Remote, remoteName, remoteUrl, resolveRemote)
import qualified Bit.IO.Platform as Platform

-- | Whether to verify local working tree or a specific remote.
data VerifyTarget = VerifyLocal | VerifyRemotePath Remote
  deriving (Show, Eq)

-- | Whether to prompt before repairing or repair automatically.
data RepairMode = PromptRepair | AutoRepair
  deriving (Show, Eq)

-- | Verify files at the target. Reports issues and offers/performs repair.
verify :: VerifyTarget -> RepairMode -> Concurrency -> BitM ()
verify VerifyLocal repairMode concurrency = do
    cwd <- asks envCwd
    bitDir <- asks envBitDir
    liftIO $ verifyFilesystem cwd bitDir cwd Nothing repairMode concurrency

verify (VerifyRemotePath remote) repairMode concurrency = do
    cwd <- asks envCwd
    bitDir <- asks envBitDir
    isFs <- isFilesystemRemote remote
    if isFs
        then liftIO $ verifyFilesystem cwd bitDir (remoteUrl remote) (Just remote) repairMode concurrency
        else liftIO $ verifyCloud cwd bitDir remote repairMode concurrency

-- | Repair = verify with AutoRepair.
repair :: VerifyTarget -> Concurrency -> BitM ()
repair target = verify target AutoRepair

-- ============================================================================
-- Filesystem verify (local and filesystem remotes share this path)
-- ============================================================================

-- | Verify a filesystem path (local or filesystem remote).
-- cwd: the bit repository root (for loading repair sources).
-- targetPath: the path to scan (cwd for local, remoteUrl for remote).
-- mTargetRemote: Nothing for local target, Just remote for remote target.
verifyFilesystem :: FilePath -> FilePath -> FilePath -> Maybe Remote -> RepairMode -> Concurrency -> IO ()
verifyFilesystem cwd bitDir targetPath mTargetRemote repairMode concurrency = do
    let label = maybe "local" (\r -> "remote '" ++ remoteName r ++ "'") mTargetRemote
    putStrLn $ "Verifying " ++ label ++ " files..."

    -- Build verbose phase callback
    let onPhase = \case
            Verify.PhaseCollected n ->
                hPutStrLn stderr $ "Collecting files... " ++ show n ++ " found."
            Verify.PhaseCacheResult cached toHash totalBytes ->
                hPutStrLn stderr $ "Checking cache... " ++ show cached ++ " cached, "
                    ++ show toHash ++ " need hashing (" ++ formatBytes totalBytes ++ ")."
            Verify.PhaseAllCached n ->
                hPutStrLn stderr $ "All " ++ show n ++ " files cached, no hashing needed."

    -- Verify with bandwidth detection
    (result, skipped) <- Verify.verifyWithAbort targetPath Nothing concurrency (Just onPhase)

    -- Report results
    printVerifyResultWithSkipped result skipped

    -- Repair flow
    let indexDir = bitDir </> "index"
        loadMeta = Verify.loadCommittedBinaryMetadata indexDir
        loadTextPaths = Verify.loadCommittedTextPaths indexDir
    unless (null result.vrIssues) $
        repairFlow cwd loadMeta loadTextPaths mTargetRemote result.vrIssues repairMode concurrency

-- ============================================================================
-- Cloud verify (uses bundle + rclone)
-- ============================================================================

-- | Verify a cloud remote using the fetched bundle.
verifyCloud :: FilePath -> FilePath -> Remote -> RepairMode -> Concurrency -> IO ()
verifyCloud cwd _bitDir remote repairMode concurrency = do
    layout <- Device.readRemoteLayout cwd (remoteName remote)
    case layout of
      Device.LayoutBare -> do
        putStrLn "[OK] Bare remote — CAS blobs are self-verifying."
        pure ()
      _ -> verifyCloudFull cwd remote repairMode concurrency

verifyCloudFull :: FilePath -> Remote -> RepairMode -> Concurrency -> IO ()
verifyCloudFull cwd remote repairMode concurrency = do
    putStrLn "Verifying remote files..."

    entries <- Verify.loadMetadataFromBundle (bundleForRemote (remoteName remote))
    let fileCount = length entries
    hPutStrLn stderr $ "Loading bundle metadata... " ++ show fileCount ++ " files to check."

    if fileCount > 5
      then do
        isTTY <- hIsTerminalDevice stderr
        counter <- newIORef (0 :: Int)
        let shouldShowProgress = isTTY

        reporterThread <- if shouldShowProgress
          then Just <$> forkIO (verifyProgressLoop counter fileCount)
          else pure Nothing

        result <- finally
          (Verify.verifyRemote cwd remote (Just counter) concurrency)
          (do
            traverse_ killThread reporterThread
            when shouldShowProgress clearProgress
          )

        printVerifyResultWithSkipped result []
        let loadMeta = Verify.binaryEntries <$>
                Verify.loadMetadataFromBundle (bundleForRemote (remoteName remote))
            loadTextPaths = Set.fromList . Verify.textEntryPaths <$>
                Verify.loadMetadataFromBundle (bundleForRemote (remoteName remote))
        unless (null result.vrIssues) $
            repairFlow cwd loadMeta loadTextPaths (Just remote) result.vrIssues repairMode concurrency
      else do
        result <- Verify.verifyRemote cwd remote Nothing concurrency
        printVerifyResultWithSkipped result []
        let loadMeta = Verify.binaryEntries <$>
                Verify.loadMetadataFromBundle (bundleForRemote (remoteName remote))
            loadTextPaths = Set.fromList . Verify.textEntryPaths <$>
                Verify.loadMetadataFromBundle (bundleForRemote (remoteName remote))
        unless (null result.vrIssues) $
            repairFlow cwd loadMeta loadTextPaths (Just remote) result.vrIssues repairMode concurrency

-- ============================================================================
-- Repair logic (shared between local and remote, filesystem and cloud)
-- ============================================================================

-- | A source from which files can be copied for repair.
data RepairSource
    = LocalSource FilePath [Verify.BinaryFileMeta]   -- ^ cwd, metadata
    | RemoteSource Remote [Verify.BinaryFileMeta]    -- ^ remote, metadata
    deriving (Show)

-- | Result of executing a single repair action.
data RepairResult = Repaired Path | RepairFailed Path String
    deriving (Show)

-- | Repair flow: prompt (if needed), load sources, plan and execute repairs.
-- Text files are repaired from git; binary files are repaired from other sources.
repairFlow :: FilePath -> IO [Verify.BinaryFileMeta] -> IO (Set.Set Path) -> Maybe Remote -> [Verify.VerifyIssue] -> RepairMode -> Concurrency -> IO ()
repairFlow cwd loadTargetMeta loadTextPaths mTargetRemote issues repairMode _concurrency = do
    shouldRepair <- case repairMode of
        AutoRepair -> pure True
        PromptRepair -> do
            isTTY <- hIsTerminalDevice stdin
            if isTTY
                then do
                    hPutStr stderr $ show (length issues) ++ " issues found. Repair? [y/N] "
                    hFlush stderr
                    response <- getLine
                    pure (response == "y" || response == "Y")
                else do
                    hPutStrLn stderr "Skipping repair (non-interactive). Run 'bit repair' to fix."
                    pure False

    unless shouldRepair $
        exitWith (ExitFailure 1)

    when shouldRepair $ do
        -- Partition issues into text and binary
        textPaths <- loadTextPaths
        let (textIssues, binaryIssues) = partition (isTextIssue textPaths) issues

        -- Load repair sources for binary issues
        sources <- if null binaryIssues
            then pure []
            else do
                let excludeName = remoteName <$> mTargetRemote
                    includeLocal = case mTargetRemote of
                        Nothing -> False
                        Just _  -> True
                srcs <- loadRepairSources cwd excludeName includeLocal
                unless (null srcs) $
                    hPutStrLn stderr $ "Searching " ++ show (length srcs) ++ " source(s): "
                        ++ intercalate ", " (map sourceName srcs)
                pure srcs

        -- Phase 1: Text repair from git
        textResults <- if null textIssues
            then pure []
            else do
                hPutStrLn stderr $ "Restoring " ++ show (length textIssues) ++ " text file(s) from git..."
                repairTextFiles cwd mTargetRemote (map issuePath textIssues)

        forM_ textResults $ \case
            Repaired p -> putStrLn $ "  [REPAIRED] " ++ toPosix (unPath p)
            RepairFailed p e -> hPutStrLn stderr $ "  [FAILED]   " ++ toPosix (unPath p) ++ " (" ++ e ++ ")"

        -- Phase 2: Binary repair from sources
        (binaryResults, unrepairable) <- if null binaryIssues
            then pure ([], [])
            else if null sources
                then do
                    when (null textIssues) $
                        hPutStrLn stderr "No repair sources available (no remotes configured)."
                    pure ([], map issuePath binaryIssues)
                else do
                    targetMeta <- loadTargetMeta
                    let metaMap = Map.fromList [(m.bfmPath, (m.bfmHash, m.bfmSize)) | m <- targetMeta]
                        repairIndex = buildRepairIndex sources
                        (repairable, unrep) = planRepairs binaryIssues metaMap repairIndex

                    results <- if null repairable
                        then pure []
                        else do
                            putStrLn $ "Repairing " ++ show (length repairable) ++ " file(s)..."
                            let total = length repairable
                            forM (zip [1..] repairable) $ \(i :: Int, plan) -> do
                                let label = "(" ++ show i ++ "/" ++ show total ++ ") "
                                        ++ toPosix (unPath (rpDestPath plan))
                                        ++ " from " ++ sourceName (rpSource plan)
                                executeRepairWithProgress cwd mTargetRemote plan label

                    forM_ [p | Repaired p <- results] $ \p ->
                        putStrLn $ "  [REPAIRED] " ++ toPosix (unPath p)
                    forM_ [(p, e) | RepairFailed p e <- results] $ \(p, e) ->
                        hPutStrLn stderr $ "  [FAILED]   " ++ toPosix (unPath p) ++ " (" ++ e ++ ")"

                    pure (results, unrep)

        -- Combined summary
        let textRepaired = [p | Repaired p <- textResults]
            textFailed   = [(p, e) | RepairFailed p e <- textResults]
            binRepaired  = [p | Repaired p <- binaryResults]
            binFailed    = [(p, e) | RepairFailed p e <- binaryResults]
            allRepaired  = textRepaired ++ binRepaired
            allFailed    = textFailed ++ binFailed

        forM_ unrepairable $ \p ->
            hPutStrLn stderr $ "  [UNREPAIRABLE] " ++ toPosix (unPath p)

        putStrLn ""
        putStrLn $ show (length allRepaired) ++ " repaired, "
            ++ show (length allFailed) ++ " failed, "
            ++ show (length unrepairable) ++ " unrepairable."

        unless (null allFailed && null unrepairable) $
            exitWith (ExitFailure 1)

-- | Check if an issue is for a text file.
isTextIssue :: Set.Set Path -> Verify.VerifyIssue -> Bool
isTextIssue textPaths issue = issuePath issue `Set.member` textPaths

-- | Repair text files by restoring from git.
repairTextFiles :: FilePath -> Maybe Remote -> [Path] -> IO [RepairResult]
repairTextFiles cwd Nothing paths = repairTextLocal cwd paths
repairTextFiles cwd (Just remote) paths = do
    mType <- Device.readRemoteType cwd (remoteName remote)
    let isFs = maybe False Device.isFilesystemType mType
    if isFs
        then repairTextFilesystem remote paths
        else repairTextCloud remote paths

-- | Restore text files locally: git restore in index repo, then copy to working dir.
repairTextLocal :: FilePath -> [Path] -> IO [RepairResult]
repairTextLocal cwd paths = do
    indexDir <- Git.getIndexPath
    let pathStrs = map unPath paths
    (code, _, err) <- Git.runGitAt indexDir (["restore", "--"] ++ pathStrs)
    case code of
        ExitSuccess -> forM paths $ \p -> do
            let src = indexDir </> unPath p
                dst = cwd </> unPath p
            Platform.createDirectoryIfMissing True (takeDirectory dst)
            Platform.copyFile src dst
            pure (Repaired p)
        _ -> pure [RepairFailed p ("git restore failed: " ++ err) | p <- paths]

-- | Restore text files on a filesystem remote: git restore in remote index repo, then copy.
repairTextFilesystem :: Remote -> [Path] -> IO [RepairResult]
repairTextFilesystem remote paths = do
    let remoteDir = remoteUrl remote
        indexDir = remoteDir </> ".bit" </> "index"
        pathStrs = map unPath paths
    (code, _, err) <- Git.runGitAt indexDir (["restore", "--"] ++ pathStrs)
    case code of
        ExitSuccess -> forM paths $ \p -> do
            let src = indexDir </> unPath p
                dst = remoteDir </> unPath p
            Platform.createDirectoryIfMissing True (takeDirectory dst)
            Platform.copyFile src dst
            pure (Repaired p)
        _ -> pure [RepairFailed p ("git restore failed: " ++ err) | p <- paths]

-- | Restore text files on a cloud remote: read from bundle commit, upload via rclone.
repairTextCloud :: Remote -> [Path] -> IO [RepairResult]
repairTextCloud remote paths = do
    let bundleName = bundleForRemote (remoteName remote)
    mHash <- Git.getHashFromBundle bundleName
    case mHash of
        Nothing -> pure [RepairFailed p "no bundle hash available" | p <- paths]
        Just hash -> forM paths $ \p -> do
            (code, content, _) <- Git.runGitWithOutput ["show", hash ++ ":" ++ unPath p]
            case code of
                ExitSuccess -> do
                    let tempPath = ".bit" </> "temp_text_repair"
                    Dir.createDirectoryIfMissing True (takeDirectory tempPath)
                    writeFile tempPath content
                    uploadCode <- Transport.copyToRemote tempPath remote (toPosix (unPath p))
                    safeRemoveTemp tempPath
                    case uploadCode of
                        ExitSuccess -> pure (Repaired p)
                        _ -> pure (RepairFailed p "upload to remote failed")
                _ -> pure (RepairFailed p "could not read from bundle commit")
  where
    safeRemoveTemp filePath = do
        exists <- Dir.doesFileExist filePath
        when exists $ Dir.removeFile filePath

-- | Load metadata from all configured remotes (and optionally local) as repair sources.
loadRepairSources :: FilePath -> Maybe String -> Bool -> IO [RepairSource]
loadRepairSources cwd excludeName includeLocal = do
    -- Include local metadata if requested
    localSources <- if includeLocal
        then do
            indexDir <- Git.getIndexPath
            meta <- Verify.loadCommittedBinaryMetadata indexDir
            pure [LocalSource cwd meta | not (null meta)]
        else pure []

    -- List all configured remotes
    let remotesDir = cwd </> ".bit" </> "remotes"
    dirExists <- Dir.doesDirectoryExist remotesDir
    remoteNames <- if dirExists
        then filter (\n -> Just n /= excludeName) <$> Dir.listDirectory remotesDir
        else pure []

    -- Load metadata from each remote
    remoteSources <- fmap concat $ mapM (\name -> do
        mRemote <- resolveRemote cwd name
        case mRemote of
            Nothing -> pure []
            Just remote -> do
                mType <- Device.readRemoteType cwd name
                let isFs = maybe False Device.isFilesystemType mType
                meta <- if isFs
                    then Verify.loadCommittedBinaryMetadata (remoteUrl remote </> ".bit" </> "index")
                    else do
                        entries <- Verify.loadMetadataFromBundle (bundleForRemote name)
                        pure (Verify.binaryEntries entries)
                if null meta
                    then pure []
                    else pure [RemoteSource remote meta]
        ) remoteNames

    pure (localSources ++ remoteSources)

-- | Build a content-addressable index: (hash, size) → (source, path on source).
buildRepairIndex :: [RepairSource] -> Map.Map (String, Integer) (RepairSource, Path)
buildRepairIndex sources = Map.fromList
    [ ((T.unpack (hashToText m.bfmHash), m.bfmSize), (source, m.bfmPath))
    | source <- sources
    , m <- sourceMetadata source
    ]
  where
    sourceMetadata (LocalSource _ meta)  = meta
    sourceMetadata (RemoteSource _ meta) = meta

-- | Human-readable name for a repair source.
sourceName :: RepairSource -> String
sourceName (LocalSource _ _)  = "local"
sourceName (RemoteSource r _) = "'" ++ remoteName r ++ "'"

-- | A planned repair: which file to fix and where to get the correct version.
data RepairPlan = RepairPlan
    { rpDestPath     :: Path                         -- ^ file to fix
    , rpExpectedHash :: Hash 'MD5                    -- ^ expected hash
    , rpExpectedSize :: Integer                      -- ^ expected size
    , rpSource       :: RepairSource                 -- ^ where the correct file lives
    , rpSourcePath   :: Path                         -- ^ path of the file on the source
    }

-- | Plan repairs for each issue. Returns (repairable plans, unrepairable paths).
planRepairs :: [Verify.VerifyIssue]
            -> Map.Map Path (Hash 'MD5, Integer)             -- target metadata map
            -> Map.Map (String, Integer) (RepairSource, Path) -- repair index
            -> ([RepairPlan], [Path])
planRepairs issues metaMap repairIndex = foldr go ([], []) issues
  where
    go issue (repairs, unrepairables) =
        let p = issuePath issue
        in case lookupExpected issue p of
            Nothing -> (repairs, unrepairables)  -- not in binary metadata, skip
            Just (expectedHash, expectedSize) ->
                let key = (T.unpack (hashToText expectedHash), expectedSize)
                in case Map.lookup key repairIndex of
                    Just (source, sourcePath) ->
                        (RepairPlan p expectedHash expectedSize source sourcePath : repairs, unrepairables)
                    Nothing -> (repairs, p : unrepairables)

    lookupExpected _ p = Map.lookup p metaMap

-- | Execute a single repair plan with a live progress bar during the copy.
executeRepairWithProgress :: FilePath -> Maybe Remote -> RepairPlan -> String -> IO RepairResult
executeRepairWithProgress cwd mTargetRemote plan label = do
    let fileSize = rpExpectedSize plan
    isTTY <- hIsTerminalDevice stderr

    -- | Run an action with a progress bar showing bytes / fileSize.
    let withProgressBar progressLabel action = do
            bytesRef <- newIORef (0 :: Integer)
            if isTTY && fileSize > 0
                then bracket
                    (forkIO (repairFileProgress bytesRef fileSize progressLabel))
                    (\tid -> killThread tid >> clearProgress)
                    (\_ -> action bytesRef)
                else do
                    hPutStrLn stderr $ "  " ++ progressLabel ++ "..."
                    action bytesRef

    case mTargetRemote of
        -- Target is local: copy from source to local
        Nothing -> case rpSource plan of
            RemoteSource remote _ -> do
                let localDest = cwd </> unPath (rpDestPath plan)
                    remoteSrc = Transport.remoteFilePath remote (toPosix (unPath (rpSourcePath plan)))
                Dir.createDirectoryIfMissing True (takeDirectory localDest)
                withProgressBar label $ \bytesRef -> do
                    code <- CopyProgress.rcloneCopyto remoteSrc localDest bytesRef
                    case code of
                        ExitSuccess -> do
                            restoreLocalMetadata cwd (rpDestPath plan) (rpExpectedHash plan) (rpExpectedSize plan)
                            pure (Repaired (rpDestPath plan))
                        _ -> pure (RepairFailed (rpDestPath plan) "copy from remote failed")
            LocalSource _ _ ->
                pure (RepairFailed (rpDestPath plan) "internal error: local source for local target")

        -- Target is a remote: copy from source to that remote
        Just targetRemote -> case rpSource plan of
            LocalSource localCwd _ -> do
                let localSrc = localCwd </> unPath (rpSourcePath plan)
                    remoteDst = Transport.remoteFilePath targetRemote (toPosix (unPath (rpDestPath plan)))
                withProgressBar label $ \bytesRef -> do
                    code <- CopyProgress.rcloneCopyto localSrc remoteDst bytesRef
                    case code of
                        ExitSuccess -> do
                            restoreRemoteMetadata targetRemote (rpDestPath plan) (rpExpectedHash plan) (rpExpectedSize plan)
                            pure (Repaired (rpDestPath plan))
                        _ -> pure (RepairFailed (rpDestPath plan) "copy to remote failed")
            RemoteSource sourceRemote _ -> do
                let remoteSrc = Transport.remoteFilePath sourceRemote (toPosix (unPath (rpSourcePath plan)))
                    remoteDst = Transport.remoteFilePath targetRemote (toPosix (unPath (rpDestPath plan)))
                -- Single rclone call — rclone handles the routing between remotes
                withProgressBar label $ \bytesRef -> do
                    code <- CopyProgress.rcloneCopyto remoteSrc remoteDst bytesRef
                    case code of
                        ExitSuccess -> do
                            restoreRemoteMetadata targetRemote (rpDestPath plan) (rpExpectedHash plan) (rpExpectedSize plan)
                            pure (Repaired (rpDestPath plan))
                        _ -> pure (RepairFailed (rpDestPath plan) "copy between remotes failed")

-- | Progress reporter for a single file repair.
repairFileProgress :: IORef Integer -> Integer -> String -> IO ()
repairFileProgress bytesRef totalSize progressLabel = go
  where
    go = do
        b <- readIORef bytesRef
        let pct = if totalSize > 0 then (b * 100) `div` totalSize else 0
        reportProgress $ "  " ++ progressLabel ++ " — "
            ++ formatBytes b ++ " / " ++ formatBytes totalSize
            ++ " (" ++ show pct ++ "%)"
        threadDelay 100000
        when (b < totalSize) go

-- | Restore the local .bit/index/ metadata file after repair.
restoreLocalMetadata :: FilePath -> Path -> Hash 'MD5 -> Integer -> IO ()
restoreLocalMetadata _cwd destPath expectedHash expectedSize = do
    indexDir <- Git.getIndexPath
    let metaPath = indexDir </> unPath destPath
    Dir.createDirectoryIfMissing True (takeDirectory metaPath)
    atomicWriteFileStr metaPath (serializeMetadata (MetaContent expectedHash expectedSize))

-- | Restore the remote .bit/index/ metadata file after repair.
-- Only writes if the remote has .bit/ on disk (filesystem remotes).
-- Cloud remotes skip this since their metadata lives in the bundle.
restoreRemoteMetadata :: Remote -> Path -> Hash 'MD5 -> Integer -> IO ()
restoreRemoteMetadata remote destPath expectedHash expectedSize = do
    let remotePath = remoteUrl remote
    bitExists <- Dir.doesDirectoryExist (remotePath </> ".bit")
    when bitExists $ do
        let remoteMetaPath = remotePath </> bitIndexPath </> unPath destPath
        Platform.createDirectoryIfMissing True (takeDirectory remoteMetaPath)
        atomicWriteFileStr remoteMetaPath (serializeMetadata (MetaContent expectedHash expectedSize))

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Extract the path from a VerifyIssue.
issuePath :: Verify.VerifyIssue -> Path
issuePath (Verify.HashMismatch p _ _ _ _) = p
issuePath (Verify.Missing p) = p

verifyProgressLoop :: IORef Int -> Int -> IO ()
verifyProgressLoop counter total = go
  where
    go = do
      n <- readIORef counter
      let pct = (n * 100) `div` max 1 total
      reportProgress $ "Checking files: " ++ show n ++ "/" ++ show total ++ " (" ++ show pct ++ "%)"
      threadDelay 100000
      when (n < total) go

-- | Truncate a hash string to 16 characters with ellipsis.
truncateHash :: String -> String
truncateHash s = take 16 s ++ if length s > 16 then "..." else ""

-- | Print verify result with optional skipped file info.
printVerifyResultWithSkipped :: Verify.VerifyResult -> [FilePath] -> IO ()
printVerifyResultWithSkipped result skipped = do
    if null result.vrIssues
        then do
            putStrLn $ "[OK] All " ++ show result.vrCount ++ " files match metadata."
            unless (null skipped) $
                putStrLn $ show (length skipped) ++ " files skipped (slow storage, hashing aborted)."
        else do
            mapM_ (printVerifyIssue truncateHash) result.vrIssues
            putStrLn $ "Checked " ++ show result.vrCount ++ " files. "
                ++ show (length result.vrIssues) ++ " issues found."
            unless (null skipped) $
                putStrLn $ show (length skipped) ++ " files skipped (slow storage, hashing aborted)."

fsck :: FilePath -> IO ()
fsck = Fsck.doFsck

-- | Walk historical commits, store any blobs currently present in the working tree into CAS.
casBackfill :: FilePath -> IO ()
casBackfill cwd = do
  indexDir <- Git.getIndexPath
  let bitDir = takeDirectory indexDir
      casDir = bitDir </> "cas"
  (code, revListOut, _) <- Git.runGitAt indexDir ["rev-list", "HEAD"]
  commits <- case code of
    ExitSuccess -> pure (filter (not . null) (lines revListOut))
    _ -> pure []
  -- Collect all binary hashes from all commits
  -- TODO: For large repos, stream per-commit and merge into a Set to avoid loading all metadata at once.
  allEntries <- forM commits $ \rev ->
    Verify.loadMetadata (Verify.FromCommit (filter (not . isSpace) rev)) Sequential
  let allHashes = Set.fromList [h | entries <- allEntries, Verify.BinaryEntry _ h _ <- entries]
  -- Filter to hashes not already in CAS
  toBackfill <- filterM (\h -> do
    hasBlob <- hasBlobInCas casDir h
    if hasBlob then pure False
    else isNothing <$> readManifestFromCas casDir h
    ) (Set.toList allHashes)
  -- Scan working tree to get hash -> path (first path per hash)
  -- TODO: For large repos, read hashes from .bit/index metadata and only hash files in toBackfill instead of full scan.
  entries <- Scan.scanWorkingDir cwd Sequential
  let hashToPath = Map.fromList
        [ (h, cwd </> unPath (path e))
        | e <- entries
        , File h _ _ <- [kind e]
        ]
  -- For each missing hash, if in working tree write to CAS
  written <- forM toBackfill $ \h -> do
    case Map.lookup h hashToPath of
      Just workPath -> do
        writeBlobToCas workPath casDir h
        pure True
      Nothing -> pure False
  let n = length (filter id written)
  when (n > 0) $ putStrLn $ "Stored " ++ show n ++ " blob(s) in CAS."
