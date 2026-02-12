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
    ) where

import System.FilePath ((</>), takeDirectory)
import Control.Monad (when, unless, forM_, forM)
import Data.Foldable (traverse_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Control.Exception (bracket, finally)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import System.IO (stderr, stdin, hPutStr, hPutStrLn, hFlush, hIsTerminalDevice)
import System.Exit (ExitCode(..), exitWith)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified System.Directory as Dir

import Bit.Types (BitM, BitEnv(..), Path(..), Hash(..), HashAlgo(..), hashToText)
import Bit.Concurrency (Concurrency)
import qualified Bit.Verify as Verify
import qualified Bit.Fsck as Fsck
import Internal.Config (bundleForRemote, bitIndexPath, bitRemotesDir)
import Bit.Progress (reportProgress, clearProgress)
import Bit.Utils (toPosix, atomicWriteFileStr, formatBytes)
import Bit.Internal.Metadata (MetaContent(..), serializeMetadata)
import qualified Bit.Device as Device
import qualified Bit.CopyProgress as CopyProgress
import qualified Internal.Transport as Transport

import Bit.Core.Helpers (printVerifyIssue, isFilesystemRemote)
import Bit.Remote (Remote, remoteName, remoteUrl, resolveRemote)
import qualified Bit.Platform as Platform

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
    liftIO $ verifyFilesystem cwd cwd Nothing repairMode concurrency

verify (VerifyRemotePath remote) repairMode concurrency = do
    cwd <- asks envCwd
    isFs <- isFilesystemRemote remote
    if isFs
        then liftIO $ verifyFilesystem cwd (remoteUrl remote) (Just remote) repairMode concurrency
        else liftIO $ verifyCloud cwd remote repairMode concurrency

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
verifyFilesystem :: FilePath -> FilePath -> Maybe Remote -> RepairMode -> Concurrency -> IO ()
verifyFilesystem cwd targetPath mTargetRemote repairMode concurrency = do
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
    let loadMeta = Verify.loadCommittedBinaryMetadata (targetPath </> bitIndexPath)
    unless (null result.vrIssues) $
        repairFlow cwd loadMeta mTargetRemote result.vrIssues repairMode concurrency

-- ============================================================================
-- Cloud verify (uses bundle + rclone)
-- ============================================================================

-- | Verify a cloud remote using the fetched bundle.
verifyCloud :: FilePath -> Remote -> RepairMode -> Concurrency -> IO ()
verifyCloud cwd remote repairMode concurrency = do
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
        unless (null result.vrIssues) $
            repairFlow cwd loadMeta (Just remote) result.vrIssues repairMode concurrency
      else do
        result <- Verify.verifyRemote cwd remote Nothing concurrency
        printVerifyResultWithSkipped result []
        let loadMeta = Verify.binaryEntries <$>
                Verify.loadMetadataFromBundle (bundleForRemote (remoteName remote))
        unless (null result.vrIssues) $
            repairFlow cwd loadMeta (Just remote) result.vrIssues repairMode concurrency

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
-- The loadTargetMeta action is called only after the user confirms repair.
repairFlow :: FilePath -> IO [Verify.BinaryFileMeta] -> Maybe Remote -> [Verify.VerifyIssue] -> RepairMode -> Concurrency -> IO ()
repairFlow cwd loadTargetMeta mTargetRemote issues repairMode _concurrency = do
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
        -- Load committed metadata for the target (to know expected hash+size for Missing issues)
        targetMeta <- loadTargetMeta
        let metaMap = Map.fromList [(m.bfmPath, (m.bfmHash, m.bfmSize)) | m <- targetMeta]

        -- Load repair sources (everything except the target)
        let excludeName = remoteName <$> mTargetRemote
            includeLocal = case mTargetRemote of
                Nothing -> False  -- target is local, don't include local as source
                Just _  -> True   -- target is remote, include local as source
        sources <- loadRepairSources cwd excludeName includeLocal

        if null sources
            then do
                hPutStrLn stderr "No repair sources available (no remotes configured)."
                exitWith (ExitFailure 1)
            else do
                -- Print which sources will be searched
                hPutStrLn stderr $ "Searching " ++ show (length sources) ++ " source(s): "
                    ++ intercalate ", " (map sourceName sources)
                let repairIndex = buildRepairIndex sources
                -- Plan repairs for each issue
                let plans = planRepairs issues metaMap repairIndex
                    (repairable, unrepairable) = (fst plans, snd plans)

                unless (null repairable) $ do
                    putStrLn $ "Repairing " ++ show (length repairable) ++ " file(s)..."
                    let total = length repairable
                    results <- forM (zip [1..] repairable) $ \(i :: Int, plan) -> do
                        let label = "(" ++ show i ++ "/" ++ show total ++ ") "
                                ++ toPosix (unPath (rpDestPath plan))
                                ++ " from " ++ sourceName (rpSource plan)
                        executeRepairWithProgress cwd mTargetRemote plan label

                    let repaired = [p | Repaired p <- results]
                        failed   = [(p, e) | RepairFailed p e <- results]

                    forM_ repaired $ \p ->
                        putStrLn $ "  [REPAIRED] " ++ toPosix (unPath p)
                    forM_ failed $ \(p, e) ->
                        hPutStrLn stderr $ "  [FAILED]   " ++ toPosix (unPath p) ++ " (" ++ e ++ ")"

                    putStrLn ""
                    putStrLn $ show (length repaired) ++ " repaired, "
                        ++ show (length failed) ++ " failed, "
                        ++ show (length unrepairable) ++ " unrepairable."

                    unless (null failed && null unrepairable) $
                        exitWith (ExitFailure 1)

                when (null repairable && not (null unrepairable)) $ do
                    forM_ unrepairable $ \p ->
                        hPutStrLn stderr $ "  [UNREPAIRABLE] " ++ toPosix (unPath p)
                    putStrLn ""
                    putStrLn $ "0 repaired, 0 failed, " ++ show (length unrepairable) ++ " unrepairable."
                    exitWith (ExitFailure 1)

-- | Load metadata from all configured remotes (and optionally local) as repair sources.
loadRepairSources :: FilePath -> Maybe String -> Bool -> IO [RepairSource]
loadRepairSources cwd excludeName includeLocal = do
    -- Include local metadata if requested
    localSources <- if includeLocal
        then do
            meta <- Verify.loadCommittedBinaryMetadata (cwd </> bitIndexPath)
            pure [LocalSource cwd meta | not (null meta)]
        else pure []

    -- List all configured remotes
    let remotesDir = cwd </> bitRemotesDir
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
                    then Verify.loadCommittedBinaryMetadata (remoteUrl remote </> bitIndexPath)
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
restoreLocalMetadata cwd destPath expectedHash expectedSize = do
    let metaPath = cwd </> bitIndexPath </> unPath destPath
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
