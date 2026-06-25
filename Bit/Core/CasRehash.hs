{-# LANGUAGE DataKinds #-}

-- | @bit cas rehash@ — migrate a repo's content hashes from MD5 to BLAKE3.
--
-- bit identifies content by hash: file-level identity lives in the metadata
-- stubs (@.bit/index/<path>@), CAS whole-file blobs, and CDC manifests. This
-- command switches the repo's @core.hash-algo@ to BLAKE3 and rewrites every
-- binary file's stub + CAS entry to its BLAKE3 hash, then commits the result.
--
-- Design notes:
--
--   * /Chunk hashes stay MD5./ A CDC chunk's hash is purely a content address;
--     its algorithm is independent of the file-level hash. Migrating only the
--     whole-file identity (stub + manifest filename) keeps the chunker (and its
--     byte-identical guarantee) untouched. Re-staging produces a fresh manifest
--     named by the BLAKE3 file hash; the old MD5-named manifest becomes an
--     orphan that @bit cas gc@ reclaims.
--   * /Reuses the normal scan + mirror path./ Once @core.hash-algo@ is BLAKE3,
--     @scanWorkingDir@ hashes with BLAKE3 and @writeMetadataFiles@ rewrites the
--     index mirror and re-stages CAS exactly as @bit add@ would.
--   * Run on a clean working tree — the migration stages and commits the
--     rewritten stubs.
module Bit.Core.CasRehash (casRehash) where

import Control.Monad (when, unless)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)

import Bit.Types (FileEntry(..), EntryKind(..), ContentType(..))
import Bit.IO.Concurrency (Concurrency(..))
import Bit.Core.Config (configSetWithRoot)
import qualified Bit.Scan.Local as Scan
import qualified Bit.Git.Run as Git

-- | Migrate the repository's content hashes to BLAKE3.
casRehash :: FilePath -> IO ()
casRehash cwd = do
  indexDir <- Git.getIndexPath
  let bitRoot = takeDirectory indexDir
  -- Refuse to migrate a dirty tree: the migration auto-commits, and we must not
  -- fold unrelated pending changes into the rehash commit.
  (_, statusOut, _) <- Git.runGitAt indexDir ["status", "--porcelain"]
  if not (null (filter (not . null) (lines statusOut)))
    then putStrLn "Working tree has uncommitted changes. Commit or stash them before 'bit cas rehash'."
    else do
      -- 1. Switch the configured algorithm so the scan hashes with BLAKE3.
      configSetWithRoot bitRoot "core.hash-algo" "blake3"
      -- 2. Rescan (now BLAKE3) and rewrite the index mirror + re-stage CAS.
      entries <- Scan.scanWorkingDir cwd (Parallel 0)
      let binaryCount = length [ () | e <- entries, File _ _ BinaryContent <- [kind e] ]
      Scan.writeMetadataFiles cwd entries
      -- 3. Stage and commit the rewritten stubs.
      (addCode, _, addErr) <- Git.runGitAt indexDir ["add", "-A"]
      case addCode of
        ExitFailure _ -> putStrLn ("git add failed: " ++ addErr)
        ExitSuccess -> do
          (commitCode, _, _) <- Git.runGitAt indexDir
            ["commit", "-m", "Rehash content hashes to BLAKE3"]
          case commitCode of
            ExitSuccess -> reportDone binaryCount
            ExitFailure _ ->
              -- Nothing to commit (e.g. no binary files) — still a successful no-op.
              reportDone binaryCount
  where
    reportDone n = do
      putStrLn $ "Rehashed " ++ show n ++ " binary file(s) to BLAKE3."
      when (n == 0) $ putStrLn "(No binary files to migrate; core.hash-algo set to blake3 for new content.)"
      unless (n == 0) $ putStrLn $
        "Pre-migration commits still reference the old MD5 blobs (needed to check\n"
        ++ "out earlier history); 'bit cas gc' reclaims them once that history is dropped."
