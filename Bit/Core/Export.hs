{-# LANGUAGE ScopedTypeVariables #-}

module Bit.Core.Export
    ( exportRepo
    ) where

import qualified System.Directory as Dir
import qualified Bit.IO.Platform as Platform
import System.FilePath ((</>), takeFileName)
import Control.Monad (when, forM_)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, IOException)
import Data.List (isInfixOf)
import qualified Bit.Git.Run as Git

-- | Export a bit repo back to a plain git repo.
-- First arg: optional target path (copy export). Second arg: bit repo root.
-- In-place: moves .bit/index/.git back to root, deletes .bit/.
-- To path: copies .bit/index/.git and working files to target.
exportRepo :: Maybe FilePath -> FilePath -> IO ExitCode
exportRepo Nothing root = exportInPlace root
exportRepo (Just target) root = exportToPath root target

-- | In-place export: convert bit repo back to git repo.
-- Handles two layouts:
--   Hybrid: .git is real dir at root, .bit/index/.git is gitfile → just delete .bit/
--   Legacy: .bit/index/.git is real dir, .git is gitfile/junction → move .git, delete .bit/
exportInPlace :: FilePath -> IO ExitCode
exportInPlace root = do
    let bitIndexGit = root </> ".bit" </> "index" </> ".git"
    let rootGit = root </> ".git"
    -- Detect layout: if .git at root is a real directory, it's hybrid
    rootGitIsDir <- Platform.doesDirectoryExist rootGit
    indexGitIsDir <- Platform.doesDirectoryExist bitIndexGit
    indexGitIsFile <- Platform.doesFileExist bitIndexGit
    indexGitIsLink <- Dir.pathIsSymbolicLink bitIndexGit `catch` \(_ :: IOException) -> pure False
    if rootGitIsDir && (indexGitIsFile || indexGitIsLink)
        then do
            -- Hybrid layout: .git is already the real dir, .bit/index/.git is a gitfile or symlink.
            -- Remove bundle remotes before deleting .bit/
            removeBundleRemotes (root </> ".bit" </> "index")
            if indexGitIsLink
                then Dir.removeDirectoryLink bitIndexGit
                else Dir.removeFile bitIndexGit
            Dir.removeDirectoryRecursive (root </> ".bit")
            putStrLn $ "Exported bit repository to git: " ++ root
            putStrLn "The .bit directory has been removed. This is now a plain git repo."
            pure ExitSuccess
        else if indexGitIsDir || indexGitIsFile
            then do
                -- Legacy layout: .bit/index/.git is the real dir (or gitfile to separate dir)
                -- Remove .git junction/gitlink at repo root (if any)
                removeRootGit rootGit
                -- Move .bit/index/.git -> root/.git
                if indexGitIsDir
                    then Dir.renameDirectory bitIndexGit rootGit
                    else Dir.copyFile bitIndexGit rootGit  -- gitfile: just copy
                -- Remove bundle remotes before deleting .bit/
                removeBundleRemotes (root </> ".bit" </> "index")
                -- Delete .bit/
                Dir.removeDirectoryRecursive (root </> ".bit")
                putStrLn $ "Exported bit repository to git: " ++ root
                putStrLn "The .bit directory has been removed. This is now a plain git repo."
                pure ExitSuccess
            else do
                hPutStrLn stderr "fatal: not a valid bit repository (.bit/index/.git not found)"
                pure (ExitFailure 1)

-- | Remove the .git at repo root, which may be a junction, gitlink file,
-- or absent. Junctions are removed with removeDirectoryLink (safe — does not
-- follow into the target). Gitlink files are removed with removeFile.
-- Real directories (hybrid layout) are NOT removed here — they contain
-- the actual git database.
removeRootGit :: FilePath -> IO ()
removeRootGit path = do
    isDir <- Platform.doesDirectoryExist path
    isFile <- Platform.doesFileExist path
    -- Only remove junctions (empty dir-like links) and gitfile files.
    -- Real directories with contents are the hybrid layout's git dir — skip.
    when isDir $
        Dir.removeDirectoryLink path
    when (isFile && not isDir) $
        Dir.removeFile path

-- | Export to a new directory: copy git dir and working files.
-- Handles both hybrid (.git is real dir at root) and legacy (.bit/index/.git) layouts.
exportToPath :: FilePath -> FilePath -> IO ExitCode
exportToPath root target = do
    let bitIndexGit = root </> ".bit" </> "index" </> ".git"
    let rootGit = root </> ".git"
    -- Detect layout
    rootGitIsDir <- Platform.doesDirectoryExist rootGit
    indexGitIsDir <- Platform.doesDirectoryExist bitIndexGit
    indexGitIsFile <- Platform.doesFileExist bitIndexGit
    indexGitIsLink <- Dir.pathIsSymbolicLink bitIndexGit `catch` \(_ :: IOException) -> pure False
    -- Determine the source git dir to copy
    let mGitSource
          | rootGitIsDir && (indexGitIsFile || indexGitIsLink) = Just rootGit     -- hybrid
          | indexGitIsDir                  = Just bitIndexGit  -- legacy (dir)
          | indexGitIsFile                 = Nothing           -- legacy (gitfile) - needs special handling
          | otherwise                      = Nothing
    case mGitSource of
        Nothing
          | not indexGitIsDir && not indexGitIsFile -> do
                hPutStrLn stderr "fatal: not a valid bit repository (.bit/index/.git not found)"
                pure (ExitFailure 1)
          | otherwise -> do
                hPutStrLn stderr "fatal: cannot export: .bit/index/.git is a gitlink (unsupported for copy export)"
                pure (ExitFailure 1)
        Just gitSource -> do
            targetExists <- Platform.doesDirectoryExist target
            targetFileExists <- Platform.doesFileExist target
            if targetExists || targetFileExists
                then do
                    hPutStrLn stderr $ "fatal: target path already exists: " ++ target
                    pure (ExitFailure 1)
                else do
                    -- Create target directory
                    Dir.createDirectoryIfMissing True target
                    -- Copy git dir -> target/.git
                    copyDirectoryRecursive gitSource (target </> ".git")
                    -- Copy working directory files (skip .bit and .git)
                    copyWorkingFiles root target
                    putStrLn $ "Exported bit repository to: " ++ target
                    putStrLn "Original bit repo is unchanged."
                    pure ExitSuccess

-- | Recursively copy a directory.
copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive src dst = do
    Dir.createDirectoryIfMissing True dst
    entries <- Dir.listDirectory src
    forM_ entries $ \entry -> do
        let srcPath = src </> entry
            dstPath = dst </> entry
        isDir <- Platform.doesDirectoryExist srcPath
        if isDir
            then copyDirectoryRecursive srcPath dstPath
            else Dir.copyFile srcPath dstPath

-- | Remove git remotes that point to bit's internal bundle paths.
-- These are cloud transport artifacts that won't work in a plain git repo.
removeBundleRemotes :: FilePath -> IO ()
removeBundleRemotes indexDir = do
    (code, out, _) <- Git.runGitAt indexDir ["remote"]
    when (code == ExitSuccess) $ do
        let names = filter (not . null) (lines out)
        forM_ names $ \name -> do
            (urlCode, urlOut, _) <- Git.runGitAt indexDir
                ["config", "--get", "remote." ++ name ++ ".url"]
            when (urlCode == ExitSuccess) $ do
                let url = filter (/= '\n') urlOut
                when (isBundlePath url) $
                    Git.runGitAt indexDir ["remote", "remove", name] >> pure ()
  where
    isBundlePath url = ".git/bundles/" `isInfixOf` url || ".git\\bundles/" `isInfixOf` url

-- | Copy working directory files from root to target, skipping .bit and .git.
copyWorkingFiles :: FilePath -> FilePath -> IO ()
copyWorkingFiles src dst = do
    entries <- Dir.listDirectory src
    forM_ entries $ \entry -> do
        let name = takeFileName entry
        when (name /= ".bit" && name /= ".git") $ do
            let srcPath = src </> entry
                dstPath = dst </> entry
            isDir <- Platform.doesDirectoryExist srcPath
            if isDir
                then copyDirectoryRecursive srcPath dstPath
                else Dir.copyFile srcPath dstPath
