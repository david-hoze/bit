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

-- | Export a bit repo back to a plain git repo.
-- First arg: optional target path (copy export). Second arg: bit repo root.
-- In-place: moves .bit/index/.git back to root, deletes .bit/.
-- To path: copies .bit/index/.git and working files to target.
exportRepo :: Maybe FilePath -> FilePath -> IO ExitCode
exportRepo Nothing root = exportInPlace root
exportRepo (Just target) root = exportToPath root target

-- | In-place export: convert bit repo back to git repo.
exportInPlace :: FilePath -> IO ExitCode
exportInPlace root = do
    let bitIndexGit = root </> ".bit" </> "index" </> ".git"
    hasIndexGit <- Platform.doesDirectoryExist bitIndexGit
    hasIndexGitFile <- Platform.doesFileExist bitIndexGit
    if not hasIndexGit && not hasIndexGitFile
        then do
            hPutStrLn stderr "fatal: not a valid bit repository (.bit/index/.git not found)"
            pure (ExitFailure 1)
        else do
            -- Remove .git junction/gitlink at repo root (if any)
            removeRootGit (root </> ".git")
            -- Move .bit/index/.git -> root/.git
            Dir.renameDirectory bitIndexGit (root </> ".git")
            -- Delete .bit/
            Dir.removeDirectoryRecursive (root </> ".bit")
            putStrLn $ "Exported bit repository to git: " ++ root
            putStrLn "The .bit directory has been removed. This is now a plain git repo."
            pure ExitSuccess

-- | Remove the .git at repo root, which may be a junction, gitlink file,
-- or absent. Junctions are removed with removeDirectoryLink (safe — does not
-- follow into the target). Gitlink files are removed with removeFile.
removeRootGit :: FilePath -> IO ()
removeRootGit path = do
    isDir <- Platform.doesDirectoryExist path
    isFile <- Platform.doesFileExist path
    when isDir $
        Dir.removeDirectoryLink path
    when (isFile && not isDir) $
        Dir.removeFile path

-- | Export to a new directory: copy .bit/index/.git and working files.
exportToPath :: FilePath -> FilePath -> IO ExitCode
exportToPath root target = do
    let bitIndexGit = root </> ".bit" </> "index" </> ".git"
    hasIndexGit <- Platform.doesDirectoryExist bitIndexGit
    hasIndexGitFile <- Platform.doesFileExist bitIndexGit
    if not hasIndexGit && not hasIndexGitFile
        then do
            hPutStrLn stderr "fatal: not a valid bit repository (.bit/index/.git not found)"
            pure (ExitFailure 1)
        else do
            targetExists <- Platform.doesDirectoryExist target
            targetFileExists <- Platform.doesFileExist target
            if targetExists || targetFileExists
                then do
                    hPutStrLn stderr $ "fatal: target path already exists: " ++ target
                    pure (ExitFailure 1)
                else do
                    -- Create target directory
                    Dir.createDirectoryIfMissing True target
                    -- Copy .bit/index/.git -> target/.git
                    copyDirectoryRecursive bitIndexGit (target </> ".git")
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
