{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Core.Init
    ( init
    , initializeRepoAt
    ) where

import Prelude hiding (init)
import qualified System.Directory as Dir
import qualified Bit.Platform as Platform
import System.FilePath ((</>))
import Control.Monad (unless, void)
import qualified Internal.Git as Git
import System.Process (readProcessWithExitCode)
import Bit.Utils (atomicWriteFileStr, toPosix)

init :: IO ()
init = initializeRepo

initializeRepo :: IO ()
initializeRepo = do
    cwd <- Dir.getCurrentDirectory
    putStrLn $ "Initializing bit in: " ++ cwd
    initializeRepoAt cwd
    putStrLn "bit initialized successfully!"

-- | Initialize a bit repository at the specified target directory.
-- This is used both for local `bit init` and for creating filesystem remotes.
initializeRepoAt :: FilePath -> IO ()
initializeRepoAt targetDir = do
    let targetBitDir = targetDir </> ".bit"
    let targetBitIndexPath = targetBitDir </> "index"
    let targetBitGitDir = targetBitIndexPath </> ".git"
    let targetBitDevicesDir = targetBitDir </> "devices"
    let targetBitRemotesDir = targetBitDir </> "remotes"

    -- 1. Create .bit directory
    Platform.createDirectoryIfMissing True targetBitDir

    -- 2. Create .bit/index directory (needed before git init)
    Platform.createDirectoryIfMissing True targetBitIndexPath

    -- 3. Init Git in the index directory
    hasGit <- Platform.doesDirectoryExist targetBitGitDir
    unless hasGit $ do
        -- Initialize git in .bit/index, which will create .bit/index/.git
        void $ Git.runGitAt targetBitIndexPath ["init"]
        
        -- Fix for Windows external/USB drives: add to safe.directory
        -- git 2.35.2+ rejects directories with different ownership
        absIndex <- Dir.makeAbsolute targetBitIndexPath
        let safePath = toPosix absIndex
        void $ readProcessWithExitCode "git" ["config", "--global", "--add", "safe.directory", safePath] ""

    -- 3a. Create .git/bundles directory for storing bundle files
    Platform.createDirectoryIfMissing True (targetBitGitDir </> "bundles")

    -- 4. Configure default branch name to "main" (for the repo we just created)
    void $ Git.runGitAt targetBitIndexPath ["config", "init.defaultBranch", "main"]
    
    -- 4a. Configure core.quotePath to false (display Unicode filenames properly)
    void $ Git.runGitAt targetBitIndexPath ["config", "core.quotePath", "false"]
    
    -- 5. Rename the initial branch to "main" if it's "master"
    void $ Git.runGitAt targetBitIndexPath ["branch", "-m", "master", "main"]

    -- 6. Create other .bit subdirectories
    Platform.createDirectoryIfMissing True targetBitDevicesDir
    Platform.createDirectoryIfMissing True targetBitRemotesDir

    -- 5a. Create config file with default values
    let configPath = targetBitDir </> "config"
    configExists <- Platform.doesFileExist configPath
    unless configExists $ do
        let defaultConfig = unlines
                [ "[text]"
                , "    size-limit = 1048576  # 1MB, files larger are always binary"
                , "    extensions = .txt,.md,.yaml,.yml,.json,.xml,.html,.css,.js,.py,.hs,.rs"
                ]
        atomicWriteFileStr configPath defaultConfig

    -- 5b. Merge driver: prevent Git from writing conflict markers
    void $ Git.runGitAt targetBitIndexPath ["config", "merge.bit-metadata.name", "bit metadata"]
    void $ Git.runGitAt targetBitIndexPath ["config", "merge.bit-metadata.driver", "false"]
    Platform.createDirectoryIfMissing True (targetBitGitDir </> "info")
    atomicWriteFileStr (targetBitGitDir </> "info" </> "attributes") "* merge=bit-metadata -text\n"
