{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Core.Init
    ( init
    , initializeRepoAt
    , initializeRemoteRepoAt
    , InitOptions(..)
    , defaultInitOptions
    , initializeBareRepoAt
    ) where

import Prelude hiding (init)
import qualified System.Directory as Dir
import qualified Bit.IO.Platform as Platform
import System.FilePath ((</>))
import Control.Monad (unless, void, when)
import qualified Bit.Git.Run as Git
import System.Process (readProcessWithExitCode)
import Bit.Utils (atomicWriteFileStr, toPosix)
import Bit.Path (RemotePath(..))

-- | Options for 'bit init'.
data InitOptions = InitOptions
    { initQuiet    :: Bool      -- ^ Suppress bit output
    , initBare     :: Bool      -- ^ Bare repository (stub, not yet implemented)
    , initGitFlags :: [String]  -- ^ Extra flags passed through to 'git init'
    } deriving (Show)

defaultInitOptions :: InitOptions
defaultInitOptions = InitOptions False False []

init :: IO ()
init = initWith defaultInitOptions

initWith :: InitOptions -> IO ()
initWith opts = do
    cwd <- Dir.getCurrentDirectory
    unless (initQuiet opts) $
        putStrLn $ "Initializing bit in: " ++ cwd
    when (initBare opts) $ initializeBareRepoAt cwd
    initializeRepoAt cwd opts
    unless (initQuiet opts) $
        putStrLn "bit initialized successfully!"

-- | Stub for bare repository support (not yet implemented).
initializeBareRepoAt :: FilePath -> IO ()
initializeBareRepoAt _dir = pure ()

-- | Initialize a bit repository at the specified target directory.
-- This is used both for local `bit init` and for creating filesystem remotes.
initializeRepoAt :: FilePath -> InitOptions -> IO ()
initializeRepoAt targetDir opts = do
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
    -- Check for .git as directory (normal) or file (gitlink from --separate-git-dir)
    hasGitDir <- Platform.doesDirectoryExist targetBitGitDir
    hasGitFile <- Platform.doesFileExist targetBitGitDir
    unless (hasGitDir || hasGitFile) $ do
        -- Initialize git in .bit/index, which will create .bit/index/.git
        void $ Git.runGitAt targetBitIndexPath ("init" : initGitFlags opts)

        -- Fix for Windows external/USB drives: add to safe.directory
        -- git 2.35.2+ rejects directories with different ownership
        absIndex <- Dir.makeAbsolute targetBitIndexPath
        let safePath = toPosix absIndex
        void $ readProcessWithExitCode "git" ["config", "--global", "--add", "safe.directory", safePath] ""

    -- 3a. Create .git/bundles directory for storing bundle files
    -- With --separate-git-dir, .git is a gitlink file; git resolves it transparently
    -- so we create bundles via git config dir
    when hasGitDir $
        Platform.createDirectoryIfMissing True (targetBitGitDir </> "bundles")
    when (not hasGitDir && not hasGitFile) $ do
        -- Fresh init: check if .git ended up as dir or gitlink
        isDir <- Platform.doesDirectoryExist targetBitGitDir
        if isDir
            then Platform.createDirectoryIfMissing True (targetBitGitDir </> "bundles")
            else do
                -- gitlink: resolve real git dir and create bundles there
                (_, gitDir, _) <- Git.runGitAt targetBitIndexPath ["rev-parse", "--git-dir"]
                let realGitDir = filter (/= '\n') gitDir
                Platform.createDirectoryIfMissing True (realGitDir </> "bundles")

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
    -- For gitlink case, resolve real git dir for info/attributes
    isGitDir <- Platform.doesDirectoryExist targetBitGitDir
    let getInfoDir
          | isGitDir = pure (targetBitGitDir </> "info")
          | otherwise = do
              (_, gitDir, _) <- Git.runGitAt targetBitIndexPath ["rev-parse", "--git-dir"]
              pure (filter (/= '\n') gitDir </> "info")
    infoDir <- getInfoDir
    Platform.createDirectoryIfMissing True infoDir
    atomicWriteFileStr (infoDir </> "attributes") "* merge=bit-metadata -text\n"

-- | Initialize a bit repository at a remote filesystem location.
-- Typed wrapper around 'initializeRepoAt' that accepts 'RemotePath'
-- to enforce that callers use 'Bit.IO.Platform' for remote paths.
initializeRemoteRepoAt :: RemotePath -> IO ()
initializeRemoteRepoAt (RemotePath p) = initializeRepoAt p defaultInitOptions
