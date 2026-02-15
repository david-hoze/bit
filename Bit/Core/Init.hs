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
import System.Environment (lookupEnv)
import System.Process (callCommand)
import Bit.Utils (atomicWriteFileStr, toPosix)
import Bit.Path (RemotePath(..))
import Data.Char (isSpace)

-- | Options for 'bit init'.
data InitOptions = InitOptions
    { initQuiet          :: Bool            -- ^ Suppress bit output
    , initBare           :: Bool            -- ^ Bare repository (stub, not yet implemented)
    , initGitFlags       :: [String]        -- ^ Extra flags passed through to 'git init'
    , initSeparateGitDir :: Maybe FilePath  -- ^ Place git database at this path
    } deriving (Show)

defaultInitOptions :: InitOptions
defaultInitOptions = InitOptions False False [] Nothing

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
    let separated = initSeparateGitDir opts

    -- For --separate-git-dir, compute absolute sgdir and put bit metadata there
    mAbsSgdir <- traverse Dir.makeAbsolute separated

    let targetBitDir = case mAbsSgdir of
            Just sgdir -> sgdir </> "bit"
            Nothing    -> targetDir </> ".bit"
    let targetBitIndexPath = targetBitDir </> "index"
    let targetBitGitDir = targetBitIndexPath </> ".git"
    let targetBitDevicesDir = targetBitDir </> "devices"
    let targetBitRemotesDir = targetBitDir </> "remotes"

    -- Ensure target directory exists (needed for --separate-git-dir with dir arg)
    Dir.createDirectoryIfMissing True targetDir

    -- 1. Create .bit directory (or sgdir/bit/)
    Platform.createDirectoryIfMissing True targetBitDir

    -- 2. Create .bit/index directory (needed before git init)
    Platform.createDirectoryIfMissing True targetBitIndexPath

    -- 3. Init Git in the index directory
    -- Check for .git as directory (normal) or file (gitlink from --separate-git-dir)
    hasGitDir <- Platform.doesDirectoryExist targetBitGitDir
    hasGitFile <- Platform.doesFileExist targetBitGitDir
    unless (hasGitDir || hasGitFile) $ do
        -- For separated repos, prepend --separate-git-dir to the git init flags
        let extraFlags = case mAbsSgdir of
                Just sgdir -> ["--separate-git-dir", sgdir]
                Nothing    -> []
        -- Initialize git in .bit/index, which will create .bit/index/.git
        void $ Git.runGitAt targetBitIndexPath ("init" : extraFlags ++ initGitFlags opts)

        -- Fix for Windows external/USB drives: add to safe.directory
        -- git 2.35.2+ rejects directories with different ownership
        absIndex <- Dir.makeAbsolute targetBitIndexPath
        let safePath = toPosix absIndex
        void $ Git.spawnGit ["config", "--global", "--add", "safe.directory", safePath]

    -- 3a. Create bundles directory for storing bundle files
    -- With --separate-git-dir, .git is a gitlink file; resolve real git dir
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
    Platform.createDirectoryIfMissing True (targetBitDir </> "cas")

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

    -- 7. For --separate-git-dir: write bitlink and gitlink files in working dir
    case mAbsSgdir of
        Just sgdir -> do
            -- bitlink file: .bit as FILE pointing to sgdir/bit
            atomicWriteFileStr (targetDir </> ".bit")
                ("bitdir: " ++ toPosix (sgdir </> "bit") ++ "\n")
            -- gitlink file: .git as FILE pointing to sgdir
            atomicWriteFileStr (targetDir </> ".git")
                ("gitdir: " ++ toPosix sgdir ++ "\n")
        Nothing ->
            -- 8. Create .git junction for test compatibility (BIT_GIT_JUNCTION=1).
            -- Points <targetDir>/.git -> <targetDir>/.bit/index/.git so git's
            -- repo discovery works without -C override.
            createGitJunction targetDir targetBitGitDir

-- | When BIT_GIT_JUNCTION=1, create a directory junction from
-- @\<dir\>/.git@ to @\<dir\>/.bit/index/.git@ (if the target exists
-- and the link doesn't).
createGitJunction :: FilePath -> FilePath -> IO ()
createGitJunction targetDir targetBitGitDir = do
    mJunction <- lookupEnv "BIT_GIT_JUNCTION"
    -- Trim whitespace: cmd.exe "set VAR=1 &" includes trailing space in value
    let enabled = case mJunction of
            Just v  -> filter (not . isSpace) v == "1"
            Nothing -> False
    when enabled $ do
        let link = targetDir </> ".git"
        linkExists <- Platform.doesDirectoryExist link
        fileExists <- Platform.doesFileExist link
        targetExists <- Platform.doesDirectoryExist targetBitGitDir
        when (targetExists && not linkExists && not fileExists) $ do
            absTarget <- Dir.makeAbsolute targetBitGitDir
            absLink <- Dir.makeAbsolute link
            -- Use mklink /j (directory junction) — no admin rights needed.
            -- Dir.createDirectoryLink creates a symlink which requires elevation.
            callCommand $ "mklink /j \"" ++ absLink ++ "\" \"" ++ absTarget ++ "\" >nul"

-- | Initialize a bit repository at a remote filesystem location.
-- Typed wrapper around 'initializeRepoAt' that accepts 'RemotePath'
-- to enforce that callers use 'Bit.IO.Platform' for remote paths.
initializeRemoteRepoAt :: RemotePath -> IO ()
initializeRemoteRepoAt (RemotePath p) = initializeRepoAt p defaultInitOptions
