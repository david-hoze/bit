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
import System.Exit (ExitCode(..))
import System.IO (hPutStr, stderr)
import Bit.Utils (atomicWriteFileStr, toPosix)
import Bit.Path (RemotePath(..))
import Data.List (isPrefixOf, stripPrefix)
import Data.Char (isSpace)

-- | Options for 'bit init'.
data InitOptions = InitOptions
    { initQuiet          :: Bool            -- ^ Suppress bit output
    , initBare           :: Bool            -- ^ Bare repository (stub, not yet implemented)
    , initGitFlags       :: [String]        -- ^ Extra flags passed through to 'git init'
    , initGitGlobalFlags :: [String]        -- ^ Flags before "init" (-c key=val)
    , initSeparateGitDir :: Maybe FilePath  -- ^ Place git database at this path
    } deriving (Show)

defaultInitOptions :: InitOptions
defaultInitOptions = InitOptions False False [] [] Nothing

init :: IO ()
init = initWith defaultInitOptions

initWith :: InitOptions -> IO ()
initWith opts = do
    cwd <- Dir.getCurrentDirectory
    unless (initQuiet opts) $
        putStrLn $ "Initializing bit in: " ++ cwd
    when (initBare opts) $ initializeBareRepoAt cwd
    void $ initializeRepoAt cwd opts
    unless (initQuiet opts) $
        putStrLn "bit initialized successfully!"

-- | Stub for bare repository support (not yet implemented).
initializeBareRepoAt :: FilePath -> IO ()
initializeBareRepoAt _dir = pure ()

-- | Initialize a bit repository at the specified target directory.
-- This is used both for local `bit init` and for creating filesystem remotes.
-- Returns the exit code from git init (or ExitSuccess if post-init setup succeeds).
initializeRepoAt :: FilePath -> InitOptions -> IO ExitCode
initializeRepoAt targetDir opts = do
    let separated = initSeparateGitDir opts

    -- For --separate-git-dir, compute absolute sgdir and put bit metadata there
    mAbsSgdir <- traverse Dir.makeAbsolute separated

    -- Resolve the actual .bit directory. On re-init, .bit may be a bitlink file
    -- (from a previous --separate-git-dir init) pointing to the real bit dir.
    let defaultBitDir = case mAbsSgdir of
            Just sgdir -> sgdir </> "bit"
            Nothing    -> targetDir </> ".bit"
    isBitLink <- Platform.doesFileExist (targetDir </> ".bit")
    targetBitDir <- if isBitLink && separated == Nothing
        then resolveBitLink (targetDir </> ".bit")
        else pure defaultBitDir
    let targetBitIndexPath = targetBitDir </> "index"
    let targetBitGitDir = targetBitIndexPath </> ".git"
    let targetBitDevicesDir = targetBitDir </> "devices"
    let targetBitRemotesDir = targetBitDir </> "remotes"

    -- 3. Init Git in the index directory
    -- Check for .git as directory (normal) or file (gitlink from --separate-git-dir)
    hasGitDir <- Platform.doesDirectoryExist targetBitGitDir
    hasGitFile <- Platform.doesFileExist targetBitGitDir
    let isFreshInit = not hasGitDir && not hasGitFile

    -- Create directories only on fresh init (they already exist on re-init)
    when isFreshInit $ do
        Dir.createDirectoryIfMissing True targetDir
        Platform.createDirectoryIfMissing True targetBitDir
        Platform.createDirectoryIfMissing True targetBitIndexPath

    -- ALWAYS run git init (handles both fresh init and re-init)
    let extraFlags = case mAbsSgdir of
            Just sgdir -> ["--separate-git-dir", sgdir]
            Nothing    -> []
    -- Resolve relative --template paths to absolute (git runs with -C inside
    -- .bit/index, so relative paths would resolve from the wrong directory)
    resolvedGitFlags <- resolveTemplatePaths (initGitFlags opts)
    (code, out, err) <- Git.spawnGit $
        ["-C", targetBitIndexPath]
        ++ initGitGlobalFlags opts
        ++ ("init" : extraFlags ++ resolvedGitFlags)

    -- Print git's output (tests need "Initialized empty" / "Reinitialized existing")
    putStr out
    hPutStr stderr err

    -- If git init failed, bail out (fixes garbage ref-format, invalid branch, etc.)
    case code of
        ExitFailure _ -> pure code
        ExitSuccess -> do
            -- Post-init setup ONLY on fresh init
            when isFreshInit $ do
                -- Fix for Windows external/USB drives: add to safe.directory
                -- git 2.35.2+ rejects directories with different ownership
                absIndex <- Dir.makeAbsolute targetBitIndexPath
                let safePath = toPosix absIndex
                void $ Git.spawnGit ["config", "--global", "--add", "safe.directory", safePath]

                -- Default branch: only if no explicit --initial-branch/-b
                let hasExplicitBranch = any (\f -> f == "-b"
                        || "--initial-branch" `isPrefixOf` f) (initGitFlags opts)
                unless hasExplicitBranch $ do
                    void $ Git.runGitAt targetBitIndexPath ["config", "init.defaultBranch", "main"]
                    void $ Git.runGitAt targetBitIndexPath ["branch", "-m", "master", "main"]

                -- Configure core.quotePath to false (display Unicode filenames properly)
                void $ Git.runGitAt targetBitIndexPath ["config", "core.quotePath", "false"]

            -- 3a. Create bundles directory for storing bundle files
            -- With --separate-git-dir, .git is a gitlink file; resolve real git dir
            if isFreshInit
                then do
                    -- Fresh init: check if .git ended up as dir or gitlink
                    isDir <- Platform.doesDirectoryExist targetBitGitDir
                    if isDir
                        then Platform.createDirectoryIfMissing True (targetBitGitDir </> "bundles")
                        else do
                            -- gitlink: resolve real git dir and create bundles there
                            (_, gitDir, _) <- Git.runGitAt targetBitIndexPath ["rev-parse", "--git-dir"]
                            let realGitDir = filter (/= '\n') gitDir
                            Platform.createDirectoryIfMissing True (realGitDir </> "bundles")
                else do
                    -- Re-init: ensure bundles dir exists (may have been on a fresh layout)
                    isDir <- Platform.doesDirectoryExist targetBitGitDir
                    when isDir $
                        Platform.createDirectoryIfMissing True (targetBitGitDir </> "bundles")

            -- .bit subdirs, config, merge driver, junction — ONLY on fresh init
            when isFreshInit $ do
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

            pure ExitSuccess

-- | Resolve a .bit bitlink file to the actual bit directory.
-- Format: first line is "bitdir: <path>"
resolveBitLink :: FilePath -> IO FilePath
resolveBitLink path = do
    content <- readFile path
    case lines content of
        (firstLine:_) ->
            let raw = drop 8 (filter (/= '\r') firstLine)  -- drop "bitdir: "
            in pure raw
        [] -> pure path  -- fallback: treat as normal

-- | Resolve --template paths in git init flags to absolute.
-- git runs with -C inside .bit/index, so relative paths would resolve wrong.
-- Handles both "--template=path" and "--template" "path" forms.
resolveTemplatePaths :: [String] -> IO [String]
resolveTemplatePaths [] = pure []
resolveTemplatePaths ("--template":v:rest) = do
    absV <- Dir.makeAbsolute v
    rest' <- resolveTemplatePaths rest
    pure ("--template" : absV : rest')
resolveTemplatePaths (f:rest)
    | Just v <- stripPrefix "--template=" f = do
        absV <- if null v then pure v else Dir.makeAbsolute v
        rest' <- resolveTemplatePaths rest
        pure (("--template=" ++ absV) : rest')
    | otherwise = do
        rest' <- resolveTemplatePaths rest
        pure (f : rest')

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
initializeRemoteRepoAt :: RemotePath -> IO ExitCode
initializeRemoteRepoAt (RemotePath p) = initializeRepoAt p defaultInitOptions
