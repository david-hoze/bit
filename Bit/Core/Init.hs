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

    -- Check if the repo already exists (re-init vs fresh init).
    hasBitDir <- Platform.doesDirectoryExist (targetDir </> ".bit")
    hasBitLink <- Platform.doesFileExist (targetDir </> ".bit")
    let existingRepo = hasBitDir || hasBitLink

    -- Resolve the actual .bit directory.
    -- For existing repos (re-init): always use the existing .bit dir, even if
    -- --separate-git-dir is specified. The git database moves but bit metadata stays.
    -- For fresh repos: use sgdir/bit if --separate-git-dir, else targetDir/.bit.
    targetBitDir <- if existingRepo && hasBitLink
        then resolveBitLink (targetDir </> ".bit")
        else if existingRepo
            then pure (targetDir </> ".bit")
            else case mAbsSgdir of
                Just sgdir -> pure (sgdir </> "bit")
                Nothing    -> pure (targetDir </> ".bit")
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

    -- For reinit with --separate-git-dir, git must run from targetDir (not
    -- .bit/index) because git renames the old gitdir to the new location, and
    -- CWD must not be inside the directory being renamed (Windows limitation).
    -- First normalize .git from junction to gitlink so git can update it.
    let isReinitSgdir = not isFreshInit && mAbsSgdir /= Nothing
    when isReinitSgdir $ do
        let gitPath = targetDir </> ".git"
        gitIsJunction <- Platform.doesDirectoryExist gitPath
        when gitIsJunction $ do
            -- Resolve the actual git dir through .bit/index
            (_, rawDir, _) <- Git.runGitAt targetBitIndexPath
                ["rev-parse", "--absolute-git-dir"]
            let currentGitDir = filter (\c -> c /= '\n' && c /= '\r') rawDir
            -- Remove junction (rmdir without /s doesn't follow)
            absGit <- Dir.makeAbsolute gitPath
            callCommand $ "rmdir \"" ++ absGit ++ "\" 2>nul"
            -- Write gitlink to current git dir so git can discover the repo
            atomicWriteFileStr gitPath
                ("gitdir: " ++ toPosix currentGitDir ++ "\n")

    -- ALWAYS run git init (handles both fresh init and re-init)
    let extraFlags = case mAbsSgdir of
            Just sgdir -> ["--separate-git-dir", sgdir]
            Nothing    -> []
    -- Resolve relative --template paths to absolute (git runs with -C inside
    -- .bit/index, so relative paths would resolve from the wrong directory)
    resolvedGitFlags <- resolveTemplatePaths (initGitFlags opts)
    -- For reinit+sgdir: run from targetDir so CWD isn't inside the dir being moved.
    -- For everything else: run from .bit/index as usual.
    let gitInitDir = if isReinitSgdir then targetDir else targetBitIndexPath
    (code, out, err) <- Git.spawnGit $
        ["-C", gitInitDir]
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

            -- 9. Re-init with --separate-git-dir: git renamed the old gitdir to
            -- sgdir. Recreate .bit/index/.git as a gitlink so future
            -- git -C .bit/index commands still find the repo.
            -- Two cases:
            -- (a) .bit/ was a separate directory — git only moved .bit/index/.git
            --     contents; .bit/index/ still exists at the original path.
            -- (b) .bit/ was inside the old gitdir (via bitlink) — git moved the
            --     entire directory; paths are now under sgdir.
            when isReinitSgdir $ case mAbsSgdir of
                Just sgdir -> do
                    indexStillExists <- Platform.doesDirectoryExist targetBitIndexPath
                    if indexStillExists
                        then do
                            -- Case (a): write gitlink at original .bit/index/.git
                            let indexGit = targetBitIndexPath </> ".git"
                            hasIG <- Platform.doesFileExist indexGit
                            unless hasIG $ atomicWriteFileStr indexGit
                                ("gitdir: " ++ toPosix sgdir ++ "\n")
                        else do
                            -- Case (b): bit metadata moved with the gitdir.
                            -- Write gitlink at the new location.
                            let newIndexGit = sgdir </> "bit" </> "index" </> ".git"
                            hasNG <- Platform.doesFileExist newIndexGit
                            unless hasNG $ atomicWriteFileStr newIndexGit
                                ("gitdir: " ++ toPosix sgdir ++ "\n")
                            -- Update bitlink to point to new bit dir location
                            let bitPath = targetDir </> ".bit"
                            bitIsLink <- Platform.doesFileExist bitPath
                            when bitIsLink $ atomicWriteFileStr bitPath
                                ("bitdir: " ++ toPosix (sgdir </> "bit") ++ "\n")
                Nothing -> pure ()

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


-- | Remove a .git path that may be a junction, gitlink file, or directory.
-- Uses 'rmdir' without /s for junctions (safe, doesn't follow the target).
removeGitPath :: FilePath -> IO ()
removeGitPath path = do
    isDir <- Platform.doesDirectoryExist path
    isFile <- Platform.doesFileExist path
    when isDir $ do
        absPath <- Dir.makeAbsolute path
        callCommand $ "rmdir \"" ++ absPath ++ "\" 2>nul"
    when (isFile && not isDir) $
        Dir.removeFile path

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
