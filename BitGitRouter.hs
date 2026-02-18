{- |
Git router executable: dispatches @git@ commands to either @bit@ (for .bit repos)
or real git (for .git repos).

Installed by @bit become-git@, this sits on PATH as @git.exe@ and transparently
routes commands based on whether the current directory is a bit or git repo.

Design: standalone binary with no bit module imports — keeps startup fast and
avoids pulling in the entire bit dependency tree.
-}
module Main (main) where

import System.Environment (getArgs, lookupEnv, setEnv, getExecutablePath)
import System.Exit (exitWith)
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory,
                         findExecutable)
import System.Process (rawSystem)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    args <- getArgs
    realGit <- findRealGit
    -- git init always goes to real git (preserve standard git init behavior)
    if isGitInit args
        then exec realGit args
        else do
            isBit <- walkUpForBitDir
            if isBit
                then do
                    -- Set BIT_REAL_GIT so bit's internal git calls don't
                    -- recurse through this router
                    setEnv "BIT_REAL_GIT" realGit
                    bitExe <- findBit
                    exec bitExe args
                else exec realGit args

-- | Check if the command is @git init@.
-- Skips leading flags (-c, -C, --bare) to find the subcommand.
isGitInit :: [String] -> Bool
isGitInit [] = False
isGitInit ("init":_) = True
isGitInit ("-c":_:rest) = isGitInit rest
isGitInit ("-C":_:rest) = isGitInit rest
isGitInit ("--bare":rest) = isGitInit rest
isGitInit (x:rest)
    | "-" `isPrefixOf` x = isGitInit rest
    | otherwise = False

-- | Walk up from CWD looking for a @.bit/@ directory or @.bit@ file (bitlink).
walkUpForBitDir :: IO Bool
walkUpForBitDir = getCurrentDirectory >>= go
  where
    go dir = do
        hasDir  <- doesDirectoryExist (dir </> ".bit")
        hasFile <- doesFileExist (dir </> ".bit")
        if hasDir || hasFile
            then pure True
            else let parent = takeDirectory dir
                 in if parent == dir
                    then pure False
                    else go parent

-- | Find the real git binary.
-- Priority: 1) BIT_REAL_GIT env var, 2) config file ~/.bit-router/real-git,
-- 3) search PATH excluding our own directory.
findRealGit :: IO FilePath
findRealGit = do
    -- 1. BIT_REAL_GIT env var (set by bit become-git or test shim)
    mEnv <- lookupEnv "BIT_REAL_GIT"
    case mEnv of
        Just p | not (null p) -> pure p
        _ -> do
            -- 2. Config file written by become-git
            home <- getHome
            let configFile = home </> ".bit-router" </> "real-git"
            hasConfig <- doesFileExist configFile
            if hasConfig
                then do
                    content <- readFile configFile
                    let path = filter (\c -> c /= '\n' && c /= '\r') content
                    if null path then fallbackSearch else pure path
                else fallbackSearch

-- | Search PATH for git, excluding our own directory.
fallbackSearch :: IO FilePath
fallbackSearch = do
    myPath <- getExecutablePath
    let myDir = takeDirectory myPath
    -- findExecutable searches PATH; if it returns our own dir, we need
    -- to do a manual search. For now, just use findExecutable and hope
    -- the real git is elsewhere on PATH.
    mGit <- findExecutable "git"
    case mGit of
        Just p | takeDirectory p /= myDir -> pure p
        -- If we can only find ourselves, try common locations
        _ -> do
            let candidates = [ "/usr/bin/git"
                             , "/usr/local/bin/git"
                             , "C:/Program Files/Git/cmd/git.exe"
                             , "C:/Program Files (x86)/Git/cmd/git.exe"
                             ]
            found <- mapM doesFileExist candidates
            case [c | (c, True) <- zip candidates found] of
                (c:_) -> pure c
                []    -> error "bit-git-router: cannot find real git. Set BIT_REAL_GIT or run 'bit become-git' again."

-- | Find the bit executable. Must be in the same directory as the router,
-- or on PATH.
findBit :: IO FilePath
findBit = do
    myPath <- getExecutablePath
    let myDir = takeDirectory myPath
    -- Check same directory first
    let sameDirBit = myDir </> "bit.exe"
    hasSameDir <- doesFileExist sameDirBit
    if hasSameDir
        then pure sameDirBit
        else do
            let sameDirBitNoExt = myDir </> "bit"
            hasSameDirNoExt <- doesFileExist sameDirBitNoExt
            if hasSameDirNoExt
                then pure sameDirBitNoExt
                else do
                    mBit <- findExecutable "bit"
                    case mBit of
                        Just p  -> pure p
                        Nothing -> error "bit-git-router: cannot find 'bit' executable"

-- | Get home directory (cross-platform).
getHome :: IO FilePath
getHome = do
    mHome <- lookupEnv "USERPROFILE"  -- Windows
    case mHome of
        Just h -> pure h
        Nothing -> do
            mUnix <- lookupEnv "HOME"
            case mUnix of
                Just h  -> pure h
                Nothing -> pure "."

-- | Execute a program with args, replacing this process.
-- On Windows, rawSystem is effectively exec (waits and exits with same code).
exec :: FilePath -> [String] -> IO ()
exec prog args = rawSystem prog args >>= exitWith
