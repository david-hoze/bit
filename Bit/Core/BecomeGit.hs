{-# LANGUAGE CPP #-}

{- |
Install/uninstall the git router so @git@ commands are transparently
routed to bit for .bit repos.

@bit become-git@ — install the router as @git@ on PATH
@bit become-bit@ — uninstall the router and restore system git
-}
module Bit.Core.BecomeGit
    ( becomeGit
    , becomeBit
    ) where

import System.Directory (createDirectoryIfMissing, doesFileExist, copyFile,
                         removeDirectoryRecursive, doesDirectoryExist,
                         findExecutable, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)
import System.Environment (lookupEnv, getExecutablePath)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..), exitSuccess)
import System.Process (readProcessWithExitCode)
import Data.List (isInfixOf)
import Control.Monad (when)
import Control.Exception (try, SomeException)

-- | Install the git router. Dispatches on flags:
--   --init  → copy router into extern/git-shim/ for test suite use
--   (none)  → global install into ~/.bit-router/
becomeGit :: [String] -> IO ()
becomeGit ["--init"] = becomeGitInit
becomeGit []         = becomeGitGlobal
becomeGit flags      = do
    hPutStrLn stderr $ "error: unknown flags: " ++ unwords flags
    exitWith (ExitFailure 1)

-- | Set up the router for git test suite use.
-- Copies bit-git-router as git.exe into extern/git-shim/ (replacing the bash shim).
becomeGitInit :: IO ()
becomeGitInit = do
    -- Find the router executable
    myPath <- getExecutablePath
    let myDir = takeDirectory myPath
    routerSrc <- findRouter myDir

    -- Target: extern/git-shim/ relative to repo root (CWD)
    cwd <- getCurrentDirectory
    let shimDir = cwd </> "extern" </> "git-shim"
    shimExists <- doesDirectoryExist shimDir
    if not shimExists
        then do
            hPutStrLn stderr $ "error: " ++ shimDir ++ " does not exist."
            hPutStrLn stderr "Run this from the bit repo root."
            exitWith (ExitFailure 1)
        else do
            -- Copy router as git.exe
            let dst = shimDir </> gitExeName
            copyFile routerSrc dst
            putStrLn $ "Installed router as: " ++ dst
            -- Copy bit.exe alongside so the router's findBit finds it.
            -- This may fail on Windows if the destination is locked by a
            -- running process; in that case, print a note for manual copy.
            let bitSrc = myDir </> bitExeName
                bitDst = shimDir </> bitExeName
            bitExists <- doesFileExist bitSrc
            when bitExists $ do
                result <- try (copyFile bitSrc bitDst) :: IO (Either SomeException ())
                case result of
                    Right () -> putStrLn $ "Installed bit as: " ++ bitDst
                    Left _   -> putStrLn $ "Note: could not copy bit.exe (file locked?). Copy manually:\n  cp " ++ bitSrc ++ " " ++ bitDst
            putStrLn ""
            putStrLn "For git test suite, run:"
            putStrLn $ "  export BIT_GIT_JUNCTION=1"
            putStrLn $ "  export GIT_TEST_INSTALLED=" ++ shimDir
            putStrLn $ "  cd extern/git/t && bash t0001-init.sh --verbose"

-- | Install the git router globally.
becomeGitGlobal :: IO ()
becomeGitGlobal = do
    -- 1. Find real git
    realGit <- findRealGit
    putStrLn $ "Real git found at: " ++ realGit

    -- 2. Create ~/.bit-router/ directory
    home <- getHome
    let routerDir = home </> ".bit-router"
    createDirectoryIfMissing True routerDir

    -- 3. Save real git path
    let configFile = routerDir </> "real-git"
    writeFile configFile realGit
    putStrLn $ "Saved real git path to: " ++ configFile

    -- 4. Find bit-git-router executable (same dir as bit.exe)
    myPath <- getExecutablePath
    let myDir = takeDirectory myPath
    routerSrc <- findRouter myDir
    putStrLn $ "Router found at: " ++ routerSrc

    -- 5. Copy router as git.exe / git into ~/.bit-router/
    let routerDst = routerDir </> gitExeName
    copyFile routerSrc routerDst
    putStrLn $ "Installed router as: " ++ routerDst

    -- 6. Add ~/.bit-router/ to front of PATH
    addToPath routerDir

    putStrLn ""
    putStrLn "git router installed successfully!"
    putStrLn "Restart your shell for changes to take effect."
    putStrLn ""
    putStrLn "After restart, 'git' commands in .bit/ repos will be handled by bit."
    putStrLn "In .git/ repos, they'll go to real git as usual."
    putStrLn "Run 'bit become-bit' to uninstall."

-- | Uninstall the git router.
becomeBit :: IO ()
becomeBit = do
    home <- getHome
    let routerDir = home </> ".bit-router"

    exists <- doesDirectoryExist routerDir
    if exists
        then do
            -- 1. Remove from PATH
            removeFromPath routerDir
            -- 2. Remove directory
            removeDirectoryRecursive routerDir
            putStrLn "git router uninstalled."
            putStrLn "Restart your shell for changes to take effect."
        else do
            putStrLn "git router is not installed (no ~/.bit-router/ directory)."
            exitSuccess

-- | Find the real git binary (not the router).
findRealGit :: IO FilePath
findRealGit = do
    mEnv <- lookupEnv "BIT_REAL_GIT"
    case mEnv of
        Just p | not (null p) -> pure p
        _ -> do
            mGit <- findSystemGit
            case mGit of
                Just p  -> pure p
                Nothing -> do
                    hPutStrLn stderr "error: cannot find git. Is git installed?"
                    exitWith (ExitFailure 1)

-- | Find git using platform-specific search, filtering out the router directory.
findSystemGit :: IO (Maybe FilePath)
findSystemGit = do
    home <- getHome
    let routerDir = home </> ".bit-router"
#ifdef mingw32_HOST_OS
    (code, out, _) <- readProcessWithExitCode "where" ["git"] ""
    case code of
        ExitSuccess ->
            let paths = filter (not . null) (lines out)
                filtered = filter (\p -> not (routerDir `isInfixOf` p)) paths
            in pure $ case filtered of
                (p:_) -> Just (filter (/= '\r') p)
                []    -> Nothing
        _ -> findExecutable "git"
#else
    (code, out, _) <- readProcessWithExitCode "which" ["-a", "git"] ""
    case code of
        ExitSuccess ->
            let paths = filter (not . null) (lines out)
                filtered = filter (\p -> not (routerDir `isInfixOf` p)) paths
            in pure $ case filtered of
                (p:_) -> Just p
                []    -> Nothing
        _ -> findExecutable "git"
#endif

-- | Find the bit-git-router executable.
findRouter :: FilePath -> IO FilePath
findRouter dir = do
    let candidates = [ dir </> "bit-git-router.exe"
                     , dir </> "bit-git-router"
                     ]
    found <- mapM doesFileExist candidates
    case [c | (c, True) <- zip candidates found] of
        (c:_) -> pure c
        [] -> do
            mRouter <- findExecutable "bit-git-router"
            case mRouter of
                Just p  -> pure p
                Nothing -> do
                    hPutStrLn stderr $ "error: cannot find bit-git-router executable in " ++ dir
                    hPutStrLn stderr "Run 'cabal install' to build it first."
                    exitWith (ExitFailure 1)

-- | Get home directory (cross-platform).
getHome :: IO FilePath
getHome = do
    mHome <- lookupEnv "USERPROFILE"
    case mHome of
        Just h -> pure h
        Nothing -> do
            mUnix <- lookupEnv "HOME"
            case mUnix of
                Just h  -> pure h
                Nothing -> do
                    hPutStrLn stderr "error: cannot determine home directory"
                    exitWith (ExitFailure 1)

-- | Platform-specific git executable name.
gitExeName :: FilePath
#ifdef mingw32_HOST_OS
gitExeName = "git.exe"
#else
gitExeName = "git"
#endif

-- | Platform-specific bit executable name.
bitExeName :: FilePath
#ifdef mingw32_HOST_OS
bitExeName = "bit.exe"
#else
bitExeName = "bit"
#endif

-- | Add a directory to the front of the user's PATH.
addToPath :: FilePath -> IO ()
#ifdef mingw32_HOST_OS
addToPath dir = do
    -- Read current user PATH from registry via reg query
    currentPath <- readWindowsUserPath
    if dir `isInfixOf` currentPath
        then putStrLn $ "PATH already contains: " ++ dir
        else do
            let newPath = dir ++ ";" ++ currentPath
            writeWindowsUserPath newPath
            broadcastSettingChange
            putStrLn $ "Added to PATH: " ++ dir
#else
addToPath dir = do
    home <- getHome
    let bashrc = home </> ".bashrc"
        profile = home </> ".profile"
        exportLine = "export PATH=\"" ++ dir ++ ":$PATH\"  # Added by bit become-git"
    appendToFileIfMissing bashrc exportLine
    appendToFileIfMissing profile exportLine
    putStrLn $ "Added to PATH in ~/.bashrc and ~/.profile: " ++ dir
#endif

-- | Remove a directory from the user's PATH.
removeFromPath :: FilePath -> IO ()
#ifdef mingw32_HOST_OS
removeFromPath dir = do
    currentPath <- readWindowsUserPath
    let parts = splitOn ';' currentPath
        filtered = filter (/= dir) parts
        newPath = joinWith ';' filtered
    when (newPath /= currentPath) $ do
        writeWindowsUserPath newPath
        broadcastSettingChange
    putStrLn $ "Removed from PATH: " ++ dir
#else
removeFromPath _dir = do
    home <- getHome
    let bashrc = home </> ".bashrc"
        profile = home </> ".profile"
        marker = "# Added by bit become-git"
    removeLineFromFile bashrc marker
    removeLineFromFile profile marker
    putStrLn "Removed PATH entries from ~/.bashrc and ~/.profile"
#endif

#ifdef mingw32_HOST_OS
-- | Read the user PATH from Windows registry using reg query.
readWindowsUserPath :: IO String
readWindowsUserPath = do
    (code, out, _) <- readProcessWithExitCode "reg"
        ["query", "HKCU\\Environment", "/v", "Path"] ""
    case code of
        ExitSuccess ->
            -- Output format: "    Path    REG_EXPAND_SZ    <value>"
            let ls = lines (filter (/= '\r') out)
                pathLines = filter (\l -> "Path" `isInfixOf` l || "PATH" `isInfixOf` l) ls
            in case pathLines of
                (l:_) -> pure $ extractRegValue l
                []    -> pure ""
        _ -> pure ""

-- | Extract the value from a reg query output line.
-- Format: "    Path    REG_EXPAND_SZ    C:\foo;C:\bar"
-- or:     "    Path    REG_SZ    C:\foo;C:\bar"
extractRegValue :: String -> String
extractRegValue line =
    let parts = words line
        -- Value is everything after REG_EXPAND_SZ or REG_SZ
        dropToValue [] = ""
        dropToValue (w:ws)
            | w == "REG_EXPAND_SZ" || w == "REG_SZ" = unwords ws
            | otherwise = dropToValue ws
    in dropToValue parts

-- | Write the user PATH to Windows registry using reg add.
writeWindowsUserPath :: String -> IO ()
writeWindowsUserPath value = do
    _ <- readProcessWithExitCode "reg"
        ["add", "HKCU\\Environment", "/v", "Path", "/t", "REG_EXPAND_SZ",
         "/d", value, "/f"] ""
    pure ()

-- | Broadcast WM_SETTINGCHANGE so new processes pick up PATH changes.
-- Uses setx to set a dummy variable (setx broadcasts the change as a side effect),
-- then cleans up the dummy variable.
broadcastSettingChange :: IO ()
broadcastSettingChange = do
    _ <- readProcessWithExitCode "setx" ["BIT_ROUTER_REFRESH", "1"] ""
    _ <- readProcessWithExitCode "reg"
        ["delete", "HKCU\\Environment", "/v", "BIT_ROUTER_REFRESH", "/f"] ""
    pure ()

#else
-- | Append a line to a file if it doesn't already contain the line.
appendToFileIfMissing :: FilePath -> String -> IO ()
appendToFileIfMissing path line = do
    exists <- doesFileExist path
    if exists
        then do
            content <- readFile path
            if line `isInfixOf` content
                then pure ()
                else appendFile path ("\n" ++ line ++ "\n")
        else writeFile path (line ++ "\n")

-- | Remove lines containing a marker from a file.
removeLineFromFile :: FilePath -> String -> IO ()
removeLineFromFile path marker = do
    exists <- doesFileExist path
    when exists $ do
        content <- readFile path
        let filtered = filter (\l -> not (marker `isInfixOf` l)) (lines content)
        length filtered `seq` writeFile path (unlines filtered)
#endif

-- | Split a string on a separator character.
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s  = let (w, rest) = break (== c) s
               in w : case rest of { [] -> []; (_:t) -> splitOn c t }

-- | Join strings with a separator character.
joinWith :: Char -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith c (x:xs) = x ++ [c] ++ joinWith c xs
