{-# LANGUAGE ScopedTypeVariables #-}

module Bit.Core.Import
    ( importRepo
    ) where

import qualified System.Directory as Dir
import qualified Bit.IO.Platform as Platform
import System.FilePath ((</>))
import Control.Monad (void, when, unless, forM_)
import Data.List (isInfixOf, isPrefixOf)
import qualified Bit.Git.Run as Git
import qualified Bit.Device.Identity as Device
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Bit.Utils (atomicWriteFileStr, toPosix)
import Bit.Core.Init (createGitJunction)

-- | Convert an existing git repo into a bit repo.
-- Moves .git into .bit/index/.git and sets up bit directory structure.
-- Preserves full git history. The next scan (bit add, status, etc.)
-- will populate metadata files in .bit/index/.
importRepo :: FilePath -> IO ExitCode
importRepo targetDir = do
    -- Validate: must have .git, must not have .bit
    hasGitDir <- Platform.doesDirectoryExist (targetDir </> ".git")
    hasGitFile <- Platform.doesFileExist (targetDir </> ".git")
    let hasGit = hasGitDir || hasGitFile
    hasBitDir <- Platform.doesDirectoryExist (targetDir </> ".bit")
    hasBitLink <- Platform.doesFileExist (targetDir </> ".bit")
    let hasBit = hasBitDir || hasBitLink

    when (not hasGit) $
        hPutStrLn stderr $ "fatal: '" ++ targetDir ++ "' is not a git repository (no .git found)"
    when hasBit $
        hPutStrLn stderr $ "fatal: '" ++ targetDir ++ "' is already a bit repository (.bit exists)"

    if not hasGit || hasBit
        then pure (ExitFailure 1)
        else doImport targetDir

doImport :: FilePath -> IO ExitCode
doImport targetDir = do
    let bitDir      = targetDir </> ".bit"
        indexDir    = bitDir </> "index"
        bitGitDir   = indexDir </> ".git"

    -- Create .bit/ structure
    Platform.createDirectoryIfMissing True bitDir
    Platform.createDirectoryIfMissing True indexDir
    Platform.createDirectoryIfMissing True (bitDir </> "devices")
    Platform.createDirectoryIfMissing True (bitDir </> "remotes")
    Platform.createDirectoryIfMissing True (bitDir </> "cas")

    -- Write default config
    atomicWriteFileStr (bitDir </> "config") $ unlines
        [ "[core]"
        , "    mode = lite"
        , "[text]"
        , "    size-limit = 1048576  # 1MB, files larger are always binary"
        , "    extensions = .txt,.md,.yaml,.yml,.json,.xml,.html,.css,.js,.py,.hs,.rs"
        ]

    -- Move .git -> .bit/index/.git
    Dir.renameDirectory (targetDir </> ".git") bitGitDir

    -- Post-move git config (same as bit init)
    absIndex <- Dir.makeAbsolute indexDir
    void $ Git.spawnGit ["config", "--global", "--add", "safe.directory", toPosix absIndex]
    void $ Git.runGitAt indexDir ["config", "core.quotePath", "false"]
    void $ Git.runGitAt indexDir ["config", "merge.bit-metadata.name", "bit metadata"]
    void $ Git.runGitAt indexDir ["config", "merge.bit-metadata.driver", "false"]
    void $ Git.runGitAt indexDir ["config", "core.autocrlf", "false"]

    -- Create bundles/ dir inside .git/
    Platform.createDirectoryIfMissing True (bitGitDir </> "bundles")

    -- Create .git junction at repo root (only when BIT_GIT_JUNCTION=1)
    createGitJunction targetDir bitGitDir

    -- Register existing git remotes in .bit/remotes/
    registerGitRemotes targetDir indexDir

    putStrLn $ "Imported git repository: " ++ targetDir
    putStrLn "Git history preserved. Run 'bit status' to verify."
    pure ExitSuccess

-- | Read git remotes from the imported repo and create .bit/remotes/ entries.
-- Each remote is classified (git SSH/HTTPS, filesystem, cloud) and registered
-- so that bit push/pull know which transport seam to use.
registerGitRemotes :: FilePath -> FilePath -> IO ()
registerGitRemotes repoRoot indexDir = do
    (code, out, _) <- Git.runGitAt indexDir ["remote"]
    when (code == ExitSuccess) $ do
        let remoteNames = filter (not . null) (lines out)
        forM_ remoteNames $ \name -> do
            (urlCode, urlOut, _) <- Git.runGitAt indexDir
                ["config", "--get", "remote." ++ name ++ ".url"]
            when (urlCode == ExitSuccess) $ do
                let url = filter (/= '\n') urlOut
                -- Skip and remove bit's internal bundle remotes (.git/bundles/<name>.bundle).
                -- These are cloud transport artifacts, not real git remotes.
                -- The actual cloud remote URL is lost on export; user must re-add.
                if isBundlePath url
                    then void $ Git.runGitAt indexDir ["remote", "remove", name]
                    else if isBitIndexPath url
                        -- .bit/index in URL = bit filesystem remote, not a plain git remote.
                        -- Recover the parent bit repo path and register as filesystem.
                        then do
                            let remotePath = stripBitIndex url
                            Device.writeRemoteFile repoRoot name Device.RemoteFilesystem (Just remotePath) Nothing
                            -- Update git remote to point to the correct .bit/index path
                            void $ Git.runGitAt indexDir ["remote", "set-url", name, url]
                        else
                            -- All other imported remotes are metadata-only: they come from
                            -- a git repo and have no bit binary data to sync.
                            Device.writeRemoteFile repoRoot name Device.RemoteGit (Just url) (Just Device.LayoutMetadata)

-- | Check if a git remote URL is a bit-internal bundle path (.git/bundles/ or .git\bundles/).
isBundlePath :: String -> Bool
isBundlePath url = ".git/bundles/" `isInfixOf` url || ".git\\bundles/" `isInfixOf` url

-- | Check if a git remote URL points to a .bit/index — a bit filesystem remote.
isBitIndexPath :: String -> Bool
isBitIndexPath url = ".bit/index" `isInfixOf` url || ".bit\\index" `isInfixOf` url

-- | Strip .bit/index (or .bit\index) suffix to recover the parent bit repo path.
stripBitIndex :: String -> String
stripBitIndex url = go url
  where
    go [] = url  -- fallback: return original if pattern not found
    go s@(_:rest)
        | ".bit/index" `isPrefixOf` s  = take (length url - length s) url
        | ".bit\\index" `isPrefixOf` s = take (length url - length s) url
        | otherwise = go rest
