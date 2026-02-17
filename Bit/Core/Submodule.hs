{-# LANGUAGE ScopedTypeVariables #-}

module Bit.Core.Submodule
  ( detectAndHandleSubrepo
  , syncGitlinksBack
  , syncSubmoduleToWorkingDirectory
  , rewriteGitlinkPath
  , submodule
  ) where

import System.FilePath ((</>), takeDirectory)
import System.Exit (ExitCode(..))
import qualified System.Directory as Dir
import qualified Bit.Git.Run as Git
import Data.List (isPrefixOf, isInfixOf)
import System.Environment (lookupEnv)
import Control.Monad (when, forM_)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Control.Exception (catch, IOException)

-- ============================================================================
-- Flow 1: Working Directory → Index (bit add <path>)
-- ============================================================================

-- | Detect and handle git/bit subrepos in the paths being added.
-- Called from the bit add flow after scanAndWrite and before Bit.add.
-- For each path arg that is a git subrepo (has .git/ dir) or bit subrepo
-- (has .bit/ dir), moves the child's git directory into .bit/index/ so
-- that git can detect it as a nested repo and create a 160000 entry.
-- After Bit.add completes, syncGitlinksBack should be called to rewrite
-- the gitlink for the working directory.
detectAndHandleSubrepo :: FilePath -> FilePath -> FilePath -> [String] -> IO ()
detectAndHandleSubrepo cwd bitDir _prefix args = do
    junction <- lookupEnv "BIT_GIT_JUNCTION"
    case junction of
        Just "1" -> detectAndHandleSubrepoJunction cwd bitDir args
        _ -> detectAndHandleSubrepo' cwd bitDir args

-- | Junction mode: subrepos created with bit init have a .git junction pointing
-- to .bit/index/.git/. To register them as submodules, remove the junction and
-- move the real git dir into the parent's .bit/index/ so git can detect it.
detectAndHandleSubrepoJunction :: FilePath -> FilePath -> [String] -> IO ()
detectAndHandleSubrepoJunction cwd bitDir args = do
    let paths = filter (not . ("-" `isPrefixOf`)) args
    forM_ paths $ \p -> do
        let fullPath = if isAbsolute p then p else cwd </> p
        let isRepoRoot = p == "." || fullPath == cwd
        when (not isRepoRoot) $ do
            let gitPath = fullPath </> ".git"
            -- Check for .git junction (created by bit init in junction mode)
            isJunction <- Dir.pathIsSymbolicLink gitPath
                `catch` \(_ :: IOException) -> pure False
            when isJunction $ do
                -- The junction points to .bit/index/.git/. Move the real git dir
                -- into the parent's .bit/index/ and remove the junction.
                let realGitDir = fullPath </> ".bit" </> "index" </> ".git"
                let indexTarget = bitDir </> "index" </> p </> ".git"
                realExists <- Dir.doesDirectoryExist realGitDir
                targetExists <- Dir.doesDirectoryExist indexTarget
                when (realExists && not targetExists) $ do
                    Dir.createDirectoryIfMissing True (takeDirectory indexTarget)
                    Dir.renameDirectory realGitDir indexTarget
                -- Remove the junction so scanner can descend into the directory
                Dir.removeDirectoryLink gitPath
                    `catch` \(_ :: IOException) -> pure ()
  where
    isAbsolute ('/':_) = True
    isAbsolute (_:':':_) = True
    isAbsolute _ = False

detectAndHandleSubrepo' :: FilePath -> FilePath -> [String] -> IO ()
detectAndHandleSubrepo' cwd bitDir args = do
    let paths = filter (not . ("-" `isPrefixOf`)) args
    forM_ paths $ \p -> do
        let fullPath = if isAbsolute p then p else cwd </> p
        -- Skip the repo root itself — "." or cwd is the current bit repo, not a subrepo.
        -- Without this guard, "bit add ." would see cwd/.bit and try to move
        -- .bit/index/.git, destroying the repository.
        let isRepoRoot = p == "." || fullPath == cwd
        when (not isRepoRoot) $ do
          -- Git subrepo — has .git/ directory
          hasGitDir <- Dir.doesDirectoryExist (fullPath </> ".git")
          when hasGitDir $ do
              let indexTarget = bitDir </> "index" </> p </> ".git"
              targetExists <- Dir.doesDirectoryExist indexTarget
              when (not targetExists) $ do
                  Dir.createDirectoryIfMissing True (takeDirectory indexTarget)
                  Dir.renameDirectory (fullPath </> ".git") indexTarget

          -- Bit subrepo — has .bit/ directory with index/.git/
          hasBitDir <- Dir.doesDirectoryExist (fullPath </> ".bit")
          when (hasBitDir && not hasGitDir) $ do
              let childGit = fullPath </> ".bit" </> "index" </> ".git"
              hasChildGit <- Dir.doesDirectoryExist childGit
              when hasChildGit $ do
                  let indexTarget = bitDir </> "index" </> p </> ".bit" </> "index" </> ".git"
                  targetExists <- Dir.doesDirectoryExist indexTarget
                  when (not targetExists) $ do
                      Dir.createDirectoryIfMissing True (takeDirectory indexTarget)
                      Dir.renameDirectory childGit indexTarget
  where
    isAbsolute ('/':_) = True
    isAbsolute (_:':':_) = True  -- Windows drive letter
    isAbsolute _ = False

-- | After bit add, rewrite gitlink files created by git back to the
-- working directory. Flow 1 step 4: only the gitlink needs updating;
-- the working directory already has the correct content.
syncGitlinksBack :: FilePath -> FilePath -> [String] -> IO ()
syncGitlinksBack cwd bitDir args = do
    let paths = filter (\p -> p /= "." && not ("-" `isPrefixOf` p)) args
    forM_ paths $ \p -> do
        -- Git subrepo: check .bit/index/<path>/.git
        let indexGitlink = bitDir </> "index" </> p </> ".git"
        isFile <- Dir.doesFileExist indexGitlink
        when isFile $ do
            content <- readGitlinkContent indexGitlink
            case content of
                Just gitdirLine -> do
                    let rewritten = rewriteGitlinkPath gitdirLine
                    let workingGitlink = cwd </> p </> ".git"
                    Dir.createDirectoryIfMissing True (takeDirectory workingGitlink)
                    BS.writeFile workingGitlink (encodeUtf8 (T.pack ("gitdir: " ++ rewritten ++ "\n")))
                Nothing -> pure ()

        -- Bit subrepo: check .bit/index/<path>/.bit/index/.git
        let indexBitGitlink = bitDir </> "index" </> p </> ".bit" </> "index" </> ".git"
        isBitFile <- Dir.doesFileExist indexBitGitlink
        when isBitFile $ do
            content <- readGitlinkContent indexBitGitlink
            case content of
                Just gitdirLine -> do
                    let rewritten = rewriteGitlinkPath gitdirLine
                    let workingGitlink = cwd </> p </> ".bit" </> "index" </> ".git"
                    Dir.createDirectoryIfMissing True (takeDirectory workingGitlink)
                    BS.writeFile workingGitlink (encodeUtf8 (T.pack ("gitdir: " ++ rewritten ++ "\n")))
                Nothing -> pure ()

-- ============================================================================
-- Flow 2: Index → Working Directory (bit submodule commands)
-- ============================================================================

-- | Handle `bit submodule <args>` command.
-- Routes to git -C .bit/index submodule <args> and for mutating subcommands,
-- calls syncSubmoduleToWorkingDirectory to destructively replace the
-- working directory copy.
-- Passes -c protocol.file.allow=always so local file:// clones work
-- with modern git versions that block file transport by default.
submodule :: FilePath -> FilePath -> [String] -> IO ExitCode
submodule cwd bitDir args = do
    let indexPath = bitDir </> "index"
    code <- Git.runGitRawAt indexPath
        ("-c" : "protocol.file.allow=always" : "submodule" : args)
    -- After mutating subcommands, destructively replace working directory.
    -- Skip in junction mode — git operates on .bit/index/ directly via the
    -- junction, so there's no separate working directory to sync.
    junction <- lookupEnv "BIT_GIT_JUNCTION"
    when (code == ExitSuccess && isMutatingSubcommand args && junction /= Just "1") $
        syncSubmoduleToWorkingDirectory cwd bitDir
    pure code
  where
    isMutatingSubcommand ("add":_) = True
    isMutatingSubcommand ("update":_) = True
    isMutatingSubcommand ("deinit":_) = True
    isMutatingSubcommand ("foreach":_) = True
    isMutatingSubcommand _ = False

-- | Destructively replace working directory submodule content from .bit/index/.
-- For each submodule path in .gitmodules:
-- 1. Delete <submodule-path>/ in the working directory entirely
-- 2. Replace it with the contents of .bit/index/<submodule-path>/
-- 3. Copy the gitlink file with the path rewrite applied
--
-- The index copy is the source of truth; the working directory is overwritten.
syncSubmoduleToWorkingDirectory :: FilePath -> FilePath -> IO ()
syncSubmoduleToWorkingDirectory cwd bitDir = do
    let gitmodulesPath = bitDir </> "index" </> ".gitmodules"
    subPaths <- parseGitmodulesSubmodulePaths gitmodulesPath
    forM_ subPaths $ \subPath -> do
        let indexDir = bitDir </> "index" </> subPath
        let workDir = cwd </> subPath
        indexExists <- Dir.doesDirectoryExist indexDir
        when indexExists $ do
            -- Step 1: Delete working directory copy entirely
            workExists <- Dir.doesDirectoryExist workDir
            when workExists $
                Dir.removeDirectoryRecursive workDir
                    `catch` \(_ :: IOException) -> pure ()
            -- Step 2: Replace with contents from index (excluding .git)
            Dir.createDirectoryIfMissing True workDir
            syncDirExcludingGit indexDir workDir
            -- Step 3: Copy gitlink with path rewrite
            let indexGitlink = indexDir </> ".git"
            gitlinkExists <- Dir.doesFileExist indexGitlink
            when gitlinkExists $ do
                content <- readGitlinkContent indexGitlink
                case content of
                    Just gitdirLine -> do
                        let rewritten = rewriteGitlinkPath gitdirLine
                        BS.writeFile (workDir </> ".git")
                            (encodeUtf8 (T.pack ("gitdir: " ++ rewritten ++ "\n")))
                    Nothing ->
                        Dir.copyFileWithMetadata indexGitlink (workDir </> ".git")

-- ============================================================================
-- Shared helpers
-- ============================================================================

-- | Read a gitlink file and extract the gitdir path (without the "gitdir: " prefix).
readGitlinkContent :: FilePath -> IO (Maybe String)
readGitlinkContent path = do
    bs <- BS.readFile path
    case decodeUtf8' bs of
        Left _ -> pure Nothing
        Right t ->
            let firstLine = takeWhile (\c -> c /= '\n' && c /= '\r') (T.unpack t)
            in case stripPrefixStr "gitdir: " firstLine of
                Just rest -> pure (Just rest)
                Nothing -> pure Nothing
  where
    stripPrefixStr [] ys = Just ys
    stripPrefixStr (x:xs) (y:ys) | x == y = stripPrefixStr xs ys
    stripPrefixStr _ _ = Nothing

-- | Rewrite a gitlink gitdir path for use in the working directory.
-- Inside .bit/index/, the path resolves correctly (e.g., "../../.git/modules/a/sub").
-- In the working directory, we need to insert ".bit/index/" so the path routes
-- through .bit/index/ to reach the git modules directory.
--
-- Rule: find ".git/modules/" in the path and replace with ".bit/index/.git/modules/"
rewriteGitlinkPath :: String -> String
rewriteGitlinkPath path
    | ".git/modules/" `isInfixOf` path = insertBitIndex path
    | otherwise = path
  where
    insertBitIndex [] = []
    insertBitIndex s@(c:cs)
        | ".git/modules/" `isPrefixOf` s = ".bit/index/.git/modules/" ++ drop (length (".git/modules/" :: String)) s
        | otherwise = c : insertBitIndex cs

-- | Parse .gitmodules file and extract submodule paths.
-- Handles git's INI format where path lines are tab-indented:
--   [submodule "mymod"]
--       path = mymod
--       url = /some/path
parseGitmodulesSubmodulePaths :: FilePath -> IO [FilePath]
parseGitmodulesSubmodulePaths gitmodulesPath = do
    exists <- Dir.doesFileExist gitmodulesPath
    if not exists
        then pure []
        else do
            bs <- BS.readFile gitmodulesPath
            case decodeUtf8' bs of
                Left _ -> pure []
                Right t ->
                    let ls = lines (T.unpack t)
                        paths = [trim (drop (length ("path = " :: String)) trimmed)
                                | l <- ls
                                , let trimmed = trim l
                                , "path = " `isPrefixOf` trimmed
                                ]
                    in pure paths
  where
    trim = dropWhile isWhitespace . reverse . dropWhile isWhitespace . reverse
    isWhitespace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

-- | Recursively copy directory contents, excluding .git entries.
syncDirExcludingGit :: FilePath -> FilePath -> IO ()
syncDirExcludingGit src dst = do
    Dir.createDirectoryIfMissing True dst
    entries <- Dir.listDirectory src
    forM_ entries $ \name -> do
        when (name /= ".git") $ do
            let srcPath = src </> name
            let dstPath = dst </> name
            isDir <- Dir.doesDirectoryExist srcPath
            if isDir
                then syncDirExcludingGit srcPath dstPath
                else Dir.copyFileWithMetadata srcPath dstPath
