-- | Generate three literate-programming Markdown files:
--   * bit-source-literate.md — bit.cabal + all .hs source files (excluding test/)
--   * bit-tests-literate.md  — everything under test/
--   * bit-docs-literate.md   — all .md files under docs/
--
-- After generation, stages them in git, commits, and pushes to origin.
--
-- Run: cabal run generate-literate

module Main where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List (isPrefixOf, sort)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory, createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (IOMode (WriteMode), hPutStr, hPutStrLn, hSetEncoding, utf8, withFile)
import System.Process (callProcess, readProcessWithExitCode)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Directories to skip.
excludedDirs :: [String]
excludedDirs = [".git", ".bit", "dist-newstyle", "dist", ".stack-work", ".vscode", ".history"]

-- | Ascend until we find bit.cabal.
findRepoRoot :: FilePath -> IO FilePath
findRepoRoot dir = do
  exists <- doesFileExist (dir </> "bit.cabal")
  if exists
    then return dir
    else let parent = takeDirectory dir
         in  if parent == dir
               then fail "Could not find repo root (bit.cabal)"
               else findRepoRoot parent

-- | Subdirectories of test/ to skip (generated at runtime).
excludedTestDirs :: [String]
excludedTestDirs = ["work", "work_a", "work_b"]

-- | Files inside test/ to skip.
excludedTestFiles :: [String]
excludedTestFiles = [".bit-store"]

-- | Recursively collect all .hs files under @root@, relative to @root@.
-- Skips the test/ directory (handled separately by gatherTestFiles).
gatherHsFiles :: FilePath -> IO [FilePath]
gatherHsFiles root = go ""
  where
    go rel = do
      let full = if null rel then root else root </> rel
      entries <- listDirectory full
      fmap concat $ mapM (visit rel) entries

    visit rel name
      | name `elem` excludedDirs = return []
      | name == "test"           = return []  -- test/ handled separately
      | head name == '.'         = return []
      | otherwise = do
          let relPath  = if null rel then name else rel </> name
              fullPath = root </> relPath
          isDir <- doesDirectoryExist fullPath
          if isDir
            then go relPath
            else return [ relPath | takeExtension name == ".hs" ]

-- | Collect all files under test/, relative to @root@.
gatherTestFiles :: FilePath -> IO [FilePath]
gatherTestFiles root = go "test"
  where
    go rel = do
      let full = root </> rel
      exists <- doesDirectoryExist full
      if not exists then return []
      else do
        entries <- listDirectory full
        fmap concat $ mapM (visit rel) entries

    visit rel name
      | name `elem` excludedTestDirs  = return []
      | name `elem` excludedTestFiles = return []
      | head name == '.'              = return []
      | otherwise = do
          let relPath  = rel </> name
              fullPath = root </> relPath
          isDir <- doesDirectoryExist fullPath
          if isDir
            then go relPath
            else return [relPath]

-- | Collect all .md files under docs/, relative to @root@.
gatherDocsFiles :: FilePath -> IO [FilePath]
gatherDocsFiles root = do
  let docsDir = root </> "docs"
  exists <- doesDirectoryExist docsDir
  if not exists then return []
  else do
    entries <- listDirectory docsDir
    let mdFiles = [ "docs" </> name | name <- entries
                  , takeExtension name == ".md"
                  , head name /= '.' ]
    return (sort mdFiles)

-- | Map file extension to fenced-code-block language tag.
getLang :: FilePath -> String
getLang path = case map toLower (takeExtension path) of
  ".hs"    -> "haskell"
  ".cabal" -> "cabal"
  ".sh"    -> "shell"
  ".md"    -> "markdown"
  ".yaml"  -> "yaml"
  ".yml"   -> "yaml"
  _        -> "text"  -- .test, .txt, etc.

-- | Brief explanation based on path.
getExplanation :: FilePath -> String
getExplanation rel
  | rel == "bit.cabal"                = "*Build configuration — package metadata and dependencies.*"
  | "Internal/" `isPrefixOf` rel       = "*Internal module — implementation details.*"
  | "bit/Internal/" `isPrefixOf` rel  = "*Internal module — implementation details.*"
  | rel == "bit.hs"                   = "*Entry point — main executable.*"
  | "bit/" `isPrefixOf` rel           = "*Core module — application logic.*"
  | "test/cli/" `isPrefixOf` rel       = "*CLI test — shell-based integration test.*"
  | "test/" `isPrefixOf` rel           = "*Test module.*"
  | "scripts/" `isPrefixOf` rel        = "*Build script.*"
  | "docs/" `isPrefixOf` rel           = "*Documentation file.*"
  | otherwise                          = "*Source file.*"

-- | Normalise backslashes to forward slashes.
toPosix :: FilePath -> FilePath
toPosix = map (\c -> if c == '\\' then '/' else c)

-- | Run a git command in the given directory. Returns True on success.
gitIn :: FilePath -> [String] -> IO Bool
gitIn dir args = do
  (code, _out, _err) <- readProcessWithExitCode "git" (["-C", dir] ++ args) ""
  return (code == ExitSuccess)

main :: IO ()
main = do
  cwd  <- getCurrentDirectory
  root <- findRepoRoot cwd

  hsFiles   <- gatherHsFiles root
  testFiles <- gatherTestFiles root
  docsFiles <- gatherDocsFiles root

  let sourceFiles = sort $ "bit.cabal" : hsFiles
      testSorted  = sort testFiles
      docsSorted  = sort docsFiles

  let litDir = root </> "literate-output"
  createDirectoryIfMissing True litDir

  -- Source files
  let sourcePath = litDir </> "bit-source-literate.md"
  writeDocument sourcePath
    "bit — Literate Programming Document"
    "This document contains all Haskell source files and the cabal\n\
    \file for the bit project, presented in literate-programming style."
    root sourceFiles
  putStrLn $ "Wrote " ++ sourcePath ++ " (" ++ show (length sourceFiles) ++ " files)"

  -- Test files
  let testsPath = litDir </> "bit-tests-literate.md"
  writeDocument testsPath
    "bit — Tests (Literate Programming Document)"
    "This document contains all test files for the bit project:\n\
    \Haskell test modules, shell-based integration tests, and test infrastructure."
    root testSorted
  putStrLn $ "Wrote " ++ testsPath ++ " (" ++ show (length testSorted) ++ " files)"

  -- Docs files
  let docsPath = litDir </> "bit-docs-literate.md"
  writeDocument docsPath
    "bit — Documentation (Literate Programming Document)"
    "This document contains all documentation files for the bit project:\n\
    \specifications, refactoring plans, and design documents."
    root docsSorted
  putStrLn $ "Wrote " ++ docsPath ++ " (" ++ show (length docsSorted) ++ " files)"

  -- Git: stage, commit, push (gracefully handle failures)
  putStrLn "Committing literate output..."
  addResult <- try (callProcess "git" ["-C", litDir, "add",
    "bit-source-literate.md",
    "bit-tests-literate.md",
    "bit-docs-literate.md"]) :: IO (Either SomeException ())
  case addResult of
    Left _ -> putStrLn "Warning: git add failed (continuing anyway)"
    Right () -> do
      -- Only commit if there are staged changes
      -- git diff --cached --quiet returns success (0) when there are NO changes
      noChanges <- gitIn litDir ["diff", "--cached", "--quiet"]
      if noChanges
        then putStrLn "No changes to literate output."
        else do
          commitResult <- try (callProcess "git" ["-C", litDir, "commit", "-m", "Update literate output"]) :: IO (Either SomeException ())
          case commitResult of
            Left _ -> putStrLn "Warning: git commit failed (continuing anyway)"
            Right () -> do
              putStrLn "Pushing to origin..."
              pushResult <- try (callProcess "git" ["-C", litDir, "push", "origin"]) :: IO (Either SomeException ())
              case pushResult of
                Left _ -> putStrLn "Warning: git push failed (continuing anyway)"
                Right () -> putStrLn "Done."

-- | Write a single literate document.
writeDocument :: FilePath -> String -> String -> FilePath -> [FilePath] -> IO ()
writeDocument outputPath title description root files =
  withFile outputPath WriteMode $ \h -> do
    hSetEncoding h utf8
    let write = hPutStrLn h

    write $ "# " ++ title
    write ""
    write description
    write ""
    write "---"
    write ""

    forM_ files $ \rel -> do
      let posix = toPosix rel
          lang  = getLang rel
          expl  = getExplanation rel
          full  = root </> rel

      write $ "## " ++ posix
      write ""
      write $ "**Path:** `" ++ posix ++ "`"
      write ""
      write expl
      write ""
      write $ "```" ++ lang

      -- Use strict ByteString reading to avoid lazy IO on Windows
      result <- try (BS.readFile full) :: IO (Either SomeException BS.ByteString)
      case result of
        Right bs ->
          case T.decodeUtf8' bs of
            Right content -> do
              let contentStr = T.unpack content
              hPutStr h contentStr
              if not (T.null content) && T.last content /= '\n'
                then hPutStrLn h ""
                else return ()
            Left err ->
              hPutStrLn h $ "-- Error decoding UTF-8: " ++ show err
        Left err ->
          hPutStrLn h $ "-- Error reading file: " ++ show err

      write "```"
      write ""
      write "---"
      write ""