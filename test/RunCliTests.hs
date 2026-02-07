{-# LANGUAGE ScopedTypeVariables #-}
-- Run CLI tests via shelltest (shelltestrunner).
-- Requires: cabal install shelltestrunner (so shelltest is on PATH).
-- Ensures bit from this project is on PATH when running tests.
import System.Process (callProcess, readProcess, rawSystem)
import Control.Exception (catch, SomeException)
import Control.Monad (void)
import System.Environment (getEnvironment, setEnv)
import System.FilePath (takeDirectory, (</>))
import Data.List (lookup)
import System.Info (os)

main :: IO ()
main = do
  -- Purge gdrive remote before tests (cleans orphan.txt etc. from previous runs)
  let purgeAndMkdir =
        rawSystem "rclone" ["purge", "gdrive-test:bit-test"] >>
        rawSystem "rclone" ["mkdir", "gdrive-test:bit-test"]
  _ <- catch (void purgeAndMkdir) (\(_ :: SomeException) -> return ())
  -- Prepend directory containing bit to PATH so shelltest runs use this build
  bitBin <- readProcess "cabal" ["list-bin", "bit"] ""
  let bitDir = takeDirectory (filter (`notElem` "\n\r") bitBin)
  env <- getEnvironment
  let pathSep = if os == "mingw32" || os == "win32" then ";" else ":"
  let path = case lookup "PATH" env of
        Nothing -> bitDir
        Just p  -> bitDir ++ pathSep ++ p
  setEnv "PATH" path
  -- Run shelltest on test/cli (Format 3 .test files)
  callProcess "shelltest" ["test" </> "cli"]