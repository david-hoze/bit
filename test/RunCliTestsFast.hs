-- Run a subset of CLI shell tests (no gdrive, no device prompt).
-- Same PATH setup as RunCliTests; runs only the listed .test files.
import System.Process (callProcess, readProcess)
import System.Environment (getEnvironment, setEnv)
import System.FilePath (takeDirectory, (</>))
import qualified System.Directory as Dir
import Data.List (lookup)
import System.Info (os)

-- | Test files that run without rclone gdrive or device subst. Local-only or quick.
cliFastTests :: [FilePath]
cliFastTests =
  [ "000-cleanup.test"
  , "bitignore.test"
  , "init.test"
  , "init-config.test"
  , "no-repo.test"
  , "one-repo.test"
  , "ls-files.test"
  , "status.test"
  , "skip-scan.test"
  , "scan-cache.test"
  , "fsck.test"
  , "path-type-safety.test"
  , "process-io.test"
  , "restore-checkout.test"
  , "verify.test"
  , "verify-progress.test"
  , "merge-local.test"
  , "filesystem-remote-direct.test"
  , "proof-of-possession.test"
  , "unicode.test"
  , "upstream-tracking.test"
  , "remote-show.test"
  , "remote-flag.test"
  ]

main :: IO ()
main = do
  -- Prepend directory containing bit to PATH so shelltest runs use this build
  bitBin <- readProcess "cabal" ["list-bin", "bit"] ""
  let bitDir = takeDirectory (filter (`notElem` "\n\r") bitBin)
  env <- getEnvironment
  let pathSep = if os == "mingw32" || os == "win32" then ";" else ":"
  let path = case lookup "PATH" env of
        Nothing -> bitDir
        Just p  -> bitDir ++ pathSep ++ p
  setEnv "PATH" path
  -- Set BIT_CEILING_DIRECTORIES so findBitRoot won't walk past test output dirs
  cwd <- Dir.getCurrentDirectory
  setEnv "BIT_CEILING_DIRECTORIES" (cwd </> "test" </> "cli" </> "output")
  let cliDir = "test" </> "cli"
  let testPaths = map (cliDir </>) cliFastTests
  callProcess "shelltest" testPaths
