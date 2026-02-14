module Bit.Scan.Fsck
  ( doFsck
  ) where

import qualified Bit.Git.Run as Git
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStr, stderr)
import Control.Monad (unless)

-- | Run git fsck on the internal metadata repository (.bit/index).
-- Passes through git's output and exit code.
doFsck :: FilePath -> IO ()
doFsck _cwd = do
  (code, out, err) <- Git.fsck
  putStr out
  hPutStr stderr err
  unless (code == ExitSuccess) $
    exitWith (ExitFailure 1)
