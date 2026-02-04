{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bit.Fsck
  ( doFsck
  ) where

import qualified Bit.Verify as Verify
import qualified Bit.Utils as Utils
import qualified Internal.Git as Git
import System.FilePath ((</>))
import System.Exit (ExitCode(..), exitWith)
import Control.Monad (unless, when)
import System.IO (hPutStr, hPutStrLn, hFlush, hSetBuffering, BufferMode(..), stderr)

-- | Run local-only integrity check in the spirit of git fsck: no network.
-- [1/2] Local working tree vs local metadata (rgit equivalent of checking objects).
-- [2/2] git fsck on .rgit/index/.git (metadata history integrity).
-- Prints one line per problem (git-style: "missing <path>", "hash mismatch <path>")
-- and passes through git fsck output. Exits 1 if any check finds issues.
doFsck :: FilePath -> IO ()
doFsck cwd = do
  hSetBuffering stderr NoBuffering
  -- [1/2] Working tree vs local metadata
  (_, localIssues) <- Verify.verifyLocal cwd
  let localOk = null localIssues
  unless localOk $ do
    mapM_ (printIssue Utils.toPosix) localIssues
    hFlush stderr

  -- [2/2] Metadata history (git fsck in .rgit/index/.git)
  (gitCode, gitOut, gitErr) <- Git.fsck
  let gitOk = gitCode == ExitSuccess
  unless gitOk $ do
    putStr gitOut
    hPutStr stderr gitErr

  when (not localOk || not gitOk) $
    exitWith (ExitFailure 1)

  where
    printIssue :: (FilePath -> FilePath) -> Verify.VerifyIssue -> IO ()
    printIssue toP = \case
      Verify.HashMismatch path _ _ _ _ ->
        hPutStrLn stderr $ "hash mismatch " ++ toP path
      Verify.Missing path ->
        hPutStrLn stderr $ "missing " ++ toP path
