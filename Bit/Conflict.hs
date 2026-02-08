{-# LANGUAGE DataKinds #-}

module Bit.Conflict
  ( Resolution(..)
  , ConflictInfo(..)
  , resolveConflict
  , resolveAll
  , getConflictedFilesE
  , parseConflictInfo
  ) where

import Data.Char (toLower, isSpace)
import Data.List (isPrefixOf, dropWhileEnd)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Control.Monad (void, when)
import System.Exit (ExitCode(..))
import qualified Internal.Git as Git
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import Bit.Internal.Metadata (MetaContent(..), parseMetadata, displayHash)

-- | A conflict resolution choice: keep local or take remote.
data Resolution = KeepLocal | TakeRemote
  deriving (Show, Eq)

-- | Conflict type (mirrors Internal.Git.ConflictType but doesn't depend on IO module).
data ConflictInfo
  = ContentConflict FilePath   -- both modified
  | ModifyDelete FilePath Bool -- True = deleted in HEAD (ours)
  | AddAdd FilePath            -- both added different
  deriving (Show, Eq)

-- | Get list of conflicted files
getConflictedFilesE :: IO [FilePath]
getConflictedFilesE = do
  (code, out, _) <- Git.runGitWithOutput ["diff", "--name-only", "--diff-filter=U"]
  pure $ if code /= ExitSuccess then [] else filter (not . null) (lines out)

-- | Detect conflict type
getConflictInfoE :: FilePath -> IO ConflictInfo
getConflictInfoE path = do
  (_, out, _) <- Git.runGitWithOutput ["ls-files", "-u", "--", path]
  pure (parseConflictInfo path out)

-- | Pure parsing of `git ls-files -u` output into ConflictInfo.
parseConflictInfo :: FilePath -> String -> ConflictInfo
parseConflictInfo path out =
  let stageNum line = case reverse (words (takeWhile (/= '\t') line)) of
        (s:_) | s `elem` ["1","2","3"] -> readMaybe s :: Maybe Int
        _ -> Nothing
      stageNums = mapMaybe stageNum (lines out)
      has1 = 1 `elem` stageNums
      has2 = 2 `elem` stageNums
      has3 = 3 `elem` stageNums
  in if has2 && has3 && has1 then ContentConflict path
     else if has2 && has3 && not has1 then AddAdd path
     else if has2 && not has3 then ModifyDelete path False
     else if has3 && not has2 then ModifyDelete path True
     else ContentConflict path

-- | Print a conflict type announcement (git-style message).
announceConflict :: ConflictInfo -> IO ()
announceConflict (ContentConflict path) =
  putStrLn $ "CONFLICT (content): Merge conflict in " ++ path
announceConflict (ModifyDelete path True) =
  putStrLn $ "CONFLICT (modify/delete): " ++ path ++ " deleted in HEAD and modified in origin/main"
announceConflict (ModifyDelete path False) =
  putStrLn $ "CONFLICT (modify/delete): " ++ path ++ " deleted in origin/main and modified in HEAD"
announceConflict (AddAdd path) =
  putStrLn $ "CONFLICT (add/add): Merge conflict in " ++ path

-- | Format side info (hash + size) from metadata content retrieved via `git show :N:path`.
formatSideInfo :: ExitCode -> String -> (String, String)
formatSideInfo code content = case parseMetadata content of
  Just mc -> (displayHash (metaHash mc), show (metaSize mc))
  Nothing | code /= ExitSuccess || null content -> ("(deleted)", "-")
          | otherwise -> ("(text file)", "-")

-- | Resolve a single conflict: announce type, display info, ask user, apply choice.
resolveConflict :: Int -> Int -> FilePath -> IO Resolution
resolveConflict idx total path = do
  -- 1. Detect and announce conflict type
  cinfo <- getConflictInfoE path
  announceConflict cinfo

  -- 2. Display path with progress counter
  let displayPath = if "index/" `isPrefixOf` path then drop 6 path else path
  putStrLn $ "Conflict [" ++ show idx ++ "/" ++ show total ++ "]: " ++ displayPath

  -- 3. Show local (ours, stage 2) and remote (theirs, stage 3) metadata
  (codeO, oursOut, _)   <- Git.runGitWithOutput ["show", ":2:" ++ path]
  (codeR, theirsOut, _) <- Git.runGitWithOutput ["show", ":3:" ++ path]
  let (hashO, sizeO) = formatSideInfo codeO oursOut
  let (hashR, sizeR) = formatSideInfo codeR theirsOut
  putStrLn $ "  Local:  " ++ hashO ++ " (" ++ sizeO ++ " bytes)"
  putStrLn $ "  Remote: " ++ hashR ++ " (" ++ sizeR ++ " bytes)"

  -- 4. Ask user for choice
  putStr "  Use (l)ocal or (r)emote version? "
  hFlush stdout
  choice <- getLine
  let res = if normalize choice `elem` ["r", "remote"] then TakeRemote else KeepLocal

  -- 5. Apply resolution
  applyResolution path res
  pure res

-- | Apply a resolution to one file: git checkout ours/theirs + git add.
applyResolution :: FilePath -> Resolution -> IO ()
applyResolution path res = do
  let checkoutFlag = case res of
        KeepLocal  -> "--ours"
        TakeRemote -> "--theirs"
  code <- Git.runGitRaw ["checkout", checkoutFlag, "--", path]
  when (code /= ExitSuccess) $ hPutStrLn stderr "Warning: checkout failed."
  void $ Git.runGitRaw ["add", path]

-- | Normalize user input: trim whitespace, lowercase.
normalize :: String -> String
normalize = map toLower . dropWhileEnd isSpace . dropWhile isSpace

-- | Resolve all conflicts as a structured traversal.
-- Each conflict is visited exactly once, in order, with correct numbering.
-- Returns the list of resolutions applied.
resolveAll :: [FilePath] -> IO [Resolution]
resolveAll conflicts = do
  when (null conflicts) $ putStrLn "No unmerged paths."
  let total = length conflicts
  mapM (\(i, p) -> resolveConflict i total p) (zip [1..] conflicts)
