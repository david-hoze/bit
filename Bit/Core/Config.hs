{-# LANGUAGE OverloadedStrings #-}

-- | bit config: get/set/list for .bit/config (git-style INI).
-- core.mode controls CAS writes (lite | solid).
module Bit.Core.Config
  ( BitMode(..)
  , getCoreMode
  , getCoreModeWithRoot
  , configGet
  , configSet
  , configList
  , configGetWithRoot
  , configSetWithRoot
  , configListWithRoot
  , knownConfigKeys
  , getConfigKey
  ) where

import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Control.Monad (when, unless)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, makeAbsolute)
import System.FilePath ((</>), normalise)

import Bit.Utils (atomicWriteFileStr)

-- | Resolve .bit directory from CWD (follows bitlink). Returns absolute path so read/write use the same file.
resolveBitDirConfig :: IO (Maybe FilePath)
resolveBitDirConfig = do
  cwd <- getCurrentDirectory
  let dotBit = cwd </> ".bit"
  isDir <- doesDirectoryExist dotBit
  if isDir
    then Just <$> makeAbsolute (normalise dotBit)
    else do
      isFile <- doesFileExist dotBit
      if isFile
        then do
          bs <- BS.readFile dotBit
          let content = either (const "") T.unpack (T.decodeUtf8' bs)
          case lines content of
            (firstLine:_) -> Just <$> makeAbsolute (normalise (drop 8 (filter (/= '\r') firstLine)))
            [] -> pure Nothing
        else pure Nothing

-- | Repo mode: whether bit add writes to CAS.
data BitMode = ModeLite | ModeSolid
  deriving (Show, Eq)

findIndex' :: (a -> Bool) -> [a] -> Maybe Int
findIndex' p xs = listToMaybe [i | (i, x) <- zip [0..] xs, p x]

parseKeyValueLine :: T.Text -> Maybe (String, String)
parseKeyValueLine l =
  case T.break (== '=') (T.strip l) of
    (k, v) | not (T.null k) ->
      let v' = T.strip (T.drop 1 v)
          -- Strip surrounding quotes only when length >= 2 to avoid T.init "" (e.g. key = ")
          v'' = T.unpack $ if T.isPrefixOf "\"" v' && T.length v' >= 2
                            then T.drop 1 (T.dropEnd 1 v')
                            else v'
      in Just (T.unpack (T.strip k), v'')
    _ -> Nothing

-- | Read raw config file content from bit root.
readConfigRaw :: FilePath -> IO String
readConfigRaw bitRoot = do
  let path = bitRoot </> "config"
  exists <- doesFileExist path
  if not exists then pure ""
  else T.unpack . either (const T.empty) id . T.decodeUtf8' <$> BS.readFile path

-- | Get value for key "section.key". Returns Nothing if missing or invalid key.
getConfigKey :: FilePath -> String -> IO (Maybe String)
getConfigKey bitRoot key =
  case break (== '.') key of
    (section, '.':k) | not (null section) && not (null k) -> do
      raw <- readConfigRaw bitRoot
      let sectionLines = extractSection section (T.lines (T.pack raw))
          kvs = mapMaybe parseKeyValueLine sectionLines
      pure $ listToMaybe [v | (keyPart, v) <- kvs, keyPart == k]
    _ -> pure Nothing

extractSection :: String -> [T.Text] -> [T.Text]
extractSection sectionName linesOfText =
  let sectionHeader = "[" ++ sectionName ++ "]"
      startIdx = maybe (length linesOfText) (+ 1) $
        findIndex' (\l -> T.strip l == T.pack sectionHeader) linesOfText
      rest = drop startIdx linesOfText  -- lines after the section header (content + following sections)
      endIdx = maybe (length rest) id $
        findIndex' (\l -> T.pack "[" `T.isPrefixOf` T.stripStart l) rest
  in take endIdx rest

-- | Get core.mode given .bit root. Defaults to ModeLite if unset or invalid.
getCoreModeWithRoot :: FilePath -> IO BitMode
getCoreModeWithRoot bitRoot = do
  mVal <- getConfigKey bitRoot "core.mode"
  case mVal of
    Just "solid" -> pure ModeSolid
    _ -> pure ModeLite

-- | Get current core.mode (resolves .bit from CWD). Defaults to ModeLite if unset or invalid.
getCoreMode :: IO BitMode
getCoreMode = do
  mRoot <- resolveBitDirConfig
  case mRoot of
    Nothing -> pure ModeLite
    Just bitRoot -> getCoreModeWithRoot bitRoot

-- | Set core.mode at given .bit root. Writes config and prints hints.
setCoreModeWithRoot :: FilePath -> BitMode -> IO ()
setCoreModeWithRoot bitRoot mode = do
  let path = bitRoot </> "config"
  raw <- readConfigRaw bitRoot
  let newContent = setOrReplaceSection raw "core" [("mode", modeValue mode)]
  atomicWriteFileStr path newContent
  when (mode == ModeSolid) $ do
    putStrLn "Mode set to solid. bit add will now store file content in .bit/cas/."
    putStrLn "hint: Run 'bit cas backfill' to store current files for existing commits."
  when (mode == ModeLite) $ do
    putStrLn "Mode set to lite. bit add will no longer store file content in .bit/cas/."
    putStrLn "Existing CAS data is preserved."
  where
    modeValue ModeLite = "lite"
    modeValue ModeSolid = "solid"

-- | Dump all section.key = value pairs from config.
listAllKeys :: FilePath -> IO [(String, String)]
listAllKeys bitRoot = do
  raw <- readConfigRaw bitRoot
  let lineList = lines raw
      stripLine l = dropWhile isSpace (dropWhileEnd isSpace l)
      go _ [] acc = acc
      go current (l : ls) acc =
        let s = stripLine l
        in if not (null s) && head s == '['
             then let name = takeWhile (/= ']') (drop 1 s)
                  in go (Just name) ls acc
             else case (current, parseKeyValueLine (T.pack l)) of
               (Just sec, Just (k, v)) -> go current ls ((sec ++ "." ++ k, v) : acc)
               _ -> go current ls acc
  pure $ reverse (go Nothing lineList [])

-- | Get config value and print it. For bit config <key>.
configGet :: String -> IO ()
configGet key = do
  mRoot <- resolveBitDirConfig
  case mRoot of
    Nothing -> fail "not a bit repository"
    Just bitRoot -> configGetWithRoot bitRoot key

-- | Set config value. Validates known keys (e.g. core.mode only lite|solid).
configSet :: String -> String -> IO ()
configSet key value = do
  mRoot <- resolveBitDirConfig
  case mRoot of
    Nothing -> fail "not a bit repository"
    Just bitRoot -> configSetWithRoot bitRoot key value

-- | List all config key = value. For bit config --list.
configList :: IO ()
configList = do
  mRoot <- resolveBitDirConfig
  case mRoot of
    Nothing -> fail "not a bit repository"
    Just bitRoot -> configListWithRoot bitRoot

-- | Get config value using explicit .bit path (avoids CWD ambiguity).
configGetWithRoot :: FilePath -> String -> IO ()
configGetWithRoot bitRoot key = do
  mVal <- getConfigKey bitRoot key
  case mVal of
    Just v -> putStrLn v
    Nothing -> fail ("key " ++ key ++ " not found")

-- | Known config keys (spec: unknown keys are rejected to prevent typos).
-- When adding a key here, use the generic write branch below (do not add a special when (key == "x") path).
knownConfigKeys :: [String]
knownConfigKeys = ["core.mode", "cdc.enabled", "cdc.min-size", "cdc.avg-size", "cdc.max-size"]

-- | Set config value using explicit .bit path (avoids CWD ambiguity).
configSetWithRoot :: FilePath -> String -> String -> IO ()
configSetWithRoot bitRoot key value = do
  unless (key `elem` knownConfigKeys) $
    fail ("unknown config key: " ++ key)
  when (key == "core.mode") $
    unless (value `elem` ["lite", "solid"]) $
      fail "core.mode must be 'lite' or 'solid'"
  when (key == "cdc.enabled") $
    unless (value `elem` ["true", "false"]) $
      fail "cdc.enabled must be 'true' or 'false'"
  when (key `elem` ["cdc.min-size", "cdc.avg-size", "cdc.max-size"]) $
    case reads value :: [(Int, String)] of
      [(n, "")] | n > 0 -> pure ()
      _ -> fail (key ++ " must be a positive integer")
  case break (== '.') key of
    (section, '.':k) | not (null section) && not (null k) -> do
      when (key == "core.mode") $ do
        let mode = if value == "solid" then ModeSolid else ModeLite
        setCoreModeWithRoot bitRoot mode
        pure ()
      -- Branch for other known keys (currently only core.mode is in knownConfigKeys).
      when (key /= "core.mode") $ do
        let path = bitRoot </> "config"
        raw <- readConfigRaw bitRoot
        let newContent = setOrReplaceSection raw section [(k, value)]
        atomicWriteFileStr path newContent
    _ -> fail ("invalid key " ++ key)

-- | List all config key = value using explicit .bit path.
configListWithRoot :: FilePath -> IO ()
configListWithRoot bitRoot = do
  kvs <- listAllKeys bitRoot
  mapM_ (\(k, v) -> putStrLn (k ++ "=" ++ v)) kvs

-- | Set or replace a key in a section; write back INI. Preserves other sections.
setOrReplaceSection :: String -> String -> [(String, String)] -> String
setOrReplaceSection raw section newKvs =
  let linesList = lines raw
      stripLine = dropWhile isSpace . dropWhileEnd isSpace
      (before, fromSection) = break (\l -> stripLine l == "[" ++ section ++ "]") linesList
      (contentLines, afterSection) = case fromSection of
        [] -> ([], [])
        (_ : rest) ->
          let content = takeWhile (\l -> let s = stripLine l in null s || (not (null s) && head s /= '[')) rest
              after = drop (length content) rest
          in (content, after)
      existingKvs = mapMaybe (\l -> parseKeyValueLine (T.pack l)) (filter (not . null . stripLine) contentLines)
      mergedKvs = foldr (\(k, val) acc -> (k, val) : filter ((/= k) . fst) acc) existingKvs newKvs
      sectionLines = ("[" ++ section ++ "]") : [ "    " ++ k ++ " = " ++ v | (k, v) <- mergedKvs ]
      rebuilt = before ++ sectionLines ++ afterSection
  in unlines rebuilt
