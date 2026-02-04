{-# LANGUAGE OverloadedStrings #-}

module Internal.ConfigFile
  ( TextConfig(..)
  , defaultTextConfig
  , readConfig
  , readTextConfig
  ) where

import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Internal.Config (bitDir)

-- | Configuration for text file classification
data TextConfig = TextConfig
  { textSizeLimit :: Integer  -- Files larger than this are always binary
  , textExtensions :: [String]  -- Extensions that are always text (if other checks pass)
  } deriving (Show, Eq)

-- | Default text file configuration
defaultTextConfig :: TextConfig
defaultTextConfig = TextConfig
  { textSizeLimit = 1048576  -- 1MB
  , textExtensions = [".txt", ".md", ".yaml", ".yml", ".json", ".xml", ".html", ".css", ".js", ".py", ".hs", ".rs"]
  }

-- | Path to config file
configPath :: FilePath
configPath = bitDir </> "config"

-- | Read the entire config file and return TextConfig
-- Falls back to defaultTextConfig if file doesn't exist or parsing fails
readTextConfig :: IO TextConfig
readTextConfig = do
  exists <- doesFileExist configPath
  if not exists
    then return defaultTextConfig
    else do
      content <- TIO.readFile configPath
      return $ parseConfig content

-- | Read config file (for future expansion)
readConfig :: IO TextConfig
readConfig = readTextConfig

-- | Parse config file content (INI-style format)
parseConfig :: T.Text -> TextConfig
parseConfig content = 
  let lines = T.lines content
      -- Find [text] section
      textSection = extractSection "text" lines
      -- Parse size-limit
      sizeLimit = fromMaybe (textSizeLimit defaultTextConfig) (parseSizeLimit textSection)
      -- Parse extensions
      extensions = fromMaybe (textExtensions defaultTextConfig) (parseExtensions textSection)
  in TextConfig { textSizeLimit = sizeLimit, textExtensions = extensions }

-- | Find index of first element matching predicate
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p xs = case [i | (i, x) <- zip [0..] xs, p x] of
  [] -> Nothing
  (i:_) -> Just i

-- | Extract lines for a given section (between [section] and next [section] or EOF)
extractSection :: String -> [T.Text] -> [T.Text]
extractSection sectionName lines =
  let sectionHeader = "[" ++ sectionName ++ "]"
      -- Find start of section
      startIdx = case findIndex (\l -> T.strip l == T.pack sectionHeader) lines of
        Nothing -> length lines  -- Section not found
        Just idx -> idx + 1
      -- Find end of section (next [section] or EOF)
      endIdx = case findIndex (\l -> T.stripStart l `T.isPrefixOf` T.pack "[") (drop startIdx lines) of
        Nothing -> length lines
        Just idx -> startIdx + idx
  in map T.strip $ take (endIdx - startIdx) (drop startIdx lines)

-- | Parse size-limit from section lines
parseSizeLimit :: [T.Text] -> Maybe Integer
parseSizeLimit lines =
  let findLine prefix = [T.unpack (T.drop (T.length (T.pack prefix)) (T.strip l)) | l <- lines, T.stripStart l `T.isPrefixOf` T.pack prefix]
      sizeLines = findLine "size-limit"
  in case sizeLines of
    [] -> Nothing
    (sizeStr:_) -> 
      -- Remove comments and parse
      let cleaned = takeWhile (/= '#') sizeStr
          trimmed = dropWhile isSpace $ dropWhileEnd isSpace cleaned
      in case reads trimmed of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Parse extensions from section lines
parseExtensions :: [T.Text] -> Maybe [String]
parseExtensions lines =
  let findLine prefix = [T.unpack (T.drop (T.length (T.pack prefix)) (T.strip l)) | l <- lines, T.stripStart l `T.isPrefixOf` T.pack prefix]
      extLines = findLine "extensions"
  in case extLines of
    [] -> Nothing
    (extStr:_) ->
      -- Remove comments and parse comma-separated list
      let cleaned = takeWhile (/= '#') extStr
          trimmed = dropWhile isSpace $ dropWhileEnd isSpace cleaned
          -- Split by comma and clean each extension
          exts = map (dropWhile isSpace . dropWhileEnd isSpace) $ splitComma trimmed
      in Just exts
  where
    splitComma s = case break (== ',') s of
      (part, "") -> [part]
      (part, _:rest) -> part : splitComma rest

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse
