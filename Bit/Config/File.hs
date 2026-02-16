{-# LANGUAGE OverloadedStrings #-}

module Bit.Config.File
  ( TextConfig(..)
  , defaultTextConfig
  , readConfig
  , readTextConfig
  ) where

import System.FilePath ((</>))
import System.Directory (doesFileExist, doesDirectoryExist)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

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

-- | Resolve the .bit root directory from CWD, following bitlinks if needed.
resolveBitRoot :: IO (Maybe FilePath)
resolveBitRoot = do
    isDir <- doesDirectoryExist ".bit"
    if isDir then pure (Just ".bit")
    else do
        isFile <- doesFileExist ".bit"
        if isFile then do
            bs <- BS.readFile ".bit"
            let content = either (const "") T.unpack (T.decodeUtf8' bs)
            case lines content of
                (firstLine:_) -> pure (Just (drop 8 (filter (/= '\r') firstLine)))
                [] -> pure Nothing
        else pure Nothing

-- | Read the entire config file and return TextConfig
-- Falls back to defaultTextConfig if file doesn't exist or parsing fails
-- Uses strict ByteString reading to avoid Windows file locking issues
readTextConfig :: IO TextConfig
readTextConfig = do
  mBitRoot <- resolveBitRoot
  case mBitRoot of
    Nothing -> pure defaultTextConfig
    Just bitRoot -> do
      let cfgPath = bitRoot </> "config"
      exists <- doesFileExist cfgPath
      if not exists
        then pure defaultTextConfig
        else do
          bs <- BS.readFile cfgPath
          let content = either (const T.empty) id (T.decodeUtf8' bs)
          pure $ parseConfig content

-- | Read config file (for future expansion)
readConfig :: IO TextConfig
readConfig = readTextConfig

-- | Parse config file content (INI-style format)
parseConfig :: T.Text -> TextConfig
parseConfig content = 
  let linesOfText = T.lines content
      -- Find [text] section
      textSection = extractSection "text" linesOfText
      -- Parse size-limit
      sizeLimit = fromMaybe (textSizeLimit defaultTextConfig) (parseSizeLimit textSection)
      -- Parse extensions
      extensions = fromMaybe (textExtensions defaultTextConfig) (parseExtensions textSection)
  in TextConfig { textSizeLimit = sizeLimit, textExtensions = extensions }

-- | Find index of first element matching predicate
findIndex' :: (a -> Bool) -> [a] -> Maybe Int
findIndex' p xs = listToMaybe [i | (i, x) <- zip [0..] xs, p x]

-- | Extract lines for a given section (between [section] and next [section] or EOF)
extractSection :: String -> [T.Text] -> [T.Text]
extractSection sectionName linesOfText =
  let sectionHeader = "[" ++ sectionName ++ "]"
      -- Find start of section
      startIdx = maybe (length linesOfText) (+ 1) $
        findIndex' (\l -> T.strip l == T.pack sectionHeader) linesOfText
      -- Find end of section (next [section] or EOF)
      endIdx = maybe (length linesOfText) (+ startIdx) $
        findIndex' (\l -> T.pack "[" `T.isPrefixOf` T.stripStart l) (drop startIdx linesOfText)
  in map T.strip $ take (endIdx - startIdx) (drop startIdx linesOfText)

-- | Parse size-limit from section lines
parseSizeLimit :: [T.Text] -> Maybe Integer
parseSizeLimit linesOfText =
  let findLine prefix = [T.unpack (T.drop (T.length (T.pack prefix)) (T.strip l)) | l <- linesOfText, T.pack prefix `T.isPrefixOf` T.stripStart l]
      sizeLines = findLine "size-limit"
  in listToMaybe sizeLines >>= \sizeStr ->
      let cleaned = takeWhile (/= '#') sizeStr
          trimmed = dropWhile isSpace $ dropWhileEnd isSpace cleaned
      in case reads trimmed of
        [(n, "")] -> Just n
        _ -> Nothing

-- | Parse extensions from section lines
parseExtensions :: [T.Text] -> Maybe [String]
parseExtensions linesOfText =
  let findLine prefix = [T.unpack (T.drop (T.length (T.pack prefix)) (T.strip l)) | l <- linesOfText, T.pack prefix `T.isPrefixOf` T.stripStart l]
      extLines = findLine "extensions"
  in listToMaybe extLines >>= \extStr ->
      let cleaned = takeWhile (/= '#') extStr
          trimmed = dropWhile isSpace $ dropWhileEnd isSpace cleaned
          exts = map (dropWhile isSpace . dropWhileEnd isSpace) $ splitComma trimmed
      in Just exts
  where
    splitComma s = case break (== ',') s of
      (part, "") -> [part]
      (part, _:rest) -> part : splitComma rest
