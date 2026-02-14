{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-- | Device name acquisition for filesystem remotes.
-- Supports injectable I/O for testing the interactive path.
module Bit.Device.Prompt
  ( InputSource(..)
  , acquireDeviceName
  , acquireDeviceNameAuto
  , sanitizeDeviceName
  , isValidDeviceName
  ) where

import System.Environment (lookupEnv)
import System.IO (hFlush, stdout, hIsTerminalDevice, stdin, hPutStrLn, stderr)
import Data.Maybe (fromMaybe)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Control.Monad (when, unless)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

-- | Source of user input for device name.
data InputSource
  = Interactive (IO String)  -- ^ Action that prompts and reads (e.g. getLine)
  | NonInteractive           -- ^ Use default without prompting

-- | Sanitize a label for use as device name (alphanumeric, underscores, hyphens only)
sanitizeDeviceName :: String -> String
sanitizeDeviceName s =
  let replaceSpace c = if c == ' ' then '_' else c
      withUnderscores = map replaceSpace s
      valid c = c `elem` (['a'..'z']++['A'..'Z']++['0'..'9']++"_-")
      cleaned = filter valid withUnderscores
  in if null cleaned then "device" else cleaned

-- | Validate device name: alphanumeric, underscores, hyphens
isValidDeviceName :: String -> Bool
isValidDeviceName s = not (null s) && all (\c -> c `elem` (['a'..'z']++['A'..'Z']++['0'..'9']++"_-")) s

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Acquire device name from the given input source.
-- Applies sanitization to user input. Empty/whitespace input uses default.
acquireDeviceName
  :: InputSource
  -> Maybe String           -- ^ Volume label (for default suggestion)
  -> (String -> IO Bool)    -- ^ Check if name already exists
  -> IO String
acquireDeviceName inputSource mLabel nameExists = do
  let rawDefault = fromMaybe "device" mLabel
      defaultName = sanitizeDeviceName rawDefault
  finalName <- case inputSource of
    NonInteractive -> pure defaultName
    Interactive ask -> do
      putStrLn "This path is on a storage device."
      putStrLn "bit identifies devices, not drive letters. The remote will stay linked"
      putStrLn "to this device even if the drive letter changes."
      putStrLn ""
      putStr $ "Name this device [" ++ defaultName ++ "]: "
      hFlush stdout
      line <- ask
      let name = trim line
      pure $ if null name then defaultName else sanitizeDeviceName name
  unless (isValidDeviceName finalName) $ do
    hPutStrLn stderr "fatal: Device name must be alphanumeric with underscores/hyphens only."
    exitWith (ExitFailure 1)
  exists <- nameExists finalName
  when exists $ do
    hPutStrLn stderr ("fatal: Device '" ++ finalName ++ "' already exists.")
    exitWith (ExitFailure 1)
  pure finalName

-- | Production entry point: detects TTY or BIT_USE_STDIN for testing.
acquireDeviceNameAuto
  :: Maybe String
  -> (String -> IO Bool)
  -> IO String
acquireDeviceNameAuto mLabel nameExists = do
  isTTY <- hIsTerminalDevice stdin
  useStdin <- (== Just "1") <$> lookupEnv "BIT_USE_STDIN"
  let src = if | isTTY    -> Interactive getLine
               | useStdin -> Interactive getLine  -- For tests: pipe input to stdin
               | otherwise -> NonInteractive
  acquireDeviceName src mLabel nameExists
