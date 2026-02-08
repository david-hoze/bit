{-# LANGUAGE ScopedTypeVariables #-}

import Bit.Commands
import System.IO (hSetEncoding, stdout, stderr, utf8, hIsTerminalDevice)
import System.Info (os)
import System.Process (callCommand)
import Control.Exception (catch, SomeException)
import Control.Monad (when)

main :: IO ()
main = do
    -- Set console to UTF-8 on Windows (only when interactive)
    -- Skip during automated tests to avoid git binary file issues
    isTerminal <- hIsTerminalDevice stdout
    when (os == "mingw32" && isTerminal) $
        callCommand "chcp 65001 > nul 2>&1" `catch` \(_ :: SomeException) -> pure ()
    
    -- Set UTF-8 for stdout/stderr to properly display Unicode characters
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    Bit.Commands.run
