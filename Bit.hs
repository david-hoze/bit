{-# LANGUAGE ScopedTypeVariables #-}

import Bit.Commands
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO (hSetEncoding, stdout, stderr, hIsTerminalDevice)
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
    
    -- Set UTF-8 for all IO (stdout, stderr, and subprocess pipes)
    -- This ensures readProcessWithExitCode decodes git output as UTF-8
    setLocaleEncoding utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    Bit.Commands.run
