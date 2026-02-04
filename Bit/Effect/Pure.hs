{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Bit.Effect.Pure
  ( Trace(..)
  , PureEnv(..)
  , runPure
  ) where

import Control.Monad.Free (Free(..))
import Control.Monad.State (State, runState, get, put, modify)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import Bit.Effect
import System.Exit (ExitCode(..))

-- | Trace of effects for test assertions.
data Trace
  = TGitRaw [String]
  | TGitQuery [String]
  | TRclone String [String]
  | TWrite FilePath String
  | TCopy FilePath FilePath
  | TRemove FilePath
  | TTell String
  | TTellErr String
  | TAskUser String String  -- prompt, simulated response
  | TExit ExitCode
  deriving (Show, Eq)

-- | Simulated environment for pure interpretation.
data PureEnv = PureEnv
  { pureFiles    :: Map.Map FilePath String       -- simulated filesystem
  , pureDirs     :: [FilePath]                    -- simulated directories
  , pureInputs   :: [String]                      -- simulated user inputs (consumed in order)
  , pureTrace    :: [Trace]                       -- accumulated trace (reversed)
  , pureCwd      :: FilePath                      -- simulated working directory
  }

type PureM = State PureEnv

-- | Run a free monad program purely, returning the result and accumulated trace.
runPure :: PureEnv -> Rgit a -> (a, [Trace])
runPure env program =
  let (result, finalEnv) = runState (interpretPure program) env
  in (result, reverse (pureTrace finalEnv))

interpretPure :: Rgit a -> PureM a
interpretPure (Pure a) = return a
interpretPure (Free f) = case f of
  GitRaw args k -> do
    trace (TGitRaw args)
    interpretPure (k ExitSuccess)
  GitQuery args k -> do
    trace (TGitQuery args)
    interpretPure (k (ExitSuccess, "", ""))
  RcloneExec cmd args k -> do
    trace (TRclone cmd args)
    interpretPure (k ExitSuccess)
  RcloneQuery args k -> do
    trace (TRclone "query" args)
    interpretPure (k (ExitSuccess, "", ""))
  RunProcess _ args _ k -> do
    trace (TGitQuery args)  -- reuse trace type
    interpretPure (k (ExitSuccess, "", ""))
  ReadFileE path k -> do
    env <- get
    interpretPure (k (Map.lookup path (pureFiles env)))
  WriteFileAtomicE path content next -> do
    trace (TWrite path content)
    modify (\e -> e { pureFiles = Map.insert path content (pureFiles e) })
    interpretPure next
  CopyFileE src dest next -> do
    trace (TCopy src dest)
    env <- get
    case Map.lookup src (pureFiles env) of
      Just content -> modify (\e -> e { pureFiles = Map.insert dest content (pureFiles e) })
      Nothing -> return ()
    interpretPure next
  FileExistsE path k -> do
    env <- get
    interpretPure (k (Map.member path (pureFiles env)))
  DirExistsE path k -> do
    env <- get
    interpretPure (k (path `elem` pureDirs env))
  ListDirE _path k -> interpretPure (k [])
  CreateDirE path next -> do
    modify (\e -> e { pureDirs = path : pureDirs e })
    interpretPure next
  RemoveFileE path next -> do
    trace (TRemove path)
    modify (\e -> e { pureFiles = Map.delete path (pureFiles e) })
    interpretPure next
  RemoveDirRecursiveE path next -> do
    trace (TRemove path)
    interpretPure next
  GetFileSizeE path k -> do
    env <- get
    let sz = maybe 0 (fromIntegral . length) (Map.lookup path (pureFiles env))
    interpretPure (k sz)
  ReadFileBytesE path k -> do
    env <- get
    let bs = maybe BS.empty BSC.pack (Map.lookup path (pureFiles env))
    interpretPure (k bs)
  Tell msg next -> do
    trace (TTell msg)
    interpretPure next
  TellErr msg next -> do
    trace (TTellErr msg)
    interpretPure next
  AskUser prompt k -> do
    env <- get
    case pureInputs env of
      (response:rest) -> do
        put env { pureInputs = rest }
        trace (TAskUser prompt response)
        interpretPure (k response)
      [] -> do
        trace (TAskUser prompt "")
        interpretPure (k "")
  GetCurrentDirE k -> do
    env <- get
    interpretPure (k (pureCwd env))
  ExitWithE code next -> do
    trace (TExit code)
    interpretPure next
  LiftIOE _ k -> interpretPure (k (error "liftIOE called in pure interpreter"))

  where
    trace t = modify (\e -> e { pureTrace = t : pureTrace e })
