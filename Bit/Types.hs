{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Bit.Types
  ( Path
  , HashAlgo(..)
  , Hash(..)
  , hashToText
  , EntryKind(..)
  , FileEntry(..)
  , syncHash
  , BitEnv(..)
  , BitM
  , runBitM
  ) where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Bit.Remote (Remote)

type Path = String

data HashAlgo = MD5 | SHA256
  deriving (Show, Eq, Generic)

newtype Hash (a :: HashAlgo) = Hash Text
  deriving (Show, Eq, Ord, Generic)

hashToText :: Hash a -> Text
hashToText (Hash t) = t

data EntryKind
  = File { fHash :: Hash 'MD5, fSize :: Integer, fIsText :: Bool }
  | Directory
  | Symlink FilePath
  deriving (Show, Eq, Generic)

-- | Hash to use for sync diff (MD5). File has one hash; Directory/Symlink have none.
syncHash :: EntryKind -> Maybe (Hash 'MD5)
syncHash (File h _ _) = Just h
syncHash _           = Nothing

data FileEntry = FileEntry
  { path :: FilePath
  , kind :: EntryKind
  } deriving (Show, Eq, Generic)

data BitEnv = BitEnv
    { envCwd            :: FilePath
    , envLocalFiles     :: [FileEntry]
    , envRemote         :: Maybe Remote
    , envForce          :: Bool
    , envForceWithLease :: Bool
    , envSkipVerify     :: Bool
    }

type BitM = ReaderT BitEnv IO

runBitM :: BitEnv -> BitM a -> IO a
runBitM env action = runReaderT action env
