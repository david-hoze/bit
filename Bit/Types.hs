{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Bit.Types
  ( Path(..)
  , HashAlgo(..)
  , Hash(..)
  , hashToText
  , ContentType(..)
  , EntryKind(..)
  , FileEntry(..)
  , syncHash
  , ForceMode(..)
  , BitEnv(..)
  , BitM
  , runBitM
  ) where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Bit.Remote (Remote)

newtype Path = Path { unPath :: FilePath }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, IsString)

data HashAlgo = MD5 | SHA256
  deriving (Show, Eq, Generic)

newtype Hash (a :: HashAlgo) = Hash Text
  deriving (Show, Eq, Ord, Generic)

hashToText :: Hash a -> Text
hashToText (Hash t) = t

data ContentType = TextContent | BinaryContent
  deriving (Show, Eq, Generic)

data EntryKind
  = File { fHash :: Hash 'MD5, fSize :: Integer, fContentType :: ContentType }
  | Directory
  | Symlink FilePath
  deriving (Show, Eq, Generic)

-- | Hash to use for sync diff (MD5). File has one hash; Directory/Symlink have none.
syncHash :: EntryKind -> Maybe (Hash 'MD5)
syncHash (File h _ _) = Just h
syncHash _            = Nothing

data FileEntry = FileEntry
  { path :: Path
  , kind :: EntryKind
  } deriving (Show, Eq, Generic)

-- | How to handle force pushing.
data ForceMode
    = NoForce         -- ^ Normal push (check ancestry)
    | Force           -- ^ Overwrite remote unconditionally
    | ForceWithLease  -- ^ Overwrite only if remote matches last fetch
    deriving (Show, Eq)

data BitEnv = BitEnv
    { envCwd            :: FilePath
    , envLocalFiles     :: [FileEntry]
    , envRemote         :: Maybe Remote
    , envForceMode      :: ForceMode
    , envSkipVerify     :: Bool
    }

type BitM = ReaderT BitEnv IO

runBitM :: BitEnv -> BitM a -> IO a
runBitM env action = runReaderT action env
