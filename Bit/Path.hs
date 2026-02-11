-- | Path newtypes for compile-time enforcement of path semantics.
--
-- 'RemotePath' wraps a 'FilePath' that refers to a remote filesystem location
-- (e.g. a UNC share or USB drive mount).  Functions that operate on remote
-- paths should accept 'RemotePath' instead of bare 'FilePath' so that callers
-- cannot accidentally pass them to @System.Directory@ functions, which break
-- on UNC paths under Windows.  Use 'unRemotePath' to unwrap when passing to
-- 'Bit.Platform' functions (which handle UNC internally).
module Bit.Path
    ( RemotePath(..)
    ) where

-- | A filesystem path pointing to a remote location.
-- Unwrap with 'unRemotePath' only when calling through 'Bit.Platform'.
newtype RemotePath = RemotePath { unRemotePath :: FilePath }
    deriving (Show, Eq)
