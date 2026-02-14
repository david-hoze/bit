{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Platform-safe wrappers for directory/file operations.
--
-- On Windows, GHC's System.Directory prepends @\\\\?\\UNC\\@ to UNC paths
-- for long-path support. Virtual UNC providers (RDP tsclient, WebDAV,
-- MobaXterm) reject this prefix.  For UNC paths we call Win32 directly,
-- which passes the path as-is.  Local paths still go through
-- System.Directory to keep long-path support (>260 chars).
module Bit.IO.Platform
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getFileSize
    , copyFile
    , removeFile
    , listDirectory
    , removeDirectoryRecursive
    , isUncPath
    ) where

#ifdef mingw32_HOST_OS

import qualified System.Directory as Dir
import System.FilePath (splitDirectories, joinPath, (</>))
import Control.Exception (try, IOException, bracket)
import qualified System.Win32.File as Win32
import Data.Bits ((.&.))
import Control.Monad (unless, forM_)
import Data.List (uncons)

-- | A path is UNC if it starts with @\\\\@ or @//@.
isUncPath :: FilePath -> Bool
isUncPath ('\\':'\\':_) = True
isUncPath ('/':'/':_)   = True
isUncPath _             = False

-- | Check file attributes without the @\\\\?\\@ prefix.
-- Returns Nothing when the path does not exist.
getAttrs :: FilePath -> IO (Maybe Win32.FileAttributeOrFlag)
getAttrs p = do
    r <- try (Win32.getFileAttributes p)
    pure $ case r of
        Right a -> Just a
        Left (_ :: IOException) -> Nothing

isDirectory :: Win32.FileAttributeOrFlag -> Bool
isDirectory a = a .&. Win32.fILE_ATTRIBUTE_DIRECTORY /= 0

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist p
    | isUncPath p = maybe False isDirectory <$> getAttrs p
    | otherwise   = Dir.doesDirectoryExist p

doesFileExist :: FilePath -> IO Bool
doesFileExist p
    | isUncPath p = maybe False (not . isDirectory) <$> getAttrs p
    | otherwise   = Dir.doesFileExist p

-- | Get file size without the @\\\\?\\@ prefix.
-- Uses Win32 getFileAttributesExStandard for UNC paths.
getFileSize :: FilePath -> IO Integer
getFileSize p
    | isUncPath p = fromIntegral . Win32.fadFileSize <$> Win32.getFileAttributesExStandard p
    | otherwise   = Dir.getFileSize p

-- | Recursive directory creation for UNC paths.
-- Stops at the UNC root (\\\\server\\share) — cannot create above that.
-- Filters empty components to handle trailing separators safely.
createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing recursive p
    | not (isUncPath p) = Dir.createDirectoryIfMissing recursive p
    | not recursive     = Win32.createDirectory p Nothing
    | otherwise         = do
        let parts = filter (not . null) $ splitDirectories p
            -- UNC root is \\server\share (first 2 components); skip those
            ancestors = case uncons parts of
                Nothing -> []
                Just (first, rest) -> drop 2 $ tail $
                    scanl (\acc d -> joinPath [acc, d]) first rest
        mapM_ createIfMissing ancestors
  where
    createIfMissing dir = do
        exists <- doesDirectoryExist dir
        unless exists $ Win32.createDirectory dir Nothing

copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst
    | isUncPath src || isUncPath dst = Win32.copyFile src dst False
    | otherwise                      = Dir.copyFile src dst

removeFile :: FilePath -> IO ()
removeFile p
    | isUncPath p = Win32.deleteFile p
    | otherwise   = Dir.removeFile p

listDirectory :: FilePath -> IO [FilePath]
listDirectory p
    | isUncPath p = listDirectoryWin32 p
    | otherwise   = Dir.listDirectory p

-- | List directory contents using Win32 FindFirstFile/FindNextFile.
listDirectoryWin32 :: FilePath -> IO [FilePath]
listDirectoryWin32 dir =
    bracket (Win32.findFirstFile (dir </> "*")) (Win32.findClose . fst) $ \(h, fd) -> do
        firstName <- Win32.getFindDataFileName fd
        let acc0 = [firstName | notDot firstName]
        collect h fd acc0
  where
    collect h fd acc = do
        more <- Win32.findNextFile h fd
        if more
            then do
                name <- Win32.getFindDataFileName fd
                collect h fd (if notDot name then name : acc else acc)
            else pure (reverse acc)
    notDot "."  = False
    notDot ".." = False
    notDot _    = True

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive p
    | isUncPath p = go p
    | otherwise   = Dir.removeDirectoryRecursive p
  where
    go dir = do
        entries <- listDirectory dir
        forM_ entries $ \name -> do
            let full = dir </> name
            isD <- doesDirectoryExist full
            if isD then go full else removeFile full
        Win32.removeDirectory dir

#else
-- Non-Windows: delegate everything to System.Directory

import qualified System.Directory as Dir

isUncPath :: FilePath -> Bool
isUncPath _ = False

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist = Dir.doesDirectoryExist

doesFileExist :: FilePath -> IO Bool
doesFileExist = Dir.doesFileExist

getFileSize :: FilePath -> IO Integer
getFileSize = Dir.getFileSize

createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing = Dir.createDirectoryIfMissing

copyFile :: FilePath -> FilePath -> IO ()
copyFile = Dir.copyFile

removeFile :: FilePath -> IO ()
removeFile = Dir.removeFile

listDirectory :: FilePath -> IO [FilePath]
listDirectory = Dir.listDirectory

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive = Dir.removeDirectoryRecursive

#endif
