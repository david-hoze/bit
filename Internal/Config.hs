module Internal.Config where

import System.FilePath ((</>))

-- | Logical bundle name (e.g. "fetched_remote", "bit"). NOT a file path.
newtype BundleName = BundleName String deriving (Show, Eq)

-- | Path relative to the git working directory (.bit/index/).
-- Used for git commands that run with -C .bit/index.
newtype GitRelPath = GitRelPath FilePath deriving (Show, Eq)

-- | Path relative to CWD. Used for direct filesystem operations (copyFile, doesFileExist, etc).
newtype CwdPath = CwdPath FilePath deriving (Show, Eq)

bitDir, bitTargetPath, bitIgnore, bitGitDir, bitIndexPath, bitDevicesDir, bitRemotesDir :: FilePath
bitDir           = ".bit"
bitTargetPath    = bitDir </> "target"
bitIgnore        = bitDir </> "ignore"
bitDevicesDir    = bitDir </> "devices"
bitRemotesDir    = bitDir </> "remotes"
bitIndexPath     = bitDir </> "index"
bitGitDir        = bitIndexPath </> ".git"

-- | The logical name of the fetched remote bundle
fetchedBundle :: BundleName
fetchedBundle = BundleName "fetched_remote"

-- | Legacy string name for gradual migration
fetchedBundleName :: FilePath
fetchedBundleName = "fetched_remote"

-- | Legacy CWD path for gradual migration
fetchedBundlePath :: FilePath
fetchedBundlePath = bitIndexPath </> ".git" </> (fetchedBundleName ++ ".bundle")

-- | Convert a bundle name to a path relative to git working directory (.bit/index/)
-- Use this for git commands that run with -C .bit/index
bundleGitRelPath :: BundleName -> GitRelPath
bundleGitRelPath (BundleName n) = GitRelPath (".git" </> (n ++ ".bundle"))

-- | Convert a bundle name to a path relative to CWD
-- Use this for filesystem operations (copyFile, doesFileExist, etc.)
bundleCwdPath :: BundleName -> CwdPath
bundleCwdPath (BundleName n) = CwdPath (bitIndexPath </> ".git" </> (n ++ ".bundle"))

-- | Unwrap CwdPath to FilePath
fromCwdPath :: CwdPath -> FilePath
fromCwdPath (CwdPath p) = p

-- | Unwrap GitRelPath to FilePath
fromGitRelPath :: GitRelPath -> FilePath
fromGitRelPath (GitRelPath p) = p
