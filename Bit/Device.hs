{-# LANGUAGE OverloadedStrings #-}

-- | Device-identity-based remote resolution for filesystem remotes.
-- Cloud remotes (rclone) use URL-based identity; filesystem remotes use
-- UUID + hardware serial (physical) or UUID only (network).
module Bit.Device
  ( -- Types
    StorageType(..)
  , DeviceInfo(..)
  , RemotePathType(..)
  , RemoteTarget(..)
  , ResolveResult(..)
    -- Classification
  , classifyRemotePath
  , getRcloneRemotes
    -- Volume ops
  , getVolumeRoot
  , getRelativePath
  , detectStorageType
  , getHardwareSerial
  , getVolumeLabel
    -- .bit-store
  , readBitStore
  , writeBitStore
    -- Device/remote files
  , readDeviceFile
  , writeDeviceFile
  , readRemoteFile
  , writeRemoteFile
  , listDeviceNames
  , findDeviceByUuid
    -- Resolution
  , resolveRemoteTarget
  , parseRemoteTarget
  , generateStoreUuid
  ) where

import Data.List (isPrefixOf, intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad (when, filterM)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import qualified System.Directory as Dir
import System.FilePath ((</>), pathSeparator, takeDrive, dropDrive)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import qualified System.Info as Info
import Data.UUID (UUID, toString, fromString)
import Data.UUID.V4 (nextRandom)
import Internal.Config (bitDevicesDir, bitRemotesDir)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data StorageType = Physical | Network
  deriving (Show, Eq)

data DeviceInfo = DeviceInfo
  { deviceUuid     :: UUID
  , deviceType     :: StorageType
  , hardwareSerial :: Maybe String
  }
  deriving (Show, Eq)

-- | Result of classifying a remote path
data RemotePathType
  = CloudRemote String       -- Pass to rclone as-is (e.g. "gdrive:Projects/foo")
  | FilesystemPath FilePath  -- Enter device flow
  deriving (Show, Eq)

-- | Parsed remote target from .rgit/remotes/<name>
data RemoteTarget
  = TargetCloud String           -- Cloud URL for rclone
  | TargetDevice String FilePath -- device_name : relative_path
  | TargetLocalPath FilePath     -- Legacy: path when .bit-store at volume root cannot be created
  deriving (Show, Eq)

data ResolveResult
  = Resolved FilePath     -- Runtime path (e.g. E:\Backup)
  | NotConnected String   -- Device not found
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Classification: cloud vs filesystem
-- ---------------------------------------------------------------------------

-- | Check if path is a cloud rclone remote or a filesystem path.
-- Cloud: "remotename:path" where remotename is in rclone listremotes.
classifyRemotePath :: String -> IO RemotePathType
classifyRemotePath path = do
  rcloneRemotes <- getRcloneRemotes
  case break (== ':') path of
    (prefix, _:rest) | not (null prefix) -> do
      let prefixNorm = dropWhile (== ':') prefix
      if prefixNorm `elem` rcloneRemotes
        then return (CloudRemote path)
        else return (FilesystemPath path)
    _ -> return (FilesystemPath path)

-- | Get list of configured rclone remote names (without trailing colon)
getRcloneRemotes :: IO [String]
getRcloneRemotes = do
  (code, out, _) <- readProcessWithExitCode "rclone" ["listremotes"] ""
  if code /= ExitSuccess then return []
  else return
    [ takeWhile (/= ':') (takeWhile (/= '\n') line)
    | line <- lines out
    , not (null (trim line))
    ]
  where trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- ---------------------------------------------------------------------------
-- Volume operations (platform-specific)
-- ---------------------------------------------------------------------------

-- | Get the volume root for a path (e.g. D:\Backup -> D:\, \\server\share\foo -> \\server\share\)
getVolumeRoot :: FilePath -> IO FilePath
getVolumeRoot path = do
  absPath <- Dir.makeAbsolute path
  if isWindows then return (winVolumeRoot absPath)
  else linuxVolumeRootIO absPath

isWindows :: Bool
isWindows = Info.os == "mingw32" || Info.os == "win32"

winVolumeRoot :: FilePath -> FilePath
winVolumeRoot p
  | "\\\\" `isPrefixOf` p || "//" `isPrefixOf` p =
      -- UNC path: \\server\share\path -> \\server\share\
      let sep = if pathSeparator == '\\' then '\\' else '/'
          parts = splitPathOnSep p
      in if length parts >= 3
         then intercalate [sep] (take 3 parts) ++ [sep]
         else p
  | otherwise =
      -- Drive letter: D:\path -> D:\
      let drive = takeDrive p
      in if null drive then p else addTrailingSep drive

-- | Get volume root on Linux using findmnt
linuxVolumeRootIO :: FilePath -> IO FilePath
linuxVolumeRootIO path = do
  (code, out, _) <- readProcessWithExitCode "sh" ["-c", "findmnt -n -o TARGET -T " ++ shellEscape path ++ " 2>/dev/null || echo " ++ shellEscape path] ""
  if code == ExitSuccess && not (null (trim out))
    then return (trim out)
    else return path
  where
    shellEscape s = "'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) s ++ "'"

addTrailingSep :: FilePath -> FilePath
addTrailingSep p = if not (null p) && last p == pathSeparator then p else p ++ [pathSeparator]

splitPathOnSep :: FilePath -> [String]
splitPathOnSep = splitOn (== pathSeparator)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn p s = case break p s of
  (chunk, [])     -> [chunk]
  (chunk, _:rest) -> chunk : splitOn p rest

-- | Get path relative to volume root
getRelativePath :: FilePath -> FilePath -> FilePath
getRelativePath volumeRoot fullPath = fromMaybe fullPath (stripPrefix' volumeRoot fullPath)

stripPrefix' :: FilePath -> FilePath -> Maybe FilePath
stripPrefix' prefix path =
  let norm = normalisePath
      p = norm prefix
      s = norm path
  in if p `isPrefixOf` s then Just (drop (length p) s) else Nothing

normalisePath :: FilePath -> FilePath
normalisePath = filter (/= '"') . map (\c -> if c == '/' then pathSeparator else c)

-- | Detect physical vs network storage
detectStorageType :: FilePath -> IO StorageType
detectStorageType volumeRoot
  | isWindows = detectStorageTypeWindows volumeRoot
  | otherwise = detectStorageTypeLinux volumeRoot

detectStorageTypeWindows :: FilePath -> IO StorageType
detectStorageTypeWindows volRoot = do
  let drive = take 2 (filter (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ":") volRoot)
  if null drive then return Physical  -- UNC: treat as network
  else do
    (code, out, _) <- readProcessWithExitCode "powershell" ["-NoProfile", "-Command",
      "try { (Get-PSDrive -Name " ++ [head drive] ++ " -ErrorAction SilentlyContinue).Root } catch { '' }"] ""
    (code2, out2, _) <- readProcessWithExitCode "powershell" ["-NoProfile", "-Command",
      "[int]([System.IO.DriveInfo]::new('" ++ drive ++ "').DriveType)"] ""
    if code2 == ExitSuccess
      then case trim out2 of
        "4" -> return Network  -- DriveType.Network
        _   -> return Physical
      else return Physical

detectStorageTypeLinux :: FilePath -> IO StorageType
detectStorageTypeLinux _ = do
  -- Read /proc/mounts and check mount type for the path's mount point
  (code, out, _) <- readProcessWithExitCode "findmnt" ["-n", "-o", "FSTYPE", "-T", "/"] ""
  if code == ExitSuccess
    then let fstype = trim out
         in return $ if fstype `elem` ["nfs", "nfs4", "cifs", "smb", "smbfs", "sshfs"]
                    then Network else Physical
    else return Physical

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- | Get hardware serial for a physical volume
getHardwareSerial :: FilePath -> IO (Maybe String)
getHardwareSerial volumeRoot
  | isWindows = getHardwareSerialWindows volumeRoot
  | otherwise = getHardwareSerialLinux volumeRoot

getHardwareSerialWindows :: FilePath -> IO (Maybe String)
getHardwareSerialWindows volRoot = do
  let drive = take 1 (filter (`elem` ['A'..'Z'] ++ ['a'..'z']) volRoot)
  if null drive then return Nothing
  else do
    -- Map partition to physical disk via partition number, then get disk serial
    (code, out, _) <- readProcessWithExitCode "wmic" ["diskdrive", "get", "SerialNumber,Index"] ""
    if code /= ExitSuccess then return Nothing
    else do
      (code2, out2, _) <- readProcessWithExitCode "wmic" ["path", "win32_logicaldisk", "where", "DeviceID='" ++ drive ++ ":\\'", "get", "VolumeSerialNumber"] ""
      -- VolumeSerialNumber is the FAT/NTFS serial, not disk serial. Use diskdrive.
      let lines' = filter (not . null . trim) (lines out)
          parseSerial = case lines' of
            _:rest -> listToMaybe [ trim (drop 12 l) | l <- rest, length l > 12 ]
            _      -> Nothing
      return (parseSerial)

getHardwareSerialLinux :: FilePath -> IO (Maybe String)
getHardwareSerialLinux _ = do
  (code, out, _) <- readProcessWithExitCode "sh" ["-c", "lsblk -o SERIAL,MOUNTPOINT -n 2>/dev/null | head -20"] ""
  if code == ExitSuccess
    then return (listToMaybe [ trim (takeWhile (/= ' ') l) | l <- lines out, "/" `isPrefixOf` (drop 20 l) ])
    else return Nothing

-- | Get volume label for device name suggestion
getVolumeLabel :: FilePath -> IO (Maybe String)
getVolumeLabel volumeRoot
  | isWindows = getVolumeLabelWindows volumeRoot
  | otherwise = getVolumeLabelLinux volumeRoot

getVolumeLabelWindows :: FilePath -> IO (Maybe String)
getVolumeLabelWindows volRoot = do
  let drive = take 2 (filter (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ":\\") volRoot)
  if length drive < 2 then return Nothing
  else do
    -- vol is a cmd built-in, not an executable; run via cmd /c
    (code, out, _) <- readProcessWithExitCode "cmd" ["/c", "vol", drive] ""
    if code /= ExitSuccess then return Nothing
    else
      let lines' = lines out
          volLine = listToMaybe [ l | l <- lines', "Volume" `isPrefixOf` l ]
      in return $ case volLine of
        Just l -> let after = dropWhile (/= ' ') (drop 6 l) in Just (trim after)
        _      -> Nothing

getVolumeLabelLinux :: FilePath -> IO (Maybe String)
getVolumeLabelLinux _ = do
  (code, out, _) <- readProcessWithExitCode "sh" ["-c", "lsblk -o LABEL,MOUNTPOINT -n 2>/dev/null | head -5"] ""
  if code == ExitSuccess
    then return (listToMaybe [ trim (takeWhile (/= ' ') l) | l <- lines out, not (null (trim l)) ])
    else return Nothing

-- ---------------------------------------------------------------------------
-- .bit-store (on device at volume root)
-- ---------------------------------------------------------------------------

bitStoreFileName :: FilePath
bitStoreFileName = ".bit-store"

readBitStore :: FilePath -> IO (Maybe UUID)
readBitStore volumeRoot = do
  let storePath = volumeRoot </> bitStoreFileName
  exists <- Dir.doesFileExist storePath
  if not exists then return Nothing
  else do
    content <- readFile storePath
    return (parseBitStoreUuid content)

parseBitStoreUuid :: String -> Maybe UUID
parseBitStoreUuid content =
  listToMaybe [ fromString (trim (drop 5 line)) | line <- lines content, "uuid:" `isPrefixOf` line ]
  >>= id  -- join: Maybe (Maybe UUID) -> Maybe UUID

writeBitStore :: FilePath -> UUID -> IO ()
writeBitStore volumeRoot u = do
  now <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  let content = unlines
        [ "uuid: " ++ toString u
        , "created: " ++ ts
        ]
  let storePath = volumeRoot </> bitStoreFileName
  writeFile storePath content
  when isWindows $ setHidden storePath

setHidden :: FilePath -> IO ()
setHidden path = do
  (_, _, _) <- readProcessWithExitCode "attrib" ["+H", path] ""
  return ()

generateStoreUuid :: IO UUID
generateStoreUuid = nextRandom

-- ---------------------------------------------------------------------------
-- Device files (.rgit/devices/<device_name>)
-- ---------------------------------------------------------------------------

parseDeviceFile :: String -> Maybe DeviceInfo
parseDeviceFile content =
  let ls = lines content
      getVal prefix = listToMaybe [ trim (drop (length prefix) l) | l <- ls, prefix `isPrefixOf` l ]
      muuid = getVal "uuid: " >>= fromString
      mtype = getVal "type: "
      mserial = getVal "hardware_serial: "
  in case (muuid, mtype) of
       (Just u, Just "physical") -> Just (DeviceInfo u Physical mserial)
       (Just u, Just "network")  -> Just (DeviceInfo u Network Nothing)
       _                         -> Nothing

readDeviceFile :: FilePath -> String -> IO (Maybe DeviceInfo)
readDeviceFile repoRoot deviceName = do
  let path = repoRoot </> bitDevicesDir </> deviceName
  exists <- Dir.doesFileExist path
  if not exists then return Nothing
  else parseDeviceFile <$> readFile path

writeDeviceFile :: FilePath -> String -> DeviceInfo -> IO ()
writeDeviceFile repoRoot deviceName info = do
  Dir.createDirectoryIfMissing True (repoRoot </> bitDevicesDir)
  let path = repoRoot </> bitDevicesDir </> deviceName
  let body = unlines $
        [ "uuid: " ++ toString (deviceUuid info)
        , "type: " ++ (case deviceType info of Physical -> "physical"; Network -> "network")
        ] ++ [ "hardware_serial: " ++ s | Just s <- [hardwareSerial info] ]
  writeFile path body

listDeviceNames :: FilePath -> IO [String]
listDeviceNames repoRoot = do
  let dir = repoRoot </> bitDevicesDir
  exists <- Dir.doesDirectoryExist dir
  if not exists then return []
  else filter (not . null) <$> Dir.listDirectory dir

findDeviceByUuid :: FilePath -> UUID -> IO (Maybe String)
findDeviceByUuid repoRoot targetUuid = do
  names <- listDeviceNames repoRoot
  foldr go (return Nothing) names
  where
    go name acc = do
      mInfo <- readDeviceFile repoRoot name
      case mInfo of
        Just info | deviceUuid info == targetUuid -> return (Just name)
        _ -> acc

-- ---------------------------------------------------------------------------
-- Remote files (.rgit/remotes/<remote_name>)
-- ---------------------------------------------------------------------------

data ParsedTarget = ParsedLocal FilePath | ParsedDevice String FilePath | ParsedCloud String

parseRemoteFile :: String -> Maybe ParsedTarget
parseRemoteFile content =
  let ls = lines content
      getVal prefix = listToMaybe [ trim (drop (length prefix) l) | l <- ls, prefix `isPrefixOf` l ]
  in getVal "target: " >>= parseTarget
  where
    parseTarget s
      | "local:" `isPrefixOf` s = Just (ParsedLocal (trim (drop 6 s)))
      | otherwise = case break (== ':') s of
          (device, ':' : path) | not (null device) -> Just (ParsedDevice device (trim path))
          _ -> Just (ParsedCloud s)

readRemoteFile :: FilePath -> String -> IO (Maybe RemoteTarget)
readRemoteFile repoRoot remoteName = do
  let path = repoRoot </> bitRemotesDir </> remoteName
  exists <- Dir.doesFileExist path
  if not exists then return Nothing
  else do
    raw <- parseRemoteFile <$> readFile path
    case raw of
      Nothing -> return Nothing
      Just (ParsedLocal p) -> return (Just (TargetLocalPath p))
      Just (ParsedCloud url) -> return (Just (TargetCloud url))
      Just (ParsedDevice device relPath) -> do
        mDev <- readDeviceFile repoRoot device
        return $ Just $ case mDev of
          Just _ -> TargetDevice device relPath
          Nothing -> TargetCloud (device ++ ":" ++ relPath)

writeRemoteFile :: FilePath -> String -> RemoteTarget -> IO ()
writeRemoteFile repoRoot remoteName target = do
  Dir.createDirectoryIfMissing True (repoRoot </> bitRemotesDir)
  let path = repoRoot </> bitRemotesDir </> remoteName
  let line = case target of
        TargetCloud url -> "target: " ++ url
        TargetDevice dev p -> "target: " ++ dev ++ ":" ++ p
        TargetLocalPath p -> "target: local:" ++ p
  writeFile path line

-- | Parse a target string (e.g. "black_usb:Backup" or "gdrive:Projects/foo")
parseRemoteTarget :: String -> RemoteTarget
parseRemoteTarget s = case break (== ':') s of
  (prefix, _:rest) | not (null prefix) -> TargetDevice prefix (trim rest)
  _ -> TargetCloud s

-- ---------------------------------------------------------------------------
-- Resolution: device:path -> runtime path
-- ---------------------------------------------------------------------------

resolveRemoteTarget :: FilePath -> RemoteTarget -> IO ResolveResult
resolveRemoteTarget _repoRoot (TargetCloud url) = return (Resolved url)
resolveRemoteTarget _repoRoot (TargetLocalPath p) = return (Resolved p)
resolveRemoteTarget repoRoot (TargetDevice deviceName relPath) = do
  mInfo <- readDeviceFile repoRoot deviceName
  case mInfo of
    Nothing -> return (NotConnected ("Device '" ++ deviceName ++ "' not found in .rgit/devices/"))
    Just info -> do
      mMount <- resolveDevice info
      case mMount of
        Nothing -> return (NotConnected ("Device '" ++ deviceName ++ "' is not connected"))
        Just mountRoot -> do
          let fullPath = mountRoot </> relPath
          return (Resolved fullPath)

-- | Search for a device and return its volume root if found
resolveDevice :: DeviceInfo -> IO (Maybe FilePath)
resolveDevice info =
  case deviceType info of
    Physical -> resolvePhysicalDevice info
    Network  -> resolveNetworkDevice info

resolvePhysicalDevice :: DeviceInfo -> IO (Maybe FilePath)
resolvePhysicalDevice info = do
  mounts <- getPhysicalMountPoints
  found <- filterM (checkMountForDevice info) mounts
  return (listToMaybe found)

resolveNetworkDevice :: DeviceInfo -> IO (Maybe FilePath)
resolveNetworkDevice info = do
  mounts <- getNetworkMountPoints
  found <- filterM (checkMountForDevice info) mounts
  return (listToMaybe found)

checkMountForDevice :: DeviceInfo -> FilePath -> IO Bool
checkMountForDevice info mountRoot = do
  mStoreUuid <- readBitStore mountRoot
  return $ mStoreUuid == Just (deviceUuid info)

getPhysicalMountPoints :: IO [FilePath]
getPhysicalMountPoints
  | isWindows = getWindowsPhysicalMounts
  | otherwise = getLinuxMountPoints (const False)  -- physical = not network

getNetworkMountPoints :: IO [FilePath]
getNetworkMountPoints
  | isWindows = getWindowsNetworkMounts
  | otherwise = getLinuxMountPoints (const True)  -- network only

getLinuxMountPoints :: (String -> Bool) -> IO [FilePath]
getLinuxMountPoints _typeFilter = do
  -- Parse /proc/mounts for mount points; full impl would filter by fstype
  (code, out, _) <- readProcessWithExitCode "sh" ["-c", "awk '{print $2}' /proc/mounts 2>/dev/null | sort -u"] ""
  if code /= ExitSuccess then return ["/"]
  else return (filter (not . null) (lines out))

getWindowsPhysicalMounts :: IO [FilePath]
getWindowsPhysicalMounts = do
  (code, out, _) <- readProcessWithExitCode "wmic" ["logicaldisk", "where", "DriveType=2 or DriveType=3", "get", "DeviceID"] ""
  if code /= ExitSuccess then return []
  else return
    [ trim l ++ "\\"
    | l <- lines out
    , let t = trim l
    , length t == 2
    , last t == ':'
    ]

getWindowsNetworkMounts :: IO [FilePath]
getWindowsNetworkMounts = do
  (code, out, _) <- readProcessWithExitCode "wmic" ["logicaldisk", "where", "DriveType=4", "get", "DeviceID"] ""
  if code /= ExitSuccess then return []
  else return
    [ trim l ++ "\\"
    | l <- lines out
    , let t = trim l
    , length t == 2
    , last t == ':'
    ]