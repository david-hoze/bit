{-# LANGUAGE OverloadedStrings #-}

-- | Device-identity-based remote resolution for filesystem remotes.
-- Cloud remotes (rclone) use URL-based identity; filesystem remotes use
-- UUID + hardware serial (physical) or UUID only (network).
module Bit.Device.Identity
  ( -- Types
    StorageType(..)
  , DeviceInfo(..)
  , RemotePathType(..)
  , RemoteTarget(..)
  , ResolveResult(..)
  , RemoteType(..)
    -- Classification
  , classifyRemotePath
  , getRcloneRemotes
    -- Volume ops
  , getVolumeRoot
  , getRelativePath
  , detectStorageType
  , getHardwareSerial
  , getVolumeLabel
  , isFixedDrive
    -- .bit-store
  , readBitStore
  , writeBitStore
    -- Device/remote files
  , readDeviceFile
  , writeDeviceFile
  , readRemoteFile
  , readRemoteType
  , writeRemoteFile
  , listDeviceNames
  , findDeviceByUuid
    -- Resolution
  , resolveRemoteTarget
  , parseRemoteTarget
  , generateStoreUuid
    -- Predicates
  , isFilesystemTarget
  , isFilesystemType
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf, intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad (when, filterM, join)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import qualified System.Directory as Dir
import System.FilePath ((</>), pathSeparator, takeDrive)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import qualified System.Info as Info
import Data.UUID (UUID, toString, fromString)
import Data.UUID.V4 (nextRandom)
-- Strict IO imports to avoid Windows file locking issues
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Bit.IO.AtomicWrite (atomicWriteFile)

-- ---------------------------------------------------------------------------
-- Bitlink resolution
-- ---------------------------------------------------------------------------

-- | Resolve the .bit root directory from a repo root path.
-- Follows bitlink files (e.g. "bitdir: /abs/path") for separated git dirs.
resolveBitRoot :: FilePath -> IO FilePath
resolveBitRoot repoRoot = do
    let dotBit = repoRoot </> ".bit"
    isDir <- Dir.doesDirectoryExist dotBit
    if isDir then pure dotBit
    else do
        isFile <- Dir.doesFileExist dotBit
        if isFile then do
            bs <- BS.readFile dotBit
            let content = either (const "") T.unpack (decodeUtf8' bs)
            case lines content of
                (firstLine:_) -> pure (drop 8 (filter (/= '\r') firstLine))
                [] -> pure dotBit
        else pure dotBit

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

-- | True for targets that resolve to a local filesystem path (device or direct path).
isFilesystemTarget :: RemoteTarget -> Bool
isFilesystemTarget (TargetDevice _ _) = True
isFilesystemTarget (TargetLocalPath _) = True
isFilesystemTarget (TargetCloud _) = False

data ResolveResult
  = Resolved FilePath     -- Runtime path (e.g. E:\Backup)
  | NotConnected String   -- Device not found
  deriving (Show, Eq)

-- | Classification of a remote by transport type.
data RemoteType = RemoteFilesystem | RemoteDevice | RemoteCloud
  deriving (Show, Eq)

-- | True for types that resolve to a local filesystem path.
isFilesystemType :: RemoteType -> Bool
isFilesystemType RemoteCloud = False
isFilesystemType _           = True

-- ---------------------------------------------------------------------------
-- Classification: cloud vs filesystem
-- ---------------------------------------------------------------------------

-- | Check if path is a cloud rclone remote or a filesystem path.
-- Cloud: "remotename:path" where remotename is in rclone listremotes.
classifyRemotePath :: String -> IO RemotePathType
classifyRemotePath path = do
  rcloneRemotes <- getRcloneRemotes
  case break (== ':') path of
    (prefix, _:_rest) | not (null prefix) -> do
      let prefixNorm = dropWhile (== ':') prefix
      if prefixNorm `elem` rcloneRemotes
        then pure (CloudRemote path)
        else pure (FilesystemPath path)
    _ -> pure (FilesystemPath path)

-- | Get list of configured rclone remote names (without trailing colon)
getRcloneRemotes :: IO [String]
getRcloneRemotes = do
  (code, out, _) <- readProcessWithExitCode "rclone" ["listremotes"] ""
  pure $ case code of
    ExitSuccess ->
      [ takeWhile (/= ':') (takeWhile (/= '\n') line)
      | line <- lines out
      , not (null (trimLine line))
      ]
    _ -> []
  where trimLine = dropWhileEnd (== ' ') . dropWhile (== ' ')

-- ---------------------------------------------------------------------------
-- Volume operations (platform-specific)
-- ---------------------------------------------------------------------------

-- | Get the volume root for a path (e.g. D:\Backup -> D:\, \\server\share\foo -> \\server\share\)
getVolumeRoot :: FilePath -> IO FilePath
getVolumeRoot path = do
  absPath <- Dir.makeAbsolute path
  if isWindows then pure (winVolumeRoot absPath)
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
  pure $ case code of
    ExitSuccess | not (null (trim out)) -> trim out
    _ -> path
  where
    shellEscape s = "'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) s ++ "'"

addTrailingSep :: FilePath -> FilePath
addTrailingSep p = case reverse p of
  (c:_) | c == pathSeparator -> p
  _ -> p ++ [pathSeparator]

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
  case drive of
    [] -> pure Physical  -- UNC: treat as network
    (d:_) -> do
      (_code, _out, _) <- readProcessWithExitCode "powershell" ["-NoProfile", "-Command",
        "try { (Get-PSDrive -Name " ++ [d] ++ " -ErrorAction SilentlyContinue).Root } catch { '' }"] ""
      (code2, out2, _) <- readProcessWithExitCode "powershell" ["-NoProfile", "-Command",
        "[int]([System.IO.DriveInfo]::new('" ++ drive ++ "').DriveType)"] ""
      pure $ case code2 of
        ExitSuccess -> case trim out2 of
          "4" -> Network  -- DriveType.Network
          _   -> Physical
        _ -> Physical

detectStorageTypeLinux :: FilePath -> IO StorageType
detectStorageTypeLinux _ = do
  -- Read /proc/mounts and check mount type for the path's mount point
  (code, out, _) <- readProcessWithExitCode "findmnt" ["-n", "-o", "FSTYPE", "-T", "/"] ""
  pure $ case code of
    ExitSuccess ->
      let fstype = trim out
      in if fstype `elem` ["nfs", "nfs4", "cifs", "smb", "smbfs", "sshfs"]
            then Network else Physical
    _ -> Physical

-- | Is the volume a fixed (non-removable, non-network) drive?
-- Fixed drives use RemoteFilesystem; removable/network use RemoteDevice.
isFixedDrive :: FilePath -> IO Bool
isFixedDrive volumeRoot
  | isWindows = isFixedDriveWindows volumeRoot
  | otherwise = (== Physical) <$> detectStorageType volumeRoot

isFixedDriveWindows :: FilePath -> IO Bool
isFixedDriveWindows volRoot = do
  let drive = take 2 (filter (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ":") volRoot)
  case drive of
    [] -> pure False  -- UNC: treat as network
    _ -> do
      (code, out, _) <- readProcessWithExitCode "powershell" ["-NoProfile", "-Command",
        "[int]([System.IO.DriveInfo]::new('" ++ drive ++ "').DriveType)"] ""
      pure $ case code of
        ExitSuccess -> trim out == "3"  -- DriveType.Fixed == 3
        _ -> True  -- Default to fixed if detection fails

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Get hardware serial for a physical volume
getHardwareSerial :: FilePath -> IO (Maybe String)
getHardwareSerial volumeRoot
  | isWindows = getHardwareSerialWindows volumeRoot
  | otherwise = getHardwareSerialLinux volumeRoot

getHardwareSerialWindows :: FilePath -> IO (Maybe String)
getHardwareSerialWindows volRoot = do
  let drive = take 1 (filter (`elem` ['A'..'Z'] ++ ['a'..'z']) volRoot)
  if null drive then pure Nothing
  else do
    -- Map partition to physical disk via partition number, then get disk serial
    (code, out, _) <- readProcessWithExitCode "wmic" ["diskdrive", "get", "SerialNumber,Index"] ""
    case code of
      ExitSuccess -> do
        (_code2, _out2, _) <- readProcessWithExitCode "wmic" ["path", "win32_logicaldisk", "where", "DeviceID='" ++ drive ++ ":\\'", "get", "VolumeSerialNumber"] ""
        -- VolumeSerialNumber is the FAT/NTFS serial, not disk serial. Use diskdrive.
        let lines' = filter (not . null . trim) (lines out)
            parseSerial = case lines' of
              _:rest -> listToMaybe [ trim (drop 12 l) | l <- rest, length l > 12 ]
              _      -> Nothing
        pure parseSerial
      _ -> pure Nothing

getHardwareSerialLinux :: FilePath -> IO (Maybe String)
getHardwareSerialLinux _ = do
  (code, out, _) <- readProcessWithExitCode "sh" ["-c", "lsblk -o SERIAL,MOUNTPOINT -n 2>/dev/null | head -20"] ""
  pure $ case code of
    ExitSuccess -> listToMaybe [ trim (takeWhile (/= ' ') l) | l <- lines out, "/" `isPrefixOf` (drop 20 l) ]
    _ -> Nothing

-- | Get volume label for device name suggestion
getVolumeLabel :: FilePath -> IO (Maybe String)
getVolumeLabel volumeRoot
  | isWindows = getVolumeLabelWindows volumeRoot
  | otherwise = getVolumeLabelLinux volumeRoot

getVolumeLabelWindows :: FilePath -> IO (Maybe String)
getVolumeLabelWindows volRoot = do
  let drive = take 2 (filter (`elem` ['A'..'Z'] ++ ['a'..'z'] ++ ":\\") volRoot)
  if length drive < 2 then pure Nothing
  else do
    -- vol is a cmd built-in, not an executable; run via cmd /c
    (code, out, _) <- readProcessWithExitCode "cmd" ["/c", "vol", drive] ""
    pure $ case code of
      ExitSuccess ->
        let lines' = lines out
            volLine = listToMaybe [ l | l <- lines', "Volume" `isPrefixOf` l ]
        in case volLine of
          Just l -> let after = dropWhile (/= ' ') (drop 6 l) in Just (trim after)
          _      -> Nothing
      _ -> Nothing

getVolumeLabelLinux :: FilePath -> IO (Maybe String)
getVolumeLabelLinux _ = do
  (code, out, _) <- readProcessWithExitCode "sh" ["-c", "lsblk -o LABEL,MOUNTPOINT -n 2>/dev/null | head -5"] ""
  pure $ case code of
    ExitSuccess -> listToMaybe [ trim (takeWhile (/= ' ') l) | l <- lines out, not (null (trim l)) ]
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- .bit-store (on device at volume root)
-- ---------------------------------------------------------------------------

bitStoreFileName :: FilePath
bitStoreFileName = ".bit-store"

readBitStore :: FilePath -> IO (Maybe UUID)
readBitStore volumeRoot = do
  let storePath = volumeRoot </> bitStoreFileName
  exists <- Dir.doesFileExist storePath
  if not exists then pure Nothing
  else do
    -- Use strict ByteString reading to avoid Windows file locking issues
    bs <- BS.readFile storePath
    let content = either (const "") T.unpack (decodeUtf8' bs)
    pure (parseBitStoreUuid content)

parseBitStoreUuid :: String -> Maybe UUID
parseBitStoreUuid content =
  join (listToMaybe [ fromString (trim (drop 5 line)) | line <- lines content, "uuid:" `isPrefixOf` line ])

writeBitStore :: FilePath -> UUID -> IO ()
writeBitStore volumeRoot u = do
  now <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
  let content = unlines
        [ "uuid: " ++ toString u
        , "created: " ++ ts
        ]
  let storePath = volumeRoot </> bitStoreFileName
  -- Use atomic write for crash safety and Windows compatibility
  atomicWriteFile storePath (encodeUtf8 (T.pack content))
  when isWindows $ setHidden storePath

setHidden :: FilePath -> IO ()
setHidden path = do
  (_, _, _) <- readProcessWithExitCode "attrib" ["+H", path] ""
  pure ()

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
  bitRoot <- resolveBitRoot repoRoot
  let path = bitRoot </> "devices" </> deviceName
  exists <- Dir.doesFileExist path
  if not exists then pure Nothing
  else do
    -- Use strict ByteString reading to avoid Windows file locking issues
    bs <- BS.readFile path
    let content = either (const "") T.unpack (decodeUtf8' bs)
    pure (parseDeviceFile content)

writeDeviceFile :: FilePath -> String -> DeviceInfo -> IO ()
writeDeviceFile repoRoot deviceName info = do
  bitRoot <- resolveBitRoot repoRoot
  Dir.createDirectoryIfMissing True (bitRoot </> "devices")
  let path = bitRoot </> "devices" </> deviceName
  let body = unlines $
        [ "uuid: " ++ toString (deviceUuid info)
        , "type: " ++ (case deviceType info of Physical -> "physical"; Network -> "network")
        ] ++ [ "hardware_serial: " ++ s | Just s <- [hardwareSerial info] ]
  -- Use atomic write for crash safety and Windows compatibility
  atomicWriteFile path (encodeUtf8 (T.pack body))

listDeviceNames :: FilePath -> IO [String]
listDeviceNames repoRoot = do
  bitRoot <- resolveBitRoot repoRoot
  let dir = bitRoot </> "devices"
  exists <- Dir.doesDirectoryExist dir
  if not exists then pure []
  else filter (not . null) <$> Dir.listDirectory dir

findDeviceByUuid :: FilePath -> UUID -> IO (Maybe String)
findDeviceByUuid repoRoot targetUuid = do
  names <- listDeviceNames repoRoot
  foldr go (pure Nothing) names
  where
    go name acc = do
      mInfo <- readDeviceFile repoRoot name
      case mInfo of
        Just info | deviceUuid info == targetUuid -> pure (Just name)
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
  bitRoot <- resolveBitRoot repoRoot
  let path = bitRoot </> "remotes" </> remoteName
  exists <- Dir.doesFileExist path
  if not exists then pure Nothing
  else do
    -- Use strict ByteString reading to avoid Windows file locking issues
    bs <- BS.readFile path
    let content = either (const "") T.unpack (decodeUtf8' bs)
    let raw = parseRemoteFile content
    case raw of
      Nothing -> pure Nothing
      Just (ParsedLocal p) -> pure (Just (TargetLocalPath p))
      Just (ParsedCloud url) -> pure (Just (TargetCloud url))
      Just (ParsedDevice device relPath) -> do
        mDev <- readDeviceFile repoRoot device
        pure $ Just $ maybe (TargetCloud (device ++ ":" ++ relPath))
          (const $ TargetDevice device relPath) mDev

-- | Read the remote type from .bit/remotes/<name>.
-- New format: "type: filesystem|device|cloud". Old format: inferred from "target:" line.
readRemoteType :: FilePath -> String -> IO (Maybe RemoteType)
readRemoteType repoRoot name = do
  bitRoot <- resolveBitRoot repoRoot
  let path = bitRoot </> "remotes" </> name
  exists <- Dir.doesFileExist path
  if not exists then pure Nothing
  else do
    bs <- BS.readFile path
    let content = either (const "") T.unpack (decodeUtf8' bs)
        ls = lines content
        getVal prefix = listToMaybe [ trim (drop (length prefix) l) | l <- ls, prefix `isPrefixOf` l ]
    case getVal "type: " of
      Just "filesystem" -> pure (Just RemoteFilesystem)
      Just "device"     -> pure (Just RemoteDevice)
      Just "cloud"      -> pure (Just RemoteCloud)
      _ -> -- Old format: infer from target line
        case getVal "target: " of
          Just t | "local:" `isPrefixOf` t -> pure (Just RemoteFilesystem)
          Just t -> case break (== ':') t of
            (dev, ':':_) | not (null dev) -> do
              mDev <- readDeviceFile repoRoot dev
              pure $ Just $ maybe RemoteCloud (const RemoteDevice) mDev
            _ -> pure (Just RemoteCloud)
          Nothing -> pure Nothing

writeRemoteFile :: FilePath -> String -> RemoteType -> Maybe String -> IO ()
writeRemoteFile repoRoot name remoteType mTarget = do
  bitRoot <- resolveBitRoot repoRoot
  Dir.createDirectoryIfMissing True (bitRoot </> "remotes")
  let path = bitRoot </> "remotes" </> name
  let content = case remoteType of
        RemoteFilesystem -> "type: filesystem"
        RemoteCloud      -> "type: cloud\ntarget: " ++ fromMaybe "" mTarget
        RemoteDevice     -> "type: device\ntarget: " ++ fromMaybe "" mTarget
  -- Use atomic write for crash safety and Windows compatibility
  atomicWriteFile path (encodeUtf8 (T.pack content))

-- | Parse a target string (e.g. "black_usb:Backup" or "gdrive:Projects/foo")
parseRemoteTarget :: String -> RemoteTarget
parseRemoteTarget s = case break (== ':') s of
  (prefix, _:rest) | not (null prefix) -> TargetDevice prefix (trim rest)
  _ -> TargetCloud s

-- ---------------------------------------------------------------------------
-- Resolution: device:path -> runtime path
-- ---------------------------------------------------------------------------

resolveRemoteTarget :: FilePath -> RemoteTarget -> IO ResolveResult
resolveRemoteTarget _repoRoot (TargetCloud url) = pure (Resolved url)
resolveRemoteTarget _repoRoot (TargetLocalPath p) = pure (Resolved p)
resolveRemoteTarget repoRoot (TargetDevice deviceName relPath) = do
  mInfo <- readDeviceFile repoRoot deviceName
  maybe (pure (NotConnected ("Device '" ++ deviceName ++ "' not found in .rgit/devices/")))
    (\info -> do
      mMount <- resolveDevice info
      maybe (pure (NotConnected ("Device '" ++ deviceName ++ "' is not connected")))
        (\mountRoot -> pure (Resolved (mountRoot </> relPath)))
        mMount
    ) mInfo

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
  pure (listToMaybe found)

resolveNetworkDevice :: DeviceInfo -> IO (Maybe FilePath)
resolveNetworkDevice info = do
  mounts <- getNetworkMountPoints
  found <- filterM (checkMountForDevice info) mounts
  pure (listToMaybe found)

checkMountForDevice :: DeviceInfo -> FilePath -> IO Bool
checkMountForDevice info mountRoot = do
  mStoreUuid <- readBitStore mountRoot
  pure $ mStoreUuid == Just (deviceUuid info)

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
  pure $ case code of
    ExitSuccess -> filter (not . null) (lines out)
    _ -> ["/"]

getWindowsPhysicalMounts :: IO [FilePath]
getWindowsPhysicalMounts = do
  (code, out, _) <- readProcessWithExitCode "wmic" ["logicaldisk", "where", "DriveType=2 or DriveType=3", "get", "DeviceID"] ""
  pure $ case code of
    ExitSuccess ->
      [ trim l ++ "\\"
      | l <- lines out
      , let t = trim l
      , length t == 2
      , case reverse t of
          (':':_) -> True
          _ -> False
      ]
    _ -> []

getWindowsNetworkMounts :: IO [FilePath]
getWindowsNetworkMounts = do
  (code, out, _) <- readProcessWithExitCode "wmic" ["logicaldisk", "where", "DriveType=4", "get", "DeviceID"] ""
  pure $ case code of
    ExitSuccess ->
      [ trim l ++ "\\"
      | l <- lines out
      , let t = trim l
      , length t == 2
      , case reverse t of
          (':':_) -> True
          _ -> False
      ]
    _ -> []