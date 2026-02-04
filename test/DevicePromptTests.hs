import Test.Tasty
import Test.Tasty.HUnit
import qualified Bit.DevicePrompt as DP

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "DevicePrompt"
  [ testGroup "acquireDeviceName"
    [ testCase "accepts a simple device name" $ do
        name <- DP.acquireDeviceName (DP.Interactive (pure "my_laptop")) (Just "vol") (pure . const False)
        name @?= "my_laptop"
    , testCase "sanitizes device name with spaces" $ do
        name <- DP.acquireDeviceName (DP.Interactive (pure "Local Disk")) (Just "vol") (pure . const False)
        name @?= "Local_Disk"
    , testCase "strips invalid characters from device name" $ do
        name <- DP.acquireDeviceName (DP.Interactive (pure "my<>device/name")) (Just "vol") (pure . const False)
        name @?= "mydevicename"
    , testCase "falls back to volume label on empty input" $ do
        name <- DP.acquireDeviceName (DP.Interactive (pure "")) (Just "MY_PASSPORT") (pure . const False)
        name @?= "MY_PASSPORT"
    , testCase "falls back to device on empty input when no label" $ do
        name <- DP.acquireDeviceName (DP.Interactive (pure "")) Nothing (pure . const False)
        name @?= "device"
    , testCase "treats whitespace-only input as empty" $ do
        name <- DP.acquireDeviceName (DP.Interactive (pure "   ")) (Just "default") (pure . const False)
        name @?= "default"
    , testCase "sanitizes volume label with spaces in fallback" $ do
        name <- DP.acquireDeviceName (DP.Interactive (pure "")) (Just "Local Disk") (pure . const False)
        name @?= "Local_Disk"
    , testCase "uses volume label when non-interactive" $ do
        name <- DP.acquireDeviceName DP.NonInteractive (Just "OFFICE_SHARE") (pure . const False)
        name @?= "OFFICE_SHARE"
    , testCase "uses device when non-interactive and no label" $ do
        name <- DP.acquireDeviceName DP.NonInteractive Nothing (pure . const False)
        name @?= "device"
    , testCase "sanitizes volume label in non-interactive mode" $ do
        name <- DP.acquireDeviceName DP.NonInteractive (Just "My Passport") (pure . const False)
        name @?= "My_Passport"
    ]
  , testGroup "sanitizeDeviceName"
    [ testCase "replaces spaces with underscores" $
        DP.sanitizeDeviceName "Local Disk" @?= "Local_Disk"
    , testCase "strips invalid characters" $
        DP.sanitizeDeviceName "a/b<>c" @?= "abc"
    , testCase "returns device for empty after cleaning" $
        DP.sanitizeDeviceName "<>//" @?= "device"
    , testCase "preserves valid name" $
        DP.sanitizeDeviceName "my_device-123" @?= "my_device-123"
    ]
  , testGroup "isValidDeviceName"
    [ testCase "accepts alphanumeric with underscores and hyphens" $
        DP.isValidDeviceName "my_device-123" @?= True
    , testCase "rejects empty" $
        DP.isValidDeviceName "" @?= False
    , testCase "rejects spaces" $
        DP.isValidDeviceName "my device" @?= False
    , testCase "rejects special chars" $
        DP.isValidDeviceName "my<>device" @?= False
    ]
  ]
