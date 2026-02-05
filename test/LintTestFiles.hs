import Test.Tasty
import Test.Tasty.HUnit
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.Monad (filterM, forM)
import Data.Char (toLower)
import Data.List (isInfixOf)

main :: IO ()
main = do
    -- Discover all .test files under test/cli/
    testFiles <- findTestFiles "test/cli"
    tests <- mapM createTestForFile testFiles
    defaultMain $ testGroup "Lint Test Files" tests

-- | Dangerous patterns that must not appear in test files
dangerousPatterns :: [(String, String, String)]
dangerousPatterns =
    [ ("%CD%", 
       "Windows expands %CD% before the command chain executes. If the preceding `cd` fails, commands run in the main repo directory.",
       "Use relative paths (e.g., ..\\remote_mirror) instead.")
    , ("%~dp0",
       "Batch script directory variable expands before command execution, risking sandbox escape.",
       "Use relative paths instead.")
    , ("%USERPROFILE%",
       "Could resolve to real user directories outside the test sandbox.",
       "Use relative paths or test-specific directories instead.")
    , ("%APPDATA%",
       "Could resolve to real user directories outside the test sandbox.",
       "Use relative paths or test-specific directories instead.")
    , ("%HOMEDRIVE%",
       "Could resolve to real user directories outside the test sandbox.",
       "Use relative paths or test-specific directories instead.")
    , ("%HOMEPATH%",
       "Could resolve to real user directories outside the test sandbox.",
       "Use relative paths or test-specific directories instead.")
    ]

-- | Recursively find all .test files in a directory
findTestFiles :: FilePath -> IO [FilePath]
findTestFiles dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then return []
        else do
            entries <- listDirectory dir
            let fullPaths = map (dir </>) entries
            files <- filterM (\p -> do
                isDir <- doesDirectoryExist p
                return $ not isDir && takeExtension p == ".test"
                ) fullPaths
            dirs <- filterM doesDirectoryExist fullPaths
            subFiles <- mapM findTestFiles dirs
            return $ files ++ concat subFiles

-- | Create a test case for a single test file
createTestForFile :: FilePath -> IO TestTree
createTestForFile path = do
    content <- readFile path
    let violations = scanForViolations path content
    return $ testCase path $ do
        case violations of
            [] -> return ()  -- No violations, test passes
            (v:_) -> assertFailure v  -- Report first violation

-- | Scan a file for dangerous patterns and return violation messages
scanForViolations :: FilePath -> String -> [String]
scanForViolations path content =
    let linesWithNumbers = zip [1..] (lines content)
        checkLine (lineNum, lineText) =
            [ formatViolation path lineNum lineText pattern reason fix
            | (pattern, reason, fix) <- dangerousPatterns
            , containsPattern pattern lineText
            ]
    in concatMap checkLine linesWithNumbers

-- | Case-insensitive pattern matching
containsPattern :: String -> String -> Bool
containsPattern pattern text =
    let lowerPattern = map toLower pattern
        lowerText = map toLower text
    in lowerPattern `isInfixOf` lowerText

-- | Format a violation message
formatViolation :: FilePath -> Int -> String -> String -> String -> String -> String
formatViolation path lineNum lineText pattern reason fix =
    unlines
        [ ""
        , "DANGEROUS PATTERN in " ++ path ++ ":" ++ show lineNum
        , "  Found: " ++ pattern
        , "  Line:  " ++ lineText
        , ""
        , "  Why dangerous: " ++ reason
        , "  Fix: " ++ fix
        , ""
        ]
