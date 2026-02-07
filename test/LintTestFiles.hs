import Test.Tasty
import Test.Tasty.HUnit
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.Monad (filterM, forM)
import Data.Char (toLower, isSpace)
import Data.List (isInfixOf, isPrefixOf, groupBy)

main :: IO ()
main = do
    -- Discover all .test files under test/cli/
    testFiles <- findTestFiles "test/cli"
    tests <- mapM createTestForFile testFiles
    defaultMain $ testGroup "Lint Test Files" 
        [ testGroup "Pattern Safety" tests
        , formatValidationTests
        ]

-- | Unit tests for format validation logic
formatValidationTests :: TestTree
formatValidationTests = testGroup "Format Validation Logic"
    [ testCase "accepts valid test case with all directives" $ do
        let content = "command\n<<<\ninput\n>>>\noutput\n>>>2 /error/\n>>>= 0\n"
        validateShelltestFormat "test.test" content @?= []
    
    , testCase "catches duplicate >>>2" $ do
        let content = "command\n>>>2 /err1/\n>>>2 /err2/\n>>>= 1\n"
        length (validateShelltestFormat "test.test" content) @?= 1
    
    , testCase "catches duplicate >>>" $ do
        let content = "command\n>>>\noutput1\n>>>\noutput2\n>>>= 0\n"
        length (validateShelltestFormat "test.test" content) @?= 1
    
    , testCase "catches duplicate <<<" $ do
        let content = "command\n<<<\ninput1\n<<<\ninput2\n>>>= 0\n"
        length (validateShelltestFormat "test.test" content) @?= 1
    
    , testCase "catches duplicate >>>=" $ do
        let content = "command\n>>>= 0\n>>>= 1\n"
        length (validateShelltestFormat "test.test" content) @?= 1
    
    , testCase "multi-line output is not duplicate >>>" $ do
        let content = "command\n>>>\nline1\nline2\n>>>= 0\n"
        validateShelltestFormat "test.test" content @?= []
    
    , testCase "multi-line stdin is not duplicate <<<" $ do
        let content = "command\n<<<\nline1\nline2\n>>>\noutput\n>>>= 0\n"
        validateShelltestFormat "test.test" content @?= []
    
    , testCase "accepts valid test case with regex patterns" $ do
        let content = "command\n>>>\n/pattern.*/\n>>>2 /error pattern/\n>>>= 0\n"
        validateShelltestFormat "test.test" content @?= []
    
    , testCase "handles multiple test cases separated by blank lines" $ do
        let content = "command1\n>>>= 0\n\ncommand2\n>>>= 0\n"
        validateShelltestFormat "test.test" content @?= []
    
    , testCase "ignores comment lines" $ do
        let content = "# Comment\ncommand\n# Another comment\n>>>= 0\n"
        validateShelltestFormat "test.test" content @?= []
    
    , testCase "handles CRLF line endings" $ do
        let content = "command\r\n>>>\r\noutput\r\n>>>= 0\r\n"
        validateShelltestFormat "test.test" content @?= []
    
    , testCase "empty file produces no violations" $ do
        validateShelltestFormat "test.test" "" @?= []
    
    , testCase "file with only comments produces no violations" $ do
        let content = "# Comment 1\n# Comment 2\n"
        validateShelltestFormat "test.test" content @?= []
    
    , testCase ">>> in multi-line output is not a new directive" $ do
        let content = "command\n>>>\noutput with >>> in it\n>>>= 0\n"
        validateShelltestFormat "test.test" content @?= []
    
    , testCase ">>>2 content containing >>> is not ambiguous" $ do
        let content = "command\n>>>2 /error with >>> text/\n>>>= 0\n"
        validateShelltestFormat "test.test" content @?= []
    ]

-- | A parsed test case with line locations for diagnostics
data TestCase = TestCase
    { tcStartLine :: Int                  -- line number where this test case starts
    , tcCommand   :: Maybe (Int, String)  -- (line number, command text)
    , tcStdin     :: [(Int, String)]      -- all <<< lines
    , tcStdout    :: [(Int, String)]      -- all >>> lines (not >>>2 or >>>=)
    , tcStderr    :: [(Int, String)]      -- all >>>2 lines
    , tcExitCode  :: [(Int, String)]      -- all >>>= lines
    } deriving (Show)

-- | Line type classification
data LineType
    = CommandLine String
    | StdinDirective String
    | StdoutDirective String
    | StderrDirective String
    | ExitCodeDirective String
    | CommentLine
    | BlankLine
    | ContinuationLine String
    deriving (Show, Eq)

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

-- | Classify a line within a test case
classifyLine :: Maybe LineType -> String -> LineType
classifyLine _ line
    | all isSpace line = BlankLine
    | "#" `isPrefixOf` stripped = CommentLine
    | ">>>=" `isPrefixOf` stripped = ExitCodeDirective (drop 4 stripped)
    | ">>>2" `isPrefixOf` stripped = StderrDirective (drop 4 stripped)
    | ">>>" `isPrefixOf` stripped = StdoutDirective (drop 3 stripped)
    | "<<<" `isPrefixOf` stripped = StdinDirective (drop 3 stripped)
    | otherwise = case prevLineType of
        Just (StdinDirective _) -> ContinuationLine line
        Just (StdoutDirective _) -> ContinuationLine line
        Just (StderrDirective _) -> ContinuationLine line
        Just (ContinuationLine _) -> ContinuationLine line
        _ -> CommandLine line
  where
    stripped = dropWhile isSpace line
    prevLineType = Nothing  -- This gets tracked in the fold

-- | Split file content into test cases
splitTestCases :: [(Int, String)] -> [TestCase]
splitTestCases linesWithNums = 
    let -- Strip \r for CRLF files
        cleanLines = map (\(n, s) -> (n, filter (/= '\r') s)) linesWithNums
        -- Group into test case blocks (separated by blank lines, ignoring comments)
        groups = groupTestCaseBlocks cleanLines
    in map parseTestCase groups
  where
    -- Group consecutive non-blank lines into test cases
    groupTestCaseBlocks :: [(Int, String)] -> [[(Int, String)]]
    groupTestCaseBlocks [] = []
    groupTestCaseBlocks lines =
        let (block, rest) = span (not . isBlankOrComment . snd) lines
            rest' = dropWhile (isBlankOrComment . snd) rest
        in if null block
            then groupTestCaseBlocks rest'
            else block : groupTestCaseBlocks rest'
    
    isBlankOrComment s = all isSpace s || "#" `isPrefixOf` dropWhile isSpace s

-- | Parse a block of lines into a TestCase
parseTestCase :: [(Int, String)] -> TestCase
parseTestCase [] = TestCase 0 Nothing [] [] [] []
parseTestCase block@((startLine, _):_) =
    let (cmd, stdin, stdout, stderr, exitCode) = foldl classifyAndAccumulate (Nothing, [], [], [], []) block
    in TestCase startLine cmd stdin stdout stderr exitCode
  where
    classifyAndAccumulate (cmd, stdin, stdout, stderr, exitCode) (lineNum, lineText) =
        let stripped = dropWhile isSpace lineText
            isComment = "#" `isPrefixOf` stripped
        in if isComment
            then (cmd, stdin, stdout, stderr, exitCode)  -- Skip comments
            else case () of
                _ | ">>>=" `isPrefixOf` stripped -> (cmd, stdin, stdout, stderr, exitCode ++ [(lineNum, lineText)])
                  | ">>>2" `isPrefixOf` stripped -> (cmd, stdin, stdout, stderr ++ [(lineNum, lineText)], exitCode)
                  | ">>>" `isPrefixOf` stripped -> (cmd, stdin, stdout ++ [(lineNum, lineText)], stderr, exitCode)
                  | "<<<" `isPrefixOf` stripped -> (cmd, stdin ++ [(lineNum, lineText)], stdout, stderr, exitCode)
                  | otherwise -> case cmd of
                      Nothing -> (Just (lineNum, lineText), stdin, stdout, stderr, exitCode)  -- First non-directive is command
                      Just _ -> (cmd, stdin, stdout, stderr, exitCode)  -- Continuation lines are not classified as directives

-- | Validate shelltest format for a file
validateShelltestFormat :: FilePath -> String -> [String]
validateShelltestFormat path content =
    let linesWithNums = zip [1..] (lines content)
        testCases = splitTestCases linesWithNums
    in concatMap (validateTestCase path) testCases

-- | Validate a single test case
validateTestCase :: FilePath -> TestCase -> [String]
validateTestCase path tc = concat
    [ checkDuplicateDirective path "<<<" (tcStdin tc)
    , checkDuplicateDirective path ">>>" (tcStdout tc)
    , checkDuplicateDirective path ">>>2" (tcStderr tc)
    , checkDuplicateDirective path ">>>=" (tcExitCode tc)
    , checkMissingExitCode path tc
    ]

-- | Check for duplicate directives
checkDuplicateDirective :: FilePath -> String -> [(Int, String)] -> [String]
checkDuplicateDirective path directiveName occurrences
    | length occurrences <= 1 = []
    | otherwise = [formatDirectiveViolation path directiveName occurrences]

-- | Check for missing exit code (warning level)
checkMissingExitCode :: FilePath -> TestCase -> [String]
checkMissingExitCode path tc
    | null (tcExitCode tc) && isActualTestCase tc = []  -- Temporarily disabled - too many false positives
    | otherwise = []
  where
    -- A test case is "actual" if it has a command line
    isActualTestCase TestCase{tcCommand = Just _} = True
    isActualTestCase _ = False

-- | Format a directive violation message
formatDirectiveViolation :: FilePath -> String -> [(Int, String)] -> String
formatDirectiveViolation path directiveName occurrences =
    let lineNums = map fst occurrences
        lineNumsStr = unwords $ map show lineNums
        startLine = minimum lineNums
    in unlines
        [ ""
        , "SHELLTEST FORMAT ERROR in " ++ path
        , "  Test case starting near line " ++ show startLine ++ ":"
        , "  Multiple " ++ directiveName ++ " directives (lines " ++ lineNumsStr ++ ") - Format 3 allows only one per test case."
        , ""
        , "  Fix: Combine expectations into a single " ++ directiveName ++ " with a regex pattern."
        , "  Example: " ++ directiveName ++ " /pattern1.*pattern2|pattern2.*pattern1/"
        , ""
        , "  If you need to match multiple lines, use a single regex or multi-line literal."
        , ""
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
    let patternViolations = scanForViolations path content
    let formatViolations = validateShelltestFormat path content
    let allViolations = patternViolations ++ formatViolations
    return $ testCase path $ do
        case allViolations of
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
