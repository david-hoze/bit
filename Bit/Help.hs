module Bit.Help (
    printMainHelp, printTerseHelp, printCommandHelp
) where

import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)
import Control.Monad (unless)

-- | Option or example: (item, description). Record avoids transposition vs (String, String).
data HelpItem = HelpItem { hiItem, hiDescription :: String }
  deriving (Show, Eq)

data CommandHelp = CommandHelp
    { cmdName     :: String
    , cmdSynopsis :: String
    , cmdUsage    :: String
    , cmdDesc     :: [String]
    , cmdOptions  :: [HelpItem]
    , cmdExamples :: [HelpItem]
    }

lookupCommand :: String -> Maybe CommandHelp
lookupCommand name = case filter (\c -> cmdName c == name) commandRegistry of
    (x:_) -> Just x
    []    -> Nothing

-- | Print the main help summary (grouped by category) to stdout.
printMainHelp :: IO ()
printMainHelp = putStr $ unlines
    [ "Usage: bit <command> [options]"
    , ""
    , "Getting started:"
    , "  init                              Initialize a new bit repository"
    , ""
    , "Tracking changes:"
    , "  status                            Show working tree status"
    , "  add <path>                        Add file contents to metadata"
    , "  commit -m <msg>                   Record changes to the repository"
    , "  diff [--staged]                   Show changes"
    , "  log                               Show commit history"
    , ""
    , "File management:"
    , "  rm [options] <path>               Remove files from tracking"
    , "  restore [options] [--] <path>     Restore working tree files"
    , "  checkout [options] -- <path>      Checkout files from index"
    , "  ls-files                          List tracked files"
    , ""
    , "Syncing:"
    , "  push [-u] [<remote>]              Push to remote"
    , "  pull [<remote>] [options]         Pull from remote"
    , "  fetch [<remote>]                  Fetch metadata from remote"
    , ""
    , "Remote management:"
    , "  remote add <name> <url>           Add a remote"
    , "  remote show [<name>]              Show remote information"
    , "  remote repair [<name>]            Verify and repair files against remote"
    , ""
    , "Integrity:"
    , "  verify [--remote]                 Verify files match committed metadata"
    , "  fsck                              Check metadata repository integrity"
    , ""
    , "Merge and branching:"
    , "  merge --continue|--abort          Continue or abort merge"
    , "  branch --unset-upstream           Unset upstream tracking"
    , ""
    , "Remote workspace:"
    , "  --remote <name> <cmd>             Target a remote workspace (portable)"
    , "  @<remote> <cmd>                   Shorthand (needs quoting in PowerShell)"
    , "  Supported: init, add, commit, status, log, ls-files"
    , ""
    , "See 'bit help <command>' for more information on a specific command."
    ]

-- | Print terse help for a command (-h): just the usage line.
printTerseHelp :: String -> IO ()
printTerseHelp name = case lookupCommand name of
    Just cmd -> putStrLn $ "usage: " ++ cmdUsage cmd
    Nothing  -> unknownCommand name

-- | Print detailed help for a command (--help / help <cmd>).
printCommandHelp :: String -> IO ()
printCommandHelp name = case lookupCommand name of
    Just cmd -> do
        putStrLn $ "usage: " ++ cmdUsage cmd
        putStrLn ""
        mapM_ putStrLn (cmdDesc cmd)
        unless (null (cmdOptions cmd)) $ do
            putStrLn ""
            putStrLn "Options:"
            mapM_ printOption (cmdOptions cmd)
        unless (null (cmdExamples cmd)) $ do
            putStrLn ""
            putStrLn "Examples:"
            mapM_ printExample (cmdExamples cmd)
    Nothing -> unknownCommand name

printOption :: HelpItem -> IO ()
printOption item = putStrLn $ "  " ++ padRight 34 (hiItem item) ++ hiDescription item

printExample :: HelpItem -> IO ()
printExample item = putStrLn $ "  " ++ padRight 34 (hiItem item) ++ hiDescription item

padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 1 (n - length s)) ' '

unknownCommand :: String -> IO ()
unknownCommand name = do
    hPutStrLn stderr $ "bit: '" ++ name ++ "' is not a bit command. See 'bit help'."
    exitWith (ExitFailure 1)

-- | Registry of all commands with their help metadata.
commandRegistry :: [CommandHelp]
commandRegistry =
    [ CommandHelp
        { cmdName     = "init"
        , cmdSynopsis = "Initialize a new bit repository"
        , cmdUsage    = "bit init"
        , cmdDesc     = [ "Create an empty bit repository in the current directory."
                        , "This creates a .bit/ directory with an internal Git repository"
                        , "for tracking file metadata." ]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit init" "Initialize in current directory"]
        }
    , CommandHelp
        { cmdName     = "status"
        , cmdSynopsis = "Show working tree status"
        , cmdUsage    = "bit status"
        , cmdDesc     = [ "Show the state of the working tree: which files are modified,"
                        , "added, or deleted relative to the last commit." ]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit status" "Show status"]
        }
    , CommandHelp
        { cmdName     = "add"
        , cmdSynopsis = "Add file contents to metadata"
        , cmdUsage    = "bit add <path>"
        , cmdDesc     = [ "Compute metadata (hash, size) for the specified files and stage"
                        , "the changes in the internal Git repository."
                        , ""
                        , "Use 'bit add .' to add all modified and new files." ]
        , cmdOptions  = []
        , cmdExamples = [ HelpItem "bit add file.txt" "Add a single file"
                        , HelpItem "bit add ." "Add all files" ]
        }
    , CommandHelp
        { cmdName     = "commit"
        , cmdSynopsis = "Record changes to the repository"
        , cmdUsage    = "bit commit -m <msg>"
        , cmdDesc     = ["Commit staged metadata changes with the given message."]
        , cmdOptions  = [HelpItem "-m <msg>" "Commit message"]
        , cmdExamples = [HelpItem "bit commit -m \"Add new files\"" "Commit with message"]
        }
    , CommandHelp
        { cmdName     = "diff"
        , cmdSynopsis = "Show changes"
        , cmdUsage    = "bit diff [--staged]"
        , cmdDesc     = [ "Show hash/size changes between the working tree and the index."
                        , "With --staged, show changes between the index and HEAD." ]
        , cmdOptions  = [HelpItem "--staged" "Show staged changes"]
        , cmdExamples = [ HelpItem "bit diff" "Show unstaged changes"
                        , HelpItem "bit diff --staged" "Show staged changes" ]
        }
    , CommandHelp
        { cmdName     = "log"
        , cmdSynopsis = "Show commit history"
        , cmdUsage    = "bit log"
        , cmdDesc     = ["Show the commit history of the repository."]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit log" "Show full log"]
        }
    , CommandHelp
        { cmdName     = "rm"
        , cmdSynopsis = "Remove files from tracking"
        , cmdUsage    = "bit rm [options] <path>"
        , cmdDesc     = ["Remove files from tracking and optionally from the working tree."]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit rm file.txt" "Remove a file"]
        }
    , CommandHelp
        { cmdName     = "restore"
        , cmdSynopsis = "Restore working tree files"
        , cmdUsage    = "bit restore [options] [--] <path>"
        , cmdDesc     = [ "Restore working tree files from the index or a specific commit."
                        , "Supports full git restore syntax: --staged, --worktree, --source=, etc." ]
        , cmdOptions  = [ HelpItem "--staged" "Restore staged changes"
                        , HelpItem "--worktree" "Restore working tree (default)"
                        , HelpItem "--source=<commit>" "Restore from specific commit" ]
        , cmdExamples = [ HelpItem "bit restore -- file.txt" "Restore file from index"
                        , HelpItem "bit restore --staged file.txt" "Unstage a file" ]
        }
    , CommandHelp
        { cmdName     = "checkout"
        , cmdSynopsis = "Checkout files from index"
        , cmdUsage    = "bit checkout [options] -- <path>"
        , cmdDesc     = [ "Restore working tree files from the index (legacy syntax)."
                        , "Prefer 'bit restore' for new usage." ]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit checkout -- file.txt" "Restore file from index"]
        }
    , CommandHelp
        { cmdName     = "ls-files"
        , cmdSynopsis = "List tracked files"
        , cmdUsage    = "bit ls-files"
        , cmdDesc     = ["List all files tracked by bit."]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit ls-files" "List all tracked files"]
        }
    , CommandHelp
        { cmdName     = "push"
        , cmdSynopsis = "Push to remote"
        , cmdUsage    = "bit push [-u|--set-upstream] [<remote>]"
        , cmdDesc     = [ "Push metadata and files to a remote. Verifies local files match"
                        , "committed metadata before pushing (proof of possession)."
                        , ""
                        , "If no remote is specified, pushes to the upstream remote or falls"
                        , "back to 'origin' if it exists." ]
        , cmdOptions  = [ HelpItem "-u, --set-upstream <remote>" "Push and set upstream tracking"
                        , HelpItem "--force" "Force push (skip ancestry check)"
                        , HelpItem "--force-with-lease" "Force push if remote matches expected state" ]
        , cmdExamples = [ HelpItem "bit push" "Push to default remote"
                        , HelpItem "bit push origin" "Push to named remote"
                        , HelpItem "bit push -u origin" "Push and set upstream tracking" ]
        }
    , CommandHelp
        { cmdName     = "pull"
        , cmdSynopsis = "Pull from remote"
        , cmdUsage    = "bit pull [<remote>] [options]"
        , cmdDesc     = [ "Pull metadata and files from a remote. Verifies remote files"
                        , "match remote metadata before pulling (proof of possession)." ]
        , cmdOptions  = [ HelpItem "--accept-remote" "Accept remote file state as truth"
                        , HelpItem "--manual-merge" "Interactive per-file conflict resolution" ]
        , cmdExamples = [ HelpItem "bit pull" "Pull from default remote"
                        , HelpItem "bit pull origin" "Pull from named remote"
                        , HelpItem "bit pull --accept-remote" "Accept remote state" ]
        }
    , CommandHelp
        { cmdName     = "fetch"
        , cmdSynopsis = "Fetch metadata from remote"
        , cmdUsage    = "bit fetch [<remote>]"
        , cmdDesc     = [ "Fetch metadata from a remote without syncing files."
                        , "Only transfers the metadata bundle, no file content is downloaded." ]
        , cmdOptions  = []
        , cmdExamples = [ HelpItem "bit fetch" "Fetch from default remote"
                        , HelpItem "bit fetch origin" "Fetch from named remote" ]
        }
    , CommandHelp
        { cmdName     = "remote"
        , cmdSynopsis = "Manage remotes"
        , cmdUsage    = "bit remote <subcommand>"
        , cmdDesc     = [ "Manage remote repositories."
                        , ""
                        , "Available subcommands:"
                        , "  add <name> <url>   Add a remote"
                        , "  show [<name>]      Show remote information"
                        , "  repair [<name>]    Verify and repair files against remote" ]
        , cmdOptions  = []
        , cmdExamples = [ HelpItem "bit remote add origin gdrive:Projects/foo" "Add a cloud remote"
                        , HelpItem "bit remote show" "Show all remotes"
                        , HelpItem "bit remote repair origin" "Repair files against remote" ]
        }
    , CommandHelp
        { cmdName     = "remote add"
        , cmdSynopsis = "Add a remote"
        , cmdUsage    = "bit remote add <name> <url>"
        , cmdDesc     = [ "Add a named remote pointing to the given URL."
                        , "Does not set upstream tracking (use 'bit push -u' for that)."
                        , ""
                        , "For network shares under Git Bash / MINGW, use forward slashes:"
                        , "  //server/share/path   (not \\\\server\\share\\path)" ]
        , cmdOptions  = []
        , cmdExamples = [ HelpItem "bit remote add origin gdrive:Projects/foo" "Add a cloud remote"
                        , HelpItem "bit remote add backup /mnt/usb/myproject" "Add a filesystem remote"
                        , HelpItem "bit remote add nas //server/share/project", "Add a network share" ]
        }
    , CommandHelp
        { cmdName     = "remote show"
        , cmdSynopsis = "Show remote information"
        , cmdUsage    = "bit remote show [<name>]"
        , cmdDesc     = [ "Show information about configured remotes."
                        , "With no arguments, lists all remotes."
                        , "With a name, shows detailed information about that remote." ]
        , cmdOptions  = []
        , cmdExamples = [ HelpItem "bit remote show" "List all remotes"
                        , HelpItem "bit remote show origin" "Show details for origin" ]
        }
    , CommandHelp
        { cmdName     = "remote repair"
        , cmdSynopsis = "Verify and repair files against remote"
        , cmdUsage    = "bit remote repair [<name>]"
        , cmdDesc     = [ "Verify both local and remote files against their metadata,"
                        , "then repair broken or missing files by copying from the other side."
                        , "Uses content-addressable lookup (matches by hash, not path)." ]
        , cmdOptions  = [HelpItem "--sequential" "Run verification sequentially (no parallelism)"]
        , cmdExamples = [ HelpItem "bit remote repair" "Repair against default remote"
                        , HelpItem "bit remote repair origin" "Repair against named remote" ]
        }
    , CommandHelp
        { cmdName     = "verify"
        , cmdSynopsis = "Verify files match committed metadata"
        , cmdUsage    = "bit verify [--remote]"
        , cmdDesc     = [ "Verify that files match their committed metadata (hash, size)."
                        , "Without --remote, checks local working tree files."
                        , "With --remote, checks files on the remote." ]
        , cmdOptions  = [ HelpItem "--remote" "Verify remote files instead of local"
                        , HelpItem "--sequential" "Run verification sequentially" ]
        , cmdExamples = [ HelpItem "bit verify" "Verify local files"
                        , HelpItem "bit verify --remote" "Verify remote files" ]
        }
    , CommandHelp
        { cmdName     = "fsck"
        , cmdSynopsis = "Check metadata repository integrity"
        , cmdUsage    = "bit fsck"
        , cmdDesc     = [ "Run 'git fsck' on the internal metadata repository (.bit/index)."
                        , "Checks the integrity of the object store -- that all commits,"
                        , "trees, and blobs are valid and consistent." ]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit fsck" "Check integrity"]
        }
    , CommandHelp
        { cmdName     = "merge"
        , cmdSynopsis = "Continue or abort merge"
        , cmdUsage    = "bit merge --continue|--abort"
        , cmdDesc     = [ "Manage an in-progress merge."
                        , ""
                        , "Available subcommands:"
                        , "  --continue   Continue after conflict resolution"
                        , "  --abort      Abort current merge" ]
        , cmdOptions  = []
        , cmdExamples = [ HelpItem "bit merge --continue" "Continue merge after resolving conflicts"
                        , HelpItem "bit merge --abort" "Abort current merge" ]
        }
    , CommandHelp
        { cmdName     = "merge --continue"
        , cmdSynopsis = "Continue after conflict resolution"
        , cmdUsage    = "bit merge --continue"
        , cmdDesc     = ["Continue a merge after manually resolving conflicts."]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit merge --continue" "Continue merge"]
        }
    , CommandHelp
        { cmdName     = "merge --abort"
        , cmdSynopsis = "Abort current merge"
        , cmdUsage    = "bit merge --abort"
        , cmdDesc     = ["Abort the current merge operation and restore the pre-merge state."]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit merge --abort" "Abort merge"]
        }
    , CommandHelp
        { cmdName     = "branch"
        , cmdSynopsis = "Branch management"
        , cmdUsage    = "bit branch --unset-upstream"
        , cmdDesc     = [ "Branch management commands."
                        , ""
                        , "Available subcommands:"
                        , "  --unset-upstream   Remove upstream tracking configuration" ]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit branch --unset-upstream" "Remove upstream tracking"]
        }
    , CommandHelp
        { cmdName     = "branch --unset-upstream"
        , cmdSynopsis = "Unset upstream tracking"
        , cmdUsage    = "bit branch --unset-upstream"
        , cmdDesc     = ["Remove the upstream tracking configuration for the current branch."]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit branch --unset-upstream" "Remove upstream tracking"]
        }
    ]
