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
    , "  import [<directory>]              Convert a git repo to a bit repo"
    , "  export [<path>]                   Convert a bit repo back to a git repo"
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
    , "  mv <src> <dst>                    Move or rename a tracked file"
    , "  restore [options] [--] <path>     Restore working tree files"
    , "  checkout [options] -- <path>      Checkout files from index"
    , "  reset [options]                   Reset staging area"
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
    , ""
    , "Integrity:"
    , "  verify                            Verify files match committed metadata"
    , "  repair                            Verify and repair files from remotes"
    , "  fsck                              Check metadata repository integrity"
    , ""
    , "Config and CAS:"
    , "  config <key> [<value>]            Get or set config (e.g. core.mode lite|solid)"
    , "  config --list                     List all config"
    , "  cas backfill                      Store current working tree blobs into CAS"
    , ""
    , "Merge and branching:"
    , "  merge --continue|--abort          Continue or abort merge"
    , "  branch --unset-upstream           Unset upstream tracking"
    , ""
    , "Remote workspace (cloud remotes only):"
    , "  --remote <name> <cmd>             Target a remote workspace (portable)"
    , "  @<remote> <cmd>                   Shorthand (needs quoting in PowerShell)"
    , "  Supported: init, add, commit, status, log, ls-files, verify, repair"
    , ""
    , "Git compatibility:"
    , "  become-git                        Install git router (route git to bit in .bit repos)"
    , "  become-bit                        Uninstall git router (restore system git)"
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
        { cmdName     = "import"
        , cmdSynopsis = "Convert a git repo to a bit repo"
        , cmdUsage    = "bit import [<directory>]"
        , cmdDesc     = [ "Convert an existing git repository into a bit repository."
                        , "Moves .git/ into .bit/index/.git/ and sets up bit directory structure."
                        , "Git history is fully preserved." ]
        , cmdOptions  = []
        , cmdExamples = [ HelpItem "bit import" "Import current directory"
                        , HelpItem "bit import /path/to/repo" "Import a specific repo" ]
        }
    , CommandHelp
        { cmdName     = "export"
        , cmdSynopsis = "Convert a bit repo back to a plain git repo"
        , cmdUsage    = "bit export [<path>]"
        , cmdDesc     = [ "Export a bit repository back to a plain git repository."
                        , "Without a path: in-place export (removes .bit, restores .git)."
                        , "With a path: copies to a new directory as a plain git repo." ]
        , cmdOptions  = []
        , cmdExamples = [ HelpItem "bit export" "In-place export (destructive)"
                        , HelpItem "bit export /tmp/copy" "Export to a new directory" ]
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
        { cmdName     = "mv"
        , cmdSynopsis = "Move or rename a tracked file"
        , cmdUsage    = "bit mv <src> <dst>"
        , cmdDesc     = ["Move or rename a file in both the working tree and metadata."]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit mv old.txt new.txt" "Rename a file"]
        }
    , CommandHelp
        { cmdName     = "reset"
        , cmdSynopsis = "Reset staging area"
        , cmdUsage    = "bit reset [options]"
        , cmdDesc     = ["Reset the staging area. Unstages changes without modifying files."]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit reset" "Unstage all changes"]
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
                        , "If no remote is specified, pushes to the configured upstream remote."
                        , "Use 'bit push -u <remote>' to set upstream tracking." ]
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
                        , "  show [<name>]      Show remote information" ]
        , cmdOptions  = []
        , cmdExamples = [ HelpItem "bit remote add origin gdrive:Projects/foo" "Add a cloud remote"
                        , HelpItem "bit remote show" "Show all remotes"
                        , HelpItem "bit --remote origin verify" "Verify remote files" ]
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
                        , HelpItem "bit remote add nas //server/share/project" "Add a network share" ]
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
        { cmdName     = "verify"
        , cmdSynopsis = "Verify files match committed metadata"
        , cmdUsage    = "bit verify"
        , cmdDesc     = [ "Verify that files match their committed metadata (hash, size)."
                        , "On slow storage, measures throughput and offers to skip hashing."
                        , "If issues are found, offers to repair from configured remotes."
                        , ""
                        , "Use 'bit --remote <name> verify' to verify a remote." ]
        , cmdOptions  = [HelpItem "--sequential" "Run verification sequentially"]
        , cmdExamples = [ HelpItem "bit verify" "Verify local files"
                        , HelpItem "bit --remote origin verify" "Verify remote files" ]
        }
    , CommandHelp
        { cmdName     = "repair"
        , cmdSynopsis = "Verify and repair files from remotes"
        , cmdUsage    = "bit repair"
        , cmdDesc     = [ "Same as 'bit verify' but repairs automatically without prompting."
                        , "Searches all configured remotes for correct versions of"
                        , "corrupted or missing files."
                        , ""
                        , "Use 'bit --remote <name> repair' to repair a remote." ]
        , cmdOptions  = [HelpItem "--sequential" "Run verification sequentially"]
        , cmdExamples = [ HelpItem "bit repair" "Repair local files"
                        , HelpItem "bit --remote origin repair" "Repair remote files" ]
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
    , CommandHelp
        { cmdName     = "config"
        , cmdSynopsis = "Get or set repo config"
        , cmdUsage    = "bit config <key> [<value>]"
        , cmdDesc     = [ "Read or write .bit/config (git-style INI)."
                        , "  core.mode   lite (default) or solid; solid stores file content in .bit/cas/ on add." ]
        , cmdOptions  = [ HelpItem "--list" "List all key=value pairs" ]
        , cmdExamples = [ HelpItem "bit config core.mode" "Print current mode"
                        , HelpItem "bit config core.mode solid" "Enable CAS storage on add" ]
        }
    , CommandHelp
        { cmdName     = "cas"
        , cmdSynopsis = "Content-addressed store"
        , cmdUsage    = "bit cas backfill"
        , cmdDesc     = [ "CAS subcommands."
                        , "  backfill   Walk historical commits and store blobs currently in the working tree into .bit/cas/." ]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit cas backfill" "Backfill CAS from current files"]
        }
    , CommandHelp
        { cmdName     = "cas backfill"
        , cmdSynopsis = "Store working tree blobs into CAS"
        , cmdUsage    = "bit cas backfill"
        , cmdDesc     = [ "Walk all commits in history and, for each binary blob hash"
                        , "referenced in metadata, if that blob is not yet in .bit/cas/"
                        , "and the current working tree has a file with that hash,"
                        , "copy it into the CAS. Optional after switching to solid mode." ]
        , cmdOptions  = []
        , cmdExamples = [HelpItem "bit cas backfill" "Backfill CAS from current files"]
        }
    , CommandHelp
        { cmdName     = "become-git"
        , cmdSynopsis = "Install git router"
        , cmdUsage    = "bit become-git"
        , cmdDesc     = [ "Install a git router that transparently routes 'git' commands"
                        , "to bit when inside a .bit/ repo, and to real git otherwise."
                        , ""
                        , "This copies the bit-git-router executable as 'git' into"
                        , "~/.bit-router/ and adds that directory to the front of PATH."
                        , ""
                        , "After installation, you can use 'git add', 'git commit', etc."
                        , "in bit repos and they'll be handled by bit automatically."
                        , "'git init' always creates a standard git repo." ]
        , cmdOptions  = []
        , cmdExamples = [ HelpItem "bit become-git" "Install the git router"
                        , HelpItem "git status" "Routes to bit in .bit repos, git in .git repos" ]
        }
    , CommandHelp
        { cmdName     = "become-bit"
        , cmdSynopsis = "Uninstall git router"
        , cmdUsage    = "bit become-bit"
        , cmdDesc     = [ "Uninstall the git router and restore the system git."
                        , "Removes ~/.bit-router/ directory and cleans up PATH." ]
        , cmdOptions  = []
        , cmdExamples = [ HelpItem "bit become-bit" "Restore system git" ]
        }
    ]
