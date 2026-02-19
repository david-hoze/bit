# File Tracking

## Metadata File Format

Each metadata file under `.bit/index/` mirrors the path of its corresponding real file and contains **ONLY**:

```
hash: md5:a1b2c3d4e5f6...
size: 1048576
```

Two fields only:
- `hash` -- MD5 hash of file content (prefixed with `md5:`)
- `size` -- File size in bytes

Parsing and serialization are handled by a single canonical module which enforces the round-trip property: `parseMetadata . serializeMetadata == Just`.

**Known deviation from original spec**: The original spec called for SHA256 and `hash=`/`size=` (equals-sign) format. The implementation uses MD5 (matching rclone's native hash) and `hash: `/`size: ` (colon-space) format. This is intentional -- MD5 is used everywhere for consistency with rclone comparisons. SHA256 may be added later alongside MD5, not as a replacement.

## File Handling

- Regular files (binary -> metadata, text -> content stored directly in index)
- Symlinks -- **ignored** (too many edge cases with cloud remotes)
- Device files, sockets, named pipes -- **ignored**
- Empty directories -- **ignored** (not tracked; many cloud backends don't support them)

## Command Line Interface (Git-Compatible)

**CRITICAL**: The CLI mirrors Git's interface. Users familiar with Git should feel immediately at home. Exit codes follow git's convention: 0 for success, 1 for failure.

Help (`bit help`, `bit -h`, `bit --help`, and `bit help <command>`) works without a repository; all other commands require `.bit` to exist (except `bit init` and `bit import`).

### Command Mapping

| Command | Git Equivalent | bit Behavior |
|---------|---------------|---------------|
| `bit init` | `git init` | Initialize `.bit/` with internal Git repo |
| `bit import [<dir>]` | -- | Convert existing git repo to bit repo (preserves history) |
| `bit export [<path>]` | -- | Convert bit repo back to plain git repo (inverse of import) |
| `bit init --bare` | `git init --bare` | Create standard bare repo with `bit/cas/` |
| `bit init --separate-git-dir <dir>` | `git init --separate-git-dir` | Place git DB and bit metadata at `<dir>` |
| `bit add <path>` | `git add` | Compute metadata, write to `.bit/index/`, stage in Git |
| `bit add .` | `git add .` | Add all modified/new files |
| `bit commit -m "msg"` | `git commit` | Commit staged metadata changes |
| `bit status` | `git status` | Show working tree vs metadata vs staged |
| `bit diff` | `git diff` | Show hash/size changes (human-readable) |
| `bit diff --staged` | `git diff --staged` | Show staged metadata changes |
| `bit log` | `git log` | Show commit history |
| `bit restore [options] [--] <path>` | `git restore` | Restore metadata; full git syntax |
| `bit checkout [options] -- <path>` | `git checkout --` | Restore working tree from index (legacy syntax) |
| `bit reset` | `git reset` | Reset staging area |
| `bit rm <path>` | `git rm` | Remove file from tracking |
| `bit mv <src> <dst>` | `git mv` | Move/rename tracked file |
| `bit branch` | `git branch` | Branch management |
| `bit merge` | `git merge` | Merge branches |
| `bit remote add <name> <url>` | `git remote add` | Add named remote (does NOT set upstream) |
| `bit remote add <name> <url> --bare` | -- | Add cloud remote with bare (CAS-only) layout |
| `bit remote show [name]` | `git remote show` | Show remote status |
| `bit config <key>` | `git config <key>` | Get config value |
| `bit config <key> <value>` | `git config <key> <value>` | Set config value |
| `bit config --list` | `git config --list` | List all config entries |
| `bit repair` | -- | Verify and auto-repair local files from remotes |
| `bit push [<remote>]` | `git push [<remote>]` | Push to specified or default remote |
| `bit push -u <remote>` | `git push -u <remote>` | Push and set upstream tracking |
| `bit pull [<remote>]` | `git pull [<remote>]` | Pull from specified or default remote |
| `bit pull --accept-remote` | -- | Accept remote file state as truth |
| `bit pull --manual-merge` | -- | Interactive per-file conflict resolution |
| `bit fetch [<remote>]` | `git fetch [<remote>]` | Fetch metadata only |
| `bit verify` | -- | Verify local files match committed metadata |
| `bit --remote <name> verify` | -- | Verify remote files match committed metadata |
| `bit --remote <name> repair` | -- | Verify and auto-repair remote files |
| `bit fsck` | `git fsck` | Check integrity of internal metadata repository |
| `bit merge --continue` | `git merge --continue` | Continue after conflict resolution |
| `bit merge --abort` | `git merge --abort` | Abort current merge |
| `bit branch --unset-upstream` | `git branch --unset-upstream` | Remove tracking config |
| `bit --remote <name> init` | -- | Create empty bundle on remote (ephemeral) |
| `bit --remote <name> add <path>` | -- | Scan remote, write metadata, auto-commit, push bundle |
| `bit --remote <name> commit -m <msg>` | -- | Commit in ephemeral workspace and push bundle |
| `bit --remote <name> status` | -- | Scan remote and show status (read-only, ephemeral) |
| `bit --remote <name> log` | -- | Show remote workspace history (read-only) |
| `bit --remote <name> ls-files` | -- | Show tracked files on remote (read-only) |
| `bit @<remote> <cmd>` | -- | Shorthand for `--remote` (needs quoting in PowerShell) |

### Remote Show

For the given remote (or default), bit prints the remote name, Fetch URL, and Push URL. For cloud remotes, Push URL is N/A (push goes via rclone to the same target); layout is always shown (`Layout: full` or `Layout: bare`). For filesystem remotes, both URLs are shown (no layout -- filesystem remotes are always full repos). When the local branch has an upstream, status (ahead/behind) is also shown.

**Cloud remotes:** Status is derived from the local bundle at `.bit/index/.git/bundles/<n>.bundle`. If that bundle does not exist, `bit remote show <n>` fetches it first, then shows status. If the fetch fails, bit prints only the URLs and reports status as (unknown).
