# Remote Commands Tutorial

## The Problem

You have 10 GB of video footage already sitting on Google Drive. You want bit to
track it, but downloading everything locally just to run `bit add` and then
uploading it all back is a waste of time and bandwidth. The files are already
where they need to be.

Remote commands let you build metadata *for files already on the remote* without
downloading them. bit reads hashes directly from the cloud, downloads only small
text files for classification, and never touches the large binaries.

## Syntax

Two equivalent forms:

```
bit --remote <name> <command>     # portable, works everywhere
bit @<name> <command>             # shorthand, needs quoting in PowerShell
```

Use `--remote` in scripts. Use `@` interactively if your shell supports it.

In PowerShell, `@` is the splatting operator, so `@origin` is silently eaten.
Either quote it (`'@origin'`) or use `--remote origin` instead.

## Quick Start

```bash
# 1. Create a local bit repo (no files needed locally)
mkdir my-project && cd my-project
bit init

# 2. Point it at your remote
bit remote add origin gdrive:Projects/footage

# 3. Initialize tracking on the remote
bit --remote origin init

# 4. Scan the remote and commit metadata
bit --remote origin add .

# 5. Pull metadata locally (instant — just the bundle, not the files)
bit pull
```

After step 4, the remote has a `.bit/bit.bundle` containing full Git history of
every file's hash and size. After step 5, your local repo knows about all 847
files even though your working directory is still empty. From here you can
`bit push`, `bit pull`, or inspect history with `bit log`.

## Commands

### `init` — Create an empty repository on the remote

```bash
bit --remote origin init
```

This creates an empty Git bundle on the remote at `.bit/bit.bundle`. It does
**not** scan files — it only establishes the history so that subsequent commands
have something to work with.

Running it twice errors out:

```
fatal: remote already has a bit repository.
```

### `add` — Scan, classify, and commit metadata

```bash
bit --remote origin add .
bit --remote origin add data/
bit --remote origin add models/weights.bin
```

This is the workhorse command. On every call it:

1. Fetches the bundle from the remote
2. Inflates it into a temporary workspace
3. Scans the remote via `rclone lsjson --hash --recursive`
4. Classifies each file as text or binary:
   - Files above 1 MB or with known binary extensions (.mp4, .zip, etc.) are
     binary — no download needed
   - Small files are downloaded to a temp directory and inspected
5. Writes metadata into the workspace:
   - Binary files get a 2-line `hash:`/`size:` stub
   - Text files get their actual content
6. Stages the specified paths (or `.` for everything)
7. Auto-commits if there are changes
8. Re-bundles and pushes back to the remote

If nothing changed since the last add:

```
Nothing to add — remote metadata is up to date.
```

Example output for a fresh remote:

```
Scanning remote...
Found 12 files on remote.
  8 binary files (by size/extension)
  4 small files to classify...
Downloading 3 text files...
Remote metadata updated.
Creating metadata bundle...
Pushing bundle to remote...
Changes pushed to remote.
```

For a typical 10 GB media repo, this downloads maybe 50 KB of text files while
skipping the 10 GB of video.

### `status` — Check the workspace state

```bash
bit --remote origin status
```

Read-only. Fetches the bundle, inflates it, runs `git status`, and cleans up.
Nothing is pushed back. After a successful `add`, this shows:

```
On branch main
nothing to commit, working tree clean
```

All flags are passed through to git, so `bit --remote origin status --short`
works too.

### `log` — View commit history

```bash
bit --remote origin log
bit --remote origin log --oneline
bit --remote origin log --oneline -5
```

Read-only. Shows the Git history of the remote metadata. Example:

```
a3f1b2c Update remote metadata
96c9f37 Initial remote repository
```

### `commit` — Amend or extend commits

```bash
bit --remote origin commit --amend -m "Initial metadata scan"
```

This is for editing the history after `add` has auto-committed. Common uses:

- Amend the last commit message
- Create additional commits after manual workspace modifications

The command fetches the bundle, inflates it, runs `git commit` with your args,
re-bundles, and pushes. If the commit fails (e.g., nothing to commit), the
bundle is not pushed.

## Complete Walkthrough

### Scenario: Media production archive on Google Drive

You have a Google Drive folder `gdrive:Archive/2025-shoot` containing:

```
2025-shoot/
├── raw/
│   ├── take01.mov      (2.1 GB)
│   ├── take02.mov      (1.8 GB)
│   └── take03.mov      (3.4 GB)
├── audio/
│   ├── ambient.wav     (450 MB)
│   └── interview.wav   (820 MB)
├── notes.txt           (2 KB)
├── shot-list.md        (5 KB)
└── README.md           (1 KB)
```

#### Step 1: Set up local repo

```bash
mkdir shoot-tracker && cd shoot-tracker
bit init
bit remote add origin gdrive:Archive/2025-shoot
```

#### Step 2: Initialize the remote

```bash
$ bit --remote origin init
Initialized bit repository on remote 'origin'.
```

#### Step 3: Scan and commit metadata

```bash
$ bit --remote origin add .
Scanning remote...
Found 8 files on remote.
  5 binary files (by size/extension)
  3 small files to classify...
Downloading 3 text files...
Remote metadata updated.
Creating metadata bundle...
Pushing bundle to remote...
Changes pushed to remote.
```

bit scanned all 8 files. The 5 large media files (.mov, .wav) were classified
as binary by extension — their MD5 hashes came directly from Google Drive's
metadata API via `rclone lsjson --hash`, no download required. The 3 small text
files (notes.txt, shot-list.md, README.md) were downloaded, classified, and
their content stored directly in Git.

Total data downloaded: ~8 KB (the three text files).
Total data **not** downloaded: ~8.57 GB (the five media files).

#### Step 4: Give it a better commit message

```bash
$ bit --remote origin commit --amend -m "Archive 2025 shoot: 5 media + 3 text"
[main 7e2a1f3] Archive 2025 shoot: 5 media + 3 text
 Date: Mon Feb 9 12:00:00 2026 +0000
 8 files changed, 13 insertions(+)
Creating metadata bundle...
Pushing bundle to remote...
Changes pushed to remote.
```

#### Step 5: Verify and pull locally

```bash
$ bit --remote origin log --oneline
7e2a1f3 Archive 2025 shoot: 5 media + 3 text
96c9f37 Initial remote repository

$ bit pull
```

Now your local bit repo has the full metadata history. `bit log` shows the same
commits. `bit status` knows about all 8 files. If you later want the actual
media files locally, `bit pull` will download them.

### Scenario: Filesystem remote (USB drive)

Remote commands work with filesystem remotes too, though they are most useful
with cloud storage where you want to avoid unnecessary transfers.

```bash
bit init
bit remote add usb E:\backups\project
bit --remote usb init
bit --remote usb add .
```

### Scenario: Re-scanning after remote changes

If files change on the remote (someone uploads new footage), run `add` again:

```bash
$ bit --remote origin add .
Scanning remote...
Found 10 files on remote.
  7 binary files (by size/extension)
  3 small files to classify...
Downloading 3 text files...
Remote metadata updated.
Creating metadata bundle...
Pushing bundle to remote...
Changes pushed to remote.
```

Each `add` does a full re-scan. The workspace is ephemeral — it starts from
the bundle every time and reconstructs the complete metadata from the current
remote state. Additions, modifications, and deletions are all detected
automatically.

## How It Works Under the Hood

Every remote command follows the same pattern:

```
fetch bundle from remote
  → inflate into system temp directory
    → operate (scan, add, commit, status, log)
    → re-bundle if changes were made
  → push new bundle to remote
→ clean up temp directory
```

There is no persistent workspace on disk between commands. The remote's
`.bit/bit.bundle` is the sole source of truth. Temp directories are cleaned up
even if the command fails (exception-safe via `bracket`).

Read-write commands (`add`, `commit`) compare Git HEAD before and after the
action. If HEAD didn't change, the push is skipped — no unnecessary uploads.

Read-only commands (`status`, `log`) never push, even if they technically could.

## Error Messages

| Situation | Message |
|-----------|---------|
| Remote not initialized | `fatal: no bit repository on remote. Run 'bit @remote init' first.` |
| Already initialized | `fatal: remote already has a bit repository.` |
| Remote not configured | `fatal: remote 'foo' not found.` |
| Not in a bit repo | `fatal: not a bit repository (or any of the parent directories): .bit` |
| Network failure | `fatal: network error: <details>` |
| Unsupported command | `error: command not supported in remote context: push` |

## Tips

- **`init` then `add`**: Always run `init` before `add`. `init` creates the
  empty history; `add` populates it. They are separate because `init` is a
  one-time setup, while `add` can be run repeatedly as files change.

- **Amend freely**: The auto-commit message from `add` is generic ("Update
  remote metadata"). Use `commit --amend -m "..."` to give it a meaningful name
  before pulling locally.

- **Check before pulling**: Use `log --oneline` and `status` to inspect the
  remote state before pulling metadata into your local repo.

- **Cloud remotes benefit most**: For filesystem remotes, you could just `cd`
  to the remote and run bit commands there directly. Remote commands shine when
  the remote is a cloud backend where direct access isn't possible.

- **Text classification is conservative**: Files that can't be downloaded for
  classification are treated as binary. This is safe — binary metadata is
  always correct, while misclassifying a binary as text would store garbage in
  Git.
