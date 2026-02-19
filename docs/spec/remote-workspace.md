# Remote Workspace

## Remote-Targeted Commands (`@<remote>` / `--remote <name>`)

### Problem Statement

When a user has large amounts of data already on a remote (e.g., 10GB of files on Google Drive), the traditional workflow requires downloading everything locally, running `bit init`/`add`/`commit`, then pushing back. This is wasteful when the files are already at the destination.

### Solution: Ephemeral Remote Workspaces

The `--remote <name>` flag (or its `@<remote>` shorthand) allows commands to operate against a cloud remote as if it were a working directory, while only downloading small files (for text classification). Large binary files stay on the remote -- bit just reads their hashes from `rclone lsjson --hash`.

Remote workspaces are only supported for cloud remotes; for filesystem remotes, navigate to the directory and run bit commands there. (`verify` and `repair` work for both types via `--remote`.)

**Key architectural property**: Each command is fully ephemeral:
1. Fetch the bundle from the remote
2. Inflate it into a temporary directory
3. Operate on it (scan, add, commit, status, log, ls-files)
4. Re-bundle the changes (if any)
5. Push the new bundle back to the remote
6. Clean up the temporary workspace

No persistent local workspace state exists between commands. The remote's `.bit/bit.bundle` is the sole source of truth.

---

## The `--remote` Flag

Two equivalent syntaxes exist:

```bash
bit --remote origin init      # portable -- works in all shells
bit @origin init              # shorthand -- needs quoting in PowerShell
```

**Why `--remote` exists**: The `@<remote>` prefix doesn't work in PowerShell because `@` is the splatting operator. `--remote <name>` is the portable alternative.

**Placement**: Both must appear as the first argument(s) to `bit`.

---

## User Workflow

```bash
# Files already exist on gdrive:Projects/footage (10GB of video + some .txt/.md)
bit init                                    # local repo (empty working dir)
bit remote add origin gdrive:Projects/footage

bit --remote origin init                    # create empty bundle on remote
bit --remote origin add .                   # scan remote, write metadata, auto-commit, push bundle

bit pull                                    # pull metadata locally (instant -- just the bundle)
# Working dir is still empty, but bit knows about all 847 files
```

---

## Supported Commands

| Command | Behavior |
|---------|----------|
| `bit --remote <name> init` | Create empty bundle on remote (no scan -- just initializes history) |
| `bit --remote <name> add <path>` | Fetch bundle -> scan remote -> classify files -> write metadata -> auto-commit -> push bundle |
| `bit --remote <name> commit <args>` | Fetch bundle -> commit with provided args -> push bundle |
| `bit --remote <name> status` | Fetch bundle -> scan remote -> write metadata -> show git status (read-only, no push) |
| `bit --remote <name> log` | Fetch bundle -> show git log (read-only, no push) |
| `bit --remote <name> ls-files` | Fetch bundle -> show tracked files (read-only, no push) |
| `bit --remote <name> verify` | Verify remote files match committed remote metadata |
| `bit --remote <name> repair` | Verify and auto-repair remote files |

All other commands are not supported in remote context.

---

## Ephemeral Workspace Pattern

No persistent workspace on disk. Each command creates a temporary directory in the system temp folder, operates, and cleans up:

```
%TEMP%/
├── bit-remote-init/    ← used by 'init' (one-time)
├── bit-remote-ws/      ← used by 'add', 'commit' (read-write)
│   ├── bit.bundle      ← fetched bundle
│   ├── workspace/      ← inflated git repo
│   │   ├── .git/
│   │   └── <metadata>
│   └── new.bundle      ← re-bundled after changes
└── bit-remote-ro/      ← used by 'status', 'log' (read-only)
```

All temporary directories are exception-safe via `bracket`.

### Bundle Inflation

Inflating a bundle into a workspace uses:

```
git init --initial-branch=main
git config core.quotePath false
git fetch <bundle> +refs/heads/*:refs/remotes/bundle/*
git reset --hard refs/remotes/bundle/main
```

**Why this specific sequence:**
- `core.quotePath false` ensures non-ASCII paths are not quoted in git output
- Fetching into `refs/remotes/bundle/*` avoids "refusing to fetch into checked out branch" error
- `git reset --hard` (not `checkout -B`) ensures the working tree is populated
- `git clone` was avoided because on Windows, `removeDirectoryRecursive` can fail to clean up temp directories

### Text File Classification Without Full Download

The `rclone lsjson --hash` scan returns hashes for all files, but classifies everything as binary. bit's text classification:
- Files above `textSizeLimit` (default 1MB) -> binary, no download needed
- Files with `binaryExtensions` (`.mp4`, `.zip`, etc.) -> binary, no download needed
- Remaining small files -> download to temp dir, classify with `hashAndClassifyFile`

For a typical 10GB media repo, this downloads maybe 50KB of text files while skipping the 10GB of video.

---

## Command Details

### `bit --remote origin init`

Does NOT use `withRemoteWorkspace` -- there's no bundle to fetch yet.

1. Creates a temp directory
2. Checks if bundle already exists on remote (errors if it does)
3. `git init --initial-branch=main` in the temp workspace
4. `git commit --allow-empty -m "Initial remote repository"`
5. `git bundle create` from the workspace
6. Pushes bundle to remote at `.bit/bit.bundle` via rclone
7. Cleans up temp directory

### `bit --remote origin add <path>`

Uses `withRemoteWorkspace` (read-write):

1. Fetches and inflates the bundle
2. **Scans** the remote via `rclone lsjson --hash --recursive`
3. **Classifies** files into binary (by size/extension) and text candidates
4. Downloads text candidates, classifies via `hashAndClassifyFile`
5. **Clears** the workspace (removes all files except `.git`)
6. **Writes** metadata: text files get their content downloaded from remote, binary files get `hash:/size:` metadata
7. `git add` the specified paths (or `.`)
8. If changes exist, auto-commits with "Update remote metadata"
9. Re-bundles and pushes if HEAD changed

The scan+classify+write step happens on **every** `add` call -- the workspace starts fresh each time.

### `bit --remote origin status`

Uses `withRemoteWorkspaceReadOnly`. Scans the remote via `scanAndWriteMetadata` to detect untracked files, then runs `git status`. After writing metadata, runs `git add -u && git reset HEAD` to update index stat info without staging changes. No changes are pushed back.

---

## Error Handling

- **Bundle fetch fails (no bundle)**: "fatal: no bit repository on remote. Run 'bit @remote init' first." (exit 1)
- **Network error**: Propagated with "fatal: network error: ..." message
- **Push fails**: "fatal: failed to push bundle to remote." (exit 1)
- **Init when already initialized**: "fatal: remote already has a bit repository."
- **Temp directory cleanup**: `bracket` ensures cleanup runs on all code paths

---

## Limitations

This feature does NOT provide:
- **Selective file download** -- that's a separate feature (sparse working tree)
- **Incremental remote re-scan** -- `bit --remote origin add` always scans from scratch
- **`bit --remote origin push`** -- pushing *to* a remote workspace doesn't make sense
- **Conflict resolution in remote context** -- not needed (single-writer)
- **`--remote` after the subcommand** -- must appear before the subcommand
