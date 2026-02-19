# `bit import` — Convert a Git Repo to a Bit Repo

## Synopsis

```
bit import [<directory>]
```

## Description

`bit import` converts an existing git repository into a bit repository. It restructures the repo so that `.git` lives inside `.bit/index/.git`, sets up bit's directory layout, and preserves the full git history (all commits, branches, refs).

If `<directory>` is omitted, the current working directory is used.

After import, the working files remain in place. The next command that triggers a scan (`bit status`, `bit add`, `bit commit`, etc.) populates `.bit/index/` with metadata files. Until then, `bit status` shows working files as modified (the git index still references raw file content, not bit metadata).

## Preconditions

- `<directory>/.git` must exist (the target must be a git repo).
- `<directory>/.bit` must NOT exist (the target must not already be a bit repo).

Both are validated before any changes are made.

## Steps

1. **Validate** — check `.git` exists and `.bit` does not.

2. **Create `.bit/` structure** — create directories:
   - `.bit/`
   - `.bit/index/`
   - `.bit/devices/`
   - `.bit/remotes/`
   - `.bit/cas/`

3. **Write default config** — write `.bit/config` with default `[core] mode = lite` and `[text]` settings (same defaults as `bit init`).

4. **Move `.git` to `.bit/index/.git`** — uses `renameDirectory`, which is atomic on the same filesystem. Preserves all history, branches, and refs.

5. **Post-move git config** — configure the relocated git database:
   - `safe.directory` (global) for the new index path
   - `core.quotePath = false` (display Unicode filenames)
   - `merge.bit-metadata.name` and `.driver = false` (prevent conflict markers in metadata)
   - `core.autocrlf = false` (metadata files use LF)

6. **Create `bundles/` directory** inside `.bit/index/.git/` for future bundle storage.

7. **Create `.git` junction** at the repo root pointing to `.bit/index/.git` (only when `BIT_GIT_JUNCTION=1`). This allows git commands to discover the repo without `-C`.

## Post-Import Workflow

After `bit import`, the typical next step is:

```
bit add .
bit commit -m "Initial bit metadata"
```

This transitions the tracked content from raw files to bit metadata format. From this point on, all bit commands work normally.

## Error Messages

| Condition | Message | Exit Code |
|-----------|---------|-----------|
| No `.git` in target | `fatal: '<dir>' is not a git repository (no .git found)` | 1 |
| `.bit` already exists | `fatal: '<dir>' is already a bit repository (.bit exists)` | 1 |

## Examples

```bash
# Import the current directory
cd my-git-repo
bit import

# Import a repo at a specific path
bit import /path/to/my-git-repo

# Verify the import
bit status
bit log       # shows preserved git history
```

## Implementation

- **Module**: `Bit.Core.Import` (`Bit/Core/Import.hs`)
- **Entry point**: `importRepo :: FilePath -> IO ExitCode`
- **Dispatch**: handled in `Bit/Commands.hs` before repo discovery (alongside `init`, `become-git`)
- Reuses `createGitJunction` from `Bit.Core.Init`

## Relationship to `bit init`

`bit init` creates a fresh git repo inside `.bit/index/.git`. `bit import` moves an existing `.git` there instead. Both produce the same `.bit/` directory layout and apply the same post-init git configuration. The difference is only in how `.bit/index/.git` is populated.
