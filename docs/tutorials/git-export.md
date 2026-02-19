# Tutorial: Exporting a bit Repo Back to Git

`bit export` converts a bit repository back into a plain git repository. It's the inverse of `bit import` — your full git history is preserved, and the `.bit/` structure is removed.

## Quick Start

```bash
bit export
```

That's it. Your bit repo is now a git repo.

## In-Place Export

```bash
cd my-project
bit export
```

```
Exported bit repository to git: /path/to/my-project
```

This:
1. Moves `.bit/index/.git/` back to `.git/`
2. Removes the `.bit/` directory
3. Leaves your working files untouched

After export, `git status`, `git log`, and all git commands work normally.

## Export to a New Directory

```bash
bit export /tmp/my-project-git
```

This copies the repository to a new location as a plain git repo, leaving the original bit repo intact. Useful for creating a git-only snapshot without modifying the original.

## What Gets Exported

- **Git history**: all commits, branches, tags, and refs
- **Working files**: unchanged — they stay exactly where they are
- **Metadata files**: the `.bit/index/` metadata (hash + size files for binaries) remains in the git history as regular tracked files

## What Gets Removed

- `.bit/` directory (index, CAS, config, remotes, devices, cache)
- bit-specific configuration

The CAS content (`.bit/cas/`) is not exported. If you were using solid mode, those stored blobs are lost. Make sure you have the files you need in the working tree before exporting.

## When to Use Export

- **Switching back to git**: you no longer need bit's binary file handling
- **Sharing with non-bit users**: create a plain git repo that anyone can clone
- **CI/CD pipelines**: some tools expect a standard `.git/` directory

## Verifying the Export

After export:

```bash
git status    # should work normally
git log       # shows your full history
ls -la .git   # the git directory is back
ls -la .bit   # should not exist
```

## Round-Trip

Import and export are inverses:

```bash
# Start with a git repo
cd my-project
bit import            # git → bit
# ... use bit ...
bit export            # bit → git
```

Your git history is identical before and after — the only additions are the metadata transition commits from `bit add`.
