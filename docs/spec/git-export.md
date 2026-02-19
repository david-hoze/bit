# bit export

Converts a bit repository back to a plain git repository. Inverse of `bit import`.

## Synopsis

```
bit export            # in-place: convert current bit repo to git repo
bit export <path>     # copy: export to a new directory as a plain git repo
```

## In-place Export

Converts the current bit repo to a git repo by:

1. Removing the `.git` junction/gitlink at the repo root.
2. Moving `.bit/index/.git` to the repo root as `.git`.
3. Deleting the `.bit/` directory.

After export, the directory is a standard git repo. Working files are untouched. Git history is preserved.

**This is destructive** — the `.bit/` directory (config, remotes, devices, CAS) is deleted. The git history inside `.bit/index/.git` is preserved by moving it back to `.git`.

## Export to Path

Copies the repo to a new directory as a plain git repo:

1. Creates `<path>`.
2. Copies `.bit/index/.git` to `<path>/.git` (recursive copy).
3. Copies all working directory files from the repo root to `<path>`, skipping `.bit/` and `.git/`.

The original bit repo is left untouched. The target path must not already exist.

## Errors

- `fatal: not a bit repository` — no `.bit/` found in cwd.
- `fatal: not a valid bit repository (.bit/index/.git not found)` — `.bit/` exists but git database is missing.
- `fatal: target path already exists: <path>` — target directory already exists (export-to-path mode).

## Examples

```bash
# In-place export
cd my-bit-repo
bit export
git log   # works — this is now a plain git repo

# Export to a new directory
cd my-bit-repo
bit export /tmp/plain-git-copy
cd /tmp/plain-git-copy
git log   # works
```
