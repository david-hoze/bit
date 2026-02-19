# Tutorial: Importing an Existing Git Repo into bit

You have a git repository with history you want to keep, and you want to start using bit for binary file versioning. `bit import` converts the repo in place — no data loss, no re-cloning.

## Quick Start

```bash
cd my-project
bit import
bit add .
bit commit -m "Initial bit metadata"
```

That's it. Your git history is preserved, and the repo is now a bit repo.

## What Happens

`bit import` does three things:

1. **Moves `.git/` to `.bit/index/.git/`** — bit stores its git database inside `.bit/index/`. Your commits, branches, tags, and refs all come along unchanged.

2. **Creates the `.bit/` directory structure** — sets up `index/`, `devices/`, `remotes/`, `cas/`, and a default config file.

3. **Configures git** — applies the same settings that `bit init` would: safe.directory, merge driver, autocrlf, etc.

After import, your working files are untouched. They're still sitting in the project root exactly where they were. The only difference is that `.git/` has moved inside `.bit/`.

## Step by Step

### 1. Import the repo

From inside the git repo:

```bash
cd my-project
bit import
```

Or from outside:

```bash
bit import /path/to/my-project
```

You'll see:

```
Imported git repository: /path/to/my-project
Git history preserved. Run 'bit status' to verify.
```

### 2. Check status

```bash
bit status
```

You'll see your files listed as "modified". This is expected — git previously tracked the raw file content, but bit tracks metadata files instead. The files haven't actually changed; bit just needs to commit its metadata format.

### 3. Commit the metadata transition

```bash
bit add .
bit commit -m "Initial bit metadata"
```

This creates a single commit that transitions from raw file tracking to bit's metadata format.

### 4. Verify

```bash
bit log       # shows your original git history + the new metadata commit
bit status    # should show "nothing to commit"
```

Your full git history is intact. The only new commit is the metadata transition.

## Adding a Remote

After import, you can add remotes and push just like any bit repo:

```bash
# Cloud remote (stores both metadata and files)
bit remote add origin gdrive:my-project

# Filesystem remote (USB drive, NAS, etc.)
bit remote add backup /mnt/usb/my-project

# Push
bit push -u origin
```

## FAQ

**Q: Is the conversion reversible?**

The git history is fully preserved inside `.bit/index/.git/`. If you ever need to go back, you could move `.bit/index/.git` back to the repo root as `.git/`. But in practice there's no reason to — bit passes through all git commands transparently.

**Q: What about submodules?**

Submodules are preserved. Their entries in `.git/config` and `.gitmodules` carry over with the moved `.git/` directory.

**Q: What about `.gitignore`?**

Your `.gitignore` continues to work. bit also supports `.bitignore` for bit-specific ignore patterns. If you have a `.bitignore`, it gets synced to `.bit/index/.gitignore` on the next scan.

**Q: Can I import a bare repo?**

No. `bit import` requires a working tree (a non-bare repo with `.git/`). For bare repos, use `bit init --bare` instead.

**Q: What if I'm already inside a bit repo?**

`bit import` will refuse with an error: `fatal: '<dir>' is already a bit repository (.bit exists)`.
