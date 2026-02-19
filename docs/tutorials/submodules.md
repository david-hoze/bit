# Tutorial: Submodules

bit supports two kinds of submodules: **git subrepos** (directories with a `.git/` directory) and **bit subrepos** (directories with a `.bit/` directory). Both work with the parent bit repo through git's standard submodule mechanism.

## Adding a Git Submodule

### From a Remote

```bash
bit submodule add https://github.com/user/lib.git extern/lib
```

This clones the repo into `extern/lib/` and registers it as a submodule. The submodule's source files appear in the working directory where editors expect them.

### From a Local Directory

If you already have a git repo in your project:

```bash
bit add extern/lib
```

bit detects that `extern/lib/.git/` exists, recognizes it as a nested repo, and registers it as a submodule. git creates a `160000` entry (a "gitlink" pointing to a commit hash).

## Adding a bit Submodule

If a subdirectory has its own `.bit/` directory:

```bash
bit add my-data/.bit/index
```

bit registers the child's git metadata as a submodule. The child's working files (large binaries, CAS, config) stay in the working directory — only the git metadata is tracked by the parent.

## Updating Submodules After Clone

When you clone or pull a parent repo that has submodules:

```bash
bit submodule update --init
```

This populates the submodule working directories from the index.

For bit subrepos, you'll also need to initialize the child's bit structure:

```bash
cd my-data/
bit init
# or bit clone <url> .
```

## Day-to-Day Workflow

### Working Inside a Git Submodule

```bash
cd extern/lib/
# .git is a gitlink → resolves through to the parent's modules dir
# Edit files, git add, git commit — works normally
```

### Working Inside a bit Submodule

```bash
cd my-data/
# This is an independent bit repo with its own .bit/
# bit add, bit commit — works normally
# Large files managed by the child's own CAS and remotes
```

### Updating the Parent

After making commits inside a submodule, update the parent:

```bash
cd /repo-root/
bit add extern/lib              # for git subrepos
bit add my-data/.bit/index      # for bit subrepos
bit commit -m "Bump submodule"
```

## Other Submodule Commands

These work as you'd expect:

```bash
bit submodule status              # show submodule status
bit submodule update --init       # initialize and populate
bit submodule deinit extern/lib   # remove submodule's working files
bit submodule foreach 'git pull'  # run a command in each submodule
```

Read-only commands (`status`, `sync`) run directly. Commands that modify the working tree (`add`, `update`, `deinit`, `foreach`) sync the index to the working directory after git finishes.

## How It Works

bit organizes submodule operations into two flows:

1. **Working Directory -> Index** (`bit add <path>`): the user has a local subrepo and wants to register it with the parent. bit moves the child's `.git/` into the parent's `.bit/index/`, runs `git add`, and rewrites the gitlink.

2. **Index -> Working Directory** (`bit submodule` commands): a git operation has populated `.bit/index/` and the working directory needs to catch up. The index copy replaces the working directory copy entirely.

### Scanner Behavior

The scanner completely ignores subrepo directories. When it encounters a directory containing `.git/` or `.bit/`, it does not descend into it. Subrepo boundaries are opaque.

## What bit Does NOT Do

- **No automatic child init**: cloning a parent with bit subrepos requires manually initializing each child's `.bit/` structure
- **No CAS nesting**: each bit repo manages its own CAS independently — the parent's CAS does not store the child's large files
- **No submodule handling in the scanner**: the scanner treats subrepo directories as opaque boundaries
