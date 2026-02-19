# Tutorial: Using bit as git

This tutorial shows how to make `git` commands transparently work with bit repos.

## Prerequisites

- bit installed (`cabal install`)
- git installed
- Both `bit` and `bit-git-router` on PATH (both are built by `cabal install`)

## Step 1: Install the router

```bash
bit become-git
```

This:
1. Finds your real git and saves its path
2. Copies the `bit-git-router` executable as `git` into `~/.bit-router/`
3. Adds `~/.bit-router/` to the front of your PATH

**Restart your shell** after running this command.

## Step 2: Verify it works

After restarting your shell, verify the router is active:

```bash
which git
# Should show: ~/.bit-router/git (or similar)

# In a bit repo:
cd my-bit-repo
git status
# Output comes from bit

# In a git repo:
cd my-git-repo
git status
# Output comes from real git
```

## Step 3: Use git commands naturally

In any bit repo, standard git commands are handled by bit:

```bash
git add file.txt
git commit -m "Add file"
git push
git pull
git log
git diff
```

Commands bit doesn't handle are passed through to real git automatically.

## The `git init` rule

In normal mode, `git init` always creates a standard git repo, never a bit repo:

```bash
git init new-project    # Creates .git/ (standard git)
bit init new-project    # Creates .bit/ (bit repo)
```

This ensures compatibility with scripts and tools that expect `git init` behavior.

## How it works

The router (`bit-git-router`) is a small, fast executable with two modes:

**Normal mode** (default):
1. Checks if the command is `init` → always routes to real git
2. Walks up from CWD looking for `.bit/` → routes to `bit`
3. Otherwise → routes to real git

**Junction mode** (`BIT_GIT_JUNCTION=1`):
- Routes **all** commands to bit unconditionally (used for git test suite)
- See `docs/git-test-suite.md` for details

In both modes, it sets `BIT_REAL_GIT` before calling bit so bit's internal git calls don't recurse through the router.

## Test suite setup

To set up the router for running git's own test suite:

```bash
bit become-git --init    # Copies router + bit into extern/git-shim/
```

Then run tests with `BIT_GIT_JUNCTION=1`:

```bash
cd extern/git/t
BIT_GIT_JUNCTION=1 GIT_TEST_INSTALLED=/path/to/extern/git-shim bash t0001-init.sh --verbose
```

## Uninstalling

To restore your system git:

```bash
bit become-bit
```

This removes `~/.bit-router/` from PATH and deletes the directory. Restart your shell afterward.

## Environment variables

- `BIT_REAL_GIT` — path to the real git binary. Set automatically by the router; can be overridden manually for testing.

## Troubleshooting

**"cannot find real git"**: The router can't locate git. Set `BIT_REAL_GIT` to the full path of your git executable.

**"cannot find bit-git-router"**: Run `cabal install` to build both `bit` and `bit-git-router`.

**git commands still go to real git in bit repos**: Check that `~/.bit-router/` is at the front of your PATH (`echo $PATH`). You may need to restart your shell or open a new terminal.

**Reverting in case of issues**: If the router causes problems, you can manually remove it:
```bash
# Remove the router directory
rm -rf ~/.bit-router

# On Windows, also clean up the registry PATH entry:
# Open System Properties > Environment Variables and remove ~/.bit-router from Path

# On Unix, remove the export line from ~/.bashrc and ~/.profile
```
