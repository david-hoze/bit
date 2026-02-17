# Running Git's Test Suite Against bit

This guide explains how to run Git's own test suite with `bit.exe` acting as `git`,
to verify compatibility and discover what needs special implementation.

## How it works

The `extern/git-shim/` directory contains:
- **`git`** — a shell script that sets `BIT_REAL_GIT` and `BIT_GIT_JUNCTION=1`,
  then `exec bit "$@"`. All command routing (aliases, passthrough, init) is
  handled by bit itself.
- **`setup.sh`** — creates the `GIT-BUILD-OPTIONS` stub and `templates/blt/`
  directory inside the `extern/git/` submodule, which the test harness requires.

When `GIT_TEST_INSTALLED` points to the shim directory, the test harness uses our
shim as `git`. The shim calls `bit`, which handles known commands, expands git
aliases, and forwards unknown commands to the real git.

### Environment variables

| Variable | Set by | Purpose |
|----------|--------|---------|
| `BIT_REAL_GIT` | shim | Path to the real git binary. `spawnGit` uses this instead of `git` to avoid recursion through the shim. |
| `BIT_GIT_JUNCTION` | shim | When `=1`, `initializeRepoAt` creates a `.git` directory junction pointing to `.bit/index/.git` after init. Allows git's repo discovery to work naturally. Uses `mklink /j` (no admin rights). |

### Junction mode behavior

Junction mode (`BIT_GIT_JUNCTION=1`) is **only used for git test suite compatibility**.
When active, several bit subsystems adapt their behavior:

- **Scanner**: Skips `.bit` opaque boundary checks (`.bit` dirs are bit internal state,
  not subrepo markers). `.git` directories still trigger opaque boundaries as normal.
- **Subrepo detection** (`detectAndHandleSubrepoJunction`): When `bit add` encounters a
  subdirectory with a `.git` junction (created by a nested `bit init`), it removes the
  junction and moves the real git dir from `.bit/index/.git/` into the parent's
  `.bit/index/` so git can detect the submodule and create a 160000 entry. This uses
  `pathIsSymbolicLink` to distinguish junctions from real `.git` directories.
- **ls-files**: When `-o`/`--others` is used, runs without `-C .bit/index` so pathspecs
  and `-X` file paths resolve from the actual working directory.
- **Submodule sync**: Skips `syncSubmoduleToWorkingDirectory` since git operates on
  `.bit/index/` directly via the junction.

## Prerequisites

| Tool | Notes |
|------|-------|
| bit.exe | `cabal install --overwrite-policy=always` |
| Git submodule | `git submodule update --init extern/git` |
| Bash (MSYS2) | Ships with Git for Windows / PortableGit |

## One-time setup

```bash
extern/git-shim/setup.sh
```

This creates:
- `extern/git/GIT-BUILD-OPTIONS` — stub with `PERL_PATH`, `SHELL_PATH`, `X=''`
- `extern/git/templates/blt/` — empty directory the test harness checks for

Both are inside the submodule and gitignored by it, so they won't show up in
`git status`.

## Running a single test

```bash
cd extern/git/t
GIT_TEST_INSTALLED=/path/to/extern/git-shim bash t0001-init.sh --verbose
```

Add `--verbose-log` to write output to log files instead of the terminal.

## Current status

Implemented:
- **Alias expansion** — bit reads git aliases from local and global config,
  expands simple aliases and re-dispatches, forwards shell aliases (`!`) to git
- **Catch-all passthrough** — unknown commands forwarded to git via
  `runGitRawAt` (inside repo) or `runGitGlobal` (outside repo)
- **Inside-git-dir detection** — when CWD has a `HEAD` file (e.g. user `cd`'d
  into `.git`), passthrough uses `runGitHere` (no `-C` override) so git's own
  repo discovery works
- **`BIT_REAL_GIT`** — prevents recursion when shim is on `PATH`
- **`BIT_GIT_JUNCTION`** — creates `.git` junction for test compatibility
- **Init before repo discovery** — `bit init` runs before `findBitRoot`, so
  nested init inside an existing repo works correctly
- **Global flags** — `--exec-path`, `--version`, `--html-path`, etc.
- **Known bit commands** — `add`, `commit`, `diff`, `status`, `log`, etc.

### Test results

| Test Suite | Pass | Fail | Notes |
|------------|------|------|-------|
| t0001-init.sh | 91/91 | 0 | All init tests pass |
| t0002-gitfile.sh | 14/14 | 0 | All gitfile tests pass |
| t0003-attributes.sh | 54/54 | 0 | All attribute tests pass |
| t0004-unwritable.sh | 9/9 | 0 | 8 skipped (missing POSIXPERM/SANITY) |
| t0005-signals.sh | 5/5 | 0 | 3 skipped (missing !MINGW) |
| t0006-date.sh | 129/129 | 0 | All date parsing tests pass |
| t0007-git-var.sh | 27/27 | 0 | 2 skipped (missing !AUTOIDENT, POSIXPERM) |
| t0008-ignores.sh | 397/397 | 0 | All ignore tests pass |
| t0010-racy-git.sh | 10/10 | 0 | All racy git tests pass |
| t0012-help.sh | 5/179 | 174 | Help tests require git's man pages / html docs (not available via shim) |
| t0013-sha1dc.sh | 1/1 | 0 | SHA-1 collision detection passes |
| t0014-alias.sh | — | — | Hangs: bit's alias expansion lacks loop detection for nested aliases |

## Naming constraint on Windows

Copying `bit.exe` as `git.exe` causes GHC runtime crashes (malloc/VirtualAlloc
failures). The cause is unknown but consistent. The shell script shim avoids this
by calling `bit` by its original name.

## Troubleshooting

**"GIT-BUILD-OPTIONS missing"** — Run `extern/git-shim/setup.sh`.

**"You haven't built things yet"** — The `templates/blt/` directory is missing.
Run `extern/git-shim/setup.sh`.

**"there is no working Git"** — The shim's `git` script isn't found or isn't
executable. Check `chmod +x extern/git-shim/git`.
