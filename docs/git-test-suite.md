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

### t0001-init.sh results

33 of 91 tests pass. Key passing tests:
- Tests 1-4: plain init, nested init, aliased init (with and without existing repo)
- Directory creation tests (23, 25, 27)
- Re-init tests (40, 86-90)

Main failure categories:
- `--bare` init (bit stub, not yet implemented)
- `--template` handling (bit doesn't pass through template flags to the junction)
- `git -C <dir>` in passthrough (bit prepends its own `-C .bit/index`, breaking
  relative `-C` from test scripts)
- ref format / object format tests (git internals bit doesn't expose)

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
