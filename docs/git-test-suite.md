# Running Git's Test Suite Against bit

This guide explains how to run Git's own test suite with `bit.exe` acting as `git`,
to verify compatibility and discover what needs special implementation.

## How it works

The `extern/git-shim/` directory contains:
- **`git.exe`** — the compiled git router (`bit-git-router`), installed by
  `bit become-git --init`. When `BIT_GIT_JUNCTION=1` is set, it routes all
  commands to bit unconditionally (same as the old bash shim, but without the
  extra bash process). When the env var is not set, it uses normal routing
  (init → real git, `.bit/` repos → bit, else → real git).
- **`bit.exe`** — copied alongside the router by `bit become-git --init` so
  the router's `findBit` locates it without relying on PATH.
- **`setup.sh`** — creates the `GIT-BUILD-OPTIONS` stub and `templates/blt/`
  directory inside the `extern/git/` submodule, which the test harness requires.

When `GIT_TEST_INSTALLED` points to the shim directory, the test harness uses
the router as `git`. With `BIT_GIT_JUNCTION=1`, the router calls bit for every
command. Bit handles known commands, expands git aliases, and forwards unknown
commands to the real git.

### Environment variables

| Variable | Set by | Purpose |
|----------|--------|---------|
| `BIT_REAL_GIT` | router | Path to the real git binary. `spawnGit` uses this instead of `git` to avoid recursion through the router. |
| `BIT_GIT_JUNCTION` | test runner scripts | When `=1`, the router routes all commands to bit (no `.bit/` walk-up check). Also tells `initializeRepoAt` to create a `.git` directory junction pointing to `.bit/index/.git` after init. Uses `mklink /j` (no admin rights). |

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
extern/git-shim/setup.sh    # Create GIT-BUILD-OPTIONS and templates
bit become-git --init        # Install compiled router as extern/git-shim/git.exe
```

This creates:
- `extern/git/GIT-BUILD-OPTIONS` — stub with `PERL_PATH`, `SHELL_PATH`, `X=''`
- `extern/git/templates/blt/` — default template files the test harness requires
- `extern/git/t/helper/test-tool` — bash stub for the test-tool binary (see below)

All are inside the submodule and gitignored by it, so they won't show up in
`git status`.

## Running a single test

```bash
cd extern/git/t
BIT_GIT_JUNCTION=1 GIT_TEST_INSTALLED=/path/to/extern/git-shim bash t0001-init.sh --verbose
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
- **`BIT_REAL_GIT`** — prevents recursion when router is on `PATH`
- **`BIT_GIT_JUNCTION`** — router routes all commands to bit; creates `.git` junction for test compatibility
- **Init before repo discovery** — `bit init` runs before `findBitRoot`, so
  nested init inside an existing repo works correctly
- **Global flags** — `--exec-path`, `--version`, `--html-path`, etc.
- **Known bit commands** — `add`, `commit`, `diff`, `status`, `log`, etc.

### Test results (2026-03-02, hybrid .git architecture)

**Full run (2026-03-02)**: 914 test scripts pass, 0 fail, ~110 skip (svn/p4/cvs/platform).

Using the throttled runner (`extern/run-throttled-suite.sh`) with 4 agents:
- Batch A (t0-t2): 231/231 pass
- Batch B (t3-t4): 268/268 pass
- Batch C (t5): 170/170 pass
- Batch D (t6-t9): 245/245 pass (+110 skips: svn/p4/cvs/platform)

**Zero failures, zero timeouts.** The throttled runner's load-aware semaphore
eliminates the false timeouts seen in earlier parallel runs.

**Skipped tests** (~154 scripts) are all platform limitations on Windows/MINGW64
— missing tools (svn, p4, cvs, httpd, gpg, sudo, mkfifo, JGit), filesystem
constraints (no tabs in filenames, case-insensitive FS, backslash path separator),
or dangerous tests (write to filesystem root). See `docs/git-test-suite-efficiency.md`
"Optimization 1" for the complete skip reference with explanations.

**Highlighted passing suites** (exercising core git compatibility):
- t0001-init (102/102), t0003-attributes (54/54), t0008-ignores (397/397)
- t0020-crlf (36/36), t0021-conversion (42/42), t0027-auto-crlf (~1898 tests)
- t1092-sparse-checkout-compatibility (104/104), t1517-outside-repo (369/369)
- t1300-config (480/480), t1500-rev-parse (79/79), t1510-repo-setup (109/109)
- t5510-fetch (207/207), t5516-fetch-push (123/123)
- t7610-mergetool (31/31), t3432-rebase-fast-forward (225/225)

### test-tool

The real `test-tool.exe` is compiled from the git submodule using w64devkit.
See `docs/compiling-git-test-tool.md` for build instructions.

The compiled binary lives at `extern/git/t/helper/test-tool.exe`. The old bash
stub (`test-tool`) has been renamed to `test-tool.sh.bak` so that MSYS2 resolves
the `.exe` binary instead of the extensionless script.

## Naming constraint on Windows

Copying `bit.exe` as `git.exe` causes GHC runtime crashes (malloc/VirtualAlloc
failures). The cause is unknown but consistent. The router avoids this because it
is a separate binary (`bit-git-router`) that calls `bit` by its original name.

## Troubleshooting

**"GIT-BUILD-OPTIONS missing"** — Run `extern/git-shim/setup.sh`.

**"You haven't built things yet"** — The `templates/blt/` directory is missing.
Run `extern/git-shim/setup.sh`.

**"there is no working Git"** — The router's `git.exe` isn't found. Run
`bit become-git --init` from the repo root.
