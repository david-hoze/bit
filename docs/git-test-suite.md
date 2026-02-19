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
- `extern/git/templates/blt/` — default template files the test harness requires
- `extern/git/t/helper/test-tool` — bash stub for the test-tool binary (see below)

All are inside the submodule and gitignored by it, so they won't show up in
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

**Full run**: 1007 test suites, **855 pass** (85%), 142 fail.
See `docs/git-test-suite-report.md` for the full categorized breakdown.

**Failure categories** (not bit regressions):
- Infrastructure (GPG, Perl, man pages, SSH helper): ~15 suites
- Git version mismatch (v2.47 tests vs v2.48+ git): ~20 suites
- Known upstream breakage (`# TODO known breakage`): ~15 suites
- Submodule known breakage: ~15 suites
- Trace2 output differences (shim adds events): 4 suites
- Windows/MINGW issues (TAR, symlinks, file locks): ~10 suites
- TAR helper extraction failures (cascading): ~8 suites

**Highlighted passing suites** (exercising core git compatibility):
- t0001-init (90/91), t0003-attributes (54/54), t0008-ignores (397/397)
- t0020-crlf (36/36), t0021-conversion (42/42), t0027-auto-crlf (2600/2600)
- t1000-read-tree-m-3way (83/83), t1300-config (480/480)
- t1500-rev-parse (79/79), t1510-repo-setup (109/109)
- t0033-safe-directory (22/22), t0610-reftable-basics (89/89)

### test-tool

The real `test-tool.exe` is compiled from the git submodule using w64devkit.
See `docs/compiling-git-test-tool.md` for build instructions.

The compiled binary lives at `extern/git/t/helper/test-tool.exe`. The old bash
stub (`test-tool`) has been renamed to `test-tool.sh.bak` so that MSYS2 resolves
the `.exe` binary instead of the extensionless script.

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
