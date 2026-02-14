# Running Git's Test Suite Against bit

This guide explains how to run Git's own test suite with `bit.exe` acting as `git`,
to verify compatibility and discover what needs special implementation.

## How it works

The `extern/git-shim/` directory contains:
- **`git`** — a shell script that forwards commands to `bit.exe`. After `bit init`,
  it creates a Windows directory junction `.git` → `.bit/index/.git` so that tests
  expecting a standard git layout can find the git directory.
- **`setup.sh`** — creates the `GIT-BUILD-OPTIONS` stub and `templates/blt/`
  directory inside the `extern/git/` submodule, which the test harness requires.

When `GIT_TEST_INSTALLED` points to the shim directory, the test harness uses our
shim as `git`. The shim calls `bit`, which handles known commands and forwards the
rest to the real git on `PATH`.

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

## Running with verbose output

Add `--verbose` to see each test case and its output:

```bash
GIT_TEST_INSTALLED=/path/to/extern/git-shim bash t0001-init.sh --verbose
```

Add `--verbose-log` to write output to log files instead of the terminal.

## Current status

The shim handles:
- **No-args** — forwarded to real git (exits 1)
- **Global flags** — `--exec-path`, `--version`, `--html-path`, etc.
- **`init [flags] [dir]`** — all git init flags parsed and passed through;
  shim creates `.git` directory junction for test compatibility
- **Known bit commands** — `add`, `commit`, `diff`, `status`, `log`, etc.
  routed through `.bit/index/`

Not yet implemented:
- **Catch-all passthrough** — unknown commands (e.g. `git config`, `git stash`)
  currently error instead of forwarding to git
- **Git aliases** — bit doesn't expand aliases from `.gitconfig`
- **`BIT_REAL_GIT`** — when the shim is on `PATH`, bit's internal
  `readProcessWithExitCode "git"` calls could recurse; needs an env var to
  break the loop

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

**"not a bit repository"** — The command fell through to bit's repo check.
This happens for commands bit doesn't handle yet when no `.bit/` directory exists.
