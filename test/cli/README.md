# CLI tests

CLI tests use **[shelltest](https://hackage.haskell.org/package/shelltestrunner)** (shelltestrunner). Test files use the `.test` extension and Format 3: command line, `<<<` (stdin), `>>>` expected stdout (literal or `/regex/`), `>>>=` expected exit code.

**Run from the repository root** so paths like `test\cli\output\work_merge_a` resolve correctly.

## Forbidden Patterns

Test files **must not** use Windows environment variables that expand before command execution. These patterns are **banned** and enforced by the `lint-tests` test suite and the pre-commit hook:

### Banned Patterns

- **`%CD%`** — Current directory. Expands before command chains execute, so if a `cd` fails, subsequent commands run in the wrong directory (potentially the main repo).
- **`%~dp0`** — Batch script directory. Same timing issue as `%CD%`.
- **`%USERPROFILE%`**, **`%APPDATA%`**, **`%HOMEDRIVE%`**, **`%HOMEPATH%`** — User directories. Could resolve outside the test sandbox.

### Why Dangerous

Example of the problem:

```batch
cd test\cli\output\work_mytest & bit remote add origin "%CD%\test\cli\output\remote_mirror"
```

On Windows, `%CD%` expands **before** the command chain runs. If the `cd` command fails (or hasn't executed yet due to timing), `bit remote add origin` runs in the **main repository directory**, changing the development repo's remote URL instead of the test repo's. This corrupts the development environment.

### The Fix: Use Relative Paths

```batch
# WRONG (banned):
cd test\cli\output\work_mytest & bit remote add origin "%CD%\test\cli\output\remote_mirror"

# CORRECT:
cd test\cli\output\work_mytest & bit remote add origin ..\remote_mirror
```

Relative paths resolve at the time the command executes, so they work correctly even if the working directory changes.

### Enforcement

1. **`cabal test lint-tests`** — Scans all `.test` files and fails with a detailed error if violations are found
2. **Pre-commit hook** — Run `scripts\install-hooks.bat` to install a git hook that blocks commits containing these patterns
3. **CI** — The lint test runs in continuous integration, catching violations before merge

## Format Validation

The `lint-tests` suite also validates **shelltest Format 3 syntax** to catch parse errors before test execution.

### Format 3 Rules

Each test case can have at most **one** of each directive:
- **`<<<`** — stdin input (one per test case)
- **`>>>`** — expected stdout (one per test case)
- **`>>>2`** — expected stderr (one per test case)
- **`>>>=`** — expected exit code (one per test case)

Test cases are separated by blank lines.

### Violations Caught

**Critical (causes parse errors — test won't run):**

1. **Multiple `>>>` in one test case** — Only one stdout expectation allowed
2. **Multiple `>>>2` in one test case** — Only one stderr expectation allowed
3. **Multiple `>>>=` in one test case** — Only one exit code expectation allowed
4. **Multiple `<<<` in one test case** — Only one stdin block allowed

### Example Violation

```batch
# WRONG - This test case has TWO >>>2 directives:
command
>>>2 /error pattern 1/
>>>2 /error pattern 2/
>>>= 1
```

**Result**: Shelltestrunner reports a generic parse error and the test never executes.

### The Fix

Combine expectations into a single directive using regex:

```batch
# CORRECT - One >>>2 directive with alternation:
command
>>>2 /error pattern 1|error pattern 2/
>>>= 1
```

For multi-line output, use a multi-line literal or a regex that matches across newlines:

```batch
# CORRECT - One >>> with multiple lines:
command
>>>
line 1
line 2
>>>= 0
```

### Enforcement

Same as forbidden patterns:
1. **`cabal test lint-tests`** — Parses and validates all `.test` files
2. **Pre-commit hook** — Blocks commits with format violations
3. **CI** — Runs in continuous integration

## Test Infrastructure

### Directory Naming

Each test file uses **unique directory names** to prevent interference:
- `gdrive-remote.test`: `work_gdrive_a`, `work_gdrive_b`
- `merge-local.test`: `work_merge_a`, `work_merge_b`, `shared_merge_remote`
- `filesystem-remote-direct.test`: `work_direct`, `fs_remote_direct`

### Global Cleanup

`000-cleanup.test` runs first (alphabetically) and cleans up all known test directories. This ensures a clean state even if previous test runs were interrupted.

### Windows File Handles

Cleanup commands use `timeout /t 1 >nul &` before `rmdir` to give Windows time to release file handles.

## Running tests

1. Install shelltest: `cabal install shelltestrunner`
2. Build bit: `cabal build bit`
3. Run CLI tests: `cabal test cli`

The test runner puts the built `bit` on `PATH` and invokes `shelltest test/cli`, so all `.test` files under `test/cli/` are run.

### Fast tests (over 3 minutes)

To run the main test suites in one go (unit tests, lint, a subset of CLI shell tests, and literate doc generation):

- **Suites:** **lint-tests**, **pipeline**, **device-prompt**, **cli-fast**, **generate-literate-docs**
- **cli-fast** runs a subset of CLI tests (no gdrive, no device prompt): init, status, merge-local, fsck, verify, scan-cache, filesystem-remote-direct, etc. Full **cli** (all shell tests including gdrive) is not part of fast tests.
- One-liner: `cabal test lint-tests pipeline device-prompt cli-fast generate-literate-docs`
- Script: `scripts\run-fast-tests.bat` (from repo root)

Expect runtime over 3 minutes. For quicker feedback (under one minute), run only unit/lint: `cabal test lint-tests pipeline device-prompt`.

To run a single CLI file: `shelltest test/cli/init.test` (ensure `bit` is on `PATH`, e.g. `cabal exec -- env PATH="$(cabal list-bin bit):$PATH" shelltest test/cli/init.test` on Unix).

## gdrive-remote.test

Tests rclone Google Drive remote: push, pull, fetch, and corruption recovery.

**Prerequisites:**

- `rclone` on PATH
- rclone remote named **gdrive-test** configured (e.g. `rclone config` → Google Drive)
- Remote path **gdrive-test:bit-test** is used; the test purges and recreates it

**What it does:**

- **Two repos:** `work_gdrive_a` (pusher) and `work_gdrive_b` (puller), both use `gdrive-test:bit-test` as origin
- **Push/pull/fetch:** Repo A adds a file, commits, pushes; Repo B fetches and pulls; verifies file content
- **Corruption:** Uses `rclone deletefile` to remove a file on the remote (simulates partial/corrupt state)
- **Verify --remote:** Repo B runs `bit verify --remote` and expects missing-file issues
- **Recovery:** Repo A pushes again (re-syncs files); Repo B fetches/pulls; verify is clean
- **Orphan file:** Adds a file on the remote with `rclone copyto` (not in metadata); verifies behavior
- **Cleanup:** Removes the orphan from the remote at the end

To run only the gdrive tests (requires rclone + gdrive-test remote): `shelltest test/cli/gdrive-remote.test` (with `bit` on PATH).

## device-prompt.test

Tests the device name prompt when adding a filesystem remote. Uses `BIT_USE_STDIN=1` so piped input is read as the device name (enables non-TTY testing). Uses `subst` on Windows to create a writable volume for the device flow. Verifies the remote is stored (either device name or fallback path).

**Unit tests** for the device prompt logic (sanitization, validation, interactive vs non-interactive) live in the `device-prompt` test-suite: `cabal test device-prompt`.

## fsck.test

Tests `bit fsck` (local-only, git-style: terse output, one line per issue, exit 1 on any failure). Covers:
- Fresh repo / repo with committed files: fsck prints nothing and exits 0 when OK.
- Corrupted working-tree file: prints `hash mismatch <path>`, exits 1.
- Missing tracked file: prints `missing <path>`, exits 1.

Fsck does not check remote; use `bit verify --remote` for that.

## remote-repair.test

Tests `bit remote repair`: verifies both local and remote files against their metadata, then repairs broken files by copying verified files from the other side using content-addressable (hash+size) lookup. Covers:
- No remote configured: prints error and exits 1.
- Nothing to repair: push then repair — all files verified.
- Local corruption: corrupt a local binary file — repair copies from remote.
- Unrepairable: corrupt same file on both sides — reports unrepairable.

## unicode.test

Tests Unicode/Hebrew support in bit on Windows. Verifies that:
- `core.quotePath` is set to `false` during `bit init` (so git displays Unicode filenames properly instead of octal escapes)
- Files with Hebrew characters (שלום.txt) can be added, committed, and displayed correctly
- Mixed Unicode filenames (Hebrew קובץ, Arabic ملف, Chinese 文件, Emoji 📁) work end-to-end
- Git commands show actual Unicode characters, not escape sequences like `\327\251...`

This test validates the UTF-8 encoding setup in `Bit.hs` (Windows codepage 65001, locale encoding, stdout/stderr encoding) and the git configuration in `Bit/Core.hs`.
