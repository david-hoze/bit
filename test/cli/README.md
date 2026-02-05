# CLI tests

CLI tests use **[shelltest](https://hackage.haskell.org/package/shelltestrunner)** (shelltestrunner). Test files use the `.test` extension and Format 3: command line, `<<<` (stdin), `>>>` expected stdout (literal or `/regex/`), `>>>=` expected exit code.

**Run from the repository root** so paths like `test\cli\work_merge_a` resolve correctly.

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

The test runner puts the built `bit` on `PATH` and invokes `shelltest test/cli`, so all `.test` files under `test/cli/` are run. To run a single file: `shelltest test/cli/init.test` (ensure `bit` is on `PATH`, e.g. `cabal exec -- env PATH="$(cabal list-bin bit):$PATH" shelltest test/cli/init.test` on Unix).

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

## remote-check.test

Tests `bit remote check`: runs **rclone check** between local working tree and the configured remote (excludes `.bit`). Requires rclone on PATH. Covers:
- No remote configured: prints "Error: No remote URL configured." and exits 0.
- Local directory as "remote" (remote_mirror): add remote, change local file, run `bit remote check` → exits 1 and reports differences (e.g. "differences found", "size differ", "hash differ").

## unicode.test

Tests Unicode/Hebrew support in bit on Windows. Verifies that:
- `core.quotePath` is set to `false` during `bit init` (so git displays Unicode filenames properly instead of octal escapes)
- Files with Hebrew characters (שלום.txt) can be added, committed, and displayed correctly
- Mixed Unicode filenames (Hebrew קובץ, Arabic ملف, Chinese 文件, Emoji 📁) work end-to-end
- Git commands show actual Unicode characters, not escape sequences like `\327\251...`

This test validates the UTF-8 encoding setup in `Bit.hs` (Windows codepage 65001, locale encoding, stdout/stderr encoding) and the git configuration in `Bit/Core.hs`.
