# Git Test Suite Compatibility

## Git Shim (`extern/git-shim/`)

The shim allows running git's own test suite with bit acting as git. It sets two environment variables and delegates everything to bit:

- **`BIT_REAL_GIT`**: Path to the real git binary. Prevents recursion when bit's internal git calls would go through the shim. `spawnGit` in `Bit/Git/Run.hs` checks this variable and uses the real git instead of the shim.
- **`BIT_GIT_JUNCTION=1`**: Tells `initializeRepoAt` to create a `.git` directory junction pointing to `.bit/index/.git` after creating a repo. This allows git's repo discovery to work naturally -- tests that do `cd .git && git config` find the git directory through the junction.

The junction uses Windows `mklink /j` (directory junction), which does not require admin privileges. It is only created when `BIT_GIT_JUNCTION=1` is set -- normal bit usage does not create junctions.

### Junction Mode Adaptations

In junction mode, several subsystems adapt:
- The scanner skips `.bit` opaque boundary checks
- Subrepo detection uses `pathIsSymbolicLink` to distinguish `.git` junctions from real directories
- `ls-files -o` runs without `-C .bit/index` so pathspecs resolve from the actual CWD
- `syncSubmoduleToWorkingDirectory` is skipped since git operates on `.bit/index/` directly via the junction

---

## Running Tests

```bash
cd extern/git/t
GIT_TEST_INSTALLED=/path/to/extern/git-shim bash t0001-init.sh --verbose
```

---

## Test Results Summary

| Test Suite | Pass | Fail | Notes |
|------------|------|------|-------|
| t0001-init.sh | 91/91 | 0 | All init tests pass |
| t0002-gitfile.sh | 14/14 | 0 | All gitfile tests pass |
| t0003-attributes.sh | 54/54 | 0 | All attribute tests pass (including test 52: submodule `builtin_objectmode`) |
| t0004-unwritable.sh | 9/9 | 0 | 8 skipped (missing POSIXPERM/SANITY -- not available on Windows) |
| t0005-signals.sh | 5/5 | 0 | 3 skipped (missing !MINGW -- signal propagation not available on Windows) |
| t0006-date.sh | 129/129 | 0 | All date parsing tests pass |
| t0007-git-var.sh | 27/27 | 0 | 2 skipped (missing !AUTOIDENT, POSIXPERM) |
| t0008-ignores.sh | 397/397 | 0 | All ignore tests pass (junction detection distinguishes `.git` junctions from real dirs) |
| t0010-racy-git.sh | 10/10 | 0 | All racy git tests pass |
| t0012-help.sh | 5/179 | 174 | Help tests require git's man pages / html docs (not available via shim) |
| t0013-sha1dc.sh | 1/1 | 0 | SHA-1 collision detection passes |
| t0014-alias.sh | 5/5 | 0 | All alias tests pass (loop detection + config passthrough) |
| CLI tests | 57/57 files (984 tests) | 0 | All 57 test files pass (including network-remote and gdrive-remote) |
