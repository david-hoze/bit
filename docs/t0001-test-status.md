# t0001-init.sh Test Status

Current results from running Git's `t0001-init.sh` test suite against bit.

## Summary

| Metric | Count |
|--------|-------|
| Total tests | 91 |
| Pass | 91 |
| Fail | 0 |

## Full Run Results

### Passing tests (91)

1–91 (all tests pass)

### Failing tests

None.

## History

| Date | Pass | Fail | Notes |
|------|------|------|-------|
| 2026-02-15 (v1) | 41 | 50 | Before init fixes |
| 2026-02-15 (v2) | 48 | 43 | After init root cause fixes (5 fixes) |
| 2026-02-15 (v3) | 75 | 16 | After scan cache crash fix (`.git` filter in `collectScannedPaths`) |
| 2026-02-15 (v4) | 83 | 8 | Template path resolution, shared repo config, GIT_DIR=/dev/null bypass, `.git` junction passthrough |
| 2026-02-15 (v5) | 91 | 0 | GIT_DIR bypass, shell alias passthrough, re-init with --separate-git-dir, linked worktree support |

## Fixes in v5

### GIT_DIR passthrough (tests 10, 13)
When `GIT_DIR` is set (any non-empty value), bit passes through to git directly without `.bit` interception.

### Shell alias passthrough (test 6)
Shell aliases (`!./script`) now run via `runGitGlobal` (no `-c color.ui=auto`), preventing `GIT_CONFIG_*` env vars from leaking into alias scripts.

### Local git directory detection (tests 10, 13)
When CWD is a git directory (has `HEAD` + `refs/`) but not a bit repo, bit passes through to git. This prevents `findBitRoot` from walking up to a parent `.bit/` repo.

### Re-init with --separate-git-dir (tests 41, 42, 44)
On re-init, bit resolves the existing `.bit/index` directory and runs git init from `targetDir` (not `.bit/index`) to avoid CWD-inside-renamed-dir issues on Windows. Junction-to-gitlink normalization before git init, and gitlink recreation after git moves the database.

### handleDashC cd+runCommand (test 59)
`-C <dir>` for bit repos now cd's to the target and runs through normal bit dispatch, ensuring correct path resolution for scan, commit, and init operations.
