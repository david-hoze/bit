# t0001-init.sh Test Status

Current results from running Git's `t0001-init.sh` test suite against bit.

## Summary

| Metric | Count |
|--------|-------|
| Total tests | 91 |
| Pass | 83 |
| Fail | 8 |

## Full Run Results

### Passing tests (83)

1, 2, 3, 4, 5, 7, 8, 9, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 43, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91

### Failing tests (8)

| Test | Name | Category |
|------|------|----------|
| 6 | No extra GIT_* on alias scripts | GIT_DIR/env |
| 10 | GIT_DIR bare | GIT_DIR/env |
| 13 | GIT_DIR & GIT_WORK_TREE (1) | GIT_DIR/env |
| 41 | re-init to update git link | Separate gitdir |
| 42 | re-init to move gitdir | Separate gitdir |
| 44 | re-init to move gitdir with linked worktrees | Linked worktree |
| 45 | re-init to move gitdir within linked worktree | Linked worktree |
| 59 | extensions.refStorage with files backend | Ref format |

## Failures by Category

### GIT_DIR / GIT_WORK_TREE environment (3 tests: 6, 10, 13)

bit does not honor `GIT_DIR` and `GIT_WORK_TREE` when they point to non-standard locations. The alias test (6) expects no extra `GIT_*` variables to leak through.

### Separate git dir / re-init with gitdir move (2 tests: 41, 42)

bit supports `--separate-git-dir` for initial init, but re-init scenarios that move or update the gitdir link are not handled.

### Linked worktrees (2 tests: 44, 45)

Linked worktree support is not implemented. Re-init and gitdir move within linked worktrees fail.

### Ref format (1 test: 59)

`extensions.refStorage` validation with files backend. bit does not intercept or validate ref storage format settings.

## History

| Date | Pass | Fail | Notes |
|------|------|------|-------|
| 2026-02-15 (v1) | 41 | 50 | Before init fixes |
| 2026-02-15 (v2) | 48 | 43 | After init root cause fixes (5 fixes) |
| 2026-02-15 (v3) | 75 | 16 | After scan cache crash fix (`.git` filter in `collectScannedPaths`) |
| 2026-02-15 (v4) | 83 | 8 | Template path resolution, shared repo config, GIT_DIR=/dev/null bypass, `.git` junction passthrough |
