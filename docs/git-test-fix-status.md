# Git Test Suite Fix — Status

**Date:** 2026-02-22
**Baseline:** 434 pass / 589 fail (1024 test scripts)
**Commits:** 9eab86f, d657bff, c57340b

## Changes Made

### 1. Junction-mode working tree (9eab86f, c57340b)

**Problem:** In junction mode (`BIT_GIT_JUNCTION=1`), `.git` is a symlink to `.bit/index/.git/`. But `runGitRaw` and `runGitRawIn` used `git -C .bit/index`, making git operate on `.bit/index/` as its working tree instead of CWD. Commands like `git add`, `git rm`, `git reset --hard` couldn't find files the user created in CWD.

**Fix:** In junction mode, `runGitRaw` and `runGitRawIn` now delegate to `runGitHere`, which runs git without `-C` so it discovers the repo via the `.git` junction and uses CWD as working tree.

**Files:** `Bit/Git/Run.hs`

### 2. Encoding crash fix (d657bff, c57340b)

**Problem:** `hGetContents` reads with the system text encoding. Non-UTF8 data (e.g. ISO-8859-1 commit messages like `geändert`) caused `cannot decode byte sequence` errors, which then triggered `thread blocked indefinitely in an MVar operation` deadlocks.

**Fix (v1 — d657bff):** Set `hSetBinaryMode` on output handles in `spawnGit`, `runGitHere`, `runGitGlobal`.

**Problem with v1:** Binary mode reads each byte as a separate `Char`. When `putStr` writes these chars, it re-encodes them as UTF-8, causing double-encoding (`geÃÂ¤ndert` instead of `geÃ¤ndert`).

**Fix (v2 — c57340b):** Passthrough functions (`runGitRaw`, `runGitRawAt`, `runGitHere`, `runGitGlobal`) now inherit stdout/stderr handles, so git writes directly to the terminal. No Haskell encoding involvement. Internal functions that capture output (`spawnGit` for `runGit`, `runGitWithOutput`) still use binary mode since they process strings internally.

**Files:** `Bit/Git/Run.hs`

### 3. Remove unnecessary scanAndWrite (9eab86f)

**Problem:** `scanAndWrite` copies all CWD text files into `.bit/index/`, creating untracked files that block subsequent git operations (merge sees dirty working tree, checkout refuses to overwrite).

**Fix:** Removed `scanAndWrite` from dispatch of: `reset`, `mv`, `checkout`, `restore`, `revert`, `merge`.

**Files:** `Bit/Commands.hs`

### 4. Diff-based working tree sync (9eab86f)

**Problem:** In normal (non-junction) mode, commands like `checkout`, `reset --hard`, `pull` modify files in `.bit/index/` but not in CWD.

**Fix:** Added `syncWorkingTreeFromDiff` helper that diffs old vs new HEAD and copies/deletes/renames files between `.bit/index/` and CWD.

**Files:** `Bit/Git/Passthrough.hs`, `Bit/Core.hs`

### 5. Catch-all passthrough (9eab86f)

**Problem:** Unknown git commands (e.g. `git fetch --all`, `git remote -v`) hit the catch-all and errored.

**Fix:** Changed catch-all from error to `Git.runGitRawAt (bitDir </> "index") cmd`.

**Files:** `Bit/Commands.hs`

### 6. Multi-agent coordination (9eab86f, d657bff)

- Split single `test` resource into `test-cli`, `test-binary`, `test-git`
- Removed `install` resource (redundant with `dev-bin/` workflow)
- Linked `CLAUDE_COLLAB.md` from `CLAUDE.md`
- Added never-stop-listening rule

**Files:** `.claude/agents/resources.json`, `CLAUDE.md`, `CLAUDE_COLLAB.md`

## Test Results (sample)

Tests verified with the latest binary (post-c57340b):

| Test | Before | After | Notes |
|------|--------|-------|-------|
| t2020-checkout-detach | FAIL | PASS | Junction-mode checkout works |
| t7102-reset | FAIL | PASS | Encoding fix + junction mode |
| t1005-read-tree-reset | FAIL | PASS | Junction mode |
| t2022-checkout-paths | FAIL | PASS | Junction mode |
| t2012-checkout-last | FAIL | PASS | Junction mode |
| t0001-init | FAIL | FAIL | `--separate-git-dir` + default branch name |
| t7600-merge | FAIL | FAIL | scanAndWrite pollution (pre-existing) |
| t3200-branch | FAIL | FAIL | Needs investigation |
| t3400-rebase | FAIL | FAIL | Needs investigation |

## Remaining Failure Categories

### 1. scanAndWrite pollution (highest impact)
`Scan.writeMetadataFiles` copies ALL CWD text files into `.bit/index/`, creating untracked files that persist across operations. This breaks merge, checkout, and other commands that expect a clean `.bit/index/` working tree. Agent `git-tester-2` is investigating a junction-mode guard to skip scanAndWrite entirely.

### 2. Git version mismatch
Test suite is git v2.47; our real git is v2.48+. Some tests check exact hint text, deprecated command behavior, or remote HEAD handling that changed between versions. These are false failures — not bit bugs.

### 3. Non-deterministic trash directory state
Tests reuse the trash directory from previous runs. If a prior run left the repo in a broken state (e.g. no HEAD), subsequent runs cascade-fail. Deleting the trash directory before running fixes some tests but not all (device-busy on Windows).

### 4. rewriteGitHints lost
Switching to handle inheritance means `rewriteGitHints` no longer filters git hints in passthrough commands. Users will see raw git hints (e.g. "hint: Using 'master' as the name..."). This is cosmetic, not functional. A binary-safe hint filter could be added later.

## Next Steps

1. **Full test suite run** with latest binary to get accurate pass/fail count (estimated improvement: ~100-150 tests fixed)
2. **scanAndWrite junction guard** — skip in junction mode (agent `git-tester-2` working on this)
3. **Investigate t3200-branch** failures
4. **Consider binary-safe hint rewriting** for passthrough commands
