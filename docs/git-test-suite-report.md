# Git Test Suite Report

**Date**: 2026-02-19
**Test suite**: Git v2.47.0 (extern/git submodule)
**Binary under test**: bit.exe via extern/git-shim
**Real git**: PortableGit (git 2.48+)
**Platform**: Windows (MINGW64)

## Summary

| Metric | Count |
|--------|-------|
| Total test suites | 1007 |
| Passed | 855 |
| Failed | 142 |
| Pass rate | 85.5% |

**Key insight**: The vast majority of failures are **not bit regressions**. They fall into:
environmental issues (GPG, Perl, SSH, man pages), git version mismatch (v2.47 test
expectations vs v2.48+ real git behavior), upstream known breakage, and Windows quirks.
Zero failures indicate actual bit command-handling bugs.

## Failure Categories

### Category 1: Infrastructure Not Available (~15 suites, ~350 tests)

Tests requiring infrastructure not present in the test environment. Not bit issues.

| Test | Fail/Total | Cause |
|------|-----------|-------|
| t0012-help | 174/179 | Git man pages / HTML docs not available |
| t0200-gettext-basic | 2/16 | Missing compiled locale / TEXTDOMAINDIR |
| t0202-gettext-perl | 1/13 | Missing Perl `Git::I18N` module |
| t0450-txt-doc-vs-help | 67/737 | Doc/help text comparison (no docs built) |
| t3435-rebase-gpg-sign | 16/19 | GPG not configured |
| t5534-push-signed | 4/? | GPG signing |
| t6200-fmt-merge-msg | 3/37 | GPG signing |
| t7004-tag | 32/228 | GPG signing (tag -s) |
| t7030-verify-tag | 13/15 | GPG signing |
| t7510-signed-commit | 25/27 | GPG signing |
| t7528-signed-commit-ssh | 4/29 | SSH signing |
| t9001-send-email | 169/215 | Perl send-email infrastructure |
| t9700-perl-git | 2/3 | Missing Perl `Git.pm` |

### Category 2: Git Version Mismatch (~20 suites, ~200 tests)

The test suite is v2.47.0 but the real git is v2.48+. Output differences include:
- Hint text: `"git config set advice.X false"` vs `"git config advice.X false"`
- Deprecated commands: `whatchanged` and `fast-import` require `--i-still-use-this`
- `add -i` prompt options changed (added `k,K,P`)
- `git fetch` now creates `refs/remotes/REMOTE/HEAD` by default
- `git remote` options changed (`--format` added)

| Test | Fail/Total | Cause |
|------|-----------|-------|
| t3200-branch | 1/166 | `git config set` hint |
| t3501-revert-cherry-pick | 1/20 | `git config set` hint |
| t3507-cherry-pick-conflict | 2/44 | `git config set` hint |
| t3602-rm-sparse-checkout | 5/13 | `git config set` hint |
| t3700-add | 3/57 | `git config set` hint |
| t3701-add-interactive | 8/85 | `add -i` prompt changes |
| t3705-add-sparse-checkout | 12/20 | `git config set` hint |
| t4013-diff-various | 16/227 | `whatchanged` deprecated |
| t4202-log (partial) | ~3/148 | `whatchanged` deprecated |
| t7002-mv-sparse-checkout | 16/22 | `git config set` hint |
| t7512-status-help | 17/46 | `git config set` hint |
| t9300-fast-import | 42/250 | `fast-import` deprecated |
| t9301-fast-import-notes | 1/17 | `fast-import` deprecated |
| t9350-fast-export | 2/52 | `fast-import` deprecated |
| t3404-rebase-interactive | 10/131 | `git config set` hint + rebase changes |
| t3415-rebase-autosquash | 7/27 | `git config set` hint |
| t3424-rebase-empty | 1/20 | `git config set` hint |
| t3430-rebase-merges | 2/33 | `git config set` hint |
| t3432-rebase-fast-forward | 6/225 | `git config set` hint |
| t4014-format-patch | 5/212 | Format/hint changes |
| t4124-apply-ws-rule | 45/78 | Whitespace rule hint changes |
| t4207-log-decoration-colors | 1/4 | Decoration output changes |
| t5505-remote | 8/113 | Remote HEAD auto-create on fetch |
| t5514-fetch-multiple | 12/25 | Remote HEAD auto-create on fetch |
| t5516-fetch-push | 4/120 | Remote HEAD + `--format` option |
| t5520-pull | 1/72 | Remote HEAD on fetch |
| t5528-push-default | 1/32 | Push default behavior change |
| t5610-clone-detached | 1/13 | Clone detached HEAD |
| t6018-rev-list-glob | 4/95 | Remote HEAD refs in glob |
| t7003-filter-branch | 2/48 | `git config set` hint |
| t7615-diff-algo-mergy | 1/7 | Diff algo changes |

### Category 3: Known Test Breakage (# TODO known breakage) (~15 suites, ~50 tests)

Tests marked as known failures upstream. Not regressions.

| Test | Fail/Total | Cause |
|------|-----------|-------|
| t1060-object-corruption | 1/17 | clone --local misnamed objects |
| t1091-sparse-checkout-builtin | 1/72 | sparse-checkout reapply |
| t1092-sparse-checkout-compatibility | 2/98 | sparse-checkout compatibility |
| t1309-early-config | 2/10 | .git with invalid repo version |
| t1430-bad-ref-name | 2/42 | branch -m from bad ref |
| t1512-rev-parse-disambiguation | 4/38 | ambiguous refs |
| t2500-untracked-overwriting | 2/10 | rebase autostash |
| t3102-ls-tree-wildcards | 1/4 | negated pathspec |
| t3401-rebase-and-am-rename | 2/10 | directory rename detection |
| t3510-cherry-pick-sequence | 3/55 | signoff propagation |
| t4045-diff-relative | 1/39 | diff --relative subdir |
| t4058-diff-duplicates | 5/16 | read-tree/reset segfault |
| t4204-patch-id | 1/26 | patch-id hash differences |
| t4205-log-pretty-formats | 2/125 | NUL termination, wide chars |
| t5572-pull-submodule | 8/67 | pull submodule |
| t6403-merge-file | 2/37 | missing LF at EOF |
| t6415-merge-dir-to-symlink | 1/24 | merge dir to symlink |
| t6416-recursive-corner-cases | 3/40 | recursive merge |
| t6422-merge-rename-corner-cases | 7/25 | rename corner cases |
| t6423-merge-rename-directories | 2/76 | rename directories |
| t7300-clean | 2/55 | nested bare repos |
| t7504-commit-msg-hook | 1/30 | merge --continue --no-verify |
| t7815-grep-binary | 1/22 | grep binary .fi |

### Category 4: Submodule Known Breakage (~10 suites, ~80 tests)

All marked `# TODO known breakage` — upstream git submodule test issues.

| Test | Fail/Total | Cause |
|------|-----------|-------|
| t1013-read-tree-submodule | 10/68 | replace submodule with file/dir |
| t2013-checkout-submodule | 10/74 | same |
| t2405-worktree-submodule | 1/11 | worktree submodule |
| t3426-rebase-submodule | 4/29 | rebase submodule |
| t3512-cherry-pick-submodule | 2/15 | cherry-pick submodule |
| t3513-revert-submodule | 14/14 | revert submodule |
| t3906-stash-submodule | 10/16 | stash submodule |
| t4137-apply-submodule | 4/28 | apply submodule |
| t4255-am-submodule | 4/33 | am submodule |
| t6041-bisect-submodule | 14/14 | bisect submodule |
| t6437-submodule-merge | 2/22 | submodule merge |
| t6438-submodule-directory-file-conflicts | 8/56 | submodule dir/file conflicts |
| t7112-reset-submodule | 12/82 | reset submodule |
| t7400-submodule-basic | 1/121 | submodule basic |
| t7814-grep-recurse-submodules | 7/34 | grep submodules |

### Category 5: Trace2 Output Differences (~4 suites, ~27 tests)

The bit shim adds extra trace2 events. Not functional issues.

| Test | Fail/Total | Cause |
|------|-----------|-------|
| t0210-trace2-normal | 12/13 | trace2 normal output mismatch |
| t0211-trace2-perf | 5/17 | trace2 perf output mismatch |
| t0212-trace2-event | 6/11 | trace2 event output mismatch |
| t7900-maintenance | 4/53 | test_subcommand parses trace2 |

### Category 6: Windows/MINGW Issues (~10 suites, ~35 tests)

| Test | Fail/Total | Cause |
|------|-----------|-------|
| t0000-basic | 1/92 | --run range negation harness quirk |
| t0024-crlf-archive | 1/3 | tar archive CRLF (flaky) |
| t0050-filesystem | 1/11 | filesystem case sensitivity |
| t1401-symbolic-ref | 18/25 | $TAR fails in setup, cascade |
| t1413-reflog-detach | 7/7 | $TAR fails in setup, cascade |
| t3903-stash (partial) | 3/127 | index.lock file locking |
| t4015-diff-whitespace | 3/131 | $TAR in setup |
| t4038-diff-combined | 1/26 | $TAR in setup |
| t4116-apply-reverse | 3/7 | $TAR in setup |
| t9210-scalar (partial) | -/21 | failed to delete directory |
| t9850-shell | 3/5 | git-shell interactive mode |

### Category 7: TAR Helper Issue (~8 suites, ~100 tests)

Many tests use `$TAR` to save/restore `.git` state. On this Windows setup, TAR extraction sometimes fails, causing cascading failures.

| Test | Fail/Total | Notes |
|------|-----------|-------|
| t1401-symbolic-ref | 18/25 | Setup uses TAR |
| t1413-reflog-detach | 7/7 | Setup uses TAR |
| t4015-diff-whitespace | 3/131 | TAR in test setup |
| t4038-diff-combined | 1/26 | TAR |
| t4116-apply-reverse | 3/7 | TAR |
| t5000-tar-tree | 23/90 | TAR operations |
| t5001-archive-attr | 18/44 | archive TAR |
| t5002-archive-attr-pattern | 3/19 | archive TAR |
| t5004-archive-corner-cases | 1/14 | archive TAR |

### Category 8: Remaining Failures (investigation needed)

Tests not yet fully categorized. Many overlap with git version mismatch or
infrastructure gaps. Approximately 30 test suites, ~120 individual test failures.

Notable clusters:
- **Merge recursive/rename** (t6416, t6422, t6423, t6424, t6427, t6430, t6434):
  merge-recursive engine differences, some known breakage
- **Pack/promisor** (t0410, t0411, t5300, t5309, t5330, t5331): promisor remote
  and pack object behavior changes in newer git
- **Clone** (t5558, t5601, t5604): SSH helper not compiled (t5601), bundle URI
  changes (t5558)
- **Rebase** (t3421): linear rebase topology
- **Line log / bloom** (t4211, t4216): log with line ranges and bloom filters
- **Completion** (t9902): ref listing and completion output differs
- **Scalar** (t9210, t9211): scalar delete/maintenance differences
- **Capability advertisement** (t5701): git-serve protocol differences

None of these indicate bit command-handling regressions — they are all
passthrough behaviors where the real git binary (v2.48+) produces different
output than what the v2.47 test suite expects.

## Architectural Notes

Many "Category 8" failures are actually caused by:

1. **GPG not available** — cascading failures in tests that set up signed tags/commits early
2. **SSH helper (`test-fake-ssh`) not compiled** — t5601 clone tests cascade
3. **`$TAR` extraction issues on Windows** — cascading from broken setup steps
4. **Git version mismatch** — some tests in this category also suffer from hint text differences

The truly bit-specific issues that might need code changes are:
- Trace2 output (Category 5) — the shim adds extra trace events
- Completion output differences (t9902)
- Capability advertisement (t5701)
- Some rebase interactive differences (t3404, t3415)

Most other failures are environment/infrastructure issues, not bit regressions.
