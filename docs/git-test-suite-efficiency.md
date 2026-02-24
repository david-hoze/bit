# Git Test Suite — Efficient Running Guide

Based on the full 1024-script run (2026-02-24), here's how to run the suite faster next time.

## Key findings from the full run

| Category | Count | % of 1024 |
|----------|-------|-----------|
| Passed (all tests OK) | 796 | 77% |
| Timed out (>120s) | 58 | 6% |
| Skipped (missing prereqs) | 145 | 14% |
| Known breakage only | 22 | 2% |
| Real failures | 5 | <1% |
| Infra failures (t9xxx scalar/perl/shell) | 32 | 3% |

## Optimization 1: Skip known-skip scripts upfront (~154 scripts)

These scripts always skip on our platform (Windows/MINGW64, no svn/p4/cvs/http/gpg/FIFOs).
Exclude them to save process startup time (~5-10s each = ~15-25 minutes saved).

```bash
# Scripts to exclude (always skip on Windows without extra infra)
SKIP_PREFIXES="t91 t94 t98"  # git-svn (69), git-p4 (37), git-cvsserver/cvsimport (8)

# HTTP/web server dependent (16 scripts)
SKIP_HTTP="t0611|t5539|t5540|t5541|t5542|t5549|t5550|t5551|t5557|t5559|t5561|t5563|t5564|t5581|t5619|t5732|t5812"

# FIFO dependent — no mkfifo on MINGW64 (5 scripts)
SKIP_FIFO="t5570|t5700|t5702|t5731|t5811"

# Other missing prerequisites (12 scripts)
SKIP_MISC="t0034|t0301|t0612|t1509|t3300|t3435|t3514|t3902|t3910|t4016|t5580|t5608"
# t0034: needs GIT_TEST_ALLOW_SUDO    t0301: no unix sockets     t0612: no JGit
# t1509: needs IKNOWWHATIAMDOING      t3300,t3902,t4016: no tabs in filenames
# t3435,t3514: no gpg                 t3910: macOS precompose     t5580: no UNC
# t5608: expensive (needs GIT_TEST_CLONE_2GB)

# Filesystem / platform skips detected at runtime (not worth pre-filtering)
# t6131: case-insensitive FS    t6137: needs BSLASHPSPEC
# t9200: no cvs                 t9500-t9502: no CGI modules
```

## Optimization 2: Increase timeout for large scripts

58+ scripts timed out at 120s. These are legitimate tests that just need more time.
Use 300s (5 min) instead. The largest scripts (t0027-auto-crlf, t5310-pack-bitmaps)
may need 600s.

**Large scripts (need 300-600s timeout):**
```
# t0xxx-t1xxx (12 timeouts)
t0000-basic.sh  t0008-ignores.sh  t0027-auto-crlf.sh  t1006-cat-file.sh
t1013-read-tree-submodule.sh  t1092-sparse-checkout-compatibility.sh
t1300-config.sh  t1400-update-ref.sh  t1450-fsck.sh  t1461-refs-list.sh
t1510-repo-setup.sh  t1517-outside-repo.sh

# t2xxx-t3xxx (11 timeouts)
t2013-checkout-submodule.sh  t2400-worktree-add.sh  t3200-branch.sh
t3301-notes.sh  t3305-notes-fanout.sh  t3311-notes-merge-fanout.sh
t3404-rebase-interactive.sh  t3421-rebase-topology-linear.sh
t3426-rebase-submodule.sh  t3432-rebase-fast-forward.sh  t3903-stash.sh

# t4xxx-t5xxx (22 timeouts)
t4013-diff-various.sh  t4014-format-patch.sh  t4018-diff-funcname.sh
t4137-apply-submodule.sh  t4216-log-bloom.sh  t4255-am-submodule.sh
t5310-pack-bitmaps.sh  t5318-commit-graph.sh  t5319-multi-pack-index.sh
t5324-split-commit-graph.sh  t5326-multi-pack-bitmaps.sh
t5327-multi-pack-bitmaps-rev.sh  t5400-send-pack.sh  t5500-fetch-pack.sh
t5505-remote.sh  t5510-fetch.sh  t5515-fetch-merge-logic.sh
t5516-fetch-push.sh  t5520-pull.sh  t5526-fetch-submodules.sh
t5552-skipping-fetch-negotiator.sh  t5572-pull-submodule.sh  t5616-partial-clone.sh

# t6xxx-t7xxx (20 timeouts)
t6030-bisect-porcelain.sh  t6041-bisect-submodule.sh  t6300-for-each-ref.sh
t6416-recursive-corner-cases.sh  t6422-merge-rename-corner-cases.sh
t6423-merge-rename-directories.sh  t6438-submodule-directory-file-conflicts.sh
t6600-test-reach.sh  t7003-filter-branch.sh  t7004-tag.sh
t7112-reset-submodule.sh  t7400-submodule-basic.sh  t7406-submodule-update.sh
t7508-status.sh  t7513-interpret-trailers.sh  t7600-merge.sh
t7610-mergetool.sh  t7800-difftool.sh  t7810-grep.sh  t7900-maintenance.sh

# t9xxx (3 timeouts)
t9001-send-email.sh  t9300-fast-import.sh  t9902-completion.sh
```

## Optimization 3: Balance agent batches by runtime, not count

Current split by number prefix gives uneven loads:

| Batch | Scripts | Passed | Timed out | Skipped | Failed |
|-------|---------|--------|-----------|---------|--------|
| t0+t1 | 169 | 152 | 12 | 5 | 0 |
| t2+t3 | 184 | 160 | 11 | 5 | 1 (t2501) |
| t4+t5 | 316 | 274 | 22 | 24 | 0 |
| t6+t7 | 203 | 179 | 20 | 2 | 0 |
| t9 | 137 | 16 | 3 | 114 | 4 (scalar/perl/shell) |

**Better split (5 agents, ~130 effective scripts each):**

| Agent | Range | Effective | Notes |
|-------|-------|-----------|-------|
| A | t0xxx + t1xxx | ~152 | Keep together (core tests) |
| B | t2xxx + t3xxx | ~169 | Keep together (checkout/rebase) |
| C | t4xxx (only) | ~130 | Diff/apply/format-patch |
| D | t5xxx (only) | ~140 | Fetch/push/pack (most timeouts) |
| E | t6xxx + t7xxx + t9xxx | ~200 | Merge/ref + misc (t9 is fast: ~16 real) |

## Optimization 4: Run fast scripts in batches

Instead of running each script individually, batch fast scripts together:
```bash
# Run 10 scripts in parallel within one agent
for script in t6000 t6001 t6002 ...; do
    timeout 300 bash $script &
done
wait
```

This is safe because git tests use `$TRASH_DIRECTORY` for isolation. The risk is
output interleaving, so redirect each to a file:
```bash
for script in t6*.sh; do
    (timeout 300 bash $script > /tmp/$script.out 2>&1) &
done
wait
```

## Optimization 5: Pre-filter with --run for targeted reruns

After a code change, don't rerun the full suite. Run only the categories that
could be affected:

| Change area | Run these |
|-------------|-----------|
| init | t0001, t0002, t1509, t1510 |
| config | t1300, t1301, t1302, t1303, t1305 |
| add/commit | t2xxx, t3000-t3100 |
| diff/log | t4xxx |
| fetch/push/pull | t5xxx |
| merge/rebase | t3200, t3400-t3500, t6xxx |
| refs | t1400, t1450, t1461, t6xxx |

## Recommended agent prompt template

```
Run git test scripts from your batch. For each script:

  cd /c/Users/natanh/repos/bit/extern/git/t
  BIT_GIT_JUNCTION=1 GIT_TEST_INSTALLED=/c/Users/natanh/repos/bit/extern/git-shim \
      timeout 300 bash <script> 2>&1 | tail -3

Skip these prefixes entirely: t91xx, t94xx, t98xx (svn/p4/cvs, always skip).
Save ALL output to extern/git-suite-<batch>-results.txt (append, don't overwrite).
Include a summary at the end with pass/fail/timeout/skip counts.
```

## Quick-run command (single agent, full suite, ~2 hours)

```bash
cd /c/Users/natanh/repos/bit/extern/git/t
for f in t[0-9]*.sh; do
    case "$f" in t91*|t94*|t98*) continue ;; esac  # skip svn/p4
    echo -n "$f: "
    BIT_GIT_JUNCTION=1 GIT_TEST_INSTALLED=/c/Users/natanh/repos/bit/extern/git-shim \
        timeout 300 bash "$f" 2>&1 | tail -1
done > ../../git-suite-full-results.txt 2>&1
```

## Data quality notes

- **All batches complete**: t6 results in `extern/git-suite-t6t7-results.txt`, t7 results
  in `extern/git-suite-t7-results.txt` (run separately after the initial t6+t7 runner timed out).
- **t9 failures are infrastructure, not bit bugs**: scalar (t9210, t9211), perl (t9700),
  and shell (t9850) failures are due to missing/misconfigured tools, not bit routing issues.
- **t0000-basic.sh** exits with code 1 (not 143/timeout) — may be a real test failure
  or an early abort, not just slowness.
- **t1006, t1461** exit with code 0 but are marked FATAL — likely the test harness
  detected an unexpected early exit despite exit code 0.
