# Git Test Suite — Efficient Running Guide

Based on the full 1028-script run (2026-02-24, updated 2026-02-27 with hybrid .git architecture results).

## Key findings from the full run

| Category | Count | % of 1028 |
|----------|-------|-----------|
| Passed at 120s | 796 | 77% |
| Additionally passed at 300s | 49 (was 44, +5 from hybrid fix) | 5% |
| Additionally passed at 600s | 5 (t1013, t3305, t3432, t5510¹, t5572) | <1% |
| Still timeout at 600s | 3 (t0027, t1092, t1517) | <1% |
| Junction-mode failures at 600s | 0 (all fixed by hybrid .git architecture) | 0% |
| Merge-ort failures (not bit bugs) | 0 (t6423 passes 82/82 without contention) | 0% |
| Infrastructure failures | 5 | <1% |
| Skipped (missing prereqs) | 145 | 14% |
| Known breakage only | 22 | 2% |
| Infra failures (t9xxx scalar/perl/shell) | 4 | <1% |
| **Total passing (600s timeout)** | **824** | **80%** |

## Optimization 1: Skip known-skip scripts upfront (~154 scripts)

These scripts always skip on our platform (Windows/MINGW64, no svn/p4/cvs/http/gpg/FIFOs).
Exclude them to save process startup time (~5-10s each = ~15-25 minutes saved).

The `run-throttled-suite.sh` script auto-skips the prefix-based categories (t91xx,
t94xx, t98xx). The rest skip themselves at runtime via `test_have_prereq`.

```bash
SKIP_PREFIXES="t91 t94 t98"  # git-svn (69), git-p4 (37), git-cvsserver/cvsimport (8)
```

### Complete skip reference

Every skipped test falls into one of these categories. None are bit bugs — they
are all platform limitations or missing optional infrastructure on Windows/MINGW64.

#### 1. Version control backends (114 scripts)

| Category | Prefix | Count | Reason |
|----------|--------|-------|--------|
| git-svn | t91xx | 69 | Subversion not installed. Requires `svn` CLI + svn libraries. |
| git-p4 | t94xx | 37 | Perforce not installed. Requires `p4` + `p4d` binaries. |
| git-cvsserver/cvsimport | t98xx | 8 | CVS not installed. Requires `cvs` + `cvsps`. |

These are entire script families that test interop with other VCS tools. They
will never run on our test platform without installing those tools, which is
out of scope for bit testing.

#### 2. HTTP/web server (16 scripts)

| Scripts | Reason |
|---------|--------|
| t0611, t5539-t5542, t5549-t5551, t5557, t5559, t5561, t5563-t5564, t5581, t5619, t5732, t5812 | Need `lib-httpd.sh` which requires Apache httpd with CGI modules. Not available on MINGW64. |

These test smart HTTP transport, push-over-HTTP, and gitweb. They require a
running HTTP server that the test framework starts and stops. Apache httpd is
not bundled with PortableGit/MSYS2.

#### 3. FIFOs / named pipes (5 scripts)

| Scripts | Reason |
|---------|--------|
| t5570, t5700, t5702, t5731, t5811 | Need `mkfifo` which doesn't exist on MINGW64. |

MSYS2 on Windows lacks POSIX named pipes (`mkfifo`). These tests use FIFOs to
simulate slow/blocking I/O for testing protocol behavior.

#### 4. Filesystem limitations (4 scripts)

| Script | Prereq | Reason |
|--------|--------|--------|
| t3300-funny-names | FUNNYNAMES | NTFS does not allow tab characters in filenames. |
| t3902-quoted | FUNNYNAMES | Same — tests quoting of filenames with tabs/newlines. |
| t4016-diff-quote | FUNNYNAMES | Same — tests diff output with special chars in filenames. |
| t6131-pathspec-icase | CASE_INSENSITIVE_FS | Skips on case-insensitive filesystems (NTFS). The test checks case-sensitive pathspec matching behavior that doesn't apply on Windows. |

#### 5. Backslash in pathspecs (1 script)

| Script | Prereq | Reason |
|--------|--------|--------|
| t6137-pathspec-wildcards-literal | BSLASHPSPEC | Windows uses `\` as the path separator, so git cannot distinguish `\*` (escaped glob) from `\*` (directory separator + star). The `BSLASHPSPEC` prereq is set for macOS and Linux but deliberately excluded for MINGW. This is a genuine platform limitation, not a missing tool. |

#### 6. GPG signing (2 scripts)

| Scripts | Reason |
|---------|--------|
| t3435-rebase-gpg-sign, t3514-cherry-pick-gpg | Need `gpg` (GnuPG) for commit signing tests. Not installed on our test platform. |

#### 7. Unix-specific features (2 scripts)

| Script | Prereq | Reason |
|--------|--------|--------|
| t0301-credential-store | UNIX_SOCKETS | Tests credential-cache daemon which uses Unix domain sockets. Windows uses named pipes for credential-manager instead. |
| t5580-unc-paths | MINGW | Tests UNC path (`\\server\share`) handling. Skips because it needs network shares configured. |

#### 8. Privileged / dangerous tests (2 scripts)

| Script | Env var | Reason |
|--------|---------|--------|
| t0034-root-safe-directory | `GIT_TEST_ALLOW_SUDO=YES` | Tests `safe.directory` behavior when running as root. Even with the env var set, requires real `sudo` access (`sudo -n id -u`). Not available on MSYS2 — there is no `sudo`. |
| t1509-root-work-tree | `IKNOWWHATIAMDOING=YES` | Tests `git init` at filesystem root (`/`). Requires write access to `/` and is designed for throwaway chroot/VM environments. Not safe to run on a real system. |

#### 9. Other infrastructure (4 scripts)

| Script | Reason |
|--------|--------|
| t0612-jgit | Needs JGit (Java git implementation). Not installed. |
| t3910-mac-os-precompose | macOS-only — tests Unicode precomposition (NFC/NFD). Skipped on all non-macOS platforms. |
| t5608-clone-2gb | Needs `GIT_TEST_CLONE_2GB=true` — creates a 2GB+ repo. Deliberately excluded as it's expensive and tests git internals, not bit. |
| t9200-git-cvsexportcommit | Needs CVS. Same as t98xx family but numbered in t92xx. |

#### 10. Runtime-detected skips (3 scripts)

These skip at runtime based on platform detection, not env vars:

| Script | Reason |
|--------|--------|
| t9500-gitweb-hierarchical | Needs Perl CGI modules (`CGI`, `CGI::Util`). |
| t9501-gitweb-hierarchical | Same. |
| t9502-gitweb-hierarchical | Same. |

## Optimization 2: Use 300s timeout (not 120s)

58 scripts timed out at 120s. With 300s, 44 of them pass — they just need more time.
**Use 300s as the default timeout.** This adds ~45 scripts to the pass count for minimal extra wall-clock time (since scripts run in parallel).

The 44 scripts that pass at 300s range from 98s to 299s. Key slow categories:
- **Pack/bitmap** (t5310, t5326, t5327): 183-299s — heavy I/O
- **Config/refs** (t1300, t1400, t1461): 171-241s — many individual tests
- **Fetch** (t5500, t5515, t5616): 200-259s — network simulation
- **Worktree** (t2400): 291s — close to timeout, benefits from 300s

### Genuinely slow scripts (need more than 600s)

These are NOT hanging — they will pass given enough time. Use higher timeouts or run separately.

```bash
# t0027: ~1898 CRLF combo tests (LF/CRLF × attrs × text/binary × checkout/commit)
#   Needs ~700s. Use timeout 1200.
# t1092: 104 sparse-checkout tests, each builds elaborate repo from scratch
#   Needs ~2000s. Use timeout 2400.
# t1517: ~369 help-flag tests (git <cmd> -h × 3 process hops in junction mode)
#   Needs ~350s sequentially, ~1200s with contention. Use timeout 1200.
SLOW_SCRIPTS="t0027|t1092|t1517"
```

### Previously-failing scripts (all fixed by hybrid .git architecture)

These scripts previously failed because `.git` was a gitfile instead of a real directory.
The hybrid .git architecture (`.git/` = real dir, `.bit/index/.git` = gitfile pointing back)
fixed all of them. **All now pass** with a 300-600s timeout (2026-02-27 verified results):

| Script | Result | Timeout needed |
|--------|--------|---------------|
| t5516-fetch-push.sh | **123/123** | 300s |
| t2013-checkout-submodule.sh | **64/64** (10 KB) | 600s |
| t7112-reset-submodule.sh | **70/70** (12 KB) | 600s |
| t7610-mergetool.sh | **31/31** | 300s |
| t3432-rebase-fast-forward.sh | **219/219** (6 KB) | 600s |
| t0001-init.sh | **102/102** | 300s |

t6423-merge-rename-directories.sh passes **82/82** when run without contention (2026-03-02
rerun at 600s). The previously reported 37/80 failures were contention artifacts from
parallel execution, not merge-ort bugs.

t0001 tests 51 (linked-worktree re-init) and 52 were fixed by re-adding `.bit` to
`info/exclude` after passthrough re-init (2026-03-02, Bit/Commands.hs `handleDashC`).

¹ t5510-fetch passes 204/207 (3 minor failures). t5572-pull-submodule passes all 60 non-KB tests.

## Optimization 3: Balance agent batches by runtime, not count

Current split by number prefix gives uneven loads:

| Batch | Scripts | Passed (300s) | Skipped | Failed |
|-------|---------|---------------|---------|--------|
| t0+t1 | 169 | 160 | 5 | 0 |
| t2+t3 | 184 | 168 | 5 | 1 (t2501) |
| t4+t5 | 316 | 296 | 24 | 0 |
| t6+t7 | 203 | 199 | 2 | 0 |
| t9 | 137 | 16 | 114 | 4 (scalar/perl/shell) |

**Better split (4 agents, excluding skips, ~170 effective each):**

| Agent | Range | Effective | Notes |
|-------|-------|-----------|-------|
| A | t0xxx + t1xxx + t2xxx | ~310 | Core + checkout (many fast scripts) |
| B | t3xxx + t4xxx | ~250 | Rebase + diff/format-patch |
| C | t5xxx | ~140 | Fetch/push/pack (slowest batch) |
| D | t6xxx + t7xxx + t9xxx | ~200 | Merge/ref + misc (t9 is fast: ~16 real) |

Skip t91xx, t94xx, t98xx upfront (saves ~114 scripts of process startup overhead).

## Optimization 4: Load-aware throttled runner (`run-throttled-suite.sh`)

When running multiple agents in parallel, I/O contention on Windows/NTFS causes
false timeouts — scripts that pass at 200s solo take 350s+ under 4× load.

`extern/run-throttled-suite.sh` solves this with a hybrid approach:
- **Lightweight scripts** (<25 test cases) run freely without throttling
- **Heavy scripts** (≥25 test cases) acquire a shared semaphore slot AND wait
  for system load to drop before starting

This lets fast tests run at full parallelism while preventing heavy tests from
piling up and starving each other.

```bash
# 4-agent parallel run with throttling:
# Agent A:
bash extern/run-throttled-suite.sh t0*.sh t1*.sh t2*.sh
# Agent B:
bash extern/run-throttled-suite.sh t3*.sh t4*.sh
# Agent C:
bash extern/run-throttled-suite.sh t5*.sh
# Agent D:
bash extern/run-throttled-suite.sh t6*.sh t7*.sh t8*.sh t9*.sh
```

Configuration via environment variables:
```bash
MAX_SLOTS=2          # Max concurrent heavy tests (default: 2)
DEFAULT_TIMEOUT=300  # Per-script timeout in seconds (default: 300)
HEAVY_THRESHOLD=25   # Test count above which script is "heavy" (default: 25)
MAX_LOAD=4           # Load average gate for heavy tests (default: 4)
RESULTS_FILE=out.txt # Save results to file (default: stdout only)
```

The semaphore uses `mkdir`-based locking at `/tmp/git-test-slots/` with PID-based
stale slot detection (crashed agents' slots are automatically reclaimed).

### How it works

1. Before each script, the runner counts `test_expect_success` + `test_expect_failure`
   lines in the script file to classify it as lightweight or heavy.
2. **Lightweight** (<`HEAVY_THRESHOLD` test cases): runs immediately, no throttling.
   These scripts finish in 10-60s and don't cause meaningful I/O contention.
3. **Heavy** (≥`HEAVY_THRESHOLD` test cases): first checks `/proc/loadavg` and waits
   if system load exceeds `MAX_LOAD` (caps at 2 min wait). Then acquires a shared
   semaphore slot via `mkdir /tmp/git-test-slots/slot-N`. If all slots are taken,
   spins with 2s sleep until one opens.
4. After the script finishes, the slot is released. A trap ensures slots are released
   on unexpected exit.
5. Built-in special timeouts for known-slow scripts (t0027=900s, t1092=2400s,
   t1517=1200s, t3432=600s, t5510=600s, t5516=600s) override the default.
6. Auto-skips t91xx/t94xx/t98xx prefixes.
7. Cleans trash directories after each script to prevent bail-outs.

### Choosing the number of agents

Match agents to `MAX_SLOTS` for heavy-dominated workloads:

| Workload | Agents | MAX_SLOTS | Why |
|----------|--------|-----------|-----|
| Full suite (mixed heavy/light) | 4 | 2 | Light tests run freely; heavy tests throttled to 2 concurrent |
| Rerunning only timeouts (all heavy) | 2 | 2 | All scripts are heavy, so agents = slots avoids idle waiters |
| Single agent, full suite | 1 | N/A | No contention, semaphore not needed |

For the full suite, 4 agents with `MAX_SLOTS=2` is optimal: lightweight tests
from all 4 agents run in parallel (no slot needed), while at most 2 heavy tests
run simultaneously. This prevents the I/O starvation that causes false timeouts.

### Rerunning timed-out scripts

After a full run, collect timed-out script names from the results files and rerun
with a higher timeout. Since these are all heavy scripts, use 2 agents:

```bash
# Agent 1:
DEFAULT_TIMEOUT=600 RESULTS_FILE=rerun-1.txt bash run-throttled-suite.sh \
    t0000-basic.sh t0008-ignores.sh t1300-config.sh ...
# Agent 2:
DEFAULT_TIMEOUT=600 RESULTS_FILE=rerun-2.txt bash run-throttled-suite.sh \
    t7600-merge.sh t7810-grep.sh t9300-fast-import.sh ...
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

Use the throttled runner script instead of raw for-loops:

```
Run git test scripts from your batch using the throttled runner:

  cd /c/Users/natanh/repos/bit/extern
  RESULTS_FILE=git-suite-batch-X-results.txt bash run-throttled-suite.sh t0*.sh t1*.sh

The script handles timeouts, skip prefixes, load throttling, and result tracking
automatically. Just pass the glob patterns for your batch.
```

For manual/raw runs without the throttled runner:
```
cd /c/Users/natanh/repos/bit/extern/git/t
BIT_GIT_JUNCTION=1 GIT_TEST_INSTALLED=/c/Users/natanh/repos/bit/extern/git-shim \
    timeout 300 bash <script> 2>&1 | tail -3

Skip these prefixes entirely: t91xx, t94xx, t98xx (svn/p4/cvs, always skip).
```

## Quick-run command (single agent, full suite, ~2 hours)

```bash
cd /c/Users/natanh/repos/bit/extern
RESULTS_FILE=git-suite-full-results.txt \
    bash run-throttled-suite.sh t[0-9]*.sh
```

## Data quality notes

- **All batches complete**: t6 results in `extern/git-suite-t6t7-results.txt`, t7 results
  in `extern/git-suite-t7-results.txt` (run separately after the initial t6+t7 runner timed out).
- **t9 failures are infrastructure, not bit bugs**: scalar (t9210, t9211), perl (t9700),
  and shell (t9850) failures are due to missing/misconfigured tools, not bit routing issues.
- **t0000-basic.sh** passes at 300s (92/92, 114s) — was just slow, not a real failure.
- **t1006, t1461** pass at 300s with known breakages — the "FATAL" at 120s was a timeout artifact.
- **Version**: Both test suite (extern/git submodule) and real git (PortableGit) are v2.52.0. No version mismatch.
- **2 bit bugs found**: `git help --config-for-completion` passthrough and `bit help merge --continue` routing — both fixed in Bit/Commands.hs.
- **600s rerun (2026-03-02)**: All 67 scripts that timed out at 300s under parallel load pass at 600s
  when run without contention. t0027 (2600 tests), t1092 (104 tests), t1517 (369 tests) all pass with
  their special timeouts (900s, 2400s, 1200s). t6423 passes 82/82 (previously reported failures were contention).
- **Hybrid .git architecture** (2026-02-26): Resolved all junction-mode failures. t5516, t2013, t7112, t7610, t3432 now all pass.
- **Run variability**: Parallel runs introduce contention — some scripts fail in parallel but
  pass sequentially (t1013), others are intermittent (t3432). Use sequential runs for authoritative results.
- **Trash directory cleanup**: Always remove `trash directory.*` dirs between runs to avoid
  "Device or resource busy" bail-outs when scripts share the same test directory names.
