# Binary Test Traceability: Git Source → Bit Test

This document maps each bit binary test script (`test/t/t00*.sh`) back to the specific Git test suite cases (`extern/git/t/`) that inspired it. Use this for:

- **Validation**: When a git test changes upstream, find the corresponding bit test to update.
- **Extraction**: When looking for new test ideas, check the "Not yet covered" sections.
- **Auditing**: Verify that a git behavior tested upstream has a bit-side counterpart.

---

## t0001 — `binary-add-commit.sh` (28 tests)

**Git sources:** `t3700-add.sh`, `t7501-commit-basic-functionality.sh`

Neither source file has binary-specific test cases. The bit test borrows the add/commit structural pattern and applies it to bit's binary metadata classification.

| bit test case | Git source case | Notes |
|---|---|---|
| NUL byte detection, extension classification | (bit-specific) | No git equivalent — bit classifies files by content/extension |
| metadata format (`hash:` + `size:`) | (bit-specific) | Git stores binary blobs; bit stores metadata stubs |
| `bit add` stages binary file | t3700 `'Test of git add'` (line 26) | Same pattern, different content type |
| `bit commit` records binary | t7501 `'setup: initial commit'` (line 30) | Same pattern |
| add file in subdirectory | t3700 `'git add --refresh'` (line 219) | Structural analogue |
| large file (64KB) classification | (bit-specific) | Bit's 1MB threshold has no git equivalent |
| empty file classification | (bit-specific) | Bit classifies empty files as text |

---

## t0002 — `binary-status-diff.sh` (21 tests)

**Git sources:** `t4012-diff-binary.sh`, `t4031-diff-rewrite-binary.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| status shows modified binary | t4012 `'diff --shortstat output for binary file change only'` (line 46) | |
| diff shows binary change | t4012 `'diff-index with --binary'` (line 88) | |
| diff --stat with binary | t4012 `'apply --stat output for binary file change'` (line 34) | |
| diff --stat with binary | t4012 `'diff --shortstat output for binary file change'` (line 40) | |
| `bit rm` deletion tracking | (bit-specific) | Bit requires `bit rm`, not `rm` + `add` |
| binary-to-text type change | t4031 `'vanilla diff is binary'` (line 30) | Inverted: what happens when binary becomes text |
| text-to-binary type change | t4031 `'rewrite diff is binary'` (line 35) | |

---

## t0003 — `binary-merge.sh` (21 tests)

**Git sources:** `t6407-merge-binary.sh`, `t4048-diff-combined-binary.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| two-repo setup with filesystem remote | (bit-specific) | Git tests use single-repo branches; bit uses push/pull |
| fast-forward merge with binary | t6407 `'resolve'` (line 40) | Clean merge path |
| clean parallel merge (different files) | t6407 `'recursive'` (line 50) | Non-overlapping changes |
| binary conflict detection | t6407 `'resolve'` (line 40), t4048 `'setup binary merge conflict'` (line 9) | Both sides modify same binary |
| delete+modify merge | (bit-specific) | One side deletes, other modifies |
| subdirectory binary merge | (bit-specific) | Extension of t6407 setup |

---

## t0004 — `binary-push-pull.sh` (21 tests)

**Git sources:** `t5400-send-pack.sh`, `t5500-fetch-pack.sh`

No binary-specific cases in either source. Bit borrows the two-repo push/pull topology for binary content integrity testing.

| bit test case | Git source case | Notes |
|---|---|---|
| push binary file | t5400 `'pack the source repository'` (line 65) | Structural analogue |
| pull binary files | t5500 `'simple fetch in shallow repo'` (line 185) | Structural analogue |
| push/pull deletion | t5400 `'push can be used to delete a ref'` (line 90) | |
| reverse direction push | t5400 `'pushing explicit refspecs respects forcing'` (line 221) | |
| content integrity round-trip | (bit-specific) | Verifies hash/size survive push+pull |
| large file push/pull | (bit-specific) | 64KB binary round-trip |

---

## t0005 — `binary-checkout-restore.sh` (11 tests)

**Git source:** `t7201-co.sh`

No binary-specific cases in source. Bit adapts the branch-switching pattern.

| bit test case | Git source case | Notes |
|---|---|---|
| binary metadata differs between branches | t7201 `'checkout with --merge'` (line 518) | State changes on switch |
| branch-only binary appears/disappears | t7201 `'switch to another branch while carrying a deletion'` (line 213) | |
| deleted binary reappears on switch | t7201 `'switch to another branch while carrying a deletion'` (line 213) | |

---

## t0006 — `binary-stash.sh` (14 tests)

**Git source:** `t3903-stash.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| stash saves binary changes | t3903 `'stash some dirty working directory'` (line 68) | |
| stash pop restores binary | t3903 `'stash pop'` (line 237) | |
| stash new (untracked) binary | t3903 `'stash --staged with binary file'` (line 403) | Closest analogue |
| stash multiple binaries | t3903 `'apply stashed changes (including index)'` (line 121) | |
| stash mixed binary+text | t3903 `'stash some dirty working directory'` (line 68) | |

---

## t0007 — `binary-verify.sh` (12 tests)

**Bit-specific — no git source.**

The `bit verify` command has no git equivalent. Tests corruption detection, missing file detection, size mismatch, and recovery after modification.

---

## t0008 — `binary-reset.sh` (12 tests)

**Git sources:** `t7102-reset.sh`, `t7104-reset-hard.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| reset --soft keeps binary staged | t7102 `'--soft reset only should show changes in diff --cached'` (line 217) | |
| reset --mixed unstages binary | t7102 `'--mixed reset to HEAD should unadd the files'` (line 314) | |
| reset --hard restores binary metadata | t7102 `'--hard reset should change the files and undo commits permanently'` (line 257) | |
| reset --hard restores binary metadata | t7104 `'reset --hard should restore unmerged ones'` (line 28) | |
| reset single binary file | t7102 `'test resetting the index at give paths'` (line 463) | |
| reset --hard discards uncommitted | t7104 `'reset --hard did not corrupt index or cache-tree'` (line 36) | |

---

## t0009 — `binary-cherry-pick-rebase.sh` (16 tests)

**Git sources:** `t3501-revert-cherry-pick.sh`, `t3419-rebase-patch-id.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| cherry-pick binary change | t3501 `'cherry-pick after renaming branch'` (line 65) | Structural analogue |
| cherry-pick conflict detection | t3501 `'advice from failed revert'` (line 168) | Binary conflict path |
| rebase binary branch | t3419 `'do not drop patch binary'` (line 97) | Preserve non-duplicate binary |
| revert binary commit | t3501 `'revert after renaming branch'` (line 75) | |

---

## t0010 — `binary-log-show.sh` (12 tests)

**Git sources:** `t4202-log.sh`, `t7007-show.sh`

No binary-specific cases in either source. Bit applies log/show patterns to binary metadata.

| bit test case | Git source case | Notes |
|---|---|---|
| log --oneline shows all commits | t7007 `'showing two commits'` (line 31) | |
| log with path filter | t4202 `'diff-filter=M'` (line 121) | |
| log -p shows diff | t7007 `'showing a range walks (linear)'` (line 80) | |
| log --follow tracks renames | t4202 `'git log --follow'` (line 172) | |
| show HEAD displays binary change | t7007 `'showing two commits'` (line 31) | |
| diff --name-only with binary | t4202 `'diff-filter=A'` (line 111) | |

---

## t0011 — `binary-grep.sh` (10 tests)

**Git source:** `t7815-grep-binary.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| grep finds text in text files | t7815 `'git grep -I ina a'` (line 24) | Baseline |
| grep for hash/size/md5 metadata | t7815 `'git grep ina a'` (line 13) | Grep inside binary-classified file |
| grep limited to *.txt excludes binary | t7815 `'grep respects binary diff attribute'` (line 70) | |
| grep limited to *.bin | t7815 `'grep respects not-binary diff attribute'` (line 104) | |
| grep in specific revision | t7815 `'grep revision respects binary diff attribute'` (line 96) | |
| grep -c counts matches | t7815 `'git grep -c ina a'` (line 29) | |
| grep -l lists matching files | t7815 `'git grep -l bar a'` (line 35) | |

---

## t0012 — `binary-diff-rename.sh` (18 tests)

**Git sources:** `t4012-diff-binary.sh`, `t4043-diff-rename-binary.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| diff detects modified binary | t4012 `'diff-index with --binary'` (line 88) | |
| diff --stat shows binary in stats | t4012 `'apply --stat output for binary file change'` (line 34) | |
| diff --numstat with binary | t4012 `'apply --numstat notices binary file change'` (line 52) | |
| mv binary — diff detects rename | t4043 `'move the files into a "sub" directory'` (line 19) | Direct source |
| renamed binary has correct metadata | t4043 `'git show -C -C report renames'` (line 39) | |
| move binary to subdirectory | t4043 `'move the files into a "sub" directory'` (line 19) | Direct source |
| diff -C detects copy | t4043 `'git show -C -C report renames'` (line 39) | |
| diff between branches | t4012 `'diff --shortstat output for binary file change'` (line 40) | |

---

## t0013 — `binary-attributes.sh` (23 tests)

**Git sources:** `t0003-attributes.sh`, `t0020-crlf.sh`, `t4006-diff-mode.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| file over 1MB classified as binary | (bit-specific) | Bit's size threshold; no git equivalent |
| text with .bin extension is binary | t0003 `'binary macro expanded by -a'` (line 489) | Extension wins over content |
| NUL content with .txt extension is binary | t0020 `'.gitattributes says two is binary'` (line 234) | Content wins over extension |
| invalid UTF-8 classified as binary | t0003 `'query binary macro directly'` (line 501) | |
| valid UTF-8 multibyte is text | (bit-specific) | Bit's encoding detection |
| all known binary extensions | (bit-specific) | .mp4, .dll, .so, .gif, .gz, .tar, .7z, .iso |
| batch mixed types | t0003 `'attribute test'` (line 139) | |

---

## t0014 — `binary-apply-patch.sh` (14 tests)

**Git sources:** `t4103-apply-binary.sh`, `t4108-apply-threeway.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| format-patch generates patch | t4103 `'stat binary diff -- should not fail.'` (line 56) | |
| am applies binary patch | t4103 `'apply binary diff.'` (line 145), t4108 `'apply binary file patch'` (line 273) | |
| am from directory | t4103 `'apply binary diff with full-index'` (line 121) | |
| format-patch for new binary | t4103 `'apply binary diff (copy).'` (line 150) | |
| format-patch for deleted binary | t4103 `'apply binary diff -- should fail.'` (line 105) | |
| diff between branches | t4103 `'stat binary diff -- should not fail.'` (line 56) | |

---

## t0015 — `binary-merge-strategies.sh` (14 tests)

**Git sources:** `t6417-merge-ours-theirs.sh`, `t6406-merge-attr.sh`, `t6403-merge-file.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| merge -X ours resolves binary conflict | t6417 `'binary file with -Xours/-Xtheirs'` (line 56) | **Direct source** |
| merge -X ours resolves binary conflict | t6417 `'recursive favouring ours'` (line 46) | |
| merge -X theirs resolves binary conflict | t6417 `'binary file with -Xours/-Xtheirs'` (line 56) | **Direct source** |
| merge -X theirs resolves binary conflict | t6417 `'recursive favouring theirs'` (line 36) | |
| merge -s ours ignores their changes | t6417 `'plain recursive - should conflict'` (line 26) | Strategy variant |
| merge without strategy fails | t6403 `'binary files cannot be merged'` (line 356) | **Direct source** |
| -merge attribute detects conflict | t6403 `'binary files cannot be merged'` (line 356) | |
| merge=union warns cannot merge | t6406 `'binary files with union attribute'` (line 248) | **Direct source** |
| mixed binary+text conflict resolution | t6417 `'binary file with -Xours/-Xtheirs'` (line 56) | Combined scenario |

---

## t0016 — `binary-diff-advanced.sh` (22 tests)

**Git sources:** `t4017-diff-retval.sh`, `t4048-diff-combined-binary.sh`, `t4031-diff-rewrite-binary.sh`, `t4209-log-pickaxe.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| diff --exit-code returns 0 (no changes) | t4017 `'--check --exit-code returns 0 for no difference'` (line 80) | |
| diff --exit-code returns 1 (binary change) | t4017 `'git diff $option returns 1 for changed binary file'` (line 147) | **Direct source** |
| diff --quiet returns 1 (binary change) | t4017 `'git diff $option returns 1 for changed binary file'` (line 147) | **Direct source** |
| diff -B detects rewrite | t4031 `'rewrite diff is binary'` (line 35) | |
| diff -B --stat shows rewrite | t4031 `'diff --stat counts binary rewrite as 0 lines'` (line 53) | |
| show -m --stat on merge commit | t4048 `'diff -m indicates binary-ness'` (line 38) | |
| log -1 -m --name-only on merge | t4048 `'diff -m indicates binary-ness'` (line 38) | |
| log -S finds binary metadata changes | t4209 `'log -S looks into binary files'` (line 230) | **Direct source** |
| log -G finds binary metadata pattern | t4209 `'log -G looks into binary files with -a'` (line 215) | |
| diff --numstat shows dashes | t4031 `'rewrite diff --numstat shows binary changes'` (line 47) | |
| diff between tags | (extension of t4017 pattern) | |

**Not yet covered from these sources:**
- t4048 `'diff -c indicates binary-ness'` (line 50) — `--cc` combined diff format
- t4048 `'diff --cc indicates binary-ness'` (line 62) — combined diff variant
- t4048 `'diff -m respects binary attribute'` (line 93) — attribute interaction
- t4209 `'log -G ignores binary files'` (line 210) — default skip behavior
- t4209 `'log -G looks into binary files with textconv filter'` (line 220) — textconv

---

## t0017 — `binary-apply-advanced.sh` (14 tests)

**Git sources:** `t4108-apply-threeway.sh`, `t4116-apply-reverse.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| am --3way applies binary patch | t4108 `'apply binary file patch with 3way'` (line 288) | **Direct source** |
| multiple patches round-trip | t4108 `'apply binary file patch'` (line 273) | |
| am --reverse undoes binary change | t4116 `'apply in reverse'` (line 41) | **Direct source** |
| mixed binary+text patch | t4108 + t4116 `'apply in forward'` (line 32) | Combined |

**Not yet covered from these sources:**
- t4116 `'apply in forward without postimage'` (line 64) — missing blob scenario
- t4116 `'apply in reverse without postimage'` (line 75) — missing blob scenario
- t4108 `'apply full-index binary diff in new repo'` — cross-repo apply

---

## t0018 — `binary-stash-advanced.sh` (18 tests)

**Git source:** `t3903-stash.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| stash --staged with binary | t3903 `'stash --staged with binary file'` (line 403) | **Direct source** |
| stash push -- subdir/ | t3903 `'stash -- <subdir> works with binary files'` (line 1375) | **Direct source** |
| stash list shows stash | t3903 general stash list cases | |
| stash show mentions binary | t3903 `'stash show format defaults to --stat'` (line 676) | |
| multiple stashes with binary | t3903 `'drop middle stash'` (line 160) | |

---

## t0019 — `binary-grep-advanced.sh` (16 tests)

**Git sources:** `t7815-grep-binary.sh`, `t7816-grep-binary-pattern.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| grep -I skips binary metadata | t7815 `'git grep -I ina a'` (line 24) | **Direct source** |
| grep without -I shows matches | t7815 `'git grep ina a'` (line 13) | |
| grep -v inverts match | (general grep flag) | |
| grep -w matches whole words | (general grep flag) | |
| grep -i case-insensitive | t7815 `'git grep -Fi iLE a'` (line 56) | |
| grep -e OR patterns | t7816 pattern-combination tests | |
| grep --cached vs working tree | t7815 `'grep --cached respects binary diff attribute'` (line 82) | **Direct source** |
| grep in revision | t7815 `'grep revision respects binary diff attribute'` (line 96) | **Direct source** |
| grep --name-only | t7815 `'git grep -l bar a'` (line 35) | |

**Not yet covered from these sources:**
- t7815 `'grep respects binary diff attribute'` (line 70) — `-diff` attribute forces binary
- t7815 `'grep does not honor textconv'` (line 125) — textconv default off
- t7815 `'grep --textconv honors textconv'` (line 129) — textconv opt-in
- t7815 `'git grep -L bar a'` (line 41) — list non-matching files
- t7815 `'git grep -q ina a'` (line 47) — quiet mode
- t7816 locale-specific binary pattern tests

---

## t0020 — `binary-rebase-advanced.sh` (11 tests)

**Git source:** `t3419-rebase-patch-id.sh`

| bit test case | Git source case | Notes |
|---|---|---|
| rebase drops duplicate binary commit | t3419 `'detect upstream patch binary'` (line 75) | **Direct source** |
| rebase --onto replays binary commit | t3419 `'do not drop patch binary'` (line 97) | **Direct source** |
| multi-commit rebase preserves all | t3419 `'do not drop patch binary'` (line 97) | Extended |
| interleaved binary+text rebase | t3419 `'do not drop patch'` (line 91) + `'do not drop patch binary'` (line 97) | Combined |

**Not yet covered from these sources:**
- t3419 `'detect upstream patch modechange'` (line 84) — mode-change patch-id
- t3419 `'do not drop patch modechange'` (line 105) — mode-change preservation

---

## Git Sources Not Yet Extracted

The following git test files contain binary-related cases that have **no** bit test counterpart. They are candidates for future extraction, listed by priority.

### High priority

| Git test file | Binary-relevant cases | Why |
|---|---|---|
| `t2030-unresolve-info.sh` | `'rerere forget (binary)'` | Rerere must handle binary conflicts |
| `t4006-diff-mode.sh` | `'--stat output after binary chmod'`, `'--shortstat output after binary chmod'` | Mode-only change on binary |
| `t4049-diff-stat-count.sh` | `'binary changes do not count in lines'` | Stat formatting edge case |
| `t4114-apply-typechange.sh` | `'binary file becomes symlink'`, `'symlink becomes binary file'` | Type change in apply |

### Medium priority

| Git test file | Binary-relevant cases | Why |
|---|---|---|
| `t4020-diff-external.sh` | `'force diff with "diff"'` (NUL-containing file) | External diff tool on binary |
| `t4030-diff-textconv.sh` | `'diffstat does not run textconv'`, `'show --textconv blob produces text'` | Textconv filter interaction |
| `t4204-patch-id.sh` | `'patch-id detects equality binary'`, `'patch-id detects inequality binary'` | Patch-id for binary-attributed files |
| `t5000-tar-tree.sh` | Binary files in `git archive` output | Archive preservation |
| `t6404-recursive-merge.sh` | `'refuse to merge binary files'` | Error message format |
| `t6422-merge-rename-corner-cases.sh` | rename/rename(1to2) with binary | Complex rename merge |
| `t8006-blame-textconv.sh` | `git blame --textconv` on binary | Blame with textconv filter |
| `t8007-cat-file-textconv.sh` | `git cat-file --textconv` on binary | Cat-file with textconv |

### Medium priority (continued)

| Git test file | Binary-relevant cases | Why |
|---|---|---|
| `t4012-diff-binary.sh` | `diff --stat` with Unicode filenames + binary + large change count | Stat formatting with non-ASCII binary paths |
| `t4103-apply-binary.sh` | `apply --check` without `--binary` rejects binary diff; truncated binary diff rejection | Apply validation edge cases |
| `t1050-large.sh` | Large binary diff shows "Binary files differ" message | User-facing message for large binary diffs |
| `t4209-log-pickaxe.sh` | `-G` vs `-S` binary search asymmetry; `-a` flag to force text search | Pickaxe semantics with binary content |
| `t4048-diff-combined-binary.sh` | `diff --cc` on unresolved working-tree conflict + textconv | Combined diff edge case with binary conflict |

### Low priority (git-internal)

| Git test file | Binary-relevant cases | Why |
|---|---|---|
| `t0020-crlf.sh` | NUL-byte files skip CRLF conversion | Git-internal encoding |
| `t0028-working-tree-encoding.sh` | UTF-16 files look binary | Git-internal encoding |
| `t1050-large.sh` | `core.bigFileThreshold` interaction | Pack compression detail |
| `t3701-add-interactive.sh` | Interactive add with binary | Interactive mode |
| `t4150-am.sh` | Binary blob in partial clone | Clone+filter edge case |
| `t9300-fast-import.sh` | Binary blobs via fast-import | Not a bit operation |

### Low priority (continued)

| Git test file | Binary-relevant cases | Why |
|---|---|---|
| `t4011-diff-symlink.sh` | Symlink ignores binary attribute | Symlink-specific, not relevant to bit's binary handling |
| `t4012-diff-binary.sh` | `diff --no-index` binary creation roundtrip | `--no-index` is git-internal diff mode |
| `t4030-diff-textconv.sh` | Textconv doesn't act on symlinks | Symlink + textconv interaction |
| `t7201-co.sh` | Custom merge driver `recursive=binary` | Custom merge driver configuration |
