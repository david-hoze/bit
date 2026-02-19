# Binary File Test Suite

bit has a dedicated test suite for binary file handling in `test/t/`. These are bash test scripts inspired by Git's `t/` test framework, focused exclusively on verifying that bit correctly classifies, tracks, syncs, and merges binary files.

## Why a Separate Suite?

bit already passes Git's own test suite via the git shim (see [Git Test Suite](spec/git-test-suite.md)). Those tests verify git-compatibility for text files. The binary test suite covers what's unique to bit:

- Binary classification (NUL byte detection, extension-based, size threshold)
- Metadata format (`hash:` + `size:` in `.bit/index/`)
- Push/pull round-trips with binary content integrity
- Merge and conflict handling with binary metadata
- Verify/repair for binary file corruption
- Reset, cherry-pick, rebase, and revert with binary files
- Grep, diff, rename/copy detection on binary metadata
- Format-patch and am round-trips
- .gitattributes interaction and classification edge cases

## Prerequisites

| Tool | Notes |
|------|-------|
| bit | `cabal install --overwrite-policy=always` |
| Bash | Ships with Git for Windows / PortableGit |
| python3 | Optional -- used by `generate_binary` helper for deterministic large binary creation. Falls back to `/dev/urandom` if unavailable. |
| rclone | Required for push/pull tests (t0003, t0004) |

## Running Tests

```bash
# Run all tests
cd test/t
bash run-tests.sh

# Run a single test
cd test/t
bash t0001-binary-add-commit.sh

# Run specific tests by glob
cd test/t
bash run-tests.sh t0001*.sh t0002*.sh

# Keep trash directories for debugging
TEST_NO_CLEANUP=1 bash run-tests.sh

# Disable colored output
NO_COLOR=1 bash run-tests.sh
```

## Test Scripts

| Script | Tests | What it covers |
|--------|-------|----------------|
| `t0001-binary-add-commit.sh` | 28 | Binary classification (NUL bytes, extensions), metadata format, add/commit, subdirectories, large files, empty files |
| `t0002-binary-status-diff.sh` | 21 | Status with modified/deleted/new binary, diff output, rename detection, binary-to-text and text-to-binary type changes |
| `t0003-binary-merge.sh` | 21 | Fast-forward merge, clean parallel merge (different files), binary conflict detection, delete+modify merge, subdirectory binaries |
| `t0004-binary-push-pull.sh` | 21 | Push/pull round-trip via filesystem remote, content integrity, metadata matching, deletions, large files, mixed text+binary |
| `t0005-binary-checkout-restore.sh` | 11 | Branch switching with binary changes, branch-only binaries, delete on branch, metadata restoration |
| `t0006-binary-stash.sh` | 14 | Stash/pop with modified binary, new binary, multiple binaries, mixed binary+text |
| `t0007-binary-verify.sh` | 12 | Verify clean state, detect corruption/missing/size mismatch, verify after modification cycle |
| `t0008-binary-reset.sh` | 12 | Reset --soft/--mixed/--hard with binary files, single-file reset, uncommitted binary changes |
| `t0009-binary-cherry-pick-rebase.sh` | 16 | Cherry-pick binary change, cherry-pick conflict, rebase binary branch, revert binary commit |
| `t0010-binary-log-show.sh` | 12 | Log --oneline/--follow/-p/--stat with binary, show, diff between commits |
| `t0011-binary-grep.sh` | 10 | Grep in text files, grep binary metadata (hash:/size:/md5:), file patterns, revisions, -c/-l |
| `t0012-binary-diff-rename.sh` | 18 | Diff detection, --stat, --numstat, mv/rename, rename+modify, move to subdir, copy detection, branch diff |
| `t0013-binary-attributes.sh` | 23 | Size threshold (>1MB), extension-based vs content-based classification, all known binary extensions, UTF-8 edge cases, batch mixed types |
| `t0014-binary-apply-patch.sh` | 14 | Format-patch, am round-trip, format-patch for add/delete, diff between branches |
| `t0015-binary-merge-strategies.sh` | 14 | Merge -X ours/theirs, -s ours, bare conflict, -merge attribute, merge=union, mixed binary+text |
| `t0016-binary-diff-advanced.sh` | 22 | Exit codes, --quiet, -B rewrite, combined diff -m, pickaxe -S/-G, tags, --numstat |
| `t0017-binary-apply-advanced.sh` | 14 | am --3way, multiple patches, --reverse, mixed binary+text patches |
| `t0018-binary-stash-advanced.sh` | 18 | --staged, push -- subdir/, list/show, multiple stashes |
| `t0019-binary-grep-advanced.sh` | 16 | -I, -v, -w, -i, -e (OR), --cached, --name-only, revision ranges |
| `t0020-binary-rebase-advanced.sh` | 11 | Duplicate detection, --onto, multi-commit, interleaved binary+text |

**Total: 328 tests across 20 scripts.**

## Test Framework (`test-lib.sh`)

Each test script sources `test-lib.sh`, which provides:

### Test Functions

- `test_expect_success 'name' 'commands'` -- Run commands, pass if exit 0
- `test_expect_failure 'name' 'commands'` -- Expected failure (marked TODO)
- `test_done` -- Print summary and exit

### Binary Helpers

Adapted from Git's `test-lib-functions.sh`:

- `q_to_nul` -- Convert `Q` to NUL byte (`tr 'Q' '\000'`)
- `nul_to_q` -- Convert NUL to `Q` (`tr '\000' 'Q'`)
- `generate_binary <seed> <size>` -- Create deterministic binary content
- `test_cmp_bin <expected> <actual>` -- Byte-for-byte file comparison
- `test_must_fail <command>` -- Assert command exits non-zero

### bit-Specific Helpers

- `verify_binary_metadata <repo> <path>` -- Assert `.bit/index/<path>` contains `hash:` and `size:` lines
- `verify_text_metadata <repo> <path>` -- Assert `.bit/index/<path>` does NOT contain `hash:` (text content stored directly)
- `get_metadata_hash <repo> <path>` -- Extract hash value from metadata
- `get_metadata_size <repo> <path>` -- Extract size value from metadata
- `create_bit_repo <dir>` -- Create and initialize a fresh bit repo

### Isolation

Each test script creates a `trash-<test-name>/` directory for isolation. On success, the trash directory is cleaned up automatically. Set `TEST_NO_CLEANUP=1` to preserve it for debugging.

## Adapting from Git Tests

The binary tests are adapted from these Git test scripts:

| bit test | Git source | Adaptation |
|----------|------------|------------|
| t0001 | t3700-add.sh, t7501-commit.sh | Uses `bit add`/`bit commit`, verifies metadata format |
| t0002 | t4012-diff-binary.sh, t4031-diff-rewrite-binary.sh | Uses `bit status`/`bit diff`/`bit rm` |
| t0003 | t6407-merge-binary.sh, t4048-diff-combined-binary.sh | Two-repo push/pull merge via filesystem remote |
| t0004 | t5400-send-pack.sh, t5500-fetch-pack.sh | Full push/pull round-trip with content verification |
| t0005 | t7201-co.sh | Branch operations via `git -C .bit/index checkout` |
| t0006 | t3903-stash.sh | `bit stash`/`bit stash pop` with binary metadata |
| t0007 | (bit-specific) | `bit verify` with corrupted/missing binary files |
| t0008 | t7102-reset.sh, t7104-reset-hard.sh | Reset --soft/--mixed/--hard with binary metadata |
| t0009 | t3501-revert-cherry-pick.sh, t3419-rebase-patch-id.sh | Cherry-pick, rebase, revert with binary metadata |
| t0010 | t4202-log.sh, t7007-show.sh | Log and show with binary metadata files |
| t0011 | t7815-grep-binary.sh | Grep in binary metadata and text files |
| t0012 | t4012-diff-binary.sh, t4043-diff-rename-binary.sh | Diff, rename/copy detection with binary metadata |
| t0013 | t0003-attributes.sh, t0020-crlf.sh | Classification edge cases, size threshold, extensions |
| t0014 | t4103-apply-binary.sh, t4108-apply-threeway.sh | Format-patch and am round-trip with binary metadata |
| t0015 | t6417-merge-ours-theirs.sh, t6406-merge-attr.sh, t6403-merge-file.sh | Merge strategies (-X ours/theirs, -s ours) with binary metadata |
| t0016 | t4017-diff-retval.sh, t4048-diff-combined-binary.sh, t4031-diff-rewrite-binary.sh, t4209-log-pickaxe.sh | Advanced diff: exit codes, rewrite, combined, pickaxe |
| t0017 | t4108-apply-threeway.sh, t4116-apply-reverse.sh | am --3way, multiple patches, reverse apply |
| t0018 | t3903-stash.sh | Stash --staged, subdir push, multiple stashes with binary |
| t0019 | t7815-grep-binary.sh, t7816-grep-binary-pattern.sh | Advanced grep: -I, -v, -w, -i, -e, --cached, revisions |
| t0020 | t3419-rebase-patch-id.sh | Rebase duplicate detection, --onto, multi-commit |

## Adding New Tests

1. Create `test/t/t00NN-binary-<topic>.sh`
2. Start with:
   ```bash
   test_description='what this test covers'
   . ./test-lib.sh
   ```
3. Use `test_expect_success` for each test case
4. End with `test_done`
5. Use `create_bit_repo` for fresh repos, `verify_binary_metadata` for assertions
6. For push/pull tests, always specify the remote explicitly (`bit push origin`, `bit pull origin`) and use `bit push -u origin` for the first push
7. For deletions, use `bit rm` (not `rm` + `bit add`)
8. Use files (not shell variables) to share state between `test_expect_success` blocks — each block runs in a subshell
9. Use absolute paths when passing file arguments to `bit am`, `bit apply`, etc. — these commands pass paths to git which runs from `.bit/index/`
10. After switching branches, the working tree binary content may be stale — restore it manually if needed for subsequent operations

## Traceability

See [Binary Test Traceability](binary-test-traceability.md) for a detailed mapping of which Git test suite cases inspired each bit test, including cases not yet extracted.
