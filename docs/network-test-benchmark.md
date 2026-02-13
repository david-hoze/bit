# Network Remote Testing

Testing bit against slow devices (UNC paths over RDP, USB drives, NAS mounts) requires a different strategy than local or cloud remote testing. This document captures what we learned and the approach we adopted.

## The Problem: Git I/O Over High-Latency Links

Git's object store uses many small files in a fanout directory structure (`.git/objects/ab/cdef...`). Every git operation — commit, status, log, add — performs dozens of small random reads and writes to `.git/objects/`, `.git/refs/`, and `.git/index`. On local disk this is instant. Over a high-latency link like `\\tsclient` (RDP tunnel), each I/O operation incurs a full network round-trip.

**Measured impact** (UNC path `\\tsclient\c\users\david\bit-testing`, RDP tunnel):

| Operation | Time | Why |
|---|---|---|
| `bit add` + `bit commit` at UNC | **16.1s** | 50+ round-trips at ~100ms each |
| `bit status` at UNC | **10.2s** | Stats every file in the index |
| `bit log` at UNC | **3.6s** | Reads pack files and refs |
| `rmdir /s /q` on UNC `.bit/` | **9.7s** | Enumerates + deletes every git object file |
| `bit push` from local to UNC | **3.2s** | rclone batches all files in one pass |
| `bit verify` (local against UNC) | **0.7s** | Local hashing, one rclone call for remote |
| `bit pull` from UNC | **0.2s** | One rclone call + local git merge |

**Conclusion:** Running bit (or git) commands *directly on* a slow device is 10-50x slower than operating from a local repo that syncs *to* the device via rclone. The rclone approach batches many small files into one transfer operation, avoiding per-file round-trip overhead.

## Test Strategy for Slow Devices

### Principle: Only test what's device-specific

All bit logic — merge, conflict resolution, status, multi-repo pull/push, manual merge — works identically regardless of remote type. The filesystem and cloud test suites already cover these scenarios with fast local paths. The network test only needs to verify that **the transport layer works correctly over a slow link**:

- Push delivers files to the remote
- Verify checks integrity against the remote
- Repair restores corrupted files from the remote
- Status reflects remote state

Multi-repo workflows, conflict resolution, accept-remote mode, and manual merge are covered by `filesystem-remote-direct.test`, `filesystem-manual-merge.test`, and `gdrive-remote.test`.

### Technique: rclone copy for repo staging

Instead of running `bit push` to create the initial remote state (which triggers git operations at the remote), we:

1. Create and commit in a local repo
2. `rclone copy local_repo \\remote_path` to batch-copy all files (including `.bit/index/.git/objects/`) in one pass
3. `bit remote add origin \\remote_path` + `bit fetch` + `bit push -u origin` to sync tracking refs

This avoids the expensive per-file git I/O at the remote during setup. The `rclone copy` takes ~0.2s vs ~3.2s for `bit push` with equivalent content.

### Technique: rclone purge for cleanup

`rmdir /s /q` on a UNC path with git objects takes ~10s (enumerates and deletes each file individually). `rclone purge \\remote_path` is faster and works cross-platform.

## Results

**Original test (v1):** ~54s total, 80% spent on pushd operations (running bit directly at UNC) and recursive cleanup.

**Optimized test (v2):** ~3m23s total (17 tests), dominated by cleanup and the RDP tunnel latency on rclone transfers. All bit operations from the local side complete in under 4s each.

| Category | v1 Time | v2 Time | Change |
|---|---|---|---|
| Setup (repo staging) | 3.2s (bit push) | 0.2s (rclone copy) | 16x faster |
| Push/pull/verify/repair | ~5s | ~5s | Same |
| Pushd operations (bit at UNC) | ~30s | Eliminated | N/A |
| Cleanup | ~12s (rmdir) | ~3s (rclone purge) | 4x faster |
