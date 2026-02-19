# Verify and Repair

## The Proof of Possession Rule

The proof of possession rule ensures that metadata is never transferred without matching content to back it up. **The rule applies to full-layout remotes (cloud full and filesystem), not to bare-layout remotes.**

**Why the rule exists:** In a full-layout remote, binary files live at mutable paths that can drift -- files can be deleted, corrupted, or modified without updating metadata. The metadata in `.bit/index/` is just a *claim*. If that file is gone or changed, the claim is hollow. Pushing hollow claims means the remote has metadata that nobody can fulfill.

### Where the Rule Applies

```
Remote layout     Push verification        Pull verification
──────────────    ────────────────────     ──────────────────────
Full (cloud)      Verify local content     Verify remote content
Full (filesystem) Verify local content     Verify remote content
Bare (cloud)      Not needed (CAS)         Not needed (CAS)
Metadata          Not needed (no content)  Not needed (no content)
```

**Bare remotes are exempt** because CAS blobs are self-verifying by construction -- the blob filename *is* the content hash.

**Verification checks the working tree only.** When verifying, bit compares the actual working tree files against committed metadata. If a file is corrupted or missing, it is reported as an issue -- even if the correct blob exists in `.bit/cas/`. CAS is a *repair source*, not a verification shortcut. This ensures corruption is always surfaced rather than silently ignored.

```
Verification check:
  1. Does the working tree file match the committed metadata hash? → verified
  2. Otherwise?                                                    → issue reported
```

### Push Verification

1. Verify local -- every binary file in the working tree must match committed metadata
2. If verification fails, refuse to push (suggest `bit repair` to fix corrupted files first)
3. If verified, upload to remote CAS and sync readable paths, then push metadata

### Pull Verification

1. Verify remote -- every binary file on the remote's readable tree must match metadata
2. If verification fails, refuse to pull (suggest `--accept-remote`, `--force`, or `--manual-merge`)
3. If verified, pull metadata then copy files from readable paths

### Verification Failure Messages

On push failure:
```
$ bit push
error: Working tree does not match metadata.
  Modified: data/model.bin (expected md5:a1b2..., got md5:f8e9...)
  Missing:  data/weights.bin
hint: Run 'bit verify' to see all mismatches.
hint: Run 'bit add' to update metadata, or 'bit restore' to restore files.
```

On pull failure:
```
$ bit pull
error: Remote files do not match remote metadata.
  Modified: data/model.bin (expected md5:a1b2..., got md5:c3d4...)
hint: Run 'bit --remote <name> verify' to see all mismatches.
hint: Run 'bit pull --accept-remote' to accept the remote's actual file state.
hint: Run 'bit push --force' to overwrite remote with local state.
```

### Verification Cost

Verification requires hashing every binary file, which means reading the entire working tree. Mitigation: scan caching (keyed on path, mtime, size) skips re-hashing unchanged files.

**Remote verification cost by transport type:**
- **Cloud remotes (Google Drive, S3, etc.):** `rclone lsjson --hash` returns MD5 hashes as free metadata. Cheap.
- **Filesystem remotes:** Requires reading and hashing every binary file on the remote. Same cost as local.

---

## `bit verify`

Verifies local working tree files match their committed metadata. Uses a bandwidth-aware scan to hash files and compare against the committed state via `git diff`. Any difference means the file has been corrupted or modified since the last commit.

**Why scan + git diff (not direct file-vs-metadata comparison):** An earlier approach loaded metadata from `.bit/index/` on disk and compared file hashes against it. The problem: any command that scans the working directory (`bit status`, `bit add`, etc.) updates `.bit/index/` metadata to reflect current file state. If a file was corrupted and the user happened to run `bit status` first, the metadata would be updated to match the corrupted content -- and verification would pass. By comparing against the *committed* state in git, verification is immune to stale metadata.

**Verification steps:**
1. `Scan.scanWorkingDirWithAbort` -- hash files with bandwidth detection
2. `Scan.writeMetadataFiles` -- update `.bit/index/` to match scan results
3. `git diff --name-only` in `.bit/index` repo -- find files whose metadata changed from HEAD
4. For each changed binary file: read committed metadata (`git show HEAD:<path>`) for expected hash/size, read filesystem metadata for actual hash/size -> `HashMismatch`
5. For each changed text file: report `HashMismatch`
6. For missing files: `git ls-tree -r HEAD` lists committed paths, check existence -> `Missing`

### Verbose Phase Output

```
Verifying local files...
Collecting files... 1247 found.
Checking cache... 1200 cached, 47 need hashing (892.1 MB).
Hashing: 23/47 files, 156.3 MB / 892.1 MB (17%)     ← live progress bar (TTY only)
Hashed 47 files (892.1 MB).                           ← final summary replaces bar
Comparing against committed metadata...
[OK] All 1247 files match metadata.
```

Fast path (everything cached):
```
Collecting files... 1247 found.
All 1247 files cached, no hashing needed.
Comparing against committed metadata...
[OK] All 1247 files match metadata.
```

### Bandwidth-Aware Hashing

1. **Cache check**: Each file's mtime+size is compared against `.bit/cache/` entries. Cache hits skip re-hashing.
2. **Threshold**: If total uncached bytes are below 20 MB, skip bandwidth check and hash all files.
3. **Throughput measurement**: If >100 MB of uncached files remain, reads a 10 MB sample and estimates total time.
4. **Abort prompt**: If estimated time exceeds 60 seconds, prompts the user with option for size-only verification.
5. **Size-only fallback**: If the user declines, uncached files are verified by size only -- same-size content corruption is not detected but covers the majority of real-world corruption (partial writes, truncation, failed transfers).

### Error Display

- `[ERROR] Metadata mismatch: file.bin` -- hash differs
- `[ERROR] Size mismatch: file.bin` -- only size differs (shows expected/actual)
- `[ERROR] Missing: file.bin` -- file doesn't exist on disk

---

## `bit --remote <name> verify` / `bit --remote <name> repair`

Verifies a remote's files match committed metadata. Routes by remote type:

- **Filesystem remotes**: Scans the remote working directory using `Verify.verifyWithAbort` (same bandwidth-aware approach as local).
- **Cloud remotes**: Uses the already-fetched local bundle for committed metadata (no fetch during verify). Scans remote files via `rclone lsjson --hash` and compares.

When issues are found:
- `verify` prompts "Repair? [y/N]" on TTY, skips in non-interactive mode
- `repair` repairs automatically without prompting

---

## `bit repair`

Same as `bit verify` but repairs automatically without prompting. Searches all configured remotes for correct versions of corrupted or missing files.

**Content-addressable repair**: Files are matched by (hash, size), not by path. If `photos/song.mp3` is corrupted locally but `backup/song_copy.mp3` on a remote has the same hash and size, it will be used as the repair source.

**Repair sources:** local repo + all other configured remotes. Before planning, bit logs the sources:
```
Searching 2 source(s): local, 'gdrive'
```

**Repair is two-phase:**
1. **Text files:** Restored from git, then copied to the target. No other remotes needed.
2. **Binary files:** Content-addressable: (hash, size) -> (source, path). Target excluded from sources. After a successful binary copy, metadata is restored in `.bit/index/`.

**Unrepairable:** Binary issues with no (hash, size) match in any source are reported as "UNREPAIRABLE".

**Per-file progress**: Each repair copy shows live progress on TTY:
```
Repairing 3 file(s)...
  (1/3) data/model.bin from 'gdrive' — 45.2 MB / 120.0 MB (37%)
```

Remote-to-remote repairs use a single `rclone copyto` call -- rclone handles the routing directly.

---

## `bit fsck`

Runs `git fsck` on the internal metadata repository (`.bit/index`). Checks the integrity of the object store -- that all commits, trees, and blobs are valid and consistent. This is a passthrough to git's own integrity check. Use `bit verify` to check file integrity instead.
