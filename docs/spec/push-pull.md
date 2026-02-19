# Push and Pull

## Remote Synchronization (Two-Phase, Action-Based)

### Key Insight: Diff-Based Sync, Not Blind Sync

We do **NOT** use `rclone sync`. Instead:

1. Compute diff between current state and desired state
2. Generate minimal action list (Copy, Move, Delete)
3. Execute actions via rclone

This saves bandwidth. For example: renaming a 1GB file becomes `rclone moveto` instead of delete + upload.

### Sync Order (CRITICAL)

**On Push:**
1. **First**: Sync files via rclone (content must exist before metadata claims it does)
2. **Then**: Push metadata bundle via rclone

**On Pull:**
1. **First**: Fetch metadata bundle via rclone
2. **Then**: Git operation (merge or checkout) updates `.bit/index/`
3. **Then**: Mirror index changes to working directory (download binaries, copy text from index)

**Rationale**: Push files first so the remote is never in a state where metadata references missing content. Pull metadata first so we know what content to fetch. After the git operation, the index is authoritative -- we only need to bring actual files into alignment.

---

## Transport Strategies

The transport strategy is determined by `RemoteType` classification:

```
Device.readRemoteType / isFilesystemType
  ├── RemoteCloud      → Cloud transport (bundle + rclone)
  ├── RemoteDevice     → Filesystem transport (full repo at remote)
  ├── RemoteFilesystem → Filesystem transport (full repo at remote)
  └── RemoteGit        → Git transport (native git push/fetch)
```

### Cloud Transport (Bundle + Rclone)

For cloud remotes, bit uses **dumb storage**:
- Metadata is serialized as a Git bundle and uploaded via rclone
- Files are synced via rclone copy/move/delete operations
- The remote is just a directory of files -- no Git repo, no bit commands work there

**Full layout** (`layout: full`, default): Files at human-readable paths on the cloud AND in CAS.

```
gdrive:Projects/foo/
├── .bit/
│   └── bit.bundle          # Metadata bundle
├── cas/                     # Content-addressed store
│   └── a1/
│       └── a1b2c3d4e5f6...
├── lectures/
│   └── lecture-01.mp3       # Actual files at readable paths
└── data/
    └── dataset.bin
```

**Bare layout** (`layout: bare`): Files only in CAS layout.

```
gdrive:Backup/foo/
├── .bit/
│   └── bit.bundle          # Metadata bundle
└── cas/
    ├── a1/
    │   └── a1b2c3d4e5f6...  # Content blob (keyed by hash)
    └── ...
```

**Push behavior (both layouts):** bit always uploads content into the remote's CAS layout (`cas/<first-2-chars>/<full-hash>`), then uploads the metadata bundle. For full-layout remotes, bit additionally syncs files to human-readable paths. This works regardless of local mode -- a lite-mode repo streams content directly to the remote CAS without populating local CAS.

**Pull behavior (both layouts):** bit fetches the metadata bundle and merges. For full remotes, uses diff-based sync from readable paths. For bare remotes, downloads blobs from `cas/<prefix>/<hash>` and places them at working tree paths.

#### Cloud Bundle Lifecycle

- **On the remote:** The metadata bundle is always stored at **`.bit/bit.bundle`** (single file).
- **Fetch:** (1) Download to staging path `.bit/temp_remote.bundle`. (2) Copy to per-remote path `.bit/index/.git/bundles/<sanitized-name>.bundle` and remove temp. (3) Register as git remote and run `git fetch <name>`. (4) If `git fetch` does not set the ref, use `git bundle list-heads` and manually update the tracking ref.
- **Push:** Create the bundle at `.bit/index/.git/bundles/<name>.bundle` (`git bundle create ... --all`), then upload via rclone to `.bit/bit.bundle`.
- **Per-remote local bundles:** Each remote has its own local bundle file so sequential fetches do not overwrite one another.

### Filesystem Transport (Full Repo)

For filesystem remotes, bit creates a **complete bit repository** at the remote:
- The remote has `.bit/index/.git/` just like a local repo
- Anyone at the remote location can run `bit status`, `bit log`, `bit commit`, etc.
- No bundles needed -- Git talks directly repo-to-repo

**Key insight**: Bundles exist to serialize git history over dumb transports that can only copy files. With filesystem access, git speaks its native protocol.

**Filesystem remote receives pushes via direct path**: The remote side does not need a named git remote pointing back. Different machines can push to the same filesystem remote.

### Text vs Binary File Sync

For filesystem remotes, file sync distinguishes text from binary by examining the metadata file content:

- **Text files**: Content lives in `.bit/index/path`. After git merge/checkout, copy from index to working tree.
- **Binary files**: Content lives in working tree. Copy from source working tree to destination working tree.

### The `git push` Antipattern

Do NOT use `git push` to a non-bare repo. Git refuses to update the checked-out branch. The correct approach: Have the remote **fetch** from local, then **merge --ff-only**.

---

## Unified Push Architecture

Push uses a single code path for both cloud and filesystem remotes. The only difference is the metadata transport -- abstracted behind a `PushSeam`:

```
PushSeam:
  ptFetchHistory :: IO (Maybe String)  -- Fetch remote history, return remote hash
  ptPushMetadata :: IO ()              -- Push metadata to remote after file sync
```

Everything else is shared: `classifyRemoteState`, `syncRemoteFiles`, ancestry checks, `--force`/`--force-with-lease`, verification.

**Cloud seam** (`mkCloudSeam`): Downloads/uploads git bundles via rclone. Bundles stored per-remote. For both layouts, file sync uploads into CAS. For full-layout, additionally syncs readable paths.

**Filesystem seam** (`mkFilesystemSeam`): Uses native git fetch/pull. Metadata pushed via `git pull --ff-only` at the remote side.

**Git seam** (`mkGitSeam`): For `RemoteGit` and metadata-only remotes. Uses native `git fetch`/`git push` directly. No bundles, no rclone. File sync skipped entirely.

**Push flow** (all transports):
1. **Verify local** (proof of possession -- full-layout remotes only; skipped for bare and metadata-only)
2. **First-push detection** (filesystem only): Create and initialize remote `.bit/` if needed
3. **Classify remote state** via rclone
4. **Fetch remote history** via seam
5. **Ancestry/force checks**
6. **Sync files** -- CAS upload; for full remotes, additionally sync readable paths; skipped for metadata-only
7. **Push metadata** via seam
8. **Update tracking ref**

---

## Unified Pull Architecture

Pull mirrors the push design -- a single code path with only the metadata fetch abstracted behind a `PullSeam`:

```
PullSeam:
  psFetchMetadata :: IO Bool           -- Fetch remote metadata, return success
  psVerifyRemote  :: FilePath -> IO ()  -- Verify remote before merge
```

**Cloud seam** (`mkCloudPullSeam`): Downloads bundle via rclone, runs `git fetch <name>`.

**Filesystem seam** (`mkFilesystemPullSeam`): Runs `git fetch <remotePath>/.bit/index` directly.

**Git seam** (`mkGitPullSeam`): Runs `git fetch <name>` using the git remote URL directly. Verification is a no-op.

**Pull flow** (all transports):
1. **Fetch remote metadata** via seam
2. **Verify remote** via seam -- full-layout only; skipped for bare, metadata-only, `--accept-remote`, `--manual-merge`
3. **Dispatch by mode**:
   - Normal: `git merge --no-commit --no-ff`, then `applyMergeToWorkingDir`
   - `--accept-remote`: Force-checkout (with `--no-track`), then sync files
   - `--manual-merge`: Detect divergence, create conflict directories
4. **Sync files**: Text from index, binary via rclone; for bare remotes, download from CAS; skipped for metadata-only
5. **Update tracking ref**

---

## Push Force Flags

`--force` and `--force-with-lease` are mutually exclusive.

**`--force`:** Skips the ancestry check. Push proceeds even when remote has diverged or is ahead. For a non-bit-occupied remote, only `--force` allows overwriting.

**`--force-with-lease`:** Proceeds only if nobody else has pushed since our last fetch. Captures the tracking ref before the seam fetch, compares to the post-fetch remote ref. If they differ, error: "Remote has changed since last fetch!"

Neither flag affects verification -- push always verifies local working tree first (for full-layout remotes); they only affect ancestry and overwrite policy.
