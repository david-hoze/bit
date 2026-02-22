# Implementation Status

## Key Types

| Type | Module | Purpose |
|------|--------|---------|
| `Hash (a :: HashAlgo)` | `Bit.Types` | Phantom-typed hash -- compiler distinguishes MD5 vs SHA256 |
| `Path` | `Bit.Types` | Domain path (bit-tracked file path). `newtype` over `FilePath` to prevent transposition bugs. |
| `FileEntry` | `Bit.Types` | Tracked `Path` + `EntryKind` (hash, size, `ContentType`) |
| `BitEnv` | `Bit.Types` | Reader environment: cwd, prefix (subdirectory relative to root), remote, force flags |
| `BitM` | `Bit.Types` | `ReaderT BitEnv IO` -- the application monad |
| `MetaContent` | `Bit.Config.Metadata` | Canonical metadata: hash + size, single parser/serializer |
| `Remote` | `Bit.Remote` | Resolved remote: name + URL. Smart constructor, `remoteUrl` for Transport only |
| `RemoteState` | `Bit.Remote` | Remote classification: StateEmpty, StateValidBit, StateNonBitOccupied, StateNetworkError |
| `FetchResult` | `Bit.Remote` | Bundle fetch result: BundleFound, RemoteEmpty, NetworkError |
| `FetchOutcome` | `Bit.Core.Fetch` | UpToDate, Updated { foOldHash, foNewHash }, FetchedFirst, FetchError |
| `RcloneAction` | `Bit.Domain.Plan` | Copy, Move, Delete, Swap -- concrete rclone operations |
| `Resolution` | `Bit.Core.Conflict` | KeepLocal or TakeRemote -- conflict resolution choice |
| `DeletedSide` | `Bit.Git.Run` | DeletedInOurs or DeletedInTheirs -- which side deleted in modify/delete conflict |
| `ConflictInfo` | `Bit.Core.Conflict` | ContentConflict, ModifyDelete path DeletedSide, AddAdd -- conflict type |
| `NameStatusChange` | `Bit.Git.Run` | Added, Deleted, Modified, Renamed, Copied -- parsed `git diff --name-status` output |
| `DivergentFile` | `Bit.Core.Pull` | Record for remote divergence: path, expected/actual hash and size |
| `ScannedEntry` | `Bit.Scan.Local` | ScannedFile / ScannedDir -- internal type for scan pass |
| `FileToSync` | `Bit.Rclone.Sync` | TextToSync / BinaryToSync -- classifies files for sync |
| `BinaryFileMeta` | `Bit.Scan.Verify` | Record: bfmPath, bfmHash, bfmSize |
| `VerifyResult` | `Bit.Scan.Verify` | Record: vrCount, vrIssues |
| `VerifyTarget` | `Bit.Core.Verify` | VerifyLocal or VerifyRemotePath Remote |
| `DeviceInfo` | `Bit.Device.Identity` | UUID + storage type + optional hardware serial |
| `RemoteType` | `Bit.Device.Identity` | RemoteFilesystem, RemoteDevice, RemoteCloud, RemoteGit |
| `RemoteLayout` | `Bit.Device.Identity` | LayoutFull, LayoutBare, LayoutMetadata |
| `BitMode` | `Bit.Core.Config` | ModeLite, ModeSolid |
| `ChunkConfig` | `Bit.CDC.Types` | CDC parameters: minSize, avgSize, maxSize |
| `ChunkManifest` | `Bit.CDC.Types` | File hash + size + ordered list of `ChunkRef` |
| `ChunkRef` | `Bit.CDC.Types` | Single chunk: hash + offset + length |

---

## Module Map

| Module | Role |
|--------|------|
| `Bit/Commands.hs` | CLI dispatch, `findBitRoot`, prefix computation, env setup; `init` dispatched before repo discovery; bare repos pass unknown commands through; `tryAlias` expands git aliases; `resolveBitDir` follows bitlinks |
| `Bit/Help.hs` | Command help metadata; `HelpItem` for options/examples |
| `Bit/Core.hs` | Re-exports public API from `Bit/Core/*.hs` sub-modules |
| `Bit/Core/Push.hs` | Push logic + PushSeam transport abstraction |
| `Bit/Core/Pull.hs` | Pull + merge + conflict resolution |
| `Bit/Core/Fetch.hs` | Fetch + remote state classification |
| `Bit/Core/Helpers.hs` | Shared types (PullMode, PullOptions) + utility functions |
| `Bit/Core/RemoteManagement.hs` | Remote add/show, device-name prompting, `--bare` layout flag |
| `Bit/Core/Config.hs` | `bit config` command: get/set/list for `.bit/config`; per-key validation |
| `Bit/Core/Conflict.hs` | Conflict resolution: Resolution, DeletedSide, ConflictInfo, resolveAll |
| `Bit/Git/Run.hs` | Git command wrapper; prefix-aware dispatch; `AncestorQuery`; `spawnGit` respects `BIT_REAL_GIT`; `indexPathRef` IORef for dynamic base flags |
| `Bit/Git/Passthrough.hs` | Git command passthrough (add, commit, diff, log, merge, revert, etc.); `syncTextFilesFromIndex` restores binaries from CAS (whole blob or CDC reassembly) |
| `Bit/Rclone/Run.hs` | Rclone command wrapper; `remoteFilePath` for trailing-slash-safe path construction |
| `Bit/Rclone/Sync.hs` | Shared action derivation + working-tree sync |
| `Bit/Rclone/Progress.hs` | Progress tracking for file copy operations |
| `Bit/Config/Paths.hs` | Path constants |
| `Bit/Config/File.hs` | Config file parsing (strict ByteString) |
| `Bit/Config/Metadata.hs` | Canonical metadata parser/serializer |
| `Bit/CAS.hs` | Content-addressed store: `casBlobPath`, `writeBlobToCas`, `hasBlobInCas`, `copyBlobFromCasTo` |
| `Bit/CDC/FastCDC.hs` | FastCDC boundary detection + streaming file chunking |
| `Bit/CDC/Gear.hs` | 256-entry gear hash table (MD5-generated, deterministic) |
| `Bit/CDC/Types.hs` | `ChunkConfig`, `Chunk`, `ChunkRef`, `ChunkManifest` |
| `Bit/CDC/Config.hs` | Read CDC config from `.bit/config`; enabled by default |
| `Bit/CDC/Manifest.hs` | Serialize/parse/read/write chunk manifests |
| `Bit/CDC/Reassemble.hs` | Reassemble files from CAS chunks |
| `Bit/CDC/Store.hs` | Write chunked blobs to CAS |
| `Bit/Types.hs` | Core types: Hash, FileEntry, BitEnv (with `envPrefix`), BitM |
| `Bit/Path.hs` | `RemotePath` newtype for remote filesystem paths |
| `Bit/Remote.hs` | Remote type, resolution, RemoteState, FetchResult |
| `Bit/Remote/ChunkIndex.hs` | `queryRemoteBlobs`: list CAS blobs on remote via `rclone lsf` for push dedup |
| `Bit/Utils.hs` | Path utilities, filtering, atomic write re-exports |
| `Bit/Domain/Plan.hs` | `RcloneAction` type and `resolveSwaps` |
| `Bit/Scan/Local.hs` | Working directory scanning, hash computation, bandwidth-aware scanning, parallel metadata writing |
| `Bit/Scan/Remote.hs` | Remote file scanning via rclone |
| `Bit/Scan/Verify.hs` | Local and remote verification; unified metadata loading via `MetadataSource` |
| `Bit/Scan/Fsck.hs` | Passthrough to `git fsck` |
| `Bit/Device/Identity.hs` | Device identity, volume detection, `RemoteType` classification |
| `Bit/Device/Prompt.hs` | Interactive device setup prompts |
| `Bit/Device/RemoteWorkspace.hs` | Ephemeral remote workspace: `initRemote`, `addRemote`, `commitRemote`, `statusRemote`, `logRemote` |
| `Bit/IO/Concurrency.hs` | Bounded parallelism helpers |
| `Bit/IO/ConcurrentIO.hs` | Type-safe concurrent IO newtype (no MonadIO) |
| `Bit/IO/ConcurrentFileIO.hs` | Strict ByteString file operations |
| `Bit/IO/AtomicWrite.hs` | Atomic file writes, directory locking, lock registry |
| `Bit/IO/Process.hs` | Strict process output capture |
| `Bit/IO/Platform.hs` | UNC-safe wrappers for `System.Directory` |
| `Bit/Progress/Report.hs` | Centralized progress reporting |

---

## Implemented and Working

- `bit init` -- creates `.bit/`, initializes Git; supports `BIT_GIT_JUNCTION=1`, re-init, `--initial-branch`, `--separate-git-dir`, bitlinks
- `bit init --bare` -- passes through to `git init --bare`, creates `bit/cas/`
- `bit add` -- scans files, computes MD5 hashes, writes metadata, stages in Git; solid mode populates CAS
- `bit config` -- get/set/list `.bit/config`; `core.mode` gates CAS writes
- `bit cas backfill` -- walk historical commits, store blobs into CAS
- CAS -- `.bit/cas/<prefix>/<hash>`; restore/checkout/revert copy from CAS (whole blob or CDC reassembly); verification consults CAS
- `bit commit`, `diff`, `status`, `log`, `restore`, `checkout`, `revert`, `reset`, `rm`, `mv`, `branch`, `merge` -- delegate to Git; restore/checkout/revert sync binary files from CAS after git updates metadata
- `bit remote add/show` -- named remotes with device-aware resolution
- `bit push` -- Cloud: diff-based file sync + metadata bundle. Filesystem: fetch+merge + file sync
- `bit pull` -- Cloud: fetch bundle + diff-based sync. Filesystem: fetch from remote + merge + sync
- `bit pull --accept-remote` -- force-checkout remote branch + sync
- `bit pull --manual-merge` -- interactive per-file conflict resolution
- `bit merge --continue / --abort` -- merge lifecycle management
- `bit fetch` -- fetch metadata bundle only
- `bit verify` -- local verification against committed metadata (scan + git diff)
- `bit repair` -- verify + auto-repair from all remotes
- `bit --remote <name> verify / repair` -- remote file verification and repair
- `bit fsck` -- passthrough to `git fsck`
- `bit --remote <name>` / `bit @<remote>` -- ephemeral remote workspace commands
- Shared `deriveActions` with `resolveSwaps` swap detection and property tests
- Device-identity system for filesystem remotes
- Filesystem remote transport (full bit repo, direct git fetch/merge)
- Conflict resolution with structured traversal (always commits when MERGE_HEAD exists)
- `.gitattributes` sync
- `--git-dir`/`--work-tree` flag bypass and `GIT_DIR` env var bypass
- `GIT_CEILING_DIRECTORIES` support
- Subdirectory passthrough with CWD restoration
- Git alias expansion and catch-all passthrough
- Git test suite via compiled router (`bit-git-router`) with `BIT_REAL_GIT` and `BIT_GIT_JUNCTION`; `bit become-git --init` for setup
- Unified metadata parsing/serialization
- `oldHead` pattern for diff-based working-tree sync
- Strict ByteString IO throughout
- Atomic file writes with Windows retry logic
- Concurrent file scanning with bounded parallelism and progress reporting
- HLint enforcement of IO safety rules
- Proof of possession verification for push and pull
- CDC (content-defined chunking) -- enabled by default in solid mode; FastCDC with gear hash; batch CAS upload/download via `rclone --files-from`; manifest-based reassembly on pull; push dedup via `queryRemoteBlobs` (skips chunks already on remote); pull dedup via `hasBlobInCas` (skips chunks already in local CAS); parallel chunk transfers (`--transfers 32`) for CAS uploads/downloads to overcome per-chunk latency

---

## Test Infrastructure

### Lint Test Suite (Pattern Safety + Format Validation)

**Purpose**: Prevent dangerous Windows environment variable patterns and shelltest format errors in test files.

**Two Categories of Violations**:

1. **Pattern Safety**: Dangerous Windows environment variables that can cause commands to escape test sandboxes (e.g., `%CD%` expands before command chains execute)
2. **Format Validation**: Shelltest Format 3 syntax violations (duplicate directives cause parse errors that silently prevent tests from running)

**Lint Test** (`test/LintTestFiles.hs`, `cabal test lint-tests`):
- Recursively scans all `.test` files under `test/cli/`
- Checks for dangerous patterns: `%CD%`, `%~dp0`, `%USERPROFILE%`, `%APPDATA%`, `%HOMEDRIVE%`, `%HOMEPATH%`
- Validates Format 3 syntax: detects duplicate directives (`<<<`, `>>>`, `>>>2`, `>>>=`) within test cases
- Fails with detailed error showing file, line number, violation type, and fix

**Pre-commit Hook** (`scripts/pre-commit`): Optional secondary guard scanning staged `.test` files.

**Correct Pattern**: Use relative paths that resolve at command execution time:
```batch
# WRONG (banned):
cd test\cli\output\work_mytest & bit remote add origin "%CD%\test\cli\output\remote_mirror"

# CORRECT:
cd test\cli\output\work_mytest & bit remote add origin ..\remote_mirror
```

### Binary File Test Suite

**Purpose**: Verify bit's binary file handling — classification, metadata format, push/pull round-trips, merge, stash, verify, reset, cherry-pick, rebase, revert, grep, diff/rename, attributes, and format-patch/am.

**Location**: `test/t/` — 20 bash test scripts, 328 tests total.

**Run**: `cd test/t && bash run-tests.sh`

**Framework**: `test/t/test-lib.sh` — minimal git-style test framework with `test_expect_success`, binary helpers (`q_to_nul`, `generate_binary`), and bit-specific assertions (`verify_binary_metadata`, `get_metadata_hash`).

**Details**: See [Binary File Test Suite](../binary-test-suite.md).

---

## Conformance

A **conformance proof** document provides normative traceability and evidence that the implementation satisfies the spec: see **[SPEC-CONFORMANCE.md](../SPEC-CONFORMANCE.md)**. For each requirement it gives (1) a logical proof from code paths and invariants, and (2) empirical evidence via the test suite.
