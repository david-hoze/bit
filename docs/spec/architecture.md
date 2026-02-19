# Architecture

## Directory Structure

### Non-bare repo (`bit init`)

```
project/
├── actual_files/           # User's working directory (large files live here)
│   ├── src/
│   │   └── video.mp4
│   └── data/
│       └── dataset.bin
│
└── .bit/
    ├── index/              # Metadata files live here (Git working tree)
    │   ├── .git/           # Git's internal directory
    │   │   └── bundles/    # Per-remote bundle files (cloud: <name>.bundle)
    │   ├── src/
    │   │   └── video.mp4   # Metadata file (NOT the actual video)
    │   └── data/
    │       └── dataset.bin # Metadata file (NOT the actual data)
    ├── cache/              # Scan/verify cache (mtime+size) to skip re-hashing
    ├── cas/                # Content-addressed store (populated when mode=solid)
    ├── config              # Repo-local config (INI format, git-style sections)
    ├── remotes/            # Named remote configs (typed)
    │   └── origin          # Remote type + optional target
    ├── devices/            # Device identity files
    └── target              # Legacy: single remote URL
```

### Bare repo (`bit init --bare project.git`)

```
project.git/                # Standard git bare repo
├── HEAD
├── config
├── objects/
├── refs/
└── bit/                    # bit-specific data (note: bit/, not .bit/)
    └── cas/                # Content-addressed store (populated when mode=solid)
```

Bare repos are standard git bare repos with a `bit/` subdirectory. Git commands work naturally (git discovers the bare repo). bit does not wrap bare repos in `.bit/index/`. The `bit/` directory (not `.bit/`) is used because bare repos have no hidden directory convention -- all contents are visible at the top level.

### Separated repo (`bit init --separate-git-dir <sgdir> [dir]`)

```
sgdir/                      # Git database (objects, refs, HEAD, config)
├── objects/
├── refs/
├── HEAD
├── config
├── bundles/
├── bit/                    # bit metadata (same structure as .bit/)
│   ├── index/              # Git working tree
│   │   └── .git            # gitlink file: "gitdir: /abs/sgdir"
│   ├── cas/
│   ├── devices/
│   ├── remotes/
│   └── config

workdir/
├── .bit                    # FILE (bitlink): "bitdir: /abs/sgdir/bit"
├── .git                    # FILE (gitlink): "gitdir: /abs/sgdir"
├── actual_files/           # User's working files
```

Separated repos place the git database and bit metadata at a separate directory (`sgdir`), leaving only bitlink and gitlink pointer files in the working directory. This is useful when the working directory is on a slow or space-constrained device. The bitlink file (`.bit` as a file, not a directory) contains `bitdir: <absolute-path>` pointing to the resolved bit metadata directory. All bit operations follow the bitlink transparently.

### Transient paths

Fetch downloads the cloud bundle to `.bit/temp_remote.bundle` before copying it to `.bit/index/.git/bundles/<name>.bundle`. Cloud text-file repair uses a temp file under `.bit/` when uploading restored content.

---

## Layer Contract

The codebase follows strict layer boundaries:

```
Bit/Commands.hs → Bit/Core/*.hs → Bit/Rclone/Run.hs → rclone (only here!)
                      ↓
                   Bit/Git/Run.hs → git (only here!)
```

- **Bit/Rclone/Run.hs** -- Dumb rclone wrapper. Exposes `copyToRemote`, `copyFromRemote`, `copyFromRemoteDetailed`, `moveRemote`, `deleteRemote`, `listRemoteJson`, `listRemoteJsonWithHash`, `checkRemote`, etc. Takes `Remote` + relative paths. Does NOT know about `.bit/`, bundles, `RemoteState`, or `FetchResult`. Captures rclone JSON output as raw UTF-8 bytes to correctly handle non-ASCII filenames (Hebrew, Chinese, emoji, etc.). Uses `bracket` for exception-safe subprocess resource cleanup. **copyFromRemoteDetailed** returns **CopyResult** (NotFound, NetworkError, OtherError) by parsing rclone stderr so fetch and remote-workspace can distinguish "no bundle" from network or other errors.
- **Bit/Git/Run.hs** -- Dumb git wrapper. Knows how to run git commands. Takes args. Does NOT interpret results in domain terms.
- **Bit/Core/*.hs** -- Smart business logic, split by concern. All domain knowledge lives here. Calls Rclone.Run and Git.Run, never calls `readProcessWithExitCode` directly.
- **Bit/Commands.hs** -- Entry point. Discovers repository root via `findBitRoot` (walks up from cwd), computes subdirectory prefix, parses CLI, resolves the remote, builds `BitEnv`, dispatches to `Bit.Core`.

---

## Sync Pipeline

Both push and pull derive file actions from `git diff --name-status` via a shared `deriveActions` function in `Bit.Rclone.Sync`:

- `deriveActions` calls `getDiffNameStatus oldRef newRef` (or `getFilesAtCommit` for first sync), maps each change to an `RcloneAction` via `nameStatusToAction`, then applies `resolveSwaps` to detect mirrored Move pairs (A->B and B->A) and replace each pair with a single `Swap` action.
- This avoids expensive remote scanning -- all planning is derived from git metadata diffs with zero network I/O.

### Working Tree Sync: The `oldHead` Pattern

After any git operation that changes HEAD (merge, checkout), the working directory must be updated to reflect what changed in `.bit/index/`. The mechanism:

1. Capture HEAD *before* the git operation
2. Git changes HEAD + index (merge or checkout)
3. Diff old HEAD vs new HEAD, sync files via `applyMergeToWorkingDir`

`applyMergeToWorkingDir` uses `git diff --name-status oldHead newHead` to determine what changed, then:
- **Added/Modified files**: download binary from remote, or copy text from index
- **Deleted files**: remove from working directory
- **Renamed files**: delete old, download/copy new

**CRITICAL**: `applyMergeToWorkingDir` always reads the actual HEAD after merge from git (via `getLocalHeadE`). It never accepts `newHead` as a parameter. This prevents the bug where `remoteHash` (the remote's tip) was passed instead of the actual merged HEAD, causing local-only files to appear deleted during merge.

The only exception is **first pull** (`oldHead = Nothing`): there is no previous HEAD to diff against, so `syncAllFilesFromHEAD` is used as fallback.

### Unified File Transfer via rclone

All file transfers (cloud and filesystem) use `rcloneCopyFiles` as the universal transfer primitive. One `rclone copy --files-from` subprocess per sync operation, regardless of whether the destination is local or cloud. Text files (content stored in index metadata) are copied individually from the index; only binary files go through the rclone batch.

**Remote path construction**: All remote file paths go through `Transport.remoteFilePath`, which normalizes trailing slashes on the remote URL before joining. This prevents double-slash issues when the remote URL already ends with `/`.

**`toPosix` on rclone arguments**: All paths passed to rclone are converted to forward slashes via `toPosix`. Rclone on Windows accepts both slash styles for local paths, but cloud remote paths (e.g., `gdrive:folder/file`) require forward slashes. Since rclone handles both uniformly, `toPosix` is applied to all arguments for consistency.

---

## IO Safety and Concurrency

### Strict IO for Windows Compatibility

**Problem**: Lazy IO on Windows causes "permission denied" and "file is locked" errors due to file handles remaining open until garbage collection. When scanning hundreds of files concurrently, this manifests as random failures.

**Solution**: Eliminate all lazy IO operations and use strict `ByteString` operations exclusively.

**Strict IO Modules**:
- **`Bit.IO.ConcurrentFileIO`** -- Drop-in replacements for `Prelude` file operations using `ByteString.readFile`/`ByteString.writeFile` which read/write the entire file and close the handle before returning.
- **`Bit.IO.Process`** -- Strict process output capture using strict `Data.ByteString.hGetContents`, concurrent stdout/stderr reading via `async`, and `bracket` for cleanup.
- **`Bit.IO.ConcurrentIO`** -- Type-safe concurrent IO newtype with unexported constructor to prevent `liftIO` smuggling. No `MonadIO` instance (intentional). Async integration via explicit `runConcurrentIO` at concurrency boundaries.
- **`Bit.IO.AtomicWrite`** -- Atomic file writes with Windows retry logic (up to 5 retries with linear backoff, ~750ms total window). Directory-level locking via MVar-based thread coordination.

**HLint Enforcement**: `.hlint.yaml` bans lazy IO functions project-wide (`Prelude.readFile`, `Data.ByteString.Lazy.readFile`, `System.IO.hGetContents`, etc.), preventing lazy IO from being reintroduced.

### Concurrent File Scanning and Metadata Writing

**Directory exclusion** (`collectScannedPaths`): The recursive walk filters `.bit` directories by name at every level. For `.git`, only *directories* are excluded (git internals); `.git` *files* (gitlink pointers in submodules) are allowed through so the scanner copies them to `.bit/index/`, enabling git to detect submodules and create `160000` entries.

**Scanning**: `QSem` limits concurrent file reads to **ioConcurrency** (4x cores, min 4). Each file is fully read, hashed, and closed before moving to next. Per-chunk byte tracking updates every 64 KB chunk for smooth progress.

**Metadata Writing** (`writeMetadataFiles`): Parallel execution with bounded concurrency. Skip-unchanged optimization checks if metadata already matches before writing. Three-phase write: (1) create directories, (2) create parent directories, (3) write files in parallel. Atomic writes for binary metadata using temp-file-rename pattern.

### File Copy Progress Reporting

- Chunked binary copy with byte-level progress (64KB chunks, strict `ByteString`)
- Small files (<1MB) use plain `copyFile`; large files use chunked copy with progress
- TTY detection: shows in-place progress on terminal, silent on non-TTY
- Push/Pull use `rcloneCopyFiles` with rclone's `--use-json-log` output for byte progress
- Repair uses `rcloneCopyto` with per-file JSON progress parsing

---

## Performance Optimizations

### Scan-on-Demand Architecture

**Problem**: The original design scanned the entire working directory *before* dispatching to a command, then maintained a growing `skipScan` whitelist. This was backwards -- wrong default, duplication, fragile.

**Solution**: Invert the logic -- scan on demand, not by default. Commands are classified by whether they need env, scan, or both:

1. **No env needed**: `remote add`, `fsck`, `merge --abort`, `branch --unset-upstream`, `log`, `ls-files`, `branch` (with args). `init` is dispatched even earlier -- before `findBitRoot`.
2. **Env, no scan**: `remote show`, `verify`, `repair`, `rm`, `push`.
3. **Scan only, no env**: `add`, `commit`, `diff`, `mv`, `reset`, `merge` (generic).
4. **Scan + env**: `status`, `restore`, `checkout`, `merge --continue`, `fetch`, `pull`.

**Performance Impact**: Read-only commands have instant response times. Push derives file actions from git metadata diffs with zero network I/O for planning.

---

## Subdirectory Support

bit commands work from any subdirectory of a bit repository, matching git's behavior.

**Repository Discovery** (`findBitRoot`): Starting from the current working directory, walks up the directory tree looking for a `.bit/` directory. If found, `setCurrentDirectory` to the repo root. If no `.bit/` is found and the command requires a repo, exits with "fatal: not a bit repository".

**Prefix Computation**: The relative path from the repo root to the user's original cwd becomes the "prefix" (empty string at root). This prefix is stored in `BitEnv.envPrefix` and threaded through the application.

**Git Dispatch**: `runGitRawIn prefix` and `runGitWithOutputIn prefix` target `.bit/index/<prefix>` via `git -C`, so git natively resolves the user's path arguments relative to their subdirectory.

**Passthrough Functions**: IO passthrough functions (`add`, `commit`, `diff`, `log`, `lsFiles`, etc.) take `FilePath` prefix as their first parameter. BitM passthrough functions (`status`, `rm`, `doRestore`, `doCheckout`) extract prefix from `envPrefix`.

**Unchanged**: Internal repo-level operations (`mergeContinue`, `doMergeAbort`, `unsetUpstream`) and all Core modules (Push, Pull, Fetch, Verify, etc.) continue using un-prefixed git operations -- they always operate on the full repository.
