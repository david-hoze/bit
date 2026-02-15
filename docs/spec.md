# bit Implementation Specification (v3)

## Context and Vision

**bit** is a version control system designed for large files that leverages Git as a metadata-tracking engine while storing actual file content separately. The core insight: Git excels at tracking small text files, so we feed it exactly that — tiny metadata files instead of large binaries.

**Mental Model**: bit = Git(metadata) + rclone(sync) + [CAS(content) when mode=solid]

### bit-lite vs Git vs bit-solid: Content Authority

The three systems differ in where content lives and what guarantees that provides:

**Git** stores file content directly in its object store (`.git/objects/`). Every blob, tree, and commit is content-addressed by SHA-1. When you push, you're transferring objects that are self-verifying. When you pull, the objects you receive are self-verifying. Metadata (commits, trees) and content (blobs) live in the same store. The object store is the single source of truth, and it can always back up any metadata claim.

**bit-solid** adds a content-addressed store (CAS) alongside Git's metadata tracking. Like Git, every version of every file is stored by its hash. The CAS backs up every metadata claim unconditionally — if the metadata says a file existed at commit N with hash X, the CAS has that blob. This enables full binary history, time travel, and sparse checkout.

**bit-lite** has no object store and no CAS. Git tracks only metadata files (2-line hash+size records). The actual binary content lives exclusively in the working tree. There is exactly one copy of each file — the one on disk right now. Old versions are gone the moment you overwrite a file.

**Mode is a per-repo configuration, not a separate product.** A repo can switch between lite and solid at any time via `.bit/config`. The mode controls a single behavior: whether `bit add` writes content into `.bit/cas/` alongside the metadata. See "Mode Configuration" below.

This creates a fundamental architectural constraint that Git and bit-solid don't have: **in lite mode, metadata can become hollow.** If a binary file is deleted, corrupted, or modified without running `bit add`, the metadata in `.bit/index/` claims something that is no longer true. In Git, this situation is impossible — the object store is append-only and self-verifying. In bit-lite, it's the normal consequence of working with mutable files on a regular filesystem.

This constraint gives rise to the **proof of possession** rule (see below): bit-lite must verify that content matches metadata before transferring metadata to or from another repo. Without this rule, hollow metadata propagates between repos, and the system's core value proposition — knowing the true state of your files — is undermined. In solid mode, proof of possession is trivially satisfied because the CAS backs every claim.

### Origin

The key architectural idea: instead of a custom manifest, keep small text files in `.bit/index/` mirroring the working tree's directory structure, each containing just hash and size. Define Git's working tree as `.bit/index/`, and you get `add`, `commit`, `diff`, `log`, branching, and the entire Git command set for free. Git becomes the manifest manager, diff engine, and history store — without ever seeing a large file.

### Comparison with Alternatives

bit occupies a different niche than git-lfs and git-annex:

- **git-lfs**: Stores pointer files in Git, actual files on a server. Server-dependent, transparent but limited. Pragmatic hack.
- **git-annex**: Extremely powerful distributed system with pluggable remotes and policy-driven placement. Extremely complex.
- **bit**: Minimal, explicit, correctness-oriented. Git never touches large files. Dumb remotes via rclone. Filesystem-first.

bit's killer feature is **clarity** — users always know what state their files are in, what bit is about to do, and how to recover from errors.

### Design Philosophy: Stand on git and rclone

bit's architecture rests on one principle: **use git and rclone as primitives, never reimplement what they already do.**

Git is an extraordinarily capable metadata engine — branching, merging, diffing, conflict detection, reflog, gc. rclone is a battle-tested file mover that speaks every cloud protocol. bit's job is to wire them together, not to compete with them.

In practice this means:
- **Git bundles** for serializing history over dumb cloud storage — one file, full DAG, `git fetch` unpacks it. No custom wire format.
- **Git native remotes** for filesystem peers — `git fetch /path/to/.bit/index` and `git merge`, just like two ordinary repos.
- **`git diff --name-status`** for deriving sync actions — no remote file scan, no directory diff algorithm.
- **`rclone copy --files-from`** for bulk file transfer — one subprocess call for all files, works identically for local and cloud destinations.
- **`git merge --no-commit --no-ff`** for three-way merge — git handles the hard work, bit reads the result.

When you find yourself writing logic that git or rclone already handles, stop and wire into the existing tool instead. Every custom reimplementation is a surface area for bugs that git/rclone have already fixed.

---

## Core Architecture

### Directory Structure

**Non-bare repo** (`bit init`):

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

**Bare repo** (`bit init --bare project.git`):

```
project.git/                # Standard git bare repo
├── HEAD
├── config
├── objects/
├── refs/
└── bit/                    # bit-specific data (note: bit/, not .bit/)
    └── cas/                # Content-addressed store (populated when mode=solid)
```

Bare repos are standard git bare repos with a `bit/` subdirectory. Git commands work naturally (git discovers the bare repo). bit does not wrap bare repos in `.bit/index/`. The `bit/` directory (not `.bit/`) is used because bare repos have no hidden directory convention — all contents are visible at the top level.

**Separated repo** (`bit init --separate-git-dir <sgdir> [dir]`):

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

**Transient paths (created and removed by operations):** Fetch downloads the cloud bundle to `.bit/temp_remote.bundle` before copying it to `.bit/index/.git/bundles/<name>.bundle`. Cloud text-file repair uses a temp file under `.bit/` when uploading restored content.

### Mode Configuration (lite vs solid)

The mode is stored in `.bit/config` using git-style INI format under the `[core]` section:

```ini
[core]
    mode = lite
```
or
```ini
[core]
    mode = solid
```

**Setting the mode:** Use `bit config`:

```
bit config core.mode solid    # switch to solid
bit config core.mode lite     # switch to lite
bit config core.mode          # prints current mode
```

When switching to solid, bit prints a hint:
```
$ bit config core.mode solid
Mode set to solid. bit add will now store file content in .bit/cas/.
hint: Run 'bit cas backfill' to store current files for existing commits.
```

When switching to lite:
```
$ bit config core.mode lite
Mode set to lite. bit add will no longer store file content in .bit/cas/.
Existing CAS data is preserved.
```

**Default:** `lite`. If `.bit/config` does not exist or does not contain a `core.mode` key, the repo operates in lite mode. This preserves backward compatibility — all existing repos are implicitly lite.

**What the mode controls:** A single behavior — whether `bit add` copies file content into `.bit/cas/` in addition to writing metadata to `.bit/index/`.

- **lite**: `bit add` writes metadata only. No CAS writes. Proof-of-possession verification is required on push/pull.
- **solid**: `bit add` writes metadata AND copies the blob (keyed by its hash) into `.bit/cas/`. Proof-of-possession is trivially satisfied since the CAS is append-only and self-verifying.

**Switching modes:**

**lite → solid**: `bit config core.mode solid`. From this point forward, `bit add` populates the CAS. Existing history has no CAS backing — the CAS is simply incomplete for older commits. An optional `bit cas backfill` command can walk historical commits and store any blobs that are currently present in the working tree, but it is not required.

**solid → lite**: `bit config core.mode lite`. `bit add` stops writing to the CAS. The existing `.bit/cas/` directory is preserved with all its data — nothing is deleted.

**CAS reads are mode-independent.** Regardless of the current mode, any operation that needs old file content (e.g. `bit restore` from a historical commit) checks `.bit/cas/` as a fallback. If the requested blob exists in CAS, it is used. If not (because the commit predates solid mode), the operation fails with "no content available for this version." This means the mode only gates *writes* — reads always consult the CAS if data is present.

**The mode is local-only.** It is not committed or tracked in git. Different clones of the same project can run in different modes — a laptop might use lite to save space while a NAS uses solid for full history.

**CAS garbage collection:** `bit cas gc` (future) can prune blobs that are not referenced by any reachable commit. The safe default is to never delete CAS data automatically.

### `bit config` Command

`bit config` reads and writes `.bit/config`, which uses git-style INI format with `section.key` keys (matching `git config` conventions). Every key must contain at least one dot.

```
bit config <key>              # get — prints value
bit config <key> <value>      # set — writes value
bit config --list             # dump all key=value pairs
```

`.bit/config` file format:
```ini
[core]
    mode = solid
```

**Validation:** Each key has its own validation. `core.mode` only accepts `lite` or `solid`. Unknown keys are rejected. This prevents typos from silently creating garbage config entries.

**Current config keys:**

| Key | Values | Default | Description |
|-----|--------|---------|-------------|
| `core.mode` | `lite`, `solid` | `lite` | Whether `bit add` writes to CAS |

### The Index Invariant

**Git is the sole authority over `.bit/index/`.** After any git operation that
changes HEAD (merge, checkout, commit), `.bit/index/` is correct by
definition. The only remaining work is mirroring those changes onto the actual
working directory (downloading binaries from remote, copying text files from
the index).

This invariant applies uniformly across all pull/merge paths:

- `git merge` → determines correct metadata → we sync actual files
- `git checkout` (first pull, `--accept-remote`) → determines correct metadata → we sync actual files

No path should write metadata files to `.bit/index/` directly and then commit.
Scanning the remote via rclone and writing the result to the index bypasses git
and **will** produce wrong metadata (rclone cannot distinguish text from binary,
so text files get `hash:/size:` metadata instead of their actual content).

### The Proof of Possession Rule

In Git and in bit-solid, content is always recoverable from an object store. Git stores file content in `.git/objects/`; bit-solid stores it in a content-addressed store (CAS). In both systems, metadata and content are either the same thing or the content is always reachable from the metadata. You can push any metadata you want — the objects back it up unconditionally.

bit-lite is fundamentally different. There is no object store. The working tree is the **only copy** of binary file content. The metadata in `.bit/index/` is just a *claim* — "there exists a file at this path with this hash and this size." If that file is gone, corrupted, or modified since the last commit, the claim is hollow. Pushing hollow claims to a remote means the remote now has metadata that nobody can fulfill. Pulling hollow claims from a remote means your local repo has metadata pointing to content that doesn't exist.

This is the **proof of possession** rule: a repo must not transfer metadata it cannot back up with actual content.

```
             Git / bit-solid                    bit-lite
             ──────────────                     ────────
Content:     Object store (always available)    Working tree (only copy)
Metadata:    Always backed by objects           Only valid if working tree matches
Push safety: Unconditional                      Requires verification first
Pull safety: Unconditional                      Requires remote verification first
```

In solid mode, the CAS serves the same role as Git's object store — proof of possession is trivially satisfied. The verification step can be skipped or reduced to checking that the CAS contains the expected blobs (which is a fast filename check, not a content hash).

The rule applies symmetrically:

**Push (sender must prove possession):**
1. Verify local — every binary file's hash must match its metadata
2. If verification fails, refuse to push
3. If verified, sync files then push metadata (files first so the remote never has metadata referencing missing content)

**Pull (sender must prove possession):**
1. Verify remote — every binary file on the remote must match the remote's metadata
2. If verification fails, refuse to pull (suggest `--accept-remote`, `--force`, or `--manual-merge`)
3. If verified, pull metadata then copy files

**What happens when verification fails:**

On push failure (local doesn't match metadata):
```
$ bit push
error: Working tree does not match metadata.
  Modified: data/model.bin (expected md5:a1b2..., got md5:f8e9...)
  Missing:  data/weights.bin
hint: Run 'bit verify' to see all mismatches.
hint: Run 'bit add' to update metadata, or 'bit restore' to restore files.
```

On pull failure (remote doesn't match its metadata):
```
$ bit pull
error: Remote files do not match remote metadata.
  Modified: data/model.bin (expected md5:a1b2..., got md5:c3d4...)
hint: Run 'bit --remote <name> verify' to see all mismatches.
hint: Run 'bit pull --accept-remote' to accept the remote's actual file state.
hint: Run 'bit push --force' to overwrite remote with local state.
```

The existing divergence resolution mechanisms (`--accept-remote`, `--force`, `--manual-merge`) serve as the escape hatches when the proof of possession check fails.

**Why this matters:** Without this rule, corruption propagates silently through metadata. Repo A has a corrupted file, pushes metadata claiming the file is fine, Repo B pulls that metadata, and now both repos have a lie in their history. The proof of possession rule stops corruption at the boundary — you cannot export claims you can't substantiate.

**Cost:** Verification requires hashing every binary file, which means reading the entire working tree. For a repo with 100GB of binary files, this takes real time. This is the price of not having an object store — the working tree must be checked because it's the only source of truth. Mitigation: scan caching (keyed on path, mtime, size) skips re-hashing unchanged files, so only modified files pay the hashing cost.

**Remote verification cost by transport type:**

- **Cloud remotes (Google Drive, S3, etc.):** `rclone lsjson --hash` returns MD5 hashes as free metadata — Google Drive stores them natively. Verification is essentially a single API call followed by in-memory comparison. Cheap.
- **Cloud remotes (other backends):** Some backends provide native hashes, some don't. Where hashes aren't free, rclone may need to download file content to hash it. Check per-backend.
- **Filesystem remotes:** Requires reading and hashing every binary file on the remote. Same cost as local verification.

**Implementation status:** The proof of possession rule is fully implemented as of this version:

- `bit push` always verifies local working tree before pushing (unconditional — `--force` only affects ancestry check)
- `bit pull` verifies remote before pulling (unless `--accept-remote` or `--manual-merge`)
- `bit fetch` does NOT verify (fetch only transfers metadata, no file sync happens)
- `bit fetch` is silent when already up to date (no stdout), similar to `git fetch`
- Cloud remotes: verified via `Verify.verifyRemote` using `rclone lsjson --hash`
- Filesystem remotes: verified via `Verify.verifyLocalAt` which hashes the remote's working tree
- Verification runs in parallel using bounded concurrency (`Parallel 0` = auto-detect). **Concurrency levels** (from `Bit/Concurrency.hs`): file IO and hashing use **ioConcurrency** = max(4, 4 × getNumCapabilities); network-bound work (rclone, transport) uses **networkConcurrency** = min(8, max(2, 2 × getNumCapabilities)).

### Metadata File Format

Each metadata file under `.bit/index/` mirrors the path of its corresponding real file and contains **ONLY**:

```
hash: md5:a1b2c3d4e5f6...
size: 1048576
```

Two fields only:
- `hash` — MD5 hash of file content (prefixed with `md5:`)
- `size` — File size in bytes

Parsing and serialization are handled by a single canonical module (`Bit/Config/Metadata.hs`) which enforces the round-trip property: `parseMetadata . serializeMetadata ≡ Just`.

**Known deviation from original spec**: The original spec called for SHA256 and `hash=`/`size=` (equals-sign) format. The implementation uses MD5 (matching rclone's native hash) and `hash: `/`size: ` (colon-space) format. This is intentional — MD5 is used everywhere for consistency with rclone comparisons. SHA256 may be added later alongside MD5, not as a replacement.

### Git Configuration

bit runs Git with:
- Git repository initialized in `.bit/index/.git` during `bit init`
- All git operations use the repo at `.bit/index/.git`
- Working tree is `.bit/index` (not the project root)
- Ignore rules: user creates `.bitignore` in the project root. Before any command that uses working tree state (add, commit, status, diff, restore, checkout, merge, reset, mv), bit runs **syncBitignoreToIndex**: if `.bitignore` exists it is copied to `.bit/index/.gitignore` with normalization (line endings, trim, drop empty lines); if `.bitignore` does not exist, `.bit/index/.gitignore` is removed if present so the index has no ignore file. The index working tree is thus always in sync with the user's ignore rules for those commands.

**Init additionally:** Adds the repo's absolute path to `git config --global safe.directory` so git does not report "dubious ownership" when the repo lives on an external or USB drive (e.g. Windows with git 2.35.2+). Creates `.bit/index/.git/bundles` so push/fetch have a place to store per-remote bundle files for cloud remotes. Creates `.bit/cas/` for the content-addressed store (used when mode=solid). These post-init config steps only run on fresh init — re-init (`bit init` on an existing repo) skips them and just forwards to `git init` so that "Reinitialized existing" output, exit codes, and `--initial-branch` warnings work identically to git.

**Branch naming:** On fresh init, bit sets `init.defaultBranch=main` and renames `master` to `main` — unless the user passed `--initial-branch`/`-b`, in which case git's own branch naming is respected and bit does not override it.

**Template path resolution:** `--template=<path>` and `--template <path>` flags are resolved to absolute paths before forwarding to git. This is necessary because git runs inside `.bit/index/` via `-C`, so relative template paths would resolve from the wrong directory.

**Re-init:** Running `bit init` on an existing repo always forwards to `git init` (including on `.bit` bitlink repos from `--separate-git-dir`). Git prints "Reinitialized existing Git repository" and handles format changes, gitdir moves, etc. bit propagates git's exit code and output verbatim.

**Global flags before init:** `bit -c key=val init [dir]` and `bit --bare init [dir]` are dispatched correctly — `-c` pairs and `--bare` are peeled from before the `init` subcommand and forwarded to git in the right position.

### Bare Init

`bit init --bare [dir]` passes through directly to `git init --bare`, then creates a `bit/cas/` subdirectory inside the resulting bare repo. bit does not create `.bit/index/` or any other bit-specific structure — the bare repo is a standard git bare repo that git can discover and operate on natively.

Known bit commands (`add`, `commit`, `status`, etc.) are rejected in bare repos with "fatal: this operation must be run in a work tree" (exit 128). Unknown commands pass through to git, which discovers the bare repo naturally.

### File Handling

- Regular files (binary → metadata, text → content stored directly in index)
- Symlinks — **ignored** (too many edge cases with cloud remotes)
- Device files, sockets, named pipes — **ignored**
- Empty directories — **ignored** (not tracked; many cloud backends don't support them)

---

## Module Architecture

### Layer Contract

The codebase follows strict layer boundaries:

```
Bit/Commands.hs → Bit/Core/*.hs → Bit/Rclone/Run.hs → rclone (only here!)
                      ↓
                   Bit/Git/Run.hs → git (only here!)
```

- **Bit/Rclone/Run.hs** — Dumb rclone wrapper. Exposes `copyToRemote`, `copyFromRemote`, `copyFromRemoteDetailed`, `moveRemote`, `deleteRemote`, `listRemoteJson`, `listRemoteJsonWithHash`, `checkRemote`, etc. Takes `Remote` + relative paths. Does NOT know about `.bit/`, bundles, `RemoteState`, or `FetchResult`. Captures rclone JSON output as raw UTF-8 bytes to correctly handle non-ASCII filenames (Hebrew, Chinese, emoji, etc.). Uses `bracket` for exception-safe subprocess resource cleanup. **copyFromRemoteDetailed** returns **CopyResult** (NotFound, NetworkError, OtherError) by parsing rclone stderr so fetch and remote-workspace can distinguish "no bundle" from network or other errors for user-facing messaging and retry.
- **Bit/Git/Run.hs** — Dumb git wrapper. Knows how to run git commands. Takes args. Does NOT interpret results in domain terms.
- **Bit/Core/*.hs** — Smart business logic, split by concern. `Bit.Core` re-exports the public API. Sub-modules: `Bit.Core.Push` (push logic + PushSeam), `Bit.Core.Pull` (pull + merge + conflict), `Bit.Core.Fetch` (fetch + remote state classification), `Bit.Rclone.Sync` (shared action derivation + working-tree sync), `Bit.Core.Verify` (verify + repair), `Bit.Core.Init` (repo initialization), `Bit.Core.Helpers` (shared types + utilities), `Bit.Core.RemoteManagement` (remote add/show), `Bit.Core.Config` (bit config get/set/list), `Bit.Git.Passthrough` (git command delegation), `Bit.Core.Conflict` (merge conflict resolution). All domain knowledge lives here. Calls Rclone.Run and Git.Run, never calls `readProcessWithExitCode` directly.
- **Bit/Commands.hs** — Entry point. Discovers repository root via `findBitRoot` (walks up from cwd), computes subdirectory prefix, parses CLI, resolves the remote, builds `BitEnv`, dispatches to `Bit.Core`.

### Key Types

| Type | Module | Purpose |
|------|--------|---------|
| `Hash (a :: HashAlgo)` | `bit.Types` | Phantom-typed hash — compiler distinguishes MD5 vs SHA256 |
| `Path` | `bit.Types` | Domain path (bit-tracked file path). `newtype` over `FilePath` to prevent transposition bugs. |
| `FileEntry` | `bit.Types` | Tracked `Path` + `EntryKind` (hash, size, `ContentType`) |
| `BitEnv` | `bit.Types` | Reader environment: cwd, prefix (subdirectory relative to root), remote, force flags |
| `BitM` | `bit.Types` | `ReaderT BitEnv IO` — the application monad |
| `MetaContent` | `Bit.Config.Metadata` | Canonical metadata: hash + size, single parser/serializer |
| `Remote` | `bit.Remote` | Resolved remote: name + URL. Smart constructor, `remoteUrl` for Transport only |
| `RemoteState` | `bit.Remote` | Remote classification: StateEmpty, StateValidBit, StateNonBitOccupied, StateNetworkError |
| `FetchResult` | `bit.Remote` | Bundle fetch result: BundleFound, RemoteEmpty, NetworkError |
| `FetchOutcome` | `bit.Core.Fetch` | UpToDate, Updated { foOldHash, foNewHash }, FetchedFirst, FetchError |
| `RcloneAction` | `Bit.Domain.Plan` | Copy, Move, Delete, Swap — concrete rclone operations. Swap is produced by `resolveSwaps`. |
| `Resolution` | `Bit.Core.Conflict` | KeepLocal or TakeRemote — conflict resolution choice |
| `DeletedSide` | `Bit.Git.Run` (re-exported by `Bit.Core.Conflict`) | DeletedInOurs or DeletedInTheirs — which side deleted in modify/delete conflict |
| `ConflictInfo` | `Bit.Core.Conflict` | ContentConflict, ModifyDelete path DeletedSide, AddAdd — conflict type from git ls-files -u |
| `NameStatusChange` | `Bit.Git.Run` | Added, Deleted, Modified, Renamed, Copied — parsed `git diff --name-status` output (replaces bare `(Char, FilePath, Maybe FilePath)` tuple) |
| `DivergentFile` | `bit.Core.Pull` | Record for remote divergence: path, expected/actual hash and size (replaces bare 5-tuple) |
| `ScannedEntry` | `Bit.Scan.Local` | ScannedFile / ScannedDir — internal type for scan pass, replaces (FilePath, Bool) |
| `FileToSync` | `Bit.Rclone.Sync` | TextToSync / BinaryToSync — classifies files for sync (both carry FilePath only) |
| `BinaryFileMeta` | `Bit.Scan.Verify` | Record: bfmPath, bfmHash, bfmSize — replaces bare (Path, Hash, Integer) tuple |
| `VerifyResult` | `Bit.Scan.Verify` | Record: vrCount, vrIssues — replaces bare (Int, [VerifyIssue]) tuple |
| `VerifyTarget` | `bit.Core.Verify` | VerifyLocal or VerifyRemotePath Remote — whether verify checks local or a specific remote |
| `DeviceInfo` | `Bit.Device.Identity` | UUID + storage type + optional hardware serial |
| `RemoteType` | `Bit.Device.Identity` | RemoteFilesystem, RemoteDevice, RemoteCloud |
| `RemoteLayout` | `Bit.Device.Identity` | LayoutFull, LayoutBare — cloud remote storage layout (set via `--bare` on `remote add`) |
| `BitMode` | `Bit.Core.Config` | ModeLite, ModeSolid — repo mode controlling CAS writes (set via `bit config core.mode`) |

### Sync Pipeline

Both push and pull derive file actions from `git diff --name-status` via a
shared `deriveActions` function in `Bit.Rclone.Sync`:

```haskell
deriveActions    :: Maybe String -> String -> IO [RcloneAction]  -- Nothing = first sync
nameStatusToAction :: NameStatusChange -> RcloneAction           -- Added/Modified → Copy, Deleted → Delete, Renamed → Move
resolveSwaps     :: [RcloneAction] → [RcloneAction]              -- pure! (detects pairwise path swaps)
```

`deriveActions` calls `getDiffNameStatus oldRef newRef` (or `getFilesAtCommit`
for first sync), maps each change to an `RcloneAction` via `nameStatusToAction`,
then applies `resolveSwaps` to detect mirrored Move pairs (A→B and B→A) and
replace each pair with a single `Swap` action.

This avoids expensive remote scanning — all planning is derived from git
metadata diffs with zero network I/O.

### Working Tree Sync: The `oldHead` Pattern

After any git operation that changes HEAD (merge, checkout), the working
directory must be updated to reflect what changed in `.bit/index/`.
The mechanism:

```haskell
oldHead <- getLocalHeadE                        -- 1. Capture HEAD *before* the git operation
-- ... git merge / git checkout ...             -- 2. Git changes HEAD + index
applyMergeToWorkingDir remoteRoot cwd oldHead   -- 3. Diff old HEAD vs new HEAD, sync files
```

`applyMergeToWorkingDir` uses `git diff --name-status oldHead newHead` to
determine what changed, then:
- **Added/Modified files**: download binary from remote, or copy text from index
- **Deleted files**: remove from working directory
- **Renamed files**: delete old, download/copy new

**CRITICAL**: `applyMergeToWorkingDir` always reads the actual HEAD after merge
from git (via `getLocalHeadE`). It never accepts `newHead` as a parameter. This
prevents the bug where `remoteHash` (the remote's tip) was passed instead of the
actual merged HEAD, causing local-only files to appear deleted during merge.

This is used consistently across:
- Clean merges (fast-forward and three-way)
- Conflict resolution merges
- `--accept-remote` (force-checkout)
- `mergeContinue`

The only exception is **first pull** (`oldHead = Nothing`): there is no
previous HEAD to diff against, so `syncAllFilesFromHEAD` (full sync from
current HEAD after checkout) is used as fallback.

### Unified File Transfer via rclone

All file transfers (cloud and filesystem) use `rcloneCopyFiles` as the universal
transfer primitive. One `rclone copy --files-from` subprocess per sync operation,
regardless of whether the destination is local or cloud. Text files (content
stored in index metadata) are copied individually from the index; only binary
files go through the rclone batch.

**Key functions** (Bit.Rclone.Sync):
- `syncAllFilesFromHEAD`: First pull — lists files from HEAD, classifies text/binary, copies text from index, batches binary via `rcloneCopyFiles`
- `applyMergeToWorkingDir`: Merge pull — diffs oldHead..newHead, processes deletions, copies text from index, batches binary via `rcloneCopyFiles`

**Push** (Bit.Core.Push):
- `syncRemoteFiles`: Derives file actions from `git diff -M --name-status` between the remote tracking hash and HEAD (no remote scan needed). Partitions Copy actions from Move/Delete/Swap. Copies are batched into a single `rcloneCopyFiles` call; non-copies are executed individually. Works for both cloud and filesystem remotes.

Both cloud and filesystem paths use the same functions parameterized by a remote
root string (`remoteUrl remote`). rclone handles both local-to-local and
local-to-cloud transfers transparently. Pull, push, and merge orchestration share
the same `oldHead` capture pattern, conflict resolution (`Conflict.resolveAll`),
and tracking ref updates.

**Remote path construction**: All remote file paths go through `Transport.remoteFilePath`, which normalizes trailing slashes on the remote URL before joining. This prevents double-slash issues when the remote URL already ends with `/`.

**`toPosix` on rclone arguments**: All paths passed to rclone are converted to forward slashes via `toPosix`. Rclone on Windows accepts both slash styles for local paths, but cloud remote paths (e.g., `gdrive:folder/file`) require forward slashes. Since rclone handles both uniformly, `toPosix` is applied to all arguments for consistency rather than conditional conversion. UNC paths (`\\server\share`) are not a concern because rclone remotes are configured by name (e.g., `gdrive:`), not by UNC path.

### Conflict Resolution

Conflict resolution is structured as a traversal over a list of conflicts (`Bit.Core.Conflict`): `resolveAll` maps over the list with `resolveConflict`, so each conflict is visited exactly once with correct progress tracking (1/N, 2/N, ...). The decision logic (KeepLocal vs TakeRemote) is cleanly separated from the git checkout/merge mechanics.

**Critical**: After resolving all conflicts, the merge commit must **always** be
created, regardless of whether the index has staged changes. When the user
chooses "keep local" (`--ours`), `git checkout --ours` + `git add` restores
HEAD's version — the index becomes identical to HEAD. A naïve `hasStagedChanges`
check would skip the commit, leaving `MERGE_HEAD` dangling. Git's `commit`
command always succeeds when `MERGE_HEAD` exists (it knows it's recording a
merge), even if the tree is identical to HEAD's tree. Skipping the commit
breaks the next push (ancestry check fails because HEAD was never advanced
past the merge).

**merge --continue and merge --abort (implementation):** Understanding the control flow here is important for maintainers; getting it wrong can lead to data loss.

- **merge --continue** has two branches:
  1. **No git conflicts and no `.bit/conflicts` directory:** If `MERGE_HEAD` exists, create the merge commit ("Merge remote"), print "Merge complete.", then sync the working tree by calling **syncBinariesAfterMerge(remote, oldHead)** — which diffs oldHead vs current HEAD and copies binaries from the remote (and text from the index) so the working tree matches the merged metadata. This is the path when the user resolved conflicts via the interactive (l)ocal/(r)emote prompt during pull.
  2. **`.bit/conflicts` directory exists** (e.g. after `bit pull --manual-merge` with remote divergence): Before committing, **validateMetadataDir** is run on `.bit/index`. If any metadata file contains conflict markers (e.g. from a bad manual edit), the merge is aborted with "fatal: Metadata files contain conflict markers. Merge aborted." — this gate prevents committing corrupted metadata. If validation passes, the merge commit is created ("Merge remote (manual merge resolved)"), ".bit/conflicts" is removed, "Conflict directories cleaned up." is printed, then syncBinariesAfterMerge(remote, oldHead) runs as above.
- **merge --abort:** Runs `git merge --abort`. If `.bit/conflicts` exists, it is removed and "Conflict directories cleaned up." is printed.

Pull uses `applyMergeToWorkingDir` to sync after a merge; when the user runs **merge --continue** on its own (e.g. after resolving conflicts manually), sync is done by syncBinariesAfterMerge so the working tree still matches HEAD.

---

## Command Line Interface (Git-Compatible)

**CRITICAL**: The CLI mirrors Git's interface. Users familiar with Git should feel immediately at home. Exit codes follow git's convention: 0 for success, 1 for failure.

Help (`bit help`, `bit -h`, `bit --help`, and `bit help <command>`) works without a repository; all other commands require `.bit` to exist (except `bit init`).

### Command Mapping

| Command | Git Equivalent | bit Behavior |
|---------|---------------|---------------|
| `bit init` | `git init` | Initialize `.bit/` with internal Git repo |
| `bit init --bare` | `git init --bare` | Create standard bare repo with `bit/cas/` |
| `bit init --separate-git-dir <dir>` | `git init --separate-git-dir` | Place git DB and bit metadata at `<dir>`, leave bitlink/gitlink in working dir |
| `bit add <path>` | `git add` | Compute metadata, write to `.bit/index/`, stage in Git |
| `bit add .` | `git add .` | Add all modified/new files |
| `bit commit -m "msg"` | `git commit` | Commit staged metadata changes |
| `bit status` | `git status` | Show working tree vs metadata vs staged |
| `bit diff` | `git diff` | Show hash/size changes (human-readable) |
| `bit diff --staged` | `git diff --staged` | Show staged metadata changes |
| `bit log` | `git log` | Show commit history |
| `bit restore [options] [--] <path>` | `git restore` | Restore metadata; full git syntax: --staged, --worktree, --source=, etc. |
| `bit checkout [options] -- <path>` | `git checkout --` | Restore working tree from index (legacy syntax) |
| `bit reset` | `git reset` | Reset staging area |
| `bit rm <path>` | `git rm` | Remove file from tracking |
| `bit mv <src> <dst>` | `git mv` | Move/rename tracked file |
| `bit branch` | `git branch` | Branch management |
| `bit merge` | `git merge` | Merge branches |
| `bit remote add <name> <url>` | `git remote add` | Add named remote (does NOT set upstream); cloud defaults to full layout |
| `bit remote add <name> <url> --bare` | — | Add cloud remote with bare (CAS-only) layout |
| `bit remote show [name]` | `git remote show` | Show remote status (includes layout for cloud remotes) |
| `bit config <key>` | `git config <key>` | Get config value |
| `bit config <key> <value>` | `git config <key> <value>` | Set config value |
| `bit config --list` | `git config --list` | List all config entries |
| `bit repair` | — | Verify and auto-repair local files from remotes |
| `bit push [<remote>]` | `git push [<remote>]` | Push to specified or default remote |
| `bit push -u <remote>` | `git push -u <remote>` | Push and set upstream tracking |
| `bit push --set-upstream <remote>` | `git push --set-upstream <remote>` | Push and set upstream tracking (alt) |
| `bit pull [<remote>]` | `git pull [<remote>]` | Pull from specified or default remote |
| `bit pull <remote> --accept-remote` | — | Pull from remote, accept remote state |
| `bit pull --accept-remote` | — | Accept remote file state as truth |
| `bit pull --manual-merge` | — | Interactive per-file conflict resolution |
| `bit fetch [<remote>]` | `git fetch [<remote>]` | Fetch metadata from specified or default remote |
| `bit verify` | — | Verify local files match committed metadata |
| `bit --remote <name> verify` | — | Verify remote files match committed remote metadata |
| `bit --remote <name> repair` | — | Verify and auto-repair remote files |
| `bit fsck` | `git fsck` | Check integrity of internal metadata repository |
| `bit merge --continue` | `git merge --continue` | Continue after conflict resolution |
| `bit merge --abort` | `git merge --abort` | Abort current merge |
| `bit branch --unset-upstream` | `git branch --unset-upstream` | Remove tracking config |
| `bit --remote <name> init` | — | Create empty bundle on remote (ephemeral) |
| `bit --remote <name> add <path>` | — | Scan remote, write metadata, auto-commit, push bundle (ephemeral) |
| `bit --remote <name> commit -m <msg>` | — | Commit in ephemeral workspace and push bundle |
| `bit --remote <name> status` | — | Scan remote and show status including untracked files (read-only, ephemeral) |
| `bit --remote <name> log` | — | Show remote workspace history (read-only, ephemeral) |
| `bit --remote <name> ls-files` | — | Show tracked files on remote (read-only, ephemeral) |
| `bit @<remote> <cmd>` | — | Shorthand for `--remote` (needs quoting in PowerShell) |

**Remote show:** For the given remote (or default), bit prints the remote name, Fetch URL, and Push URL. For cloud remotes, Push URL is N/A (push goes via rclone to the same target); layout is always shown (`Layout: full` or `Layout: bare`). For filesystem remotes, both URLs are shown (no layout — filesystem remotes are always full repos). When the local branch has an upstream, status (ahead/behind) is also shown.

Example output for a bare cloud remote:
```
$ bit remote show backup
  Remote: backup
  Type: cloud
  Target: gdrive:Backup/myproject
  Layout: bare
  HEAD branch: main
    main pushes to main (up to date)
```

**Cloud remotes:** Status is derived from the local bundle at `.bit/index/.git/bundles/<n>.bundle`. If that bundle does not exist, `bit remote show <n>` fetches it first (same as `bit fetch`), then shows status; the user may see fetch output (e.g. “Updated: …” or “Fetched: …”) as a side effect. If the fetch fails (e.g. network error), bit prints only the URLs and reports status as (unknown) — “HEAD branch: (unknown)”, “main pushes to main (unknown)” — so the command still completes without crashing.

---

## Upstream Tracking (Git-Standard Behavior)

**IMPORTANT**: bit follows git's upstream tracking conventions exactly:

1. **`bit remote add <name> <url>` does NOT set upstream** — unlike old bit behavior, adding a remote (even "origin") does not auto-configure `branch.main.remote`. This matches git.

2. **`bit push -u <remote>` sets upstream** — the `-u` / `--set-upstream` flag pushes and configures `branch.main.remote = <remote>` in one operation. This is the standard way to establish upstream tracking.

3. **Commands accept explicit remote names**:
   - `bit push <remote>` — push to named remote (no tracking change)
   - `bit pull <remote>` — pull from named remote (no tracking change)
   - `bit fetch <remote>` — fetch from named remote (no tracking change)

4. **Default remote selection**:
   - If `branch.main.remote` is set, it's used as the default
   - If not set, **`bit push` requires explicit remote** (no fallback — matches `git push` with `push.default=simple`)
   - If not set, **`bit pull` requires explicit remote** (no fallback)
   - If not set and "origin" exists, **`bit fetch` uses it as fallback** (git-standard behavior — fetch is read-only)
   - If neither upstream nor "origin", commands fail with error suggesting `bit push <remote>` or `bit push -u <remote>`

5. **First pull does NOT set upstream**: When pulling for the first time (unborn branch), `checkoutRemoteAsMain` uses `git checkout -B main --no-track refs/remotes/<name>/main` (where `<name>` is the remote being pulled from, e.g. `origin`). This prevents automatic upstream tracking setup. Users must use `bit push -u <remote>` to explicitly configure tracking.

6. **Internal git remote vs upstream tracking**: bit's internal git repo has a remote named "origin" (used for fetching refs from bundles), but this is distinct from upstream tracking config (`branch.main.remote`). The internal remote is set up automatically; upstream tracking is never automatic. This distinction is critical: `Git.addRemote` configures the internal git remote (required for bundle operations), while `Git.setupBranchTrackingFor` sets `branch.main.remote` (must only be called from `push -u`).

This makes bit's remote behavior predictable for git users: explicit tracking setup via `-u`, explicit remote selection via argument, sensible defaults when configured, and git-standard fallback to "origin" for fetch (read-only) operations only.

---

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

**Rationale**: Push files first so the remote is never in a state where metadata references missing content. Pull metadata first so we know what content to fetch. After the git operation, the index is authoritative — we only need to bring actual files into alignment.

### Remote Types and Device Resolution

bit supports two kinds of remotes:

- **Cloud remotes**: rclone-based (e.g., `gdrive:Projects/foo`). Identified by URL. Uses bundle + rclone sync.
- **Filesystem remotes**: Local/network paths (USB drives, network shares, local directories). Creates a **full bit repository** at the remote location.
  - **UNC paths**: Under MINGW / Git Bash, UNC paths must use forward slashes (`//server/share/path`). The shell strips leading backslashes before bit receives the argument. bit normalizes forward-slash UNC paths and displays the canonical backslash form.
  - **Platform layer**: GHC's `System.Directory` prepends `\\?\UNC\` to UNC paths for long-path support; virtual UNC providers (RDP tsclient, WebDAV) reject this prefix. `Bit.IO.Platform` bypasses the prefix for UNC paths by calling Win32 directly, while keeping `System.Directory` (with long-path support) for local paths.

The `Bit.Device.Identity` module handles remote type classification and device resolution:
- `RemoteType`: `RemoteFilesystem` (fixed paths), `RemoteDevice` (removable/network drives), `RemoteCloud` (rclone-based)
- `isFilesystemType`: True for both `RemoteFilesystem` and `RemoteDevice` (both use direct git operations)
- Physical storage: Identified by UUID + hardware serial (survives drive letter changes)
- Network storage: Identified by UUID only
- Each volume can have a `.bit-store` file at its root containing its UUID
- Remote configs stored in `.bit/remotes/<name>` with typed format:
  - `type: filesystem` (path lives only in git config as named remote)
  - `type: cloud\ntarget: gdrive:Projects/foo` (rclone path for transport)
  - `type: device\ntarget: black_usb:Backup` (device identity for resolution)
- **Cloud remote layout:** Cloud remotes have a `layout` field that controls what is stored on the cloud:
  - `layout: full` (default, backward compatible): Files are stored at human-readable paths mirroring the working tree. Users can browse files in Google Drive / S3 / etc. This is the current behavior.
  - `layout: bare`: Files are stored only in CAS layout (`cas/<prefix>/<hash>`). The cloud folder is opaque to humans — no browsable file tree, just hashed blobs and a metadata bundle. This is for pure backup where browsability is not needed.
  - Example remote config for bare layout: `type: cloud\ntarget: gdrive:Backup/foo\nlayout: bare`
- **`bit remote add --bare`**: The `--bare` flag on `bit remote add` sets `layout: bare` in the remote config. It is only valid for cloud remotes — filesystem and device remotes are always full repos. Without `--bare`, cloud remotes default to `layout: full`. Examples:
  ```
  bit remote add origin gdrive:Projects/foo            # layout: full (default)
  bit remote add backup gdrive:Backup/foo --bare       # layout: bare
  ```
- **No cloud remote layout conversion.** Switching a remote between `layout: full` and `layout: bare` is not supported. To change layout, create a new remote with the desired layout and push to it. This avoids complex migration logic and keeps the remote lifecycle simple.
- **Device add fallback:** When adding a removable or network path, bit may prompt for a device name and write `.bit-store` at the volume root. If writing `.bit-store` fails (e.g. permission denied on `C:\`), the remote is added as path-only (RemoteFilesystem) so the user still has a working remote; device identity is not persisted. This silent degradation is worth specifying so users and maintainers know the fallback exists.

All remote types get a **named git remote** inside `.bit/index/.git`:
- Filesystem: `git remote add dok1 /path/to/remote/.bit/index` — Git talks directly to the remote repo.
- Cloud: `git remote add backup .git/bundles/backup.bundle` — Per-remote bundle; rclone downloads from `.bit/remotes/<name>` (rclone URL) first, then git fetches from the local bundle. Cloud has extra indirection: rclone → local bundle → git.
- Device: `git remote add usb1 /mnt/usb/.bit/index` (URL updated at operation time) — Same as filesystem once resolved.

**Cloud vs filesystem/device**: For cloud, the git remote URL and bit's actual target serve different purposes. The git remote points to a local bundle file (`.git/bundles/<name>.bundle`); bit's rclone target is in `.bit/remotes/<name>`. Rclone downloads the remote's bundle to the local file; then git fetches from that file. For filesystem/device, git talks directly to the remote repo — no bundle staging.

The `bit.Remote` module provides type-aware resolution via `resolveRemote`:
```
resolveRemote :: FilePath -> String -> IO (Maybe Remote)
-- Dispatches on RemoteType:
--   RemoteFilesystem → reads URL from git config, strips .bit/index suffix
--   RemoteDevice     → resolves device UUID to mount path
--   RemoteCloud      → reads target from remote file
--   Nothing          → backward-compat fallback (infers type from old format)
```

### Remote state classification

Before push or fetch, the implementation classifies the remote with `classifyRemoteState`: it lists the remote at **depth 2** (`listRemoteItems remote 2`), then interprets the result purely:

- **StateEmpty** — No items. Push will create and initialize the remote (e.g. `initializeRepoAt`). Fetch/pull abort with "Remote is empty. Run 'bit push' first."
- **StateValidBit** — Any of: path `.bit/bit.bundle`, path `.bit/index`, any path with prefix `.bit/`, or name `.bit` (covers path format differences). Push and fetch proceed.
- **StateNonBitOccupied** — Otherwise; up to 3 paths are shown. Push and fetch abort with "The remote path is not empty and not a bit repository" and the listed paths.

### Transport Strategies

The transport strategy is determined by `RemoteType` classification:

```
Device.readRemoteType / isFilesystemType
  ├── RemoteCloud      → Cloud transport (bundle + rclone, existing flow)
  ├── RemoteDevice     → Filesystem transport (full repo at remote)
  └── RemoteFilesystem → Filesystem transport (full repo at remote)
```

#### Cloud Transport (Bundle + Rclone)

For cloud remotes (Google Drive, S3, etc.), bit uses **dumb storage**:
- Metadata is serialized as a Git bundle and uploaded via rclone
- Files are synced via rclone copy/move/delete operations
- The remote is just a directory of files — no Git repo, no bit commands work there

Cloud remotes support two layouts:

**Full layout** (`layout: full`, default): Files are stored at human-readable paths on the cloud, mirroring the working tree structure. Users can open Google Drive and browse files directly. This is the current behavior.

```
gdrive:Projects/foo/
├── .bit/
│   └── bit.bundle          # Metadata bundle
├── lectures/
│   ├── lecture-01.mp3       # Actual files at readable paths
│   └── lecture-02.mp3
└── data/
    └── dataset.bin
```

**Bare layout** (`layout: bare`): Files are stored only in CAS layout — hashed blobs organized by prefix. No human-readable file tree exists on the cloud. This is for pure backup where browsability is not needed.

```
gdrive:Backup/foo/
├── .bit/
│   └── bit.bundle          # Metadata bundle
└── cas/
    ├── a1/
    │   └── a1b2c3d4e5f6...  # Content blob (keyed by hash)
    ├── f8/
    │   └── f8e9a0b1c2d3...
    └── ...
```

**Bare layout push behavior:** On push to a bare remote, bit hashes each file in the working tree and uploads it into the remote's CAS layout (`cas/<first-2-chars>/<full-hash>`) via rclone, then uploads the metadata bundle. This works regardless of local mode — a lite-mode repo streams content directly to the remote CAS without populating local CAS. The local repo pays no disk cost; the remote gets a complete content-addressed backup.

**Bare layout pull behavior:** On pull from a bare remote, bit fetches the metadata bundle and merges as usual. For file sync, it reads the metadata to determine which hashes are needed, then downloads the corresponding blobs from `cas/<prefix>/<hash>` on the remote and places them at the correct working tree paths.

**Bare layout verification:** For bare cloud remotes, remote verification compares the hashes in the remote's metadata bundle against the CAS blob names on the remote (the blob filename *is* the hash). No content re-hashing is needed — if `cas/a1/a1b2c3d4e5f6...` exists, the content is correct by construction.

**Cloud bundle lifecycle (both layouts):**

- **On the remote:** The metadata bundle is always stored at **`.bit/bit.bundle`** (single file).
- **Fetch:** (1) Download from remote to staging path `.bit/temp_remote.bundle`. (2) Copy to per-remote path `.bit/index/.git/bundles/<sanitized-name>.bundle` and remove the temp file. (3) Register the bundle as a git remote and run `git fetch <name>` to populate `refs/remotes/<name>/main`. (4) If `git fetch` does not set the ref (e.g. bundle format), the implementation uses `git bundle list-heads` and manually updates the tracking ref so status and pull still work.
- **Push:** Create the bundle at `.bit/index/.git/bundles/<name>.bundle` (`git bundle create ... --all`), then upload it via rclone to the remote as `.bit/bit.bundle`.
- **Per-remote local bundles:** Each remote has its own local bundle file so that sequential fetches (e.g. `bit fetch gdrive` then `bit fetch backup`) do not overwrite one another.

#### Filesystem Transport (Full Repo)

For filesystem remotes, bit creates a **complete bit repository** at the remote:
- The remote has `.bit/index/.git/` just like a local repo
- Anyone at the remote location can run `bit status`, `bit log`, `bit commit`, etc.
- No bundles needed — Git can talk directly repo-to-repo via `git fetch /path/to/other/.bit/index/.git/`

**Key insight**: Bundles exist to serialize git history over dumb transports that can only copy files. With filesystem access, git speaks its native protocol.

**Filesystem remote receives pushes via direct path**: The remote side does not need a named git remote pointing back. Different machines with different local paths can push to the same filesystem remote; the pusher passes its path directly to `git fetch`.

#### Git Native Remotes

bit registers each bit remote as a **git native remote** inside `.bit/index/.git/`, using the same name. This is how git fetch/push/merge and tracking refs work for free.

The remote URL depends on the transport:

| Remote type | git remote URL | Example |
|---|---|---|
| Filesystem | `<remotePath>/.bit/index` | `/mnt/usb/project/.bit/index` |
| Cloud | `.git/bundles/<name>.bundle` (relative to `.bit/index/`) | `.git/bundles/gdrive.bundle` |

When `bit remote add origin /mnt/usb/project` runs, it calls `Git.addRemote "origin" "/mnt/usb/project/.bit/index"` — after that, `git fetch origin` inside `.bit/index/` talks directly to the remote's index repo.

When `bit remote add gdrive gdrive:projects/video` runs, it calls `Git.addRemote "gdrive" ".git/bundles/gdrive.bundle"` — the bundle file at `.bit/index/.git/bundles/gdrive.bundle` is downloaded from the cloud via rclone before each fetch, and `git fetch gdrive` unpacks it into `refs/remotes/gdrive/main`.

This means `git log --all`, `git diff refs/remotes/origin/main`, `bit status` comparison — all of these work because git already knows about the remote refs. bit does not maintain its own ref database.

#### Unified Push Architecture

Push uses a single code path for both cloud and filesystem remotes. The only
difference is the metadata transport — abstracted behind a `PushSeam`:

```haskell
data PushSeam = PushSeam
    { ptFetchHistory :: IO (Maybe String)  -- Fetch remote history, return remote hash
    , ptPushMetadata :: IO ()              -- Push metadata to remote after file sync
    }
```

Everything else is shared: `classifyRemoteState`, `syncRemoteFiles` (git-diff-based
file sync — no remote scan), ancestry checks, `--force`/`--force-with-lease`, verification.

**Cloud seam** (`mkCloudSeam`): Downloads/uploads git bundles via rclone. Bundles
are stored per-remote at `.bit/index/.git/bundles/<name>.bundle` so `bit status`
can compare against them. For bare-layout remotes, file sync uploads content
into CAS layout (`cas/<prefix>/<hash>`) instead of readable paths — the seam
reads the remote's `layout` field from `.bit/remotes/<n>` to determine the
target path scheme.

**Filesystem seam** (`mkFilesystemSeam`): Takes current working directory and remote; uses native git fetch/pull. The local index registers the remote's `.bit/index` as a named git remote; metadata is pushed via `git pull --ff-only` at the remote side.

**Push flow** (both transports):
1. **Verify local** (proof of possession)
2. **First-push detection** (filesystem only): If remote `.bit/` doesn't exist, create and initialize it
3. **Classify remote state** via rclone (`classifyRemoteState`)
4. **Fetch remote history** via seam (`ptFetchHistory`)
5. **Ancestry/force checks** (`processExistingRemote`)
6. **Sync files** via git diff + rclone (`syncRemoteFiles`) — derives actions from `getDiffNameStatus`, no remote scan
7. **Push metadata** via seam (`ptPushMetadata`)
8. **Update tracking ref** (`Git.updateRemoteTrackingBranchToHead`) — same for both

#### Unified Pull Architecture

Pull mirrors the push design — a single code path for both cloud and filesystem
remotes, with only the metadata fetch abstracted behind a `PullSeam`:

```haskell
data PullSeam = PullSeam
    { psFetchMetadata :: IO Bool       -- Fetch remote metadata, return success
    , psVerifyRemote  :: FilePath -> IO ()  -- Verify remote before merge
    }
```

**Cloud seam** (`mkCloudPullSeam`): Downloads the bundle via rclone, saves to
`.bit/index/.git/bundles/<name>.bundle`, runs `git fetch <name>` to populate
`refs/remotes/<name>/main`.

**Filesystem seam** (`mkFilesystemPullSeam`): Runs `git fetch <remotePath>/.bit/index`
directly — no bundle, no rclone, git talks repo-to-repo.

Everything else is shared: merge orchestration, conflict resolution, file sync,
`--accept-remote`, `--manual-merge`, tracking ref updates.

**Pull flow** (both transports):
1. **Fetch remote metadata** via seam (`psFetchMetadata`)
2. **Verify remote** via seam (`psVerifyRemote`) — unless `--accept-remote` or `--manual-merge`
3. **Dispatch by mode**:
   - Normal: `git merge --no-commit --no-ff`, then `applyMergeToWorkingDir`
   - `--accept-remote`: Force-checkout (with `--no-track`), then sync files
   - `--manual-merge`: Detect divergence, create conflict directories
4. **Sync files**: Text from index, binary via rclone (same `rcloneCopyFiles`)
5. **Update tracking ref**: Set `refs/remotes/<name>/main` to the fetched hash

#### Text vs Binary File Sync

For filesystem remotes, file sync distinguishes text from binary by examining the metadata file content:

```haskell
isTextMetadataFile :: FilePath -> IO Bool
-- Returns True if file exists and does NOT contain "hash: " line
-- Text files: metadata IS the content (stored directly in index)
-- Binary files: metadata contains "hash: " and "size: " (pointer to actual file)
```

- **Text files**: Content lives in `.bit/index/path`. After git merge/checkout, copy from index to working tree.
- **Binary files**: Content lives in working tree. Copy from source working tree to destination working tree.

#### The `git push` Antipattern

Do NOT use `git push` to a non-bare repo. Git refuses to update the checked-out branch:
```
error: refusing to update checked-out branch: refs/heads/main
```

The correct approach: Have the remote **fetch** from local, then **merge --ff-only**. This is what filesystem push does.

---

## Remote-Targeted Commands (`@<remote>` / `--remote <name>`)

### Problem Statement

When a user has large amounts of data already on a remote (e.g., 10GB of files on Google Drive), the traditional workflow requires:
1. Download everything locally (expensive, time-consuming)
2. Run `bit init`, `bit add`, `bit commit` locally
3. Push everything back to the remote (redundant upload)

This is wasteful when the files are already at the destination. The user wants to **create metadata for files already on the remote** without downloading them.

### Solution: Ephemeral Remote Workspaces

The `--remote <name>` flag (or its `@<remote>` shorthand) allows commands to operate against a cloud remote as if it were a working directory, while only downloading small files (for text classification). Large binary files stay on the remote — bit just reads their hashes from `rclone lsjson --hash`. Remote workspaces are only supported for cloud remotes; for filesystem remotes, navigate to the directory and run bit commands there. (`verify` and `repair` work for both types via `--remote`.)

**Key architectural property**: Each command is fully ephemeral. The workflow for every command is:
1. Fetch the bundle from the remote
2. Inflate it into a temporary directory
3. Operate on it (scan, add, commit, status, log, ls-files)
4. Re-bundle the changes (if any)
5. Push the new bundle back to the remote
6. Clean up the temporary workspace

No persistent local workspace state exists between commands. The remote's `.bit/bit.bundle` is the sole source of truth.

### The `--remote` Flag

Two equivalent syntaxes exist for specifying a remote target:

```bash
bit --remote origin init      # portable — works in all shells
bit @origin init              # shorthand — needs quoting in PowerShell
```

**Why `--remote` exists**: The `@<remote>` prefix doesn't work in PowerShell because `@` is the splatting operator. PowerShell interprets `@gdrive` as splatting the variable `$gdrive`, which is undefined, so the argument is silently dropped. `--remote <name>` is the portable alternative that works everywhere.

**Placement**: Both `--remote <name>` and `@<remote>` must appear as the first argument(s) to `bit`. This is consistent between the two forms and avoids ambiguity with subcommand flags.

**`--remote` is recommended** for scripts and cross-shell compatibility. `@<remote>` remains available as a convenient shorthand for interactive use in bash/zsh/cmd.

### User Workflow

```bash
# Files already exist on gdrive:Projects/footage (10GB of video + some .txt/.md)
bit init                                    # local repo (empty working dir)
bit remote add origin gdrive:Projects/footage

bit --remote origin init                    # create empty bundle on remote
bit --remote origin add .                   # scan remote, write metadata, auto-commit, push bundle

bit pull                                    # pull metadata locally (instant — just the bundle)
# Working dir is still empty, but bit knows about all 847 files
# User can then selectively download, or bit pull will sync everything
```

### Architecture

#### The `@<remote>` / `--remote <name>` Prefix

Both forms are parsed in `Commands.hs` by `extractRemoteTarget` before command dispatch. When present, it switches the execution context from "local working directory" to "remote-targeted workspace."

The remote name is resolved via the existing `resolveRemote` function, same as named remotes for push/pull.

#### Ephemeral Workspace Pattern

There is no persistent workspace on disk. Each command creates a temporary directory in the system temp folder, operates, and cleans up:

```
%TEMP%/
├── bit-remote-init/    ← used by 'init' (one-time)
├── bit-remote-ws/      ← used by 'add', 'commit' (read-write)
│   ├── bit.bundle      ← fetched bundle
│   ├── workspace/      ← inflated git repo
│   │   ├── .git/
│   │   └── <metadata>
│   └── new.bundle      ← re-bundled after changes
└── bit-remote-ro/      ← used by 'status', 'log' (read-only)
```

All temporary directories are exception-safe via `bracket` — cleanup runs even if the command fails. Leftover directories from previous runs are removed at setup to prevent collisions.

#### Bundle Inflation

Inflating a bundle into a workspace uses a short sequence designed to avoid Git pitfalls:

```
git init --initial-branch=main
git config core.quotePath false
git fetch <bundle> +refs/heads/*:refs/remotes/bundle/*
git reset --hard refs/remotes/bundle/main
```

**Why this specific sequence:**
- `core.quotePath false` ensures non-ASCII paths (e.g. Hebrew, emoji) are not quoted in git output
- Fetching into `refs/remotes/bundle/*` (not `refs/heads/*`) avoids Git's "refusing to fetch into checked out branch" error
- `git reset --hard` (not `checkout -B`) ensures the working tree is populated — `checkout -B` can skip the working tree update when already on the same branch name
- `git clone` was avoided because on Windows, `removeDirectoryRecursive` can fail to fully clean up temp directories due to file locking, causing "directory already exists" errors

#### Text File Classification Without Full Download

The `rclone lsjson --hash` scan returns hashes for all files, but `fContentType = BinaryContent` for everything (rclone can't classify text without reading content). bit's text classification needs:
1. File size (available from rclone scan)
2. File extension (available from path)
3. First 8KB of content (for NULL-byte and UTF-8 checks)

Strategy:
- Files above `textSizeLimit` (default 1MB) → binary, no download needed
- Files with `binaryExtensions` (`.mp4`, `.zip`, etc.) → binary, no download needed
- Remaining small files → download to temp dir, classify with existing `hashAndClassifyFile`

For a typical 10GB media repo, this downloads maybe 50KB of text files while skipping the 10GB of video.

### Supported Commands

| Command | Behavior |
|---------|----------|
| `bit --remote <name> init` | Create empty bundle on remote (no scan — just initializes history) |
| `bit --remote <name> add <path>` | Fetch bundle → scan remote → classify files → write metadata → auto-commit → push bundle |
| `bit --remote <name> commit <args>` | Fetch bundle → commit with provided args → push bundle (useful for amending) |
| `bit --remote <name> status` | Fetch bundle → scan remote → write metadata → show git status (read-only, no push). Shows untracked files that exist on remote but aren't committed. |
| `bit --remote <name> log` | Fetch bundle → show git log (read-only, no push) |
| `bit --remote <name> ls-files` | Fetch bundle → show tracked files (read-only, no push) |
| `bit --remote <name> verify` | Verify remote files match committed remote metadata |
| `bit --remote <name> repair` | Verify and auto-repair remote files |

The `@<remote>` shorthand is equivalent (e.g., `bit @origin init`).

All other commands are not supported in remote context (e.g., `bit --remote origin push` will error).

### Implementation Details

Located in `Bit.Device.RemoteWorkspace`:

#### Core Helpers

- **`withTempDir`**: Creates a named temp directory under `%TEMP%`, exception-safe via `bracket`. Removes leftover from previous runs at setup, cleans up on exit.
- **`withRemoteWorkspace`**: Fetches bundle → inflates → runs action → if HEAD changed, re-bundles and pushes → cleans up. For read-write commands (`add`, `commit`).
- **`withRemoteWorkspaceReadOnly`**: Fetches bundle → inflates → runs action → cleans up (no push). For read-only commands (`status`, `log`).

#### 1. `bit --remote origin init` (or `bit @origin init`)

Does NOT use `withRemoteWorkspace` — there's no bundle to fetch yet.

1. Creates a temp directory
2. Checks if bundle already exists on remote (errors if it does)
3. `git init --initial-branch=main` in the temp workspace
4. `git commit --allow-empty -m "Initial remote repository"`
5. `git bundle create` from the workspace
6. Pushes bundle to remote at `.bit/bit.bundle` via rclone
7. Cleans up temp directory

#### 2. `bit --remote origin add <path>` (or `bit @origin add .`)

Uses `withRemoteWorkspace` (read-write):

1. Fetches and inflates the bundle
2. **Scans** the remote via `rclone lsjson --hash --recursive`
3. **Classifies** files into binary (by size/extension) and text candidates
4. Downloads text candidates, classifies via `hashAndClassifyFile`
5. **Clears** the workspace (removes all files except `.git`)
6. **Writes** metadata: text files get their content downloaded from remote, binary files get `hash:/size:` metadata
7. `git add` the specified paths (or `.`)
8. If changes exist, auto-commits with "Update remote metadata"
9. Re-bundles and pushes if HEAD changed

The scan+classify+write step happens on **every** `add` call — the workspace starts fresh each time, so the full remote state is reconstructed.

#### 3. `bit --remote origin commit`

Uses `withRemoteWorkspace` (read-write). Passes args through to `git commit` in the ephemeral workspace. Useful for amending the last commit message (`--amend -m "Better message"`).

#### 4. `bit --remote origin status`

Uses `withRemoteWorkspaceReadOnly`. Scans the remote via `scanAndWriteMetadata` (same as `add`) to detect untracked files, then runs `git status`. After writing metadata, runs `git add -u && git reset HEAD` to update index stat info without staging changes (prevents false "modified" reports due to stat-dirty optimization). No changes are pushed back.

#### 5. `bit --remote origin log`

Uses `withRemoteWorkspaceReadOnly`. Passes args through to `git log` in the ephemeral workspace. No changes are pushed back.

### Integration with Normal Pull

After `bit --remote origin add .`, the remote has a bundle at `.bit/bit.bundle`. A local `bit pull` will:
1. Fetch the bundle (existing `fetchBundle` logic)
2. Import it to local `.bit/index/.git/`
3. Merge the metadata
4. Sync files as needed (download from remote or copy from index)

The existing pull flow handles this transparently.

### Error Handling

- **Bundle fetch fails (no bundle)**: `withRemoteWorkspace` and `withRemoteWorkspaceReadOnly` print "fatal: no bit repository on remote. Run 'bit @remote init' first." and return `ExitFailure 1`.
- **Network error**: Propagated with "fatal: network error: ..." message.
- **Push fails**: `bundleAndPush` prints "fatal: failed to push bundle to remote." and exits.
- **Init when already initialized**: Checks for existing bundle first, errors with "fatal: remote already has a bit repository."
- **Temp directory cleanup**: `bracket` ensures cleanup runs on all code paths (success, failure, exception).

### Edge Cases

- **Remote is empty**: `scanAndWriteMetadata` finds zero files, auto-commit has nothing to stage — reports "Nothing to add."
- **Network failure during classification**: Failed downloads treated as binary, warning printed
- **Filesystem remotes**: Remote workspaces (init, add, commit, status, log, ls-files) are only supported for cloud remotes. For filesystem remotes, navigate to the directory and run bit commands there. `verify` and `repair` work for both cloud and filesystem remotes via `--remote`.
- **Both `@<remote>` and `--remote` specified**: Error with clear message
- **`--remote` without argument**: Error explaining that a name is required

### Limitations

This feature does NOT provide:
- **Selective file download** — that's a separate feature (sparse working tree). After `bit pull`, all files are expected locally.
- **Incremental remote re-scan** — `bit --remote origin add` always scans from scratch (the workspace is ephemeral).
- **`bit --remote origin push`** — pushing *to* a remote workspace doesn't make sense. Push targets the actual remote.
- **Conflict resolution in remote context** — not needed. The workspace is single-writer (the local user).
- **`--remote` after the subcommand** — `--remote <name>` must appear before the subcommand (first args).

---

## IO Safety and Concurrency

### Strict IO for Windows Compatibility

**Problem**: Lazy IO on Windows causes "permission denied" and "file is locked" errors due to file handles remaining open until garbage collection. When scanning hundreds of files concurrently, this manifests as random failures.

**Solution**: Eliminate all lazy IO operations and use strict `ByteString` operations exclusively.

### Implementation

#### 1. Strict IO Modules

**`Bit.IO.ConcurrentFileIO`** — Drop-in replacements for `Prelude` file operations:
- `readFileBinaryStrict` — strict `ByteString.readFile` wrapper
- `readFileUtf8Strict` — strict UTF-8 text reading
- `readFileMaybe` / `readFileUtf8Maybe` — safe reading with `Maybe` return
- `writeFileBinaryStrict` / `writeFileUtf8Strict` — strict writing

All operations use `ByteString.readFile` / `ByteString.writeFile` which read/write the entire file and close the handle before returning.

**`Bit.IO.Process`** — Strict process output capture:
- `readProcessStrict` — runs a process, strictly captures stdout and stderr as `ByteString`, returns `(ExitCode, ByteString, ByteString)`
- `readProcessStrictWithStderr` — runs a process with inherited stderr (for live progress), strictly captures stdout

Both functions:
- Use strict `Data.ByteString.hGetContents` (not lazy `System.IO.hGetContents`)
- Read stdout and stderr concurrently using `async` to avoid deadlocks when buffers fill
- Ensure all handles are closed and process is waited on in all code paths (using `bracket`)
- Prevent "delayed read on closed handle" errors that occur when using lazy IO with `createProcess`

**`Bit.IO.ConcurrentIO`** — Type-safe concurrent IO newtype:
- Constructor is **not exported** to prevent `liftIO` smuggling
- Only whitelisted strict operations are exposed
- No `MonadIO` instance (intentional restriction)
- Async integration is handled by explicit unwrapping via `runConcurrentIO` at concurrency boundaries (e.g. where `mapConcurrently` is called), preserving the newtype's encapsulation. The codebase does **not** use `MonadUnliftIO`: that would require the monad to be isomorphic to `ReaderT env IO`, and would let any async combinator round-trip back to `IO`, defeating the wrapper's discipline. Explicit unwrapping makes the escape hatch visible at the call site.
- Includes concurrency primitives: `mapConcurrentlyBoundedC`, `QSem` operations

**`Bit.IO.AtomicWrite`** — Atomic file writes with Windows retry logic:
- `atomicWriteFile` — temp file + rename pattern
- `DirWriteLock` — directory-level locking (MVar-based thread coordination)
- `LockRegistry` — process-wide lock registry for multiple workers
- Retry logic for Windows transient "permission denied" errors: up to 5 retries with linear backoff (rationale in Design Decisions #16).

#### 2. Module Updates

All lazy IO replaced with strict operations:
- `Bit/Scan/Local.hs` — `.gitignore` reading uses strict ByteString
- `Bit/Device/Identity.hs` — `.bit-store`, device files, remote files use strict ByteString + atomic writes
- `Bit/Core/Helpers.hs` — `readFileMaybe`, `writeFileAtomicE` (now truly atomic), metadata reading
- `Bit/Commands.hs` — `.bitignore` reading/writing uses strict ByteString + atomic writes
- `Bit/Config/File.hs` — Config reading uses strict ByteString instead of lazy Text IO

#### 3. HLint Enforcement

**`.hlint.yaml`** bans lazy IO functions project-wide:
- Banned: `Prelude.readFile`, `Prelude.writeFile`, `System.IO.hGetContents`, `System.IO.hGetLine`
- Banned: `Data.ByteString.Lazy.readFile`, `Data.Text.IO.readFile`
- Banned: Entire modules `Data.ByteString.Lazy`, `Data.Text.Lazy.IO`
- Suggests: `BS.readFile` over `readFile`, `atomicWriteFile` over `writeFile`
- Suggests: `Bit.Process.readProcessStrict` over `createProcess` for output capture

Process-specific rules:
- `System.IO.hGetContents` banned with message pointing to `Bit.Process.readProcessStrict`
- `System.IO.hGetLine` banned (reading in a loop is error-prone)

HLint errors appear in IDE and CI, preventing lazy IO from being reintroduced.

### Rationale

**Why strict ByteString?**
- Reads entire file into memory and closes handle immediately
- No lazy thunks keeping file handles open
- Predictable memory usage (acceptable for metadata files < 1MB)
- Cross-platform consistent behavior

**Why not lazy ByteString?**
- Lazy ByteString uses chunked IO, keeping handles open
- Garbage collection timing is unpredictable
- On Windows, this causes "file is locked" errors in concurrent scenarios

**Why atomic writes?**
- Crash safety — partial writes leave temp file, not corrupt destination
- Windows retry logic (linear backoff; see Design Decisions #16) handles transient locking from antivirus/indexing
- Directory-level locking coordinates concurrent writes within process

**Why HLint rules?**
- Enforcement at development time (IDE warnings)
- Prevents lazy IO from being reintroduced during refactoring
- Documents the policy explicitly

### Concurrent File Scanning and Metadata Writing

The file scanner (`Bit/Scan.hs`) uses bounded parallelism for both scanning and writing:

**Directory exclusion** (`collectScannedPaths`): The recursive walk filters `.git` and `.bit` directories by **name** at every level — not just at the root. This prevents scanning into nested `.git/` directories (e.g. `subdir/.git/`) which would pollute the cache and cause `createDirectoryIfMissing` crashes on Windows. Top-level `.bitignore` and `.gitignore` files are also excluded from the walk.

**Scanning**:
- `QSem` limits concurrent file reads to **ioConcurrency** (4× cores, min 4)
- Each file is fully read, hashed, and closed before moving to next
- Progress reporting uses `IORef` with `atomicModifyIORef'` for thread-safe updates
- Per-chunk byte tracking: `hashAndClassifyFile` accepts `Maybe (IORef Integer)` and updates it every 64 KB chunk, giving smooth progress even for large files
- Cache entries use strict ByteString read/write
- Phase callbacks (`ScanPhase`): callers receive `PhaseCollected`, `PhaseCacheResult`/`PhaseAllCached` notifications for verbose output

**Metadata Writing** (`writeMetadataFiles`):
- Parallel execution with same bounded concurrency as scanning
- Skip-unchanged optimization: Before writing, checks if metadata already matches
  - Binary files: reads existing metadata, compares hash/size
  - Text files: compares mtime/size of source vs destination
- Three-phase write: (1) create directories sequentially, (2) create file parent directories, (3) write files in parallel
- Progress reporting shows files written, skipped count, and percentage
- Atomic writes for binary metadata using temp-file-rename pattern

### File Copy Progress Reporting

File copy operations during push/pull now have progress reporting (`Bit/CopyProgress.hs`):

**Implementation**:
- Chunked binary copy with byte-level progress (64KB chunks, strict `ByteString`)
- Small files (<1MB) use plain `copyFile` (no overhead), large files use chunked copy with progress
- Progress state (`SyncProgress`) tracks: total files, bytes total/copied, current file name
- Reporter thread updates at 100ms intervals via `IORef` (thread-safe, strict updates)
- TTY detection: shows in-place progress on terminal, silent on non-TTY (for log capture)

**Progress Display**:
- **Aggregate**: `Syncing files: 3/12 files, 5.1 GB / 18.3 GB (28%)`
- **Final summary**: `Synced 12 files (18.3 GB).`
- Human-readable byte formatting: `formatBytes` (B, KB, MB, GB, TB with 1 decimal place)

**Push/Pull** use `rcloneCopyFiles` (batch `rclone copy --files-from`) for binary sync:
- **Push**: `syncRemoteFiles` (unified, derives actions from git metadata diff, executes via rclone)
- **Pull**: `syncAllFilesFromHEAD`, `applyMergeToWorkingDir` (both, parameterized by remote root)
- Progress: counts binary files only (text files are small and fast via index copy)
- Byte progress: parsed from rclone's `--use-json-log` output on stderr (stats block)

**Repair** uses `rcloneCopyto` (single-file `rclone copyto`) for per-file progress:
- Each file is copied individually with `--use-json-log --stats 0.5s -v`
- JSON stats on stderr are parsed to update an `IORef Integer` byte counter
- A reporter thread displays live progress: `(1/3) file.bin from 'gdrive' — 45.2 MB / 120.0 MB (37%)`
- The label includes the source name so the user knows where the file is coming from
- Remote-to-remote repairs use a single `rclone copyto` call — rclone handles the routing between remotes (e.g., downloading from cloud and writing directly to the destination)

**Design Notes**:
- Complies with project's strict IO rules: no lazy `ByteString`, no lazy IO
- Windows compatible: uses `withBinaryFile` for chunked reads/writes
- Pattern matches existing scan progress in `Bit/Scan.hs`: `IORef` + reporter thread + TTY detection + `finally` cleanup

---

## Performance Optimizations

### Scan-on-Demand Architecture

**Problem**: The original design scanned the entire working directory *before* dispatching to a command, then maintained a growing `skipScan` whitelist of commands that don't need it. This was backwards:

1. **Wrong default** — new commands scan by default, silently wasting time if someone forgets to add them to `skipScan`
2. **Duplication** — the command must be listed in *both* `skipScan` and the `case` dispatch
3. **Fragile** — easy to miss commands (e.g., `remote add` was discovered to be missing from `skipScan`, causing 860 files to be scanned for a config-only operation)

**Solution**: Invert the logic — scan on demand, not by default. Commands are classified by whether they need env, scan, or both; each branch in the dispatch explicitly declares its needs.

**Implementation** (`Bit/Commands.hs`): `baseEnv` (via `getDefaultRemote`, falls back to `"origin"`), `upstreamEnv` (via `getUpstreamRemote`, no fallback), and `scanAndWrite` (sync bitignore, scan working dir, write metadata) are defined separately. Dispatch uses: no env (direct call), `runBase` (baseEnv), `runUpstream` (upstreamEnv), `runBaseWithRemote` / `runScannedWithRemote` (named remote), `runScanned` (scanAndWrite + baseEnv), or `runScannedUpstream` (scanAndWrite + upstreamEnv).

**Key Changes**:

1. **No `skipScan` variable** — it no longer exists
2. **Separated concerns** — `baseEnv` builds a lightweight `BitEnv`; `scanAndWrite` is a separate `IO ()` action, only invoked by commands that need it
3. **Explicit tier assignment** — every command branch picks one of: no env, env only (runBase/runUpstream/runBaseWithRemote), scan only (scanAndWrite then direct call), or scan + env (runScanned/runScannedUpstream/runScannedWithRemote)
4. **Safe by default** — new commands do not scan unless the branch explicitly runs `scanAndWrite` or a runScanned variant
5. **Push requires upstream** — bare `bit push` uses `runUpstream` and requires `branch.main.remote`; `bit push <remote>` uses `runBaseWithRemote`. Push derives file actions from `git diff --name-status`, so no working directory scan is needed

**Command Classification**:

1. **No env needed**: `remote add`, `fsck`, `merge --abort`, `branch --unset-upstream`. Also `log`, `ls-files`, and `branch` (with args) — in the code they are invoked directly (no `runBase`); they call `Git.runGitRawIn` with the subdirectory prefix so git resolves paths relative to the user's cwd. They do not build or use a `BitEnv`. **Note:** `init` is dispatched even earlier — before `findBitRoot` — so it never enters the env/scan machinery.

2. **Env, no scan** (runBase or runUpstream or runBaseWithRemote):
   - `remote show` — runBase; read config/remote state
   - `verify`, `repair` — runBase; compare against existing metadata
   - `rm` — runBase
   - `push` (no args) — runUpstream; requires `branch.main.remote`
   - `push <remote>` — runBaseWithRemote; explicit remote

3. **Scan only, no env**: `add`, `commit`, `diff`, `mv`, `reset`, `merge` (generic). Each runs `scanAndWrite` then calls `Bit.add prefix` / `Bit.commit prefix` / etc. with the subdirectory prefix (no `BitEnv`). Scan is for metadata write or for git operations that need up-to-date index. `merge` (generic) is a pure git passthrough (`Git.runGitRawIn prefix`); remote or conflict context is only used in `merge --continue`.

4. **Scan + env** (runScanned, runScannedUpstream, or runScannedWithRemote):
   - `status`, `restore`, `checkout`, `merge --continue` — runScanned (scan + baseEnv); need working directory state
   - `fetch` — runScanned; falls back to `"origin"` like git
   - `pull` — runScannedUpstream (scan + upstreamEnv) or runScannedWithRemote when a remote is given; need working directory state for sync

**Performance Impact**: In large repositories, read-only commands now have instant response times. The old design would scan 860 files for `bit remote add`, the new design scans zero. Push performance is dramatically improved — it no longer scans the remote via `rclone lsjson --hash --recursive`, instead deriving file actions from git metadata diffs with zero network I/O for planning.

---

## Subdirectory Support

bit commands work from any subdirectory of a bit repository, matching git's behavior.

**Repository Discovery** (`findBitRoot`): Starting from the current working directory, walks up the directory tree looking for a `.bit/` directory. If found, the process `setCurrentDirectory` to the repo root. If no `.bit/` is found and the command requires a repo, exits with "fatal: not a bit repository".

**Prefix Computation**: The relative path from the repo root to the user's original cwd becomes the "prefix" (empty string at root). This prefix is stored in `BitEnv.envPrefix` and threaded through the application.

**Git Dispatch**: `runGitRawIn prefix` and `runGitWithOutputIn prefix` target `.bit/index/<prefix>` via `git -C`, so git natively resolves the user's path arguments relative to their subdirectory. When prefix is empty (user is at root), these delegate to the standard `runGitRaw`/`runGitWithOutput` — no behavior change.

**Passthrough Functions**: IO passthrough functions (`add`, `commit`, `diff`, `log`, `lsFiles`, etc.) take `FilePath` prefix as their first parameter. BitM passthrough functions (`status`, `rm`, `doRestore`, `doCheckout`) extract prefix from `envPrefix`. `doRestore` and `doCheckout` prepend prefix to user path arguments before passing them to `syncTextFilesFromIndex`, which needs paths relative to the repo root.

**Unchanged**: Internal repo-level operations (`mergeContinue`, `doMergeAbort`, `unsetUpstream`) and all Core modules (Push, Pull, Fetch, Verify, etc.) continue using un-prefixed git operations — they always operate on the full repository.

---

## Alias Expansion and Passthrough

### Alias Expansion

bit expands git aliases before dispatching, matching git's behavior. When a command is not a known bit command, `tryAlias` queries git config for `alias.<name>`:

- **Inside a bit repo**: queries local config (`.bit/index/.git/config`) and global config
- **Outside a bit repo**: queries global config only

Alias types:
- **Simple aliases** (e.g. `alias.ci = commit`): expanded and re-dispatched through `runCommand`
- **Shell aliases** (starting with `!`): forwarded to git directly via `runGitGlobal`
- **No alias found**: passed through to git (see Passthrough below)

### Passthrough

Unknown commands (not handled by bit, no alias match) are forwarded to git:

- **Inside a bit repo with `.git` junction/gitlink**: uses `runGitHere` (no `-C`) — when CWD has a `.git` directory (junction) or `.git` file (gitlink from `--separate-git-dir`), git's own repo discovery works natively. Using `-C .bit/index` would break commands like `git config -f <relative-path>` that expect paths relative to CWD.
- **Inside a bit repo without `.git`**: uses `runGitRawAt` to target `.bit/index/` via `-C`
- **Inside a bare bit repo** (has `bit/` + `HEAD`): uses `runGitGlobal` (no `-C`) — git discovers the bare repo naturally
- **Inside a git directory** (CWD has a `HEAD` file, e.g. user `cd`'d into `.git`): uses `runGitHere` — runs git without `-C` override, letting git's own repo discovery work. This is necessary because adding `-C .bit/index` would point to a nonexistent path relative to the `.git` directory.
- **Outside any repo**: uses `runGitGlobal` (no `-C`)

**`GIT_DIR=/dev/null` bypass:** When `GIT_DIR` is set to `/dev/null` (or `NUL`/`nul` on Windows via MSYS2 path conversion), bit passes through to git immediately without any command parsing or `-C` insertion. This is needed for tools like `test_cmp` in git's test suite, which use `GIT_DIR=/dev/null git diff --no-index` to compare files outside any repository.

### `-C <dir>` Flag

When invoked as `bit -C <dir> <args...>`, bit intercepts the `-C` flag before any command dispatch. This is a pure passthrough — no alias expansion, no bit command handling:

- **`<dir>` has `.bit/`** (bit repo): runs `git -C <dir>/.bit/index <args...>`
- **Otherwise** (plain git repo, bare repo, or nonexistent): runs `git -C <dir> <args...>`

This is necessary because without interception, `-C` would be treated as an unknown command, alias lookup would fail, and passthrough would prepend `-C .bit/index` — producing `git -C .bit/index -C <dir> ...`. Git resolves `-C` sequentially, so the second `-C <dir>` would resolve relative to `.bit/index/` (wrong directory).

### Init Dispatch Order

`init` is dispatched **before** repository discovery (`findBitRoot`). This is critical because `init` creates repos — it must not be affected by a parent repo's `.bit/` directory. Without this, `cd existing-repo/subdir && bit init` would `setCurrentDirectory` to the parent root and re-initialize the parent instead of creating a new repo in `subdir/`.

Before matching `("init":rest)`, the dispatch peels git "global" flags that appear before the subcommand: `-c key=val` pairs and `--bare`. This allows `bit -c init.defaultBranch=test init dir` and `bit --bare init dir` to work. The `-c` pairs are stored in `initGitGlobalFlags` and inserted before `init` in the `spawnGit` call. `--bare` is folded into the remaining args so `parseInitArgs` sees it.

`initializeRepoAt` returns `ExitCode`. On failure (e.g. `--ref-format=garbage`), the exit code propagates to the caller and the process exits with the same code git returned. Git's stdout and stderr are printed verbatim so tests can match on "Initialized empty", "Reinitialized existing", and error messages.

### Repo Discovery (`findBitRoot`)

`findBitRoot` walks up from CWD and returns a `BitRoot` sum type:

- **`NormalRoot FilePath`** — directory has `.bit/` (standard bit repo)
- **`BareRoot FilePath`** — directory has `bit/` + `HEAD` (bare bit repo)

Both are checked at each level, so a bare repo nested inside a normal repo's tree is detected at the correct (closest) level. This is important for git's test suite, where bare repos are created inside a trash directory that itself has a `.bit/` repo.

### BIT_CEILING_DIRECTORIES

Analogous to `GIT_CEILING_DIRECTORIES`. When set, `findBitRoot` stops walking up at the specified directory and will not search above it. Used by tests to prevent accidental discovery of the development repo's `.bit/`.

---

## Git Test Suite Compatibility

### Git Shim (`extern/git-shim/`)

The shim allows running git's own test suite with bit acting as git. It sets two environment variables and delegates everything to bit:

- **`BIT_REAL_GIT`**: Path to the real git binary. Prevents recursion when bit's internal git calls would go through the shim. `spawnGit` in `Bit/Git/Run.hs` checks this variable and uses the real git instead of the shim.
- **`BIT_GIT_JUNCTION=1`**: Tells `initializeRepoAt` to create a `.git` directory junction pointing to `.bit/index/.git` after creating a repo. This allows git's repo discovery to work naturally — tests that do `cd .git && git config` find the git directory through the junction.

The junction uses Windows `mklink /j` (directory junction), which does not require admin privileges. It is only created when `BIT_GIT_JUNCTION=1` is set — normal bit usage does not create junctions.

### Running Tests

```bash
cd extern/git/t
GIT_TEST_INSTALLED=/path/to/extern/git-shim bash t0001-init.sh --verbose
```

---

## Verification and Consistency

### `bit verify`

Verifies local working tree files match their committed metadata. Uses a bandwidth-aware scan (`Scan.scanWorkingDirWithAbort`) to hash files and compare against the committed state via `git diff`. Any difference means the file has been corrupted or modified since the last commit.

**Why scan + git diff (not direct file-vs-metadata comparison):** An earlier approach loaded metadata from `.bit/index/` on disk and compared file hashes against it. The problem: any command that scans the working directory (`bit status`, `bit add`, etc.) updates `.bit/index/` metadata to reflect current file state. If a file was corrupted and the user happened to run `bit status` first, the metadata would be updated to match the corrupted content — and verification would pass, silently missing the corruption. By comparing against the *committed* state in git (what was explicitly committed with `bit commit`), verification is immune to stale metadata. The committed state doesn't change just because a scan ran.

**Implementation:**
1. `Scan.scanWorkingDirWithAbort` — hash files with bandwidth detection (see below)
2. `Scan.writeMetadataFiles` — update `.bit/index/` to match scan results
3. `git diff --name-only` in `.bit/index` repo — find files whose metadata changed from HEAD
4. For each changed binary file: read committed metadata (`git show HEAD:<path>`) for expected hash/size, read filesystem metadata for actual hash/size → `HashMismatch`
5. For each changed text file: report `HashMismatch` (git diff already proves the content changed)
6. For missing files: `git ls-tree -r HEAD` lists committed paths, check existence in working tree → `Missing`

**Verbose phase output** (always on stderr):
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

**Bandwidth-aware hashing** (`scanWorkingDirWithAbort`):
1. **Cache check**: Each file's mtime+size is compared against `.bit/cache/` entries. Cache hits skip re-hashing entirely.
2. **Threshold**: If total uncached bytes are below 20 MB, the bandwidth check is skipped and all files are hashed. Otherwise:
3. **Throughput measurement**: If >100 MB of uncached files remain, reads a 10 MB sample and estimates total time.
4. **Abort prompt**: If estimated time exceeds 60 seconds, prompts the user:
   ```
   Hashing is slow (329.0 KB/s). Estimated: 12 min for 5 files.
   Size-only verification covers most corruption cases.
   Continue full hashing? [y/N]
   ```
5. **Size-only fallback**: If the user declines, uncached files are verified by size only — the committed hash is kept, but the actual file size is written to the metadata entry. `git diff` then detects any size changes. Same-size content corruption is not detected in this mode, but this covers the vast majority of real-world corruption (partial writes, truncation, failed transfers).

**Per-chunk progress**: Hashing progress updates every 64 KB chunk, not per-file. This gives smooth progress even for large files on slow storage. The byte counter (`IORef Integer`) is updated via `atomicModifyIORef'` from the `hashAndClassifyFile` streaming loop.

**Error display**: Different messages for different failure modes:
- `[ERROR] Metadata mismatch: file.bin` — hash differs (full hash path)
- `[ERROR] Size mismatch: file.bin` — only size differs (size-only path, shows `Expected size: 1.0 MB / Actual size: 524.3 KB`)
- `[ERROR] Missing: file.bin` — file doesn't exist on disk

### `bit --remote <name> verify` / `bit --remote <name> repair`

Verifies a remote's files match the committed metadata. Routes by remote type:

- **Filesystem remotes**: Scans the remote working directory using `Verify.verifyWithAbort` (bandwidth-aware scan + git diff), the same approach as local verification. Phase messages appear on stderr.
- **Cloud remotes**: Uses the **already-fetched** local bundle at `.bit/index/.git/bundles/<name>.bundle` for committed metadata (no fetch is performed during verify). Scans remote files via `rclone lsjson --hash` and compares. If the local bundle is stale (e.g. someone else updated the remote), verify compares against that stale metadata. Progress "Checking files: n/total" is shown on stderr when there are more than 5 files.

When issues are found:
- `verify` prompts "Repair? [y/N]" on TTY, skips in non-interactive mode
- `repair` repairs automatically without prompting

Repair sources: local repo + all other configured remotes. Each source's metadata is loaded according to its type — filesystem remotes load from their `.bit/index/` directory, cloud remotes from their bundle. Before planning repairs, bit logs the sources it will search:
```
Searching 2 source(s): local, 'gdrive'
```

**Verify and repair implementation details:**

- **Verify paths:** Local and filesystem-remote verify use `verifyWithAbort` (bandwidth-aware scan, cache, optional size-only fallback). Cloud remote verify uses the local bundle only (no re-fetch) and `rclone lsjson --hash`; there is no bandwidth-abort path for cloud.
- **Repair is two-phase.** (1) **Text files:** Restored from git, then copied to the target. Locally: `git restore` in `.bit/index`, then copy from index to working dir. Filesystem remote: same in the remote's `.bit/index`, then copy to remote working dir. Cloud remote: content is read from the bundle commit (`git show <hash>:<path>`), written to a temp file under `.bit/`, uploaded via rclone, then temp removed. Text repair does not use other remotes. (2) **Binary files:** Content-addressable: (hash, size) → (source, path). When repairing **local**, only remotes are used as sources (target excluded). When repairing a **remote**, local plus all other remotes (target remote excluded). After a successful binary copy, **metadata is restored:** for local or a filesystem remote, the `.bit/index/` metadata file is written. For **cloud remotes**, metadata is not written on the remote (it lives in the bundle; the next fetch/push uses the bundle).
- **Unrepairable:** Binary issues with no (hash, size) match in any source are reported as "UNREPAIRABLE".
- **Prompt:** With `bit verify`, on a TTY the user is asked "Repair? [y/N]". In non-interactive mode repair is skipped and the user is told to run `bit repair`.

### `bit repair`

Same as `bit verify` but repairs automatically without prompting. Searches all configured remotes for correct versions of corrupted or missing files.

**Content-addressable repair**: Files are matched by (hash, size), not by path. If `photos/song.mp3` is corrupted locally but `backup/song_copy.mp3` on a remote has the same hash and size, it will be used as the repair source.

**Per-file progress**: Each repair copy shows live progress on TTY, including the source name:
```
Repairing 3 file(s)...
  (1/3) data/model.bin from 'gdrive' — 45.2 MB / 120.0 MB (37%)
```
Repair uses `rclone copyto` with `--use-json-log` for byte-level progress parsing. Remote-to-remote repairs (source remote → target remote) use a single `rclone copyto` call — rclone handles the routing directly.

### `bit fsck`

Runs `git fsck` on the internal metadata repository (`.bit/index`). Checks the integrity of the object store — that all commits, trees, and blobs are valid and consistent. This is a passthrough to git's own integrity check. Use `bit verify` to check file integrity instead.

---

## Handling Remote Divergence

When remote files don't match remote metadata (detected via `bit --remote <name> verify` or during `bit pull`):

### Resolution Option 1: Accept Remote Reality (`--accept-remote`)

Force-checkout the remote branch so git puts the correct metadata in
`.bit/index/`, then mirror the changes to the working directory. This is
architecturally identical to a normal pull — just a force-checkout instead of
a merge. Git manages the index; we only sync actual files.

The flow:
1. Fetch remote bundle (git gets remote history)
2. Record current HEAD (for diff-based sync)
3. `git checkout -f -B main --no-track refs/remotes/origin/main` (force-checkout remote without setting upstream)
4. `applyMergeToWorkingDir` (diff old HEAD vs new HEAD, sync files)
5. Update tracking ref

**Important**: `--accept-remote` must NOT scan remote files via rclone and write
metadata directly. Rclone cannot distinguish text from binary files
(`fContentType = BinaryContent` for everything), so text files would get `hash:/size:`
metadata instead of their actual content.

### Resolution Option 2: Force Local (`bit push --force`)

Upload all local files, overwriting remote. Push metadata bundle. Requires confirmation.

**Push force flags:** `--force` and `--force-with-lease` are mutually exclusive; if both (or `-f` with `--force-with-lease`) are given, bit exits with "fatal: Cannot use both --force and --force-with-lease". Neither flag affects verification — push always verifies local working tree first; they only affect ancestry and overwrite policy.

**`--force`:** Skips the ancestry check. Push proceeds even when remote has diverged or is ahead. For a **non-bit-occupied** remote (path contains files but not a bit repo), only `--force` allows overwriting; without it, bit prints the existing paths and suggests "To initialize anyway (destructive): bit push --force".

**`--force-with-lease`:** Proceeds only if nobody else has pushed since our last fetch. Implementation: (1) Before fetching, read the current **tracking ref** `refs/remotes/<name>/main` (call this the pre-fetch value; it may not exist on first push). (2) Fetch remote history via the push seam and obtain the **post-fetch remote ref** (the hash the remote actually has). (3) If we had no previous tracking ref, proceed (first push or no ref yet). (4) If we have both, proceed only if pre-fetch and post-fetch are equal — meaning the remote has not changed since we last fetched. (5) If they differ, error: "Remote has changed since last fetch! Someone else pushed to the remote. Run 'bit fetch' to update your local view." and exit 1. (6) If the remote state cannot be determined after fetch, error with "Could not determine remote state." So re-implementing requires capturing the tracking ref before the seam fetch, then comparing it to the ref returned by the fetch; only then decide to execute push or abort.

### Resolution Option 3: Manual Merge (`--manual-merge`)

Interactive per-file conflict resolution:
- For each conflict, displays local hash/size vs remote hash/size
- User chooses (l)ocal or (r)emote for each file
- Resolution is applied via git checkout mechanics
- Supports `bit merge --continue` and `bit merge --abort`

---

## Design Decisions

### What We Chose

1. **Phantom-typed hashes**: `Hash (a :: HashAlgo)` — the compiler distinguishes MD5 from SHA256. Mixing algorithms is a compile error. The `DataKinds` extension is used per-module.

2. **Unified metadata parser and loader**: A single `Bit/Config/Metadata.hs` module handles all parsing and serialization, and `Bit/Scan/Verify.hs` provides unified metadata loading via the `MetadataSource` abstraction. This eliminates the class of bugs where multiple parsers or loaders handle edge cases differently. The `MetadataEntry` type forces callers to explicitly handle the binary vs. text file distinction, ensuring text files (whose git blobs may have normalized line endings) are not incorrectly hash-verified across sources.

3. **`ReaderT BitEnv IO` (no free monad)**: The application monad is `ReaderT BitEnv IO`. A free monad effect system was considered (for testability and dry-run mode) but rejected as premature — no pure tests or dry-run usage existed to justify the complexity. Direct IO with `ReaderT` for environment threading is cleaner for now.

4. **Shared `deriveActions`**: Both push and pull derive `[RcloneAction]` from `git diff --name-status` via the shared `deriveActions` function in `Bit.Rclone.Sync`. `resolveSwaps` (in `Bit.Domain.Plan`) detects pairwise Move swaps. Property tests in `test/PlanSpec.hs` verify swap detection.

5. **Structured conflict resolution**: Conflict handling is a fold over a conflict list (`Bit.Core.Conflict`), not an imperative block. Decision logic is separated from IO mechanics.

6. **Remote as opaque type**: `Remote` is exported without its constructor. Only `remoteName` is public for display. `remoteUrl` exists for Transport to extract the URL, but business logic in Bit/Core/*.hs should use `displayRemote` for user-facing messages.

7. **Tracking ref invariant**: `refs/remotes/origin/main` must always reflect
   what the remote actually has — never a local-only commit. After **push**,
   updating to HEAD is correct (the remote now has our history). After
   **pull/merge**, update to the hash from the fetched bundle, because HEAD
   includes merge commits the remote doesn't know about. Violating this
   causes the next `fetchFromBundle` to encounter a non-fast-forward update
   and silently fail to update the tracking ref, making subsequent merges
   operate against stale history.

8. **Git is the sole authority over `.bit/index/`**: No code path should write
   metadata files to the index and commit them directly. The index is always
   populated by git operations (merge, checkout, commit via `bit add`). After
   any git operation that changes HEAD, we only need to mirror those changes
   onto the actual working directory. This invariant applies to all pull paths
   including `--accept-remote`.

9. **Always commit when MERGE_HEAD exists**: After conflict resolution,
   `git commit` must always be called — never guarded by `hasStagedChanges`.
   When the user chooses "keep local" (`--ours`), the index becomes identical
   to HEAD. A `hasStagedChanges` check would skip the commit, leaving
   `MERGE_HEAD` dangling. Git's `commit` succeeds when `MERGE_HEAD` exists
   regardless of index state. Skipping the commit breaks the next push
   (ancestry check fails because HEAD was never advanced).

10. **The `oldHead` capture pattern**: Before any git operation that changes
    HEAD (merge, checkout), capture HEAD so `applyMergeToWorkingDir` can diff
    old vs new and sync only what changed. This pattern appears in `pullLogic`,
    `mergeContinue`, and `pullAcceptRemoteImpl`. The only exception is first
    pull (`oldHead = Nothing`), which falls back to `syncAllFilesFromHEAD`.

11. **Proof of possession on push/pull**: bit-lite must verify that the sender's
    working tree matches its metadata before transferring that metadata. On push,
    the local repo is verified; on pull, the remote is verified. This is necessary
    because bit-lite has no object store — the working tree is the only copy of
    binary content, and metadata without matching content is meaningless. Git and
    bit-solid don't need this rule because their object stores back up every claim
    unconditionally. The existing divergence resolution mechanisms
    (`--accept-remote`, `--force`, `--manual-merge`) serve as escape hatches when
    verification fails.

12. **Seam pattern for transport differences**: Both push and pull use a single
    code path for cloud and filesystem remotes. The only difference — how
    metadata is fetched/pushed — is isolated behind a small seam record
    (`PushSeam` for push, `PullSeam` for pull). Everything else is shared:
    remote state classification, file sync via `deriveActions` + rclone,
    merge orchestration, conflict resolution, `oldHead` capture, tracking
    ref updates, MERGE_HEAD handling. This split is keyed off
    `Device.readRemoteType`/`isFilesystemType` and happens once at the top
    of `Bit.Core.push` and `Bit.Core.pull`.

13. **Filesystem remotes are full repos**: When pushing to a filesystem path,
    bit creates a complete bit repository at the remote (via `initializeRepoAt`).
    Anyone at that location can run bit commands directly. This is the natural
    model for USB drives, network shares, and local collaboration directories.
    Bundles are skipped entirely — git talks repo-to-repo via filesystem paths.

14. **Upstream tracking requires explicit `-u` flag**: Following git-standard
    behavior, `branch.main.remote` is never set automatically. Users must use
    `bit push -u <remote>` to establish upstream tracking. This includes:
    - `bit remote add` does NOT set upstream (unlike old bit behavior)
    - First pull uses `git checkout -B main --no-track` to prevent auto-tracking
    - `bit push` requires explicit remote if no upstream is set (matches `git push` with `push.default=simple`)
    - `bit pull` requires explicit remote if no upstream is set (no fallback)
    - `bit fetch` falls back to "origin" if it exists and no upstream is configured (git-standard — fetch is read-only)
    This makes bit's remote behavior predictable for git users.

15. **Strict ByteString IO exclusively**: All file operations use strict
    `ByteString` (`Data.ByteString.readFile` / `writeFile`), never lazy IO
    (`Prelude.readFile`, `Data.ByteString.Lazy`, `Data.Text.Lazy.IO`). Lazy IO
    on Windows keeps file handles open until garbage collection, causing
    "permission denied" and "file is locked" errors in concurrent scenarios.
    Strict IO reads/writes the entire file and closes the handle immediately.
    HLint rules enforce this project-wide.

16. **Atomic writes with retry logic**: All important writes use the temp-file +
    rename pattern (`Bit.IO.AtomicWrite.atomicWriteFile`). On Windows, includes
    retry logic (up to 5 retries, 6 total attempts) with linear backoff (e.g.
    50ms, 100ms, 150ms, 200ms, 250ms; total window ~750ms). Linear backoff is
    intentional: exponential backoff matters when hitting a shared remote
    service where thundering-herd effects are real — many clients backing off
    need to spread out over an increasingly wide window to let the service
    recover. For local filesystem atomic writes, contention is brief lock or
    transient OS-level conflicts; the total retry window is small and there is
    no amplification from concurrent clients hammering a network endpoint.
    Directory-level locking coordinates concurrent writers within the process.

17. **ConcurrentIO newtype without MonadIO**: For modules that need type-level
    IO restrictions, `Bit.IO.ConcurrentIO` provides a newtype wrapper where the
    constructor is not exported. This prevents smuggling arbitrary lazy IO via
    `liftIO`. Only whitelisted strict operations are exposed. Async integration
    is by explicit unwrapping via `runConcurrentIO` at concurrency boundaries,
    not via `MonadUnliftIO` (which would let any async combinator escape the
    wrapper and defeat its discipline). Used where the type system should
    enforce IO discipline (currently available but not widely adopted;
    `Bit.IO.ConcurrentFileIO` with plain `MonadIO` is used in most places for
    simplicity).

18. **Ephemeral remote workspaces (no persistent state)**: Remote-targeted
    commands (`bit @remote init/add/commit/status/log/ls-files`) use an ephemeral
    workspace pattern — each command fetches the bundle from the remote,
    inflates into a system temp directory, operates, re-bundles if changed,
    pushes back, and cleans up. No persistent workspace is stored under
    `.bit/`. This design was chosen over persistent workspaces because:
    - It avoids stale state (the remote is always the source of truth)
    - It avoids disk space accumulation from workspace copies
    - It avoids Windows file locking issues with long-lived directories
    - It makes each command self-contained and idempotent
    - Cleanup is exception-safe via `bracket`
    Bundle inflation uses `git init` + `git fetch` into tracking refs +
    `git reset --hard` (not `checkout -B`, which can skip working tree
    updates when already on the target branch; not `git clone`, which
    fails on Windows when temp directories aren't fully cleaned up).

19. **Mode is local-only, gates writes only**: The lite/solid mode is stored in
    `.bit/config` (git-style INI format, `core.mode` key, not tracked by git)
    and controlled via `bit config core.mode lite|solid`. It affects a single
    behavior: whether `bit add` writes blobs to `.bit/cas/`. CAS reads are
    always available regardless of mode — `bit restore` and similar commands
    check CAS as a fallback. This means switching from solid to lite preserves
    all previously stored history, and switching from lite to solid starts
    accumulating history from that point forward. No migration, no data loss,
    instant switch.

20. **Cloud remote layout (full vs bare) with streaming CAS**: Cloud remotes
    support two layouts: `full` (files at readable paths, browsable in Google
    Drive) and `bare` (CAS-only, opaque blobs), set at creation time via
    `bit remote add <n> <url> --bare`. A lite-mode repo can push to a bare
    remote by streaming content directly into the remote's CAS layout without
    populating local CAS — the local repo pays no disk cost while the remote
    gets a complete content-addressed backup. This is the compelling
    "laptop-constrained user who wants a safety net" use case. Layout conversion
    between full and bare is not supported — create a new remote instead.

### What We Deliberately Do NOT Do

- **`RemoteState` does not need a typed state machine.** The pattern match in push logic is clear and total.
- **`RcloneAction` does not need a Group structure.** Actions are derived fresh from git each time; inverse/compose would be dead code.
- **No Arrow syntax.** Plain `>>=` and function composition are clearer.
- **No MTL-style type classes** (`MonadGit`, `MonadRclone`). Everything is concrete.
- **No post-sync metadata rescanning.** After syncing files to the working
  directory, we do NOT re-scan and rewrite metadata. Git already put the
  correct metadata in the index. Rescanning would be redundant at best,
  wrong at worst (e.g., overwriting text file content with hash/size if the
  scan's text/binary classification differs from git's).
- **No cloud remote layout conversion.** Switching between `layout: full` and
  `layout: bare` is not supported. Create a new remote with the desired layout
  and push to it. Migration logic would be complex and error-prone, and remote
  layout changes are rare enough to not justify the implementation cost.

---

## Known Deviations and TODOs

### Remaining Work

- **Transaction logging**: For resumable push/pull operations.
- **Error messages**: Some need polish to match Git's style and include actionable hints.
- **`isTextFileInIndex` fragility**: The current check (looking for `"hash: "` prefix) works but is indirect. A more robust approach might check whether the file parses as metadata vs. has arbitrary content. Low priority since current approach works correctly.

### Future (bit-solid)

- **Mode switching**: `bit config core.mode solid` activates CAS writes on `bit add`. Switching is instant and non-destructive in both directions. See "Mode Configuration" in Core Architecture.
- **CAS structure**: `.bit/cas/<first-2-chars>/<full-hash>` for normal repos, `bit/cas/` for bare repos. Directory structure already created by `bit init`.
- **`bit cas backfill`**: Walk historical commits, store any blobs currently present in the working tree into CAS. Optional — incomplete CAS is fine.
- **`bit cas gc`**: Prune unreferenced blobs. Safe default: never auto-delete.
- **`bit restore` from CAS**: When restoring a file from a historical commit, check CAS for the blob before failing. Works regardless of current mode.
- Sparse checkout via symlinks to CAS blobs
- `bit materialize` / `bit checkout --sparse`

---

## Current Implementation State

### Implemented and Working

- `bit init` — creates `.bit/` (including `.bit/cas/`), initializes Git in `.bit/index/.git`; dispatched before repo discovery so nested init works correctly; supports `BIT_GIT_JUNCTION=1` for git test suite compatibility; returns `ExitCode` and prints git's output verbatim; re-init on existing repos forwards to `git init` without stomping config; respects `--initial-branch`/`-b` (skips branch rename); peels `-c key=val` and `--bare` from before `init` subcommand; resolves `.bit` bitlinks on re-init for `--separate-git-dir` repos
- `bit init --bare` — passes through to `git init --bare`, then creates `bit/cas/` inside the bare repo; bare repos are standard git bare repos with no `.bit/index/` wrapper; `bit --bare init dir` also dispatched correctly
- `bit init --separate-git-dir <dir> [workdir]` — places git database and bit metadata at `<dir>`, writes bitlink (`.bit` file with `bitdir:` pointer) and gitlink (`.git` file with `gitdir:` pointer) in the working directory; all operations transparently follow bitlinks; re-init resolves bitlinks to find the real `.bit` directory
- `bit add` — scans files, computes MD5 hashes, writes metadata, stages in Git
- `bit commit`, `diff`, `status`, `log`, `restore`, `checkout`, `reset`, `rm`, `mv`, `branch`, `merge` — delegate to Git
- `bit remote add/show` — named remotes with device-aware resolution
- `bit push` — Cloud: diff-based file sync via rclone, then push metadata bundle. Filesystem: fetch+merge at remote, then sync files
- `bit pull` — Cloud: fetch metadata bundle, then diff-based file sync via `applyMergeToWorkingDir`. Filesystem: fetch from remote, merge locally, sync files
- `bit pull --accept-remote` — force-checkout remote branch, then mirror changes to working directory
- `bit pull --manual-merge` — interactive per-file conflict resolution
- `bit merge --continue / --abort` — merge lifecycle management
- `bit fetch` — fetch metadata bundle only
- `bit verify` — local file verification against committed metadata (scan + git diff)
- `bit repair` — verify + auto-repair local files from all remotes
- `bit --remote <name> verify` / `bit --remote <name> repair` — remote file verification and repair
- `bit fsck` — passthrough to `git fsck` on internal metadata repository
- `bit --remote <name>` / `bit @<remote>` — ephemeral remote workspace commands (`init`, `add`, `commit`, `status`, `log`, `ls-files`, `verify`, `repair`); each command fetches bundle, inflates into temp dir, operates, re-bundles if changed, pushes, and cleans up
- Shared `deriveActions` for push/pull with `resolveSwaps` swap detection and property tests
- Device-identity system for filesystem remotes (UUID + hardware serial)
- Filesystem remote transport (full bit repo at remote, direct git fetch/merge)
- Conflict resolution module with structured traversal (always commits when MERGE_HEAD exists)
- Git alias expansion and catch-all passthrough for unknown commands
- Git test suite shim (`extern/git-shim/`) with `BIT_REAL_GIT` and `BIT_GIT_JUNCTION` env vars
- Unified metadata parsing/serialization
- `oldHead` pattern for diff-based working-tree sync across all pull/merge paths
- Strict ByteString IO throughout — no lazy IO, eliminates Windows file locking issues
- Atomic file writes with Windows retry logic (linear backoff; see Design Decisions #16) — crash-safe, handles antivirus/indexing conflicts
- Concurrent file scanning with bounded parallelism and progress reporting
- HLint enforcement of IO safety rules
- Proof of possession verification for push and pull operations
  - `verifyLocalAt` function for verifying arbitrary repo paths (used for filesystem remotes)
  - Integration with existing escape hatches (`--accept-remote`, `--manual-merge`)

### Module Map

| Module | Role |
|--------|------|
| `Bit/Commands.hs` | CLI dispatch, `findBitRoot` (walks up to discover repo, returns `BitRoot`: `NormalRoot` or `BareRoot`), prefix computation, env setup; `init` dispatched before repo discovery with `peelGitGlobalFlags` to handle `-c key=val` and `--bare` before the subcommand; bare repos pass unknown commands through to git and reject known bit commands; `tryAlias` expands git aliases and passes unknown commands through to git; `resolveBitDir` follows bitlinks for separated repos |
| `Bit/Help.hs` | Command help metadata; `HelpItem` (hiItem, hiDescription) for options/examples, replaces (String, String) |
| `Bit/Core.hs` | Re-exports public API from `Bit/Core/*.hs` sub-modules |
| `Bit/Core/Push.hs` | Push logic + PushSeam transport abstraction |
| `Bit/Core/Pull.hs` | Pull + merge + conflict resolution |
| `Bit/Core/Fetch.hs` | Fetch + remote state classification |
| `Bit/Core/Helpers.hs` | Shared types (PullMode, PullOptions) + utility functions |
| `Bit/Core/RemoteManagement.hs` | Remote add/show, device-name prompting, `--bare` layout flag for cloud remotes |
| `Bit/Core/Config.hs` | `bit config` command: get/set/list for `.bit/config` (git-style INI format); per-key validation (e.g. `core.mode` only accepts `lite`/`solid`) |
| `Bit/Core/Conflict.hs` | Conflict resolution: Resolution, DeletedSide, ConflictInfo, resolveAll |
| `Bit/Git/Run.hs` | Git command wrapper; `runGitRawIn`/`runGitWithOutputIn` for prefix-aware subdirectory dispatch; `AncestorQuery` (aqAncestor, aqDescendant) for `checkIsAhead`; `runGitAt`/`runGitRawAt` for arbitrary paths; `runGitHere` for running git without `-C` when CWD is a git directory; `spawnGit` respects `BIT_REAL_GIT` to avoid shim recursion; `indexPathRef` IORef for dynamic base flags (set via `setIndexPath`, read via `getIndexPath`) |
| `Bit/Git/Passthrough.hs` | Git command passthrough (add, commit, diff, log, merge, etc.); IO functions take prefix for subdirectory support; BitM functions extract prefix from `envPrefix` |
| `Bit/Rclone/Run.hs` | Rclone command wrapper; `remoteFilePath` for trailing-slash-safe remote path construction |
| `Bit/Rclone/Sync.hs` | Shared action derivation + working-tree sync |
| `Bit/Rclone/Progress.hs` | Progress tracking for file copy operations (push/pull sync via `rcloneCopyFiles`, repair via `rcloneCopyto` with per-file JSON progress parsing) |
| `Bit/Config/Paths.hs` | Path constants |
| `Bit/Config/File.hs` | Config file parsing (strict ByteString) |
| `Bit/Config/Metadata.hs` | Canonical metadata parser/serializer |
| `Bit/Types.hs` | Core types: Hash, FileEntry, BitEnv (with `envPrefix` for subdirectory support), BitM |
| `Bit/Path.hs` | `RemotePath` newtype — compile-time enforcement that remote filesystem paths go through `Bit.IO.Platform` |
| `Bit/Remote.hs` | Remote type, resolution, RemoteState, FetchResult |
| `Bit/Utils.hs` | Path utilities, filtering, atomic write re-exports |
| `Bit/Domain/Plan.hs` | `RcloneAction` type and `resolveSwaps` (detects pairwise Move swaps → Swap) |
| `Bit/Scan/Local.hs` | Working directory scanning, hash computation (`hashAndClassifyFile` returns `ContentType`, accepts optional `IORef Integer` for per-chunk byte progress), `ScanPhase` callbacks for verbose phase reporting, bandwidth-aware `scanWorkingDirWithAbort` with cache check + throughput measurement + size-only fallback, cache entries use `ContentType`, parallel metadata writing with skip-unchanged optimization (concurrent, strict IO). `readMetadataFile`, `getFileHashAndSize` return `Maybe MetaContent`. |
| `Bit/Scan/Remote.hs` | Remote file scanning via rclone |
| `Bit/Scan/Verify.hs` | Local and remote verification; scan + git diff approach for local (compares against committed metadata); `verifyWithAbort` threads `ScanPhase` callbacks and creates size-only entries for skipped files; unified metadata loading via `MetadataSource` abstraction (`FromFilesystem`, `FromCommit`); `MetadataEntry` type distinguishes binary (hash-verifiable) from text files (existence-only); `verifyLocalAt` for filesystem remotes |
| `Bit/Scan/Fsck.hs` | Passthrough to `git fsck` on `.bit/index` metadata repository |
| `Bit/Device/Identity.hs` | Device identity, volume detection, .bit-store (strict IO, atomic writes), `RemoteType` classification, `isFixedDrive` |
| `Bit/Device/Prompt.hs` | Interactive device setup prompts |
| `Bit/Device/RemoteWorkspace.hs` | Ephemeral remote workspace: `initRemote`, `addRemote`, `commitRemote`, `statusRemote`, `logRemote`; `withRemoteWorkspace` / `withRemoteWorkspaceReadOnly` orchestration; bundle inflation via `init+fetch+reset --hard` |
| `Bit/IO/Concurrency.hs` | Bounded parallelism helpers: concurrency level calculation, sequential/parallel mode switching |
| `Bit/IO/ConcurrentIO.hs` | Type-safe concurrent IO newtype (no MonadIO) |
| `Bit/IO/ConcurrentFileIO.hs` | Strict ByteString file operations |
| `Bit/IO/AtomicWrite.hs` | Atomic file writes, directory locking, lock registry |
| `Bit/IO/Process.hs` | Strict process output capture (concurrent stdout/stderr reading) |
| `Bit/IO/Platform.hs` | UNC-safe wrappers for `System.Directory` (CPP: Win32 direct calls for UNC paths, `System.Directory` for local); includes `getFileSize` |
| `Bit/Progress/Report.hs` | Centralized progress reporting for terminal operations |

---

## Test Infrastructure

### Lint Test Suite (Pattern Safety + Format Validation)

**Purpose**: Prevent dangerous Windows environment variable patterns and shelltest format errors in test files.

**Two Categories of Violations**:

1. **Pattern Safety**: Dangerous Windows environment variables that can cause commands to escape test sandboxes
2. **Format Validation**: Shelltest Format 3 syntax violations that cause parse errors and prevent tests from running

**Problem 1 (Pattern Safety)**: Windows expands environment variables like `%CD%` before command chains execute. Example:
```batch
cd test\cli\output\work_mytest & bit remote add origin "%CD%\test\cli\output\remote_mirror"
```
If the `cd` fails, `%CD%` still expands to the current directory (potentially the main repo), causing `bit remote add` to modify the development repo's remote URL instead of the test repo's.

**Problem 2 (Format Validation)**: Shelltest Format 3 allows only one of each directive (`<<<`, `>>>`, `>>>2`, `>>>=`) per test case. Multiple directives cause parse errors that silently prevent tests from running. Example:
```batch
# WRONG - Two >>>2 directives:
command
>>>2 /error 1/
>>>2 /error 2/
>>>= 1
```
This causes a parse error and the test never executes, giving false confidence that the test is passing.

**Solution**: Automated guards enforcing both relative path usage and shelltest format rules.

#### Lint Test (Primary Guard)

**Location**: `test/LintTestFiles.hs`

**Test Suite**: `cabal test lint-tests`

**What it does**:
- Recursively scans all `.test` files under `test/cli/`
- **Pattern Safety Checks** (case-insensitive):
  - `%CD%` — current directory, expands before command execution
  - `%~dp0` — batch script directory variable
  - `%USERPROFILE%`, `%APPDATA%`, `%HOMEDRIVE%`, `%HOMEPATH%` — user directory variables
- **Format Validation Checks**:
  - Parses test cases (separated by blank lines)
  - Detects duplicate directives within a single test case:
    - Multiple `<<<` (stdin)
    - Multiple `>>>` (stdout)
    - Multiple `>>>2` (stderr)
    - Multiple `>>>=` (exit code)
  - Distinguishes between new directives and multi-line continuation
- Fails with detailed error showing file, line number, violation type, and fix
- Runs as part of `cabal test` and CI

**Example Output on Violation**:
```
DANGEROUS PATTERN in test/cli/remote-check.test:15
  Found: %CD%
  Line:  cd test\cli\output\work_mytest & bit remote add origin "%CD%\test\cli\output\remote_mirror"

  Why dangerous: Windows expands %CD% before the command chain executes.
  If the preceding `cd` fails, commands run in the main repo directory.
  Fix: Use relative paths (e.g., ..\remote_mirror) instead.
```

#### Pre-commit Hook (Secondary Guard)

**Location**: `scripts/pre-commit`

**Installation**: `scripts\install-hooks.bat` (run once after cloning)

**What it does**:
- Scans only staged `.test` files before commit
- Checks for the same dangerous patterns
- Blocks commit if violations found
- Provides clear error message with fix guidance

**Note**: Optional developer convenience; the lint test is the primary enforcement mechanism.

#### Forbidden Patterns List

The patterns are centrally defined in `test/LintTestFiles.hs`:
- `%CD%` — expands to current directory before command chains execute
- `%~dp0` — batch script directory, same timing issue
- `%USERPROFILE%` — could resolve to real user directories
- `%APPDATA%` — could resolve to real app data directories
- `%HOMEDRIVE%` / `%HOMEPATH%` — could resolve to real user paths

**Correct Pattern**: Use relative paths that resolve at command execution time:
```batch
# WRONG (banned):
cd test\cli\output\work_mytest & bit remote add origin "%CD%\test\cli\output\remote_mirror"

# CORRECT:
cd test\cli\output\work_mytest & bit remote add origin ..\remote_mirror
```

#### Documentation

See `test/cli/README.md` "Forbidden Patterns" section for detailed explanation and examples.

---

## Guardrails

**DO NOT:**
- Reintroduce a Manifest abstraction (we removed it intentionally)
- Store content in Git (only metadata or text files in the index)
- Use `rclone sync` — use action-based sync with explicit operations
- Add fields to metadata beyond `hash` and `size`
- Track symlinks or empty directories
- Wire CAS into `bit add` without reading the mode from `.bit/config` first (mode gates all CAS writes)
- Implement cloud remote layout conversion (create a new remote instead)
- Store the mode (`lite`/`solid`) in git-tracked files (it is local-only in `.bit/config`)
- Add MTL-style type classes or free monad effects (premature)
- Bypass `deriveActions` by writing custom diff/plan logic in push or pull code paths
- Write metadata to `.bit/index/` directly and then commit (bypasses git;
  rclone scans set `fContentType = BinaryContent` for everything, producing wrong metadata
  for text files)
- Guard merge commits on `hasStagedChanges` when `MERGE_HEAD` exists (the
  commit must always be created to finalize the merge, even when the tree is
  unchanged — e.g., "keep local" resolution)
- Re-scan the working directory after sync to "fix" metadata (the index is
  already correct after the git operation; rescanning is redundant or harmful)
- Use `git push` to a filesystem remote (git refuses to update checked-out
  branches; use fetch+merge at the remote instead)
- Auto-set upstream tracking (`branch.main.remote`) on push, pull, fetch, or remote
  add operations — this must only be done via explicit `bit push -u <remote>`
- Use lazy IO (`Prelude.readFile`, `writeFile`, `hGetContents`, `Data.ByteString.Lazy`,
  `Data.Text.Lazy.IO`) — causes "file is locked" errors on Windows; use strict
  ByteString operations exclusively
- Use `createProcess` with `System.IO.hGetContents` for capturing process output —
  causes "delayed read on closed handle" errors; use `Bit.Process.readProcessStrict` instead
- Use plain `writeFile` for important files — use `atomicWriteFile` for crash safety
  and Windows compatibility
- Push metadata from an unverified working tree (the metadata may reference
  files that are missing or corrupted — see Proof of Possession rule)
- Pull metadata from an unverified remote (corruption propagates through
  metadata; verify remote first, suggest `--accept-remote` if verification fails)
- Store persistent remote workspace state under `.bit/` — remote-targeted
  commands must use ephemeral temp directories (fetch → inflate → operate →
  push → cleanup). The remote bundle is the sole source of truth.
- Use `git checkout -B` in bundle inflation — it can skip the working tree
  update when already on the target branch; use `git reset --hard` instead
- Use `git clone` in bundle inflation on Windows — temp directory cleanup may
  not fully complete due to file locking, causing "directory already exists"
  errors; use `git init` + `git fetch` + `git reset --hard` instead
- Call `git` directly via `readProcessWithExitCode "git"` or `System.Process`
  outside `Bit/Git/Run.hs` — all git commands must go through `Git.spawnGit`,
  `Git.runGitRaw`, `Git.runGitAt`, or the typed wrappers in `Bit/Git/Run.hs`.
  `spawnGit` respects `BIT_REAL_GIT` to avoid shim recursion. This keeps the
  git interface in one place, makes the `-C .bit/index` base flags consistent,
  and prevents accidental git calls against the wrong working directory
- Copy or transfer files without rclone outside `Bit/Rclone/Run.hs` —
  all file transfers between local and remote (cloud or filesystem) must use
  rclone via Rclone.Run. The only exception is small local-to-local copies
  that are part of internal bookkeeping (e.g., copying a text file from
  `.bit/index/` to the working tree, or writing a metadata file)
- Create `.bit/` from non-init code paths — only `init` and `initializeRepoAt`
  may create `.bit/` from scratch. All other functions (`scanWorkingDir`,
  `writeMetadataFiles`, `saveCacheEntry`) must check that `.bit/` already
  exists and silently no-op if it doesn't. This prevents accidental `.bit/`
  directory creation in non-repo directories (e.g., from Windows `&`-chaining
  after a failed `cd`)

**ALWAYS:**
- Prefer `rclone moveto` over delete+upload when hash matches
- Push files before metadata, pull metadata before files (cloud remotes)
- For filesystem remotes, use fetch+merge (not `git push` to non-bare repos)
- Use `Bit.IO.AtomicWrite.atomicWriteFile` for all important file writes (temp file + rename pattern)
- Use strict `ByteString` operations for all file IO — never `Prelude.readFile`, `writeFile`, or lazy ByteString/Text
- Use `Bit.IO.ConcurrentFileIO.readFileBinaryStrict` / `readFileUtf8Strict` for reading
- Match Git's CLI conventions and output format
- Route all git commands through `Bit/Git/Run.hs` (`runGitRaw`, `runGitAt`, etc.)
- Route all file transfers through `Bit/Rclone/Run.hs` (rclone wrappers)
- Keep Rclone.Run dumb — no domain knowledge in Rclone.Run
- Keep Git.Run dumb — no domain interpretation
- All business logic in Bit/Core/*.hs
- Prefer git/rclone primitives over custom implementations — if git or rclone
  already solves the problem, wire into it instead of reimplementing
- Use the unified metadata parser from `Bit/Config/Metadata.hs`
- After pull/merge, set refs/remotes/origin/main to the bundle hash, not HEAD
- Capture `oldHead` before any git operation that changes HEAD, then use
  `applyMergeToWorkingDir` to sync the working directory
- Let git manage `.bit/index/` — all pull paths (normal, `--accept-remote`,
  `--manual-merge`, `mergeContinue`) must update the index via git operations
  (merge, checkout), never by writing files directly
- Always call `git commit` after conflict resolution when `MERGE_HEAD` exists
- Update tracking ref after filesystem pull (same invariant as cloud pull)
- Use `--no-track` flag for any `git checkout` that should not set upstream
  tracking (e.g., `checkoutRemoteAsMain`, `--accept-remote` flows)
- Verify local working tree before push (proof of possession)
- Verify remote before pull (proof of possession); for cloud remotes use
  `rclone lsjson --hash` (free on Google Drive), for filesystem remotes hash
  the files
- When verification fails, refuse the operation and suggest resolution:
  `bit add` / `bit restore` for local issues, `--accept-remote` / `--force` /
  `--manual-merge` for remote issues