# Design Decisions

## What We Chose

1. **Phantom-typed hashes**: `Hash (a :: HashAlgo)` -- the compiler distinguishes MD5 from SHA256. Mixing algorithms is a compile error. The `DataKinds` extension is used per-module.

2. **Unified metadata parser and loader**: A single module handles all parsing and serialization, and a `MetadataSource` abstraction provides unified metadata loading. This eliminates the class of bugs where multiple parsers or loaders handle edge cases differently. The `MetadataEntry` type forces callers to explicitly handle the binary vs. text file distinction.

3. **`ReaderT BitEnv IO` (no free monad)**: A free monad effect system was considered (for testability and dry-run mode) but rejected as premature -- no pure tests or dry-run usage existed to justify the complexity.

4. **Shared `deriveActions`**: Both push and pull derive `[RcloneAction]` from `git diff --name-status` via the shared `deriveActions` function. `resolveSwaps` detects pairwise Move swaps. Property tests verify swap detection.

5. **Structured conflict resolution**: Conflict handling is a fold over a conflict list, not an imperative block. Decision logic is separated from IO mechanics.

6. **Remote as opaque type**: `Remote` is exported without its constructor. Only `remoteName` is public for display. `remoteUrl` exists for Transport to extract the URL, but business logic should use `displayRemote` for user-facing messages.

7. **Tracking ref invariant**: `refs/remotes/origin/main` must always reflect what the remote actually has -- never a local-only commit. After **push**, updating to HEAD is correct. After **pull/merge**, update to the hash from the fetched bundle, because HEAD includes merge commits the remote doesn't know about.

8. **Git is the sole authority over `.bit/index/`**: No code path should write metadata files to the index and commit them directly. The index is always populated by git operations (merge, checkout, commit via `bit add`).

9. **Always commit when MERGE_HEAD exists**: After conflict resolution, `git commit` must always be called -- never guarded by `hasStagedChanges`. Skipping the commit leaves `MERGE_HEAD` dangling and breaks the next push.

10. **The `oldHead` capture pattern**: Before any git operation that changes HEAD, capture HEAD so `applyMergeToWorkingDir` can diff old vs new and sync only what changed. The only exception is first pull (`oldHead = Nothing`), which falls back to `syncAllFilesFromHEAD`.

11. **Proof of possession on push/pull (full-layout remotes only)**: Verification checks the working tree against committed metadata. CAS is a repair source, not a verification shortcut -- corruption is always surfaced. Bare-layout remotes are exempt (self-verifying). The existing divergence resolution mechanisms serve as escape hatches.

12. **Seam pattern for transport differences**: Both push and pull use a single code path for cloud and filesystem remotes. The only difference -- how metadata is fetched/pushed -- is isolated behind a small seam record (`PushSeam`/`PullSeam`).

13. **Filesystem remotes are full repos**: When pushing to a filesystem path, bit creates a complete bit repository at the remote. Anyone at that location can run bit commands directly.

14. **Upstream tracking requires explicit `-u` flag**: Following git-standard behavior, `branch.main.remote` is never set automatically.

15. **Strict ByteString IO exclusively**: All file operations use strict `ByteString`, never lazy IO. Lazy IO on Windows keeps file handles open until garbage collection, causing "permission denied" and "file is locked" errors.

16. **Atomic writes with retry logic**: All important writes use the temp-file + rename pattern. On Windows, includes retry logic (up to 5 retries, 6 total attempts) with linear backoff (~750ms total window). Linear backoff is intentional: exponential backoff matters for shared remote services where thundering-herd effects are real, but for local filesystem atomic writes, contention is brief and there is no amplification from concurrent clients.

17. **ConcurrentIO newtype without MonadIO**: For modules that need type-level IO restrictions. Constructor is not exported. Async integration by explicit unwrapping via `runConcurrentIO`, not via `MonadUnliftIO`.

18. **Ephemeral remote workspaces (no persistent state)**: Remote-targeted commands use an ephemeral workspace pattern -- each command fetches the bundle, inflates, operates, re-bundles, pushes, and cleans up. No persistent workspace under `.bit/`.

19. **Mode is local-only, gates writes only**: The lite/solid mode affects only whether `bit add` writes blobs to `.bit/cas/`. CAS reads are always available regardless of mode. Switching is instant with no migration or data loss.

20. **Cloud remote layout (full vs bare), CAS always present**: Both layouts always include a `cas/` directory -- the only difference is whether the human-readable file tree also exists. Layout conversion is not supported.

---

## What We Deliberately Do NOT Do

- **`RemoteState` does not need a typed state machine.** The pattern match in push logic is clear and total.
- **`RcloneAction` does not need a Group structure.** Actions are derived fresh from git each time; inverse/compose would be dead code.
- **No Arrow syntax.** Plain `>>=` and function composition are clearer.
- **No MTL-style type classes** (`MonadGit`, `MonadRclone`). Everything is concrete.
- **No post-sync metadata rescanning.** After syncing files to the working directory, we do NOT re-scan and rewrite metadata. Git already put the correct metadata in the index.
- **No cloud remote layout conversion.** Create a new remote with the desired layout and push to it.

---

## Guardrails

**DO NOT:**
- Reintroduce a Manifest abstraction (removed intentionally)
- Store content in Git (only metadata or text files in the index)
- Use `rclone sync` -- use action-based sync with explicit operations
- Add fields to metadata beyond `hash` and `size`
- Track symlinks or empty directories
- Wire CAS into `bit add` without reading the mode from `.bit/config` first
- Implement cloud remote layout conversion
- Store the mode (`lite`/`solid`) in git-tracked files (it is local-only)
- Add MTL-style type classes or free monad effects (premature)
- Bypass `deriveActions` by writing custom diff/plan logic
- Write metadata to `.bit/index/` directly and then commit (bypasses git; rclone scans produce wrong metadata for text files)
- Guard merge commits on `hasStagedChanges` when `MERGE_HEAD` exists
- Re-scan the working directory after sync to "fix" metadata
- Use `git push` to a filesystem remote (use fetch+merge at the remote instead)
- Auto-set upstream tracking on push, pull, fetch, or remote add -- only via explicit `bit push -u <remote>`
- Use lazy IO -- causes "file is locked" errors on Windows
- Use `createProcess` with `System.IO.hGetContents` -- use `Bit.Process.readProcessStrict` instead
- Use plain `writeFile` for important files -- use `atomicWriteFile`
- Push metadata to a full-layout remote from an unverified working tree
- Pull metadata from an unverified full-layout remote
- Store persistent remote workspace state under `.bit/`
- Use `git checkout -B` in bundle inflation (can skip working tree update)
- Use `git clone` in bundle inflation on Windows (temp dir cleanup issues)
- Call `git` directly outside `Bit/Git/Run.hs`
- Copy or transfer files without rclone outside `Bit/Rclone/Run.hs`
- Create `.bit/` from non-init code paths

**ALWAYS:**
- Prefer `rclone moveto` over delete+upload when hash matches
- Push files before metadata, pull metadata before files (cloud remotes)
- For filesystem remotes, use fetch+merge (not `git push` to non-bare repos)
- Use `Bit.IO.AtomicWrite.atomicWriteFile` for all important file writes
- Use strict `ByteString` operations for all file IO
- Match Git's CLI conventions and output format
- Route all git commands through `Bit/Git/Run.hs`
- Route all file transfers through `Bit/Rclone/Run.hs`
- Keep Rclone.Run and Git.Run dumb -- no domain knowledge
- All business logic in Bit/Core/*.hs
- Prefer git/rclone primitives over custom implementations
- Use the unified metadata parser
- After pull/merge, set refs/remotes/origin/main to the bundle hash, not HEAD
- Capture `oldHead` before any git operation that changes HEAD
- Let git manage `.bit/index/`
- Always call `git commit` after conflict resolution when `MERGE_HEAD` exists
- Update tracking ref after filesystem pull
- Use `--no-track` for checkout that should not set upstream tracking
- Verify local content before push to full-layout remotes
- Verify remote before pull from full-layout remotes
- When verification fails, refuse and suggest resolution

---

## Known Deviations and TODOs

### Remaining Work

- **Transaction logging**: For resumable push/pull operations.
- **Error messages**: Some need polish to match Git's style and include actionable hints.
- **`isTextFileInIndex` fragility**: The current check (looking for `"hash: "` prefix) works but is indirect.

### Future (bit-solid)

- **`bit cas gc`**: Prune unreferenced blobs. Safe default: never auto-delete.
- Sparse checkout via symlinks to CAS blobs
- `bit materialize` / `bit checkout --sparse`
