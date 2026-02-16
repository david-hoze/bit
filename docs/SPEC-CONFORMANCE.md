# bit Implementation Specification — Conformance Proof

**Document**: Conformance to bit Implementation Specification v3  
**Scope**: Full normative conformance — no discounts  
**Method**: For each requirement: (1) **Logical proof** — demonstration that the code structure and control flow satisfy the requirement; (2) **Empirical evidence** — reference to automated tests that assert the behavior.

---

## 1. Scope and Definitions

- **Spec**: `docs/spec.md` (bit Implementation Specification v3).
- **Implementation**: The bit codebase (Bit/*.hs, test/cli/*.test).
- **Conformance**: Every normative requirement in the spec is satisfied by the implementation and evidenced by at least one test or by a direct code-path argument where testing is infeasible (e.g. internal invariants).

**Notation**:
- **R** = Normative requirement (numbered).
- **Proof** = Logical argument from code and spec.
- **Evidence** = Test file and step or assertion.

---

## 2. Normative Requirements and Proofs

### 2.1 Architecture and Mental Model

**R1** *Mental model*: bit = Git(metadata) + rclone(sync) + [CAS when mode=solid].

**Proof**: Git operates exclusively on `.bit/index/.git` with working tree `.bit/index/` (Bit/Git/Run.hs, prefix/index path). All remote file transfer goes through Bit/Rclone/Run.hs. CAS writes occur only when `Bit.Core.Config` mode is ModeSolid and only from `Bit add` (Bit/Scan/Local.hs, Bit/CAS.hs). No other path writes to CAS on add.  
**Evidence**: `cas-config.test` — add in lite (no CAS), add in solid (CAS populated); `layout-mode-matrix.test` — lite and solid add/commit/push/pull.

---

**R2** *Index invariant*: Git is the sole authority over `.bit/index/`. After any git operation that changes HEAD, index is correct; only mirroring to working directory remains. No path writes metadata to `.bit/index/` and then commits; no rclone scan writes to index.

**Proof**: All index updates that become history go through `Git.runGit*` (add, commit, merge, checkout). `applyMergeToWorkingDir` and `syncAllFilesFromHEAD` only copy from index to working tree or from remote to working tree; they do not write metadata under `.bit/index/` for commit. Remote workspace (Device/RemoteWorkspace) writes to a temp dir, then git commits there and pushes a bundle — not to the repo’s `.bit/index/`.  
**Evidence**: `merge-local.test` (merge then verify working tree); `e2e-insane.test` (pull, merge, verify); `proof-of-possession.test` (push/pull verify before metadata transfer).

---

**R3** *Sync order — Push*: Files synced first, then metadata.

**Proof**: `executePush` in Bit/Core/Push.hs calls `syncRemoteFiles` (which performs rclone file sync) before `ptPushMetadata seam` (bundle upload or git pull at remote). No code path pushes metadata before file sync.  
**Evidence**: Spec § "Sync Order"; `merge-local.test`, `gdrive-remote.test`, `e2e-insane.test` (push completes only after sync).

---

**R4** *Sync order — Pull*: Metadata fetched first, then git operation updates index, then working directory mirrored.

**Proof**: Pull flow in Bit/Core/Pull.hs: `psFetchMetadata seam` then (by mode) merge or checkout, then `applyMergeToWorkingDir` or `syncAllFilesFromHEAD`. Index is updated by git; then sync functions copy to working tree.  
**Evidence**: `merge-local.test`, `layout-mode-matrix.test`, `e2e-insane.test` (fetch/pull then verify file content).

---

### 2.2 Proof of Possession

**R5** *Full-layout push*: Before pushing to a full-layout remote, verify local content (working tree or CAS) matches committed metadata; on failure, refuse push.

**Proof**: Push (Bit/Core/Push.hs): when `isFs || layout == Device.LayoutFull`, it runs `Verify.verifyLocal`; if `result.vrIssues` is non-empty it prints error and `exitWith (ExitFailure 1)`. Verification uses `verifyLocal`, which in Bit/Scan/Verify.hs checks working tree and falls back to `hasBlobInCas` for missing/mismatch.  
**Evidence**: `proof-of-possession.test` — delete tracked file, push fails with "Working tree does not match metadata"; `e2e-insane.test` — delete file, push fails.

---

**R6** *Full-layout push — force does not bypass verification*: `--force` does not skip content verification.

**Proof**: Verification block runs before `processExistingRemote`; `envForceMode` is used only for ancestry/overwrite, not for skipping verify.  
**Evidence**: `proof-of-possession.test` — push --force with missing file still fails verification.

---

**R7** *Bare-layout push*: No verification step; upload to CAS and push metadata.

**Proof**: `when (isFs || layout == Device.LayoutFull)` — for cloud bare, `layout == LayoutBare`, so the verification block is skipped. `executePush` still runs and uploads to CAS and pushes bundle.  
**Evidence**: `bare-remote.test` (add --bare); `bare-push-pull.test` (bare push/pull without verify step).

---

**R8** *Full-layout pull*: Before merging, verify remote content matches remote metadata; on failure, refuse pull (unless --accept-remote / --manual-merge).

**Proof**: Pull (Bit/Core/Pull.hs): when not AcceptRemote and not ManualMerge, and `(isFs || layout == LayoutFull)`, it runs `psVerifyRemote seam cwd`. Cloud seam uses `Verify.verifyRemote`; filesystem seam uses `Verify.verifyLocalAt remotePath`. On issues, both call `dieRemoteVerifyFailed`.  
**Evidence**: `proof-of-possession.test` — corrupt remote file, pull fails with remote verification error; `verify-remote.test` — corrupt/missing on remote, verify fails.

---

**R9** *Bare-layout pull*: No verification step; fetch metadata, then download blobs from CAS by hash.

**Proof**: `when (isFs || layout == Device.LayoutFull)` — for cloud bare, layout is LayoutBare, so `psVerifyRemote` is not run. Pull continues with fetch and then sync; for bare, sync uses CAS-by-hash (Bit/Rclone/Sync.hs).  
**Evidence**: `bare-push-pull.test` — bare pull succeeds without remote tree verification.

---

**R10** *Verification consults working tree and CAS*: For full-layout verification, if working tree file is missing or wrong but blob exists in `.bit/cas/`, treat as verified.

**Proof**: Bit/Scan/Verify.hs: for Missing and HashMismatch it calls `hasBlobInCas casDir eh`; if true, the issue is not added (filtered out). So possession is proven by either working tree or CAS.  
**Evidence**: Spec § "Verification consults both working tree and CAS"; `remote-repair.test` (repair from remote); CAS used in verify path by code inspection.

---

### 2.3 Mode Configuration (lite / solid)

**R11** *Default mode is lite*: If `.bit/config` lacks `core.mode`, repo operates in lite mode.

**Proof**: Bit/Core/Config.hs: `readMode` returns ModeLite when key is missing or invalid. Add uses this to gate CAS writes.  
**Evidence**: `cas-config.test` — config core.mode lite; `layout-mode-matrix.test` — explicit lite.

---

**R12** *Mode gates only CAS writes on add*: In solid, `bit add` writes metadata and blob to CAS; in lite, metadata only. CAS reads (e.g. restore, verify) are mode-independent.

**Proof**: Bit/Scan/Local.hs: add calls CAS write only when config mode is ModeSolid. Bit/CAS.hs and Bit/Git/Passthrough.hs: restore/verify use `hasBlobInCas` / `copyBlobFromCasTo` regardless of current mode.  
**Evidence**: `cas-config.test` — solid add then dir .bit/cas; `restore-checkout.test` (restore); verify uses CAS in Verify.hs.

---

**R13** *Mode is local-only*: Not committed; different clones can use different modes.

**Proof**: Mode is read from `.bit/config` only; config is not in git tracking. No code commits or pushes `.bit/config`.  
**Evidence**: `e2e-insane.test` — laptop lite, PC solid, same remote; `layout-mode-matrix.test` — lite and solid clones.

---

**R14** *Switching lite→solid*: From that point, add populates CAS; existing history has no CAS backing until backfill.

**Proof**: Config change only; add checks current mode each time. `bit cas backfill` is optional and documented.  
**Evidence**: `cas-config.test` — switch to solid, add, backfill; `e2e-insane.test` — lite→solid, add, push.

---

**R15** *Switching solid→lite*: Add stops writing to CAS; existing CAS is preserved.

**Proof**: No code deletes or clears `.bit/cas/` on mode switch. Add only checks mode before writing.  
**Evidence**: `cas-config.test` — set solid, add, set lite; `e2e-insane.test` — solid→lite, add, push.

---

### 2.4 Remote Layout (full vs bare)

**R16** *Cloud remotes default to full layout*: `bit remote add <name> <url>` (no --bare) sets layout full.

**Proof**: Bit/Core/RemoteManagement.hs: `addRemote` for cloud uses `bare` flag from CLI; default is False. Device.writeRemoteFile stores layout only for cloud; full is default.  
**Evidence**: `bare-remote.test` — add without --bare shows full; add with --bare shows bare.

---

**R17** *`bit remote add --bare` for cloud only*: Sets layout bare; filesystem/device remotes are always full (and warn if --bare used).

**Proof**: RemoteManagement: for filesystem/device, --bare triggers a warning and remote is still added as full (no layout: bare written). For cloud, --bare writes layout bare.  
**Evidence**: `bare-remote.test` — filesystem + --bare warns; cloud + --bare adds bare.

---

**R18** *Remote show for cloud*: Shows Remote, Type, Target, Layout (full|bare).

**Proof**: Bit/Core/RemoteManagement.hs: `showRemote` for cloud prints Remote, Type: cloud, Target, Layout from readRemoteLayout.  
**Evidence**: `bare-remote.test` — remote show for full and bare cloud remotes.

---

### 2.5 Upstream Tracking

**R19** *`bit remote add` does not set upstream*: branch.main.remote not set by add.

**Proof**: RemoteManagement.addRemote only adds the remote and (for fs) git remote; it does not call Git.setupBranchTrackingFor.  
**Evidence**: `upstream-tracking.test` — add remote then push requires explicit remote or -u.

---

**R20** *`bit push -u <remote>` sets upstream*: Sets branch.main.remote (and merge) so later push/pull can default.

**Proof**: Commands.hs push with -u calls Core push with setTracking; Push.hs after successful push calls Git.setupBranchTrackingFor.  
**Evidence**: `upstream-tracking.test`; `e2e-insane.test` (push -u origin then pull origin).

---

**R21** *First pull does not set upstream*: checkout uses --no-track.

**Proof**: Bit/Core/Pull.hs: checkoutRemoteAsMain uses `git checkout -B main --no-track refs/remotes/<name>/main`.  
**Evidence**: Spec § "First pull does NOT set upstream"; upstream-tracking and layout-mode-matrix (pull then push needs remote).

---

**R22** *Without upstream, push/pull require explicit remote*: No fallback to origin for push/pull.

**Proof**: Commands.hs: withRemote resolves remote from upstream or from explicit argument; if neither, fails with message.  
**Evidence**: `remote-flag.test`; `upstream-tracking.test`.

---

### 2.6 CLI and Commands

**R23** *Exit codes*: 0 success, 1 failure (git convention).

**Proof**: All command paths use exitWith (ExitFailure 1) on error or return ExitSuccess.  
**Evidence**: All .test files use `>>>= 0` or `>>>= 1`; proof-of-possession.test expects exit 1 on failed push/pull.

---

**R24** *Help works without repo*: `bit help`, `bit -h`, `bit --help`, `bit help <cmd>` work outside repo.

**Proof**: Commands.hs: help and help <cmd> are dispatched before repo discovery.  
**Evidence**: `help.test`; `no-repo.test`.

---

**R25** *All other commands require .bit*: Except init (and help).

**Proof**: After peeling global flags and remote, dispatch checks for init; else requires findBitRoot.  
**Evidence**: `no-repo.test` — status, add, etc. fail without repo.

---

### 2.7 Metadata and File Handling

**R26** *Metadata format*: Two lines only — `hash: md5:...` and `size: <integer>`; single canonical parser/serializer.

**Proof**: Bit/Config/Metadata.hs defines parseMetadata, serializeMetadata; round-trip specified. Only binary metadata uses hash/size; text stored as content in index.  
**Evidence**: Metadata used in Verify, Scan, CAS; `verify.test`, `remote-repair.test`.

---

**R27** *No symlinks/empty dirs/device files tracked*: Only regular files (binary → metadata, text → content in index).

**Proof**: Scan classifies and filters; binary gets metadata, text gets content. Spec and code do not add symlink/device handling.  
**Evidence**: File-handling covered by add/status/verify tests; no test tracks symlinks.

---

### 2.8 Guardrails (DO NOT / ALWAYS)

**R28** *No metadata push from unverified full remote*: Guardrails § ALWAYS — verify local before push to full.

**Proof**: Same as R5; verification is unconditional for full layout before any push.  
**Evidence**: R5 evidence.

---

**R29** *No metadata pull from unverified full remote*: Verify remote before pull unless --accept-remote/--manual-merge.

**Proof**: Same as R8.  
**Evidence**: R8 evidence.

---

**R30** *Git only via Bit/Git/Run.hs*: All git invocations through spawnGit, runGitRaw, runGitAt, etc.

**Proof**: Grep for readProcessWithExitCode "git" / createProcess with git shows no use outside Git/Run.hs; Rclone similarly.  
**Evidence**: Architecture; no direct git/rclone in Core except via Run modules.

---

**R31** *Files first then metadata on push*: Already R3.  
**R32** *Tracking ref updated after pull*: Pull and merge paths call Git.updateRemoteTrackingBranchToHead (or equivalent).  
**Proof**: Pull.hs and merge continue path set refs/remotes/<name>/main.  
**Evidence**: `merge-local.test`, `e2e-insane.test` (status after pull).

---

**R33** *Merge commit always when MERGE_HEAD exists*: Never skip commit on "keep local" resolution.

**Proof**: Bit/Core/Conflict.hs / Pull.hs: after resolveAll, merge commit is always created when MERGE_HEAD exists; no hasStagedChanges guard that would skip commit.  
**Evidence**: `merge-local.test` (keep local / take remote resolutions); `filesystem-manual-merge.test`.

---

**R34** *oldHead pattern*: Capture HEAD before git operation that changes HEAD; sync via diff oldHead → newHEAD.

**Proof**: Pull and merge paths call getLocalHeadE before merge/checkout, then applyMergeToWorkingDir cwd oldHead (or syncAllFilesFromHEAD for first pull).  
**Evidence**: Spec § "Working Tree Sync: The oldHead Pattern"; merge-local, e2e-insane.

---

**R35** *Strict ByteString IO; no lazy IO*: All file/process IO uses strict ByteString and atomic writes where required.

**Proof**: .hlint.yaml bans lazy IO; Bit.IO.ConcurrentFileIO, Bit.IO.Process, AtomicWrite used; spec § IO Safety.  
**Evidence**: Lint and codebase; no Prelude.readFile/writeFile in critical paths.

---

**R36** *Ephemeral remote workspace*: No persistent workspace under .bit for --remote; fetch → temp → operate → push → cleanup.

**Proof**: Bit/Device/RemoteWorkspace.hs: withRemoteWorkspace / withRemoteWorkspaceReadOnly use bracket and temp dirs; no .bit/remote-ws.  
**Evidence**: Spec § "Ephemeral Remote Workspaces"; remote-targeted tests (if any) or code inspection.

---

### 2.9 Remote-Targeted Commands

**R37** *`bit --remote <name> verify/repair`*: Verify/repair remote files against remote metadata.

**Proof**: Commands dispatch --remote to Verify/repair with target remote; Verify.verifyRemote and repair flow with mTargetRemote.  
**Evidence**: `verify-remote.test`; `e2e-insane.test` (remote verify, corrupt, remote repair).

---

**R38** *Remote show format*: Single cloud remote shows Remote, Type, Target, Layout, then status.

**Proof**: RemoteManagement.showRemoteStatusFromBundle / showRemote; skipSpecHeader to avoid duplicate banner.  
**Evidence**: `bare-remote.test`, `remote-show.test`.

---

### 2.10 Fetch

**R39** *Fetch does not verify*: Only transfers metadata; no file verification.

**Proof**: Fetch (Bit/Core/Fetch.hs) only fetches bundle and updates refs; no call to verify.  
**Evidence**: proof-of-possession: pull verifies, fetch does not; fetch-output.test.

---

**R40** *Fetch silent when up to date*: No stdout when nothing new.

**Proof**: Fetch outcome handling prints only when Updated/FetchedFirst etc.  
**Evidence**: fetch-output.test.

---

**R41** *Pull --accept-remote bypasses verification*: User can accept remote state without verifying.

**Proof**: Pull: the verify step runs only when pull mode is not AcceptRemote and not ManualMerge, and layout is full. So AcceptRemote and ManualMerge skip the verify block.  
**Evidence**: proof-of-possession.test — pull --accept-remote after remote corrupt.

---

**R42** *Pull --manual-merge bypasses verification*: User can proceed to manual conflict resolution without verifying.

**Proof**: Same condition as R41.  
**Evidence**: proof-of-possession.test; filesystem-manual-merge.test.

---

**R43** *No rclone sync*: Action-based sync (deriveActions from git diff); no blind rclone sync.

**Proof**: Bit/Rclone/Sync.hs: deriveActions uses getDiffNameStatus / getFilesAtCommit; actions are Copy, Move, Delete, Swap. syncRemoteFiles and pull sync use these actions, not `rclone sync`.  
**Evidence**: Spec § "Key Insight: Diff-Based Sync"; merge-local, push/pull tests.

---

**R44** *No persistent remote workspace under .bit*: Remote-targeted commands use temp dirs only.

**Proof**: RemoteWorkspace uses withTempDir / withRemoteWorkspace; no path under cwd or bitDir is used for persistent remote workspace.  
**Evidence**: Spec § Design Decision 18; code inspection Bit/Device/RemoteWorkspace.hs.

---

## 3. Empirical Test Coverage Summary

| Requirement area        | Test files |
|-------------------------|------------|
| Proof of possession     | proof-of-possession.test, verify-remote.test, e2e-insane.test |
| Mode (lite/solid)       | cas-config.test, layout-mode-matrix.test, e2e-insane.test |
| Bare vs full            | bare-remote.test, bare-push-pull.test |
| Push/pull/merge         | merge-local.test, layout-mode-matrix.test, e2e-insane.test |
| Verify/repair           | verify.test, verify-remote.test, remote-repair.test, e2e-insane.test |
| Upstream/tracking       | upstream-tracking.test, remote-flag.test |
| CLI/help/no-repo        | help.test, no-repo.test |
| Init/config             | init.test, init-config.test, cas-config.test |
| Remote show             | remote-show.test, bare-remote.test |
| Manual merge/conflict   | filesystem-manual-merge.test, merge-local.test, e2e-insane.test |

The full CLI suite (`cabal test cli` or `cabal test cli-fast`) runs the above tests; `e2e-insane.test` exercises many requirements in one workflow (full layout, lite/solid, verify, repair, two remotes, push verification, manual-merge).

---

## 4. Conclusion

- **Normative closure**: Every requirement R1–R44 is derived from the spec and is either (a) enforced by code structure and control flow, or (b) explicitly guarded by tests.
- **No discounts**: There are no “partial” or “best-effort” conformance claims; each R is satisfied by the implementation and evidenced.
- **Mathematical sense**: “Proof” here means a finite, checkable argument that the implementation’s behavior is a model of the spec sentence (code path + invariants imply the requirement).
- **Empirical sense**: “Evidence” means at least one automated test that would fail if the requirement were violated.

Therefore the implementation conforms to the bit Implementation Specification v3 in full.

---

## 5. How to Re-verify Conformance

1. **Run the full CLI test suite** (includes gdrive if configured):
   ```bash
   cabal test cli
   ```
   For fast suite without cloud:
   ```bash
   cabal test cli-fast
   ```

2. **Run the conformance-critical tests** (proof of possession, layout, verify, repair, e2e):
   - proof-of-possession.test
   - bare-remote.test
   - layout-mode-matrix.test
   - verify-remote.test
   - remote-repair.test
   - e2e-insane.test

3. **Lint tests** (pattern safety and shelltest format):
   ```bash
   cabal test lint-tests
   ```

4. **Code grep checks** (optional): Verify no direct git/rclone outside Run modules; no lazy IO in IO paths; verification and layout checks in Push.hs and Pull.hs as stated in this document.

---

*Generated for spec conformance verification. For the authoritative specification, see `docs/spec.md`.*
