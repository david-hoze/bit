# bit vs. Lore — comparison and follow-up tasks

Comparison of [bit](https://github.com/) against Epic Games'
[Lore](https://github.com/EpicGames/lore), a next-generation version control
system announced June 2026. Written up while working on bit's content-defined
chunking (CDC). The end of this document lists concrete follow-up tasks for the
next agent, since the environment this was written in has no Haskell toolchain
and cannot build or test.

## One-line characterization

- **Lore** = a ground-up rewrite of version control. New object model, new
  hashing (BLAKE3), new wire protocol, new server, SDKs in 4 languages. "Git,
  but rebuilt from scratch with binaries as first-class citizens."
- **bit** = a thin orchestration layer over existing tools.
  `Git(metadata) + rclone(sync)`. "Don't reinvent — wire git and rclone together
  so binaries live in cloud storage / CAS while git tracks only metadata."

## Core philosophy

| | bit | Lore |
|---|---|---|
| Build vs. reuse | Never reimplement what git/rclone do. Git owns the DAG, merge, diff; rclone owns transfer. | Reimplements the entire stack in Rust — storage, VCS, server, CLI, SDKs. Reuses only *concepts*. |
| New code | Small Haskell glue layer; heavy lifting shells out to `git` and `rclone`. CDC is one of the few pieces of genuinely novel logic. | Large self-contained system. |
| Identity | Git-compatible by construction — it *is* git underneath. | Deliberately incompatible new system; git compatibility is a non-goal. |

## Architecture

| | bit | Lore |
|---|---|---|
| Topology | Distributed (inherits git's model). Peers fetch/merge directly; cloud is dumb storage (git bundles + rclone). | Centralized server-of-record for durability, access control, conflict resolution — with offline local operation layered on. |
| Metadata store | Real git repo (`.bit/index/` mirror, SHA-1 DAG). | Custom two-store split: immutable content-addressed store + mutable key-value store (branch pointers). |
| Hashing | MD5 for file + chunk identity (CAS filename = hash). | BLAKE3. |
| Binary storage | CAS (`cas/<prefix>/<hash>`) and/or readable copies; binary files replaced by metadata stubs in the index. | Content-addressed chunk store. |
| Chunking | **FastCDC, already implemented** (`Bit/CDC/*`). Gear-hash rolling fingerprint, two-mask normalization. Bare-layout remotes only. | FastCDC / fixed-size, depending on file type. First-class. |
| Dedup granularity | Per-chunk, via the shared CAS namespace (identical chunks collapse to one blob). | Per-chunk, across all files and branches. |
| Sparse / on-demand | Lite vs. solid mode; metadata-only push/pull. Working tree always materializes real files. | First-class on-demand hydration / sparse workspaces; partial-file byte-range reads. |
| Server required | No. Any rclone backend (Drive, S3, USB, NAS) works with zero server-side software. | Yes for teams (zero-config local mode exists). |

## What bit's CDC already does

This was initially under-credited in conversation. bit has a complete CDC
subsystem — it is **not** a whole-file-only CAS:

- `Bit/CDC/FastCDC.hs` — real FastCDC: gear-hash rolling fingerprint, two-mask
  (small/large) normalization toward target average size.
- `Bit/CDC/Gear.hs` — 256-entry gear table seeded from MD5.
- `Bit/CDC/Types.hs` — `ChunkConfig`, `Chunk`, `ChunkRef`, `ChunkManifest`.
- `Bit/CDC/Manifest.hs` — per-file manifest `<hash>.manifest` listing ordered
  `(chunk-hash, length)`; offsets implicit (running sum).
- `Bit/CDC/Reassemble.hs` — streaming reassembly + whole-file hash verification.
- `Bit/Remote/ChunkIndex.hs` — remote blob inventory via `rclone lsf`.
- Spec: `docs/spec/cdc-spec.md`. Tests: `test/cli/cdc-chunking.test`,
  `test/cli/cdc-default.test`.

Defaults: 32 KB / 128 KB / 512 KB (min/avg/max). Push uploads only chunks the
remote lacks; pull downloads only chunks the local CAS lacks. Benchmark in the
spec: a 6-byte edit in a 227 MB file → 2 chunks uploaded, 1,370 skipped.

## Where bit's CDC still trails Lore

| Gap | bit today | How to close it |
|---|---|---|
| File hash | MD5 by default. | **`bit cas rehash` implemented** — switches `core.hash-algo` to BLAKE3 and rewrites stubs + CAS whole-file blobs + manifest filenames to BLAKE3, then commits. Verify/add are algorithm-aware (dispatch on the stored hash's prefix). Chunk *content* hashes stay MD5 (a chunk hash is just a content address, independent of file identity), so the chunker is untouched. Pre-migration commits keep their MD5 content for history. |
| Remote chunk index | `rclone lsf` the whole CAS every push — O(remote size), slow on Drive. | **Implemented** — local `pushed-blobs` cache at `.bit/cache/pushed-blobs/<name>` as a front cache; `rclone lsf` runs only on a cache miss. |
| Merkle structure | Flat manifest (a list); integrity is end-to-end via whole-file hash, not a per-file Merkle tree. | Acceptable division of labor: git's tree objects provide the *directory* Merkle layer; manifests handle *intra-file* chunking. No need to rebuild git's tree layer. |
| Streaming chunking | **FIXED in this change** — `chunkFile` was loading the entire file into memory; now uses a bounded ~2*maxSize buffer. | See below. |
| Partial-file reads | None — reassembly is always whole-file. | Deliberate non-goal: bit materializes the real file in the working tree, unlike Lore's sparse workspace. |
| Inter-chunk delta | None (explicitly out of scope). | Lore doesn't do this either. No gap. |
| Orphan-chunk GC | **Implemented** — `bit cas gc [--dry-run]` mark-sweeps orphan blobs/chunks/manifests; live set spans all reachable commits + the current index. | Mark-sweep over live manifests. |

## Bottom line

The honest comparison is narrower than "git can't chunk." bit's CAS already does
chunk-level CDC dedup, on dumb storage, with no server — a neat result. The real
remaining gaps are MD5-vs-BLAKE3, the O(remote-size) dedup query, and (until this
change) the in-memory chunker. bit optimizes for *small teams / individuals who
want git ergonomics over arbitrary cloud storage*; Lore optimizes for *large
studios moving terabytes of binary assets* with centralized access control.

**Sources:**
- <https://github.com/EpicGames/lore>
- <https://epicgames.github.io/lore/explanation/system-design/>
- <https://epicgames.github.io/lore/faq/>

---

## Change made in this branch

**Streaming `chunkFile`** (`Bit/CDC/FastCDC.hs`). Previously `chunkFile` read the
whole file into memory (`BS.hGet h fileSize`), which would OOM on files larger
than RAM — the single most concrete limit vs. Lore's multi-GB design. Replaced
with a bounded-buffer streaming loop:

- Invariant: before every `findBoundary` call, the buffer holds **at least
  `maxSize` bytes, or all remaining bytes at EOF**. This guarantees the boundary
  search sees the identical window it would on the full file, so output is
  **byte-identical** to `chunkByteString` (proof: `endPos = min(buflen, maxSize)`
  matches `min(fileLen - off, maxSize)` exactly when buflen ≥ maxSize or EOF).
- Peak memory ~2*maxSize (1 MB at defaults) regardless of file size.
- Tail-recursive with an accumulator to avoid stack growth on files with many
  chunks. `BS.drop` is an O(1) slice; the subsequent `fill`'s `<>` copies the
  small leftover and lets the old buffer be GC'd.

Note: `chunkByteString` (the pure in-memory variant) is unchanged and still used
for testing and small inputs.

## STATUS — tasks 1–5 completed (2026-06-25)

The streaming `chunkFile` change was built, verified, and tested on a Linux/WSL
env with the Haskell toolchain. Results:

1. **Build** ✅ — compiles cleanly; the streaming change added no new warnings.
2. **Byte-identical gate** ✅ — new `cdc` tasty suite (`test/CdcSpec.hs`, wired
   into `bit.cabal`) asserts `chunkFile == chunkByteString` for empty,
   single-byte, <minSize, =minSize, between, =maxSize, exact-multiple-of-maxSize,
   many-chunks, all-zeros, plus 100 random QuickCheck inputs. All pass.
3. **Large-file smoke test** ✅ — `test/single-workflows/cdc-largefile-smoke.hs`.
   A 1 GiB file (>RAM headroom) chunks with **max residency 2.5 MB** (~2×maxSize),
   chunks tile the file contiguously, reassembled MD5 matches whole-file MD5.
   *Hazard learned: building a 4 GB fixture on a 3.7 GB-RAM WSL VM crashed the
   whole VM via page-cache flooding — see `docs/large-file-testing.md`. 1 GB is
   sufficient and safe.*
4. **Suites** ✅ — CLI 1456 passed / 0 failed (1357 local + 99 gdrive cloud),
   binary 20/20 scripts, git representative subset 10/10 scripts. The CDC CLI
   tests (`cdc-chunking`, `cdc-default`) pass 54/54.
5. **Spec/impl reconcile** ✅ — code is off-by-default; fixed the contradicting
   "default: enabled" claims in `docs/spec/cdc-spec.md` (INI comment + parameter
   table) and three tutorials (`config.md`, `bare-remotes.md`, `modes-and-cas.md`).

Open follow-up noted: `docs/spec/cdc-spec.md:1000` lists the benchmark's "default"
chunk config as 128K/512K/2M, which doesn't match the code defaults
(32K/128K/512K). Left as-is — it's a measured-benchmark record, not a normative
default — but worth relabeling.

## ORIGINAL TASKS FOR THE NEXT AGENT (env with Haskell)

This was written on an env with no `cabal`/`ghc`. The streaming change is
committed but **has not been built or tested**. Please:

1. **Build.** Per `CLAUDE.md`:
   ```bash
   cabal build
   # or, if cabal reports "Up to date" wrongly:
   cabal exec -- ghc --make -fforce-recomp -O -o bit-new.exe Bit.hs && cp bit-new.exe "$(where bit)"
   ```
   Watch for: unused-import warnings (removed `hFileSize`, added `Handle`),
   and the `Int` offset (`chunkOffset` is `Int64`, `absOff` is now `Int` —
   `fromIntegral` bridges; fine on 64-bit but confirm no warning).

2. **Verify byte-identical output.** Add/extend a test asserting
   `chunkFile cfg path == chunkByteString cfg (whole-file bytes)` for several
   inputs: empty file, file < minSize, file between minSize and maxSize, file
   spanning many chunks, and a file whose length is an exact multiple of maxSize.
   This is the core correctness property of the change.

   > **Recommendation: treat this as a blocking gate.** It is the one test that
   > proves the streaming rewrite did not change chunk boundaries. If boundaries
   > shift, existing CAS blobs no longer match newly-produced chunks, silently
   > breaking dedup (every file re-uploads in full) without any error. Do not
   > ship the streaming `chunkFile` until this assertion passes for all the
   > sizes above — especially the EOF cases (< minSize and exact-multiple-of-
   > maxSize), where the buffer-fill invariant is most likely to differ from the
   > in-memory path.

3. **Large-file smoke test.** Chunk a file larger than the test machine's
   comfortable RAM headroom (e.g. several GB sparse/random file) and confirm
   memory stays bounded (~2*maxSize) and the reassembled file's hash matches.
   Reuse `test/single-workflows/test-minio-*.sh` patterns.

4. **Run suites in order** (see `CLAUDE.md` "Running Tests"): CLI suite, then
   binary suite (`test/t/`), then git suite. The CDC tests are
   `test/cli/cdc-chunking.test` and `test/cli/cdc-default.test`.

5. **Reconcile a spec/impl discrepancy found during this work:**
   `docs/spec/cdc-spec.md` line ~715 says CDC is "default: enabled", but
   `Bit/CDC/Config.hs` makes it **off by default** (`getCdcConfig` returns `Just`
   only when `cdc.enabled` is explicitly `"true"`). Decide which is correct and
   fix the other. The conservative choice is off-by-default (matches code);
   update the spec text.

6. **Optional follow-ups** (from the gap table above), in rough priority order:
   - Local `pushed-blobs` cache to avoid `rclone lsf`-ing the whole CAS per push.
   - `bit cas gc` for orphan chunks.
   - `bit cas rehash` MD5→BLAKE3 migration (whole-file + chunks together).
