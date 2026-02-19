# Overview

## Context and Vision

**bit** is a version control system designed for large files that leverages Git as a metadata-tracking engine while storing actual file content separately. The core insight: Git excels at tracking small text files, so we feed it exactly that -- tiny metadata files instead of large binaries.

**Mental Model**: bit = Git(metadata) + rclone(sync) + [CAS(content) when mode=solid]

### bit-lite vs Git vs bit-solid: Content Authority

The three systems differ in where content lives and what guarantees that provides:

**Git** stores file content directly in its object store (`.git/objects/`). Every blob, tree, and commit is content-addressed by SHA-1. When you push, you're transferring objects that are self-verifying. When you pull, the objects you receive are self-verifying. Metadata (commits, trees) and content (blobs) live in the same store. The object store is the single source of truth, and it can always back up any metadata claim.

**bit-solid** adds a content-addressed store (CAS) alongside Git's metadata tracking. Like Git, every version of every file is stored by its hash. The CAS backs up every metadata claim unconditionally -- if the metadata says a file existed at commit N with hash X, the CAS has that blob. This enables full binary history, time travel, and sparse checkout.

**bit-lite** has no object store and no CAS. Git tracks only metadata files (2-line hash+size records). The actual binary content lives exclusively in the working tree. There is exactly one copy of each file -- the one on disk right now. Old versions are gone the moment you overwrite a file.

**Mode is a per-repo configuration, not a separate product.** A repo can switch between lite and solid at any time via `.bit/config`. The mode controls a single behavior: whether `bit add` writes content into `.bit/cas/` alongside the metadata. See [Modes and CAS](modes-and-cas.md).

This creates a fundamental architectural constraint that Git and bit-solid don't have: **in lite mode, metadata can become hollow.** If a binary file is deleted, corrupted, or modified without running `bit add`, the metadata in `.bit/index/` claims something that is no longer true. In Git, this situation is impossible -- the object store is append-only and self-verifying. In bit-lite, it's the normal consequence of working with mutable files on a regular filesystem.

This constraint gives rise to the **proof of possession** rule (see [Verify and Repair](verify-repair.md)): when syncing with full-layout remotes, bit must verify that content matches metadata before transferring. Without this rule, hollow metadata propagates between repos, and the system's core value proposition -- knowing the true state of your files -- is undermined. The rule does not apply to bare-layout remotes (CAS blobs are self-verifying). Verification checks the working tree against committed metadata -- if a file is corrupted or missing, it is always reported as an issue. CAS serves as a repair source (via `bit repair`), not a verification shortcut.

### Origin

The key architectural idea: instead of a custom manifest, keep small text files in `.bit/index/` mirroring the working tree's directory structure, each containing just hash and size. Define Git's working tree as `.bit/index/`, and you get `add`, `commit`, `diff`, `log`, branching, and the entire Git command set for free. Git becomes the manifest manager, diff engine, and history store -- without ever seeing a large file.

### Comparison with Alternatives

bit occupies a different niche than git-lfs and git-annex:

- **git-lfs**: Stores pointer files in Git, actual files on a server. Server-dependent, transparent but limited. Pragmatic hack.
- **git-annex**: Extremely powerful distributed system with pluggable remotes and policy-driven placement. Extremely complex.
- **bit**: Minimal, explicit, correctness-oriented. Git never touches large files. Dumb remotes via rclone. Filesystem-first.

bit's killer feature is **clarity** -- users always know what state their files are in, what bit is about to do, and how to recover from errors.

## Design Philosophy: Stand on git and rclone

bit's architecture rests on one principle: **use git and rclone as primitives, never reimplement what they already do.**

Git is an extraordinarily capable metadata engine -- branching, merging, diffing, conflict detection, reflog, gc. rclone is a battle-tested file mover that speaks every cloud protocol. bit's job is to wire them together, not to compete with them.

In practice this means:
- **Git bundles** for serializing history over dumb cloud storage -- one file, full DAG, `git fetch` unpacks it. No custom wire format.
- **Git native remotes** for filesystem peers -- `git fetch /path/to/.bit/index` and `git merge`, just like two ordinary repos.
- **`git diff --name-status`** for deriving sync actions -- no remote file scan, no directory diff algorithm.
- **`rclone copy --files-from`** for bulk file transfer -- one subprocess call for all files, works identically for local and cloud destinations.
- **`git merge --no-commit --no-ff`** for three-way merge -- git handles the hard work, bit reads the result.

When you find yourself writing logic that git or rclone already handles, stop and wire into the existing tool instead. Every custom reimplementation is a surface area for bugs that git/rclone have already fixed.
