# bit

**Version control for binary files.** Git's brain, none of its baggage.

---

## The problem you already have

You've got a folder of mp3 lectures. Or video projects. Or a dataset you've been building for months. You put it on Dropbox, maybe copy it to a USB drive too. Life is good.

Then one day you notice a file is missing. Did you delete it? Did Dropbox delete it? Was it the laptop or the desktop? You're not sure, so you check your USB backup — but wait, is that copy even up to date? You copied it... last month? Two months ago? Which direction did you sync last time?

You find a file called `lecture-07 (Conflicted Copy 2025-02-03).mp3`. Great. Which one is real?

**This is the problem bit solves.** Not by storing yet another copy of your files, but by *remembering what's supposed to be where.* bit keeps a tiny, lightweight record of every file's identity — its hash, its size, its path — and tracks that record with the same rigor Git uses for source code.

The result: you always know the answer to "which copy is current?", "is anything missing?", and "what actually changed?"

```
$ bit status
On branch main
Your branch is ahead of 'origin/main' by 2 commits.

$ bit verify
Verifying 847 files...
[OK] All files match metadata.

$ bit verify --remote
Verifying 847 files...
[ERROR] Missing: lectures/lecture-07.mp3
1 issue found.
```

No more guessing.

---

## What bit actually does

bit splits the problem in two:

- **Git** tracks tiny metadata files (hash + size) — so you get commits, history, diffs, and merges for free.
- **rclone** moves actual files — so any storage backend works out of the box.

Your working directory stays completely normal. No pointer files, no special filters, no server to run. bit keeps a hidden `.bit/` directory with metadata that mirrors your file tree:

```
lectures/
├── lecture-01.mp3          ← your actual 80MB file
├── lecture-02.mp3          ← your actual file
└── .bit/
    └── index/
        ├── lecture-01.mp3  ← 2 lines: hash + size
        └── lecture-02.mp3  ← 2 lines: hash + size
```

When you `push` or `pull`, bit computes the *minimal* set of operations needed — copy, move, delete — and executes them. Rename a 4GB file? bit does a server-side move, not a re-upload. Nothing changed? bit does nothing.

---

## bit-lite and bit-solid

bit comes in two flavors, designed for different needs:

### bit-lite (available now)

**Small footprint. No binary history. Full awareness.**

bit-lite doesn't store old versions of your files — it tracks the *state* of each copy. Think of it as a flight controller for your data:

- "Your USB drive is 3 commits behind your laptop — you should push."
- "The Google Drive copy is missing 2 files — something went wrong."
- "Your NAS has everything. Your laptop matches. You're good."

This is for the common case: you want to keep track of multiple copies, catch corruption early, sync intentionally, and save space. You don't need to go back in time — you just need to know where you stand *right now.*

### bit-solid (planned)

**Full history. Content-addressed storage. Time travel.**

bit-solid adds a content-addressed store (CAS) so you can go back to any previous version of any file. It also integrates with Git: when you `git checkout` an old commit, bit-solid checks out the matching binary files automatically.

This is for teams and workflows where binary history matters — game development, ML experiments, media production. Roll back your code *and* your assets to any point in time.

bit-solid is under design. bit-lite is usable today.

---

## Why not just use...

### Dropbox / Google Drive / OneDrive

Cloud sync tools are great for convenience, terrible for control:

- **No history you can reason about.** Dropbox has "version history," but try answering "which 3 files changed between Tuesday and Thursday?" Good luck.
- **Conflicted copies.** The dreaded `(Conflicted Copy)` files that pile up and nobody knows which to keep.
- **Silent deletion.** A file disappears and you don't know if you deleted it, another device deleted it, or sync ate it.
- **No integrity checking.** Is your Dropbox copy complete? Is it corrupted? Dropbox doesn't know and can't tell you.
- **Single remote.** Your files live on Dropbox's servers. Want a second copy on a USB drive? That's on you to manage manually.

bit gives you Dropbox-level ease (`bit push`, `bit pull`) with Git-level awareness (commits, diffs, verification) and total freedom over where your files live.

### git-lfs

git-lfs stores pointer files in Git and actual content on a dedicated LFS server. It works, but:

- **Requires a special server** (GitHub LFS, GitLab LFS, or self-hosted). No support for "just a folder."
- **Re-uploads on rename.** Rename a 10GB file and push — LFS uploads the entire file again.
- **Bloats Git history.** Pointer files are small, but LFS still struggles with very large repos.

### git-annex

git-annex is genuinely powerful — distributed storage, pluggable remotes, policy-driven file placement. But:

- **Massive learning curve.** Preferred content expressions, numcopies, trust levels, location tracking — it rivals Git itself in complexity.
- **Designed for a different audience.** git-annex excels at managing data across dozens of remotes with sophisticated placement policies. If that's what you need, use it. If you just want "push my files to a drive and know they're safe," it's overkill.

### rclone / rsync

Stateless sync tools. Powerful, but no memory:

- **Can't detect renames** — a renamed 4GB file becomes a delete + re-upload.
- **No conflict detection** — if the remote changed, rclone doesn't warn you.
- **No history** — "what was here last week?" is unanswerable.

bit actually *uses* rclone under the hood — but adds the memory layer that makes sync safe and intentional.

### Backup tools (borg, restic, duplicacy)

Designed for append-only snapshots, not bidirectional sync:

- **Can't tell you "this file moved"** — they just snapshot everything.
- **Can't sync two machines** — they back up, not collaborate.
- **Opaque storage** — you can't just browse your backup like a folder.

### Comparison at a glance

| | **bit** | **Dropbox** | **git-lfs** | **git-annex** | **rclone** | **borg** |
|---|---|---|---|---|---|---|
| Tracks renames efficiently | ✓ | ✗ | ✗ | ✓ | ✗ | ✗ |
| Works with any remote | ✓ | ✗ (own servers) | ✗ (LFS server) | ✓ | ✓ | ✗ (own format) |
| Commits & history | ✓ | Limited | ✓ | ✓ | ✗ | ✗ (opaque) |
| Conflict detection | ✓ (interactive) | Partial (conflicted copies) | ✓ (Git merge) | ✓ (keeps both) | ✗ | ✗ |
| Integrity verification | ✓ | ✗ | ✗ | ✓ | ✗ | ✓ |
| No special server | ✓ | ✗ | ✗ | ✓ | ✓ | ✓ |
| Git-style CLI | ✓ | N/A | ✓ | Partial | N/A | N/A |
| Lightweight (no binary history) | ✓ (bit-lite) | N/A | ✗ | Configurable | N/A | ✗ |

---

## Quick start

```bash
# Initialize a new bit repository
bit init

# Add files (computes hashes, creates metadata)
bit add .

# Commit
bit commit -m "Add raw footage"

# Set up a remote (any rclone-supported backend, or a local/USB path)
bit remote add origin gdrive:Projects/footage
# or: bit remote add backup E:\Backup

# Push files to remote
bit push

# On another machine:
bit init
bit remote add origin gdrive:Projects/footage
bit pull
```

### Checking your state

```bash
bit status                    # What's changed?
bit log                       # Commit history
bit verify                    # Do local files match metadata?
bit verify --remote           # Do remote files match metadata?
bit fsck                      # Full integrity check
bit remote check              # Compare local vs remote file-by-file
```

### Resolving conflicts

When two copies diverge, bit tells you exactly what happened and lets you choose:

```
$ bit pull
CONFLICT (content): Merge conflict in index/project.bin
Conflict [1/2]: project.bin
  Local:  md5:a1b2c3d4e5f6... (4.2 GB)
  Remote: md5:d4e5f6a7b8c9... (4.1 GB)
  Use (l)ocal or (r)emote version? l

Merge complete. 2 conflict(s) resolved.
```

Or skip the questions: `bit pull --accept-remote` to take whatever the remote has.

### USB drives and network shares

bit identifies devices by UUID, not drive letter. Plug your USB drive into a different port, a different machine — bit still finds it:

```bash
bit remote add backup E:\Backup
# bit: Device 'My_Passport' registered (physical).
# Remote 'backup' → My_Passport:Backup

# Later, on a different machine where the drive is F:
bit push    # Just works — bit finds My_Passport on F:
```

---

## How it compares to Git

bit mirrors Git's CLI, but the internals are fundamentally different:

| Git | bit |
|---|---|
| Stores file content in `.git/objects/` | Stores 2-line metadata in `.bit/index/` |
| `git push` uploads pack files to a Git server | `bit push` copies files via rclone to any backend |
| Renames detected heuristically after the fact | Renames detected by content hash before transfer |
| Large files bloat the repo permanently | Large files never enter Git |
| Requires a Git-compatible server | Requires only a folder |

---

## Install

```bash
# From source (requires GHC + cabal)
git clone https://github.com/yourorg/bit.git
cd bit
cabal install
```

---

## When to use bit

✓ Large binary files — video, audio, datasets, models, game assets
✓ Multiple copies across machines, drives, and cloud storage
✓ "Which copy is current?" — you need the answer, fast
✓ Teams sharing files via cloud storage or shared drives
✓ USB-drive workflows where drive letters change

## When NOT to use bit

✗ Source code — just use Git
✗ You need sub-file diffing (bit tracks whole files, not bytes within them)
✗ You need GitHub PRs / CI integration — bit doesn't integrate with forges (yet)
✗ You need full binary history today — wait for bit-solid

---

## Architecture (for contributors)

```
bit add / commit / status / log   →   Git (metadata only)
bit push / pull                   →   rclone (file transfer)
bit verify / fsck                 →   both
```

The sync pipeline is pure and testable:

```
scan → diff → plan → execute
       pure   pure   IO
```

`diff` computes what changed. `plan` maps each change to a concrete rclone operation. No heuristics, no state machines — just data in, actions out.

For the full design, see [docs/spec.md](docs/spec.md).

---

## Status

bit-lite is under active development. The core workflow — init, add, commit, push, pull, merge, verify, fsck — works today. Cloud remotes (via rclone) and filesystem remotes (USB drives, NAS, local directories) are both supported.

bit-solid (content-addressed storage, binary history, Git integration) is in the design phase.

## License

MIT