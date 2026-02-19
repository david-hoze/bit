# `bit`

**Version control for binary files.** Git's super-brain, without its baggage.

`bit` splits the job in two: **Git** tracks tiny metadata files (hash + size), **rclone** moves actual files. Your working directory stays completely normal — no pointer files, no special filters, no server to run. You get commits, history, diffs, branches, and merges for free, and any storage backend works out of the box.

```bash
bit init
bit add .
bit commit -m "Add raw footage"
bit remote add origin gdrive:Projects/footage
bit push
```

---

## Why `bit`?

The binary file versioning landscape is fragmented because every existing tool makes trade-offs that hurt in practice. bit is designed around the gaps they leave. It's features:

- Full Git compatibility — branches, merges, PRs, the entire workflow.
- No server required — any folder rclone can reach is a valid remote.
- Sub-file delta transfer — only changed chunks get uploaded, not entire files.
- Multi-remote by design — cloud, USB, NAS, and Git hosts in one repo.
- Flexible history — switch between lightweight tracking and full binary versioning per-repo.

### It's just like Git

`bit` does this naturally — *without* losing Git's simplicity, syntax, or semantics. It [integrates](docs/git-compatibility.md) fully with git, creating a seamless experience with binary files:

```bash
bit become-git

cd my-bit-repo
git add footage.mp4      # handled by bit — hashes and tracks the file
git commit -m "new cut"  # handled by bit — commits metadata
git push                 # handled by bit — syncs binaries via rclone, then pushes metadata
git diff file.txt        # passes all the handling to git, with full conformance
git add file.txt         # Normal git adding
git commit -m "Updated text"
git push                 # Normal git behavior

cd my-git-repo
git status               # handled by real git — bit stays out of the way
```

`bit` actually uses Git under the hood, but only for text. When `bit` needs to passthrough commands to git, it does, and when it has to meddle, it does so at minimum. `bit`'s design philosophy is  "Orchestrate, don't reimplement". `bit` always prefers using git or rclone primitives and machinery whenever possible.

`bit` targets full compatibility with Git's CLI, including its test suite. That means you can use it in scripts, CI pipelines, and existing Git-based workflows without modification — if it works with `git`, it works with `bit`.

### Push & Pull

Push and pull behave differently for binary files — but they should. `git push` in a bit repo uploads your binaries to the remote via rclone, which is what you'd expect, `bit pull` downloads them.`bit` does this using the minimal set of operations (renames and move detection), uploading **only** the data chunks that changed using FastCDC, thus cutting upload expenses.

You can use Git native remotes seamlessly. They are considered metadata-only remotes. You can push your metadata to GitHub or GitLab as a standard Git repository, and get full commit history, diffs, branches, PRs, while your actual files live on cheaper storage:

```bash
bit remote add github git@github.com:user/project.git  # metadata only
bit remote add storage gdrive:Projects/footage          # content + metadata
bit push
```

Your collaborators can then see the full Git history on GitHub. They can clone the metadata, and `bit pull storage` (or `git pull storage` with `bit become-git`) fetches the actual files from wherever you stored them.

### Your files stay out of Git's object store

git-lfs replaces your actual files in Git's working tree with pointer files containing a hash and size. Your working directory has the real file (via smudge/clean filters), but Git itself tracks pointer content. Because LFS lives inside Git's ecosystem, it's easy to end up with binaries in Git's object store: forget to configure `.gitattributes` before committing, add LFS to an existing repo, or have a teammate clone without LFS installed. Once a binary blob is in Git history, removing it means rewriting history — that's not an LFS problem, but the architecture makes it easy to get there.

bit keeps a *separate* Git working tree (`.bit/index/`) for metadata only. Your actual files live in your normal working directory, in a different directory from Git's. Git never sees your files — not as blobs, not as pointers, not at all. It just sees a small text metadata representation of them. There's no configuration to forget, no filter to misconfigure, no way for a binary to accidentally become a Git object. The separation is structural, not policy-based.

```
lectures/
├── lecture-01.mp3          ← your actual 80MB file
├── lecture-02.mp3          ← your actual file
└── .bit/
    └── index/              ← this is the Git working tree
        ├── lecture-01.mp3  ← 2 lines: hash + size
        └── lecture-02.mp3  ← 2 lines: hash + size
```

DVC takes a similar approach (`.dvc` sidecar files tracked by Git), but adds an entire ML pipeline framework on top. If you just need binary versioning, that's a lot of weight.

### No special server. No vendor lock-in.

git-lfs requires a server implementing its Batch API — GitHub, GitLab, or a self-hosted server like Rudolfs or Giftless. Perforce requires a `p4d` server (free for ≤5 users, ~$39/user/month beyond that). Both create infrastructure dependencies and, in LFS's case, bandwidth bills that can reach thousands of dollars monthly from CI alone.

`bit` needs only a folder. Any rclone backend works as a remote — Google Drive, S3, Backblaze B2, SFTP, a USB drive, a network share. No server process, no API, no per-seat licensing. Your files live wherever you put them.

### Small edits don't mean big uploads

git-lfs has no delta compression. Change one byte in a 500MB file, and LFS re-uploads the entire 500MB. DVC and git-annex have the same problem — every modification transfers the full file. Only Perforce and SVN do sub-file binary deltas, and both require dedicated servers.

bit uses **content-defined chunking** (FastCDC) on bare-layout remotes. Instead of storing whole files, bit splits binaries into variable-size chunks based on content. Each chunk is stored in the CAS by its hash. When a file changes, most chunks stay identical — only the changed regions produce new chunks, and only those get uploaded:

```
# First push: 500MB file → ~7,600 chunks uploaded
$ bit push
Uploading 7,634 chunk(s)...done.

# After a small edit: only the affected chunks
$ bit push
Uploading 3 chunk(s)...done.
```

This works on dumb storage — no server-side process needed. rclone copies the new chunks to Google Drive, S3, or wherever your remote lives, the same way it copies any other file. And because chunks are content-addressed, identical regions across *different* files are stored only once.

### Renames are free, not re-uploads

Rename a 10GB file with rclone or rsync and you get a delete + full re-upload. Cloud sync tools do the same. git-lfs handles renames correctly (same content hash, no re-upload), but only within its server ecosystem.

`bit` detects renames by content hash *before* transfer and executes server-side moves via rclone. Rename, reorganize, restructure — `bit` computes the minimal set of operations and does only what's necessary.

### You always know where you stand

Cloud sync tools can't answer "is my backup complete?" or "which copy is current?" rclone and rsync have no memory between runs — they can't tell you what changed or warn about conflicts. git-lfs relies on Git's status, which doesn't cover the actual content.

bit's core value is **clarity**. `bit status`, `bit verify`, `bit --remote origin verify`, and `bit fsck` give you complete visibility into local state, remote state, and the integrity of both. You never have to wonder.

### Multi-remote is a first-class feature

git-lfs added experimental multi-remote support in v3.3.0 (2022) — it remains limited. DVC supports multiple remotes but only one is "default." Perforce uses proxy/edge servers, which require enterprise infrastructure. Cloud sync tools lock you to one provider.

bit treats every remote as equal. Push to Google Drive for everyday sync, push to a USB drive for offline backup, push metadata to GitHub for collaboration — all in the same repo, all tracked independently:

```bash
bit remote add origin gdrive:Projects/foo          # content + metadata
bit remote add backup /mnt/usb/backup              # content + metadata
bit remote add github git@github.com:user/foo.git  # metadata only
```

### USB drives just work

`bit` identifies devices by UUID, not mount point. Plug your drive into a different port, a different machine, a different OS — bit finds it. No other tool in this space handles removable media as a first-class remote.

### git-annex power without git-annex complexity

git-annex is the closest tool in ambition: distributed storage, pluggable remotes, content-addressed integrity. It's genuinely powerful. But its learning curve is steep (50+ subcommands, preferred content expressions, trust levels, adjusted branches), Windows support remains beta-quality, and it's maintained by a single developer.

bit shares the philosophy — distributed, storage-agnostic, integrity-first — but targets the common case: "push my files, know they're safe, sync when I want to." If you need policy-driven file placement across dozens of remotes, use git-annex. If you need to manage copies of your project across a laptop, a NAS, and a cloud drive without a PhD in version control, use bit.

### Full binary history — without the storage explosion

Game developers need to roll back a broken `.uasset` to last Tuesday's build. ML teams need to reproduce a training run with the exact dataset version from three months ago. Media producers need to recover the pre-edit cut. Binary history matters — the question is what it costs.

git-lfs and git-annex store every version as a full copy. For a 50GB game project that changes daily, that's terabytes of accumulated history. Perforce handles this with server-side binary deltas — efficient, but it requires a dedicated server to compute and apply them.

bit-solid stores binary history with **content-defined chunking** — only the changed regions of each version are stored, not full copies. Ten versions of a 500MB file that changes incrementally might cost 510MB of storage, not 5GB. Unlike Perforce's deltas, bit's chunks are independently content-addressed: no server needed, and identical regions across *different* files or *different versions* are stored only once.

bit-solid gives you everything you'd expect from a version control system with full binary history: restore any file to any committed version, branch and merge binary assets alongside your Git-tracked metadata, and diff to see exactly which files changed between any two commits.

### You choose how much history to keep — and can change your mind

bit-lite and bit-solid aren't separate products. They're a per-repo configuration toggle:

```bash
bit config core.mode solid    # start storing binary history
bit config core.mode lite     # stop (existing history is preserved)
```

Start with bit-lite when you just need to track what's where and sync across machines. Switch to bit-solid when the project matures and binary history becomes valuable. Switch back if storage gets tight — existing CAS data is preserved, and `bit cas backfill` can retroactively store any version whose files are still in the working tree.

This is a genuine differentiator. git-lfs stores every version from the start or not at all. Perforce has no lightweight mode. bit lets you make the trade-off *per-repo, at any point*, without migration or re-cloning.

### Why not just fix Git?

A fair question. Git already has binary delta compression in its pack files (xdelta), partial clone can filter large blobs, and the Large Object Promisors initiative had pieces merged into mainline Git in March 2025. Why build a separate tool instead of improving Git or forking it?

**Git's object model is the wrong foundation for large files.** Git's core model assumes every object lives in a content-addressed DAG that every clone can access. Partial clone (since v2.24) works around this by lazy-loading blobs on demand, so you don't have to download everything upfront. But the workaround has real costs: lazy-loaded blobs arrive one at a time with no delta compression, making commands like `git blame` and `git log -p` painfully slow on large files. And partial clone still requires a smart server speaking the Git protocol — you can't use it with a folder on Google Drive or a USB stick. The object model has been patched, not redesigned.

**Git's remotes must speak the Git protocol.** Even promisor remotes need a process on the other end that understands Git's pack protocol. You can't `git push` to Google Drive. You can't `git fetch` from a USB stick formatted as FAT32. bit's entire value proposition is dumb-storage remotes — any folder rclone can reach. That's a fundamentally different bet about where files should live, and it's not one Git can make without becoming a different tool.

**Forking Git means maintaining Git.** Git is ~400,000 lines of C. A fork means tracking upstream security patches, maintaining compatibility with GitHub/GitLab/Bitbucket, and diverging from a codebase that the entire industry depends on. bit's core is a few thousand lines of Haskell that *calls* Git — if Git improves, bit benefits automatically. A fork competes with Git; `bit` stands on it.

**rclone already exists.** Git would need to reimplement multi-cloud file transfer to support S3, Google Drive, Backblaze B2, SFTP, WebDAV, and 70+ other backends. rclone already does this, battle-tested, with millions of users. bit wires Git and rclone together. A Git fork would need to absorb one or rebuild the other.

`bit`'s design bet is that **orchestration beats reimplementation**. Git is an extraordinarily good metadata engine. rclone is an extraordinarily good file mover. Connecting them is a small, auditable codebase. Replacing either is years of work against a moving target. bit chose to be the thin, smart layer between two proven tools — not a third tool trying to do everything.

### Comparison at a glance

| | **bit** | **git-lfs** | **git-annex** | **DVC** | **Perforce** | **rclone** | **Dropbox** |
|---|---|---|---|---|---|---|---|
| Git sees large files | Never | Pointer files | Symlinks/pointers | `.dvc` files | N/A (own VCS) | N/A | N/A |
| Full Git compliance | ✓ (is Git) | Partial (filters) | Partial (symlinks) | Separate CLI | No | No | No |
| GitHub/GitLab PRs | ✓ (metadata remotes) | ✓ | Partial | ✗ | ✗ | ✗ | ✗ |
| Special server needed | No | Yes (LFS API) | No | No | Yes (p4d) | No | Yes (own) |
| Sub-file delta transfer | ✓ (CDC) | ✗ | ✗ | ✗ | ✓ (native) | rsync only | Dropbox only |
| Rename = re-upload | No | No | No | Partial | No | Yes | Yes |
| Integrity verification | ✓ (local + remote) | ✓ (local only) | ✓ (local + remote) | ✓ (local) | Server-managed | ✗ | ✗ |
| Multi-remote | ✓ (first-class) | Experimental | ✓ (complex) | ✓ (one default) | Proxy/edge | ✓ (no versioning) | ✗ |
| USB/device-aware | ✓ (by UUID) | ✗ | ✗ | ✗ | ✗ | ✗ | ✗ |
| Conflict detection | ✓ (interactive) | Via Git | ✓ (keeps both) | Via Git | ✓ (file locking) | ✗ | Conflicted copies |
| Learning curve | Low | Low-medium | High | Medium | Medium-high | Low | Minimal |
| Cost at scale | Free | High (bandwidth) | Free (AGPL) | Free (Apache) | $39+/user/mo | Free | $10+/user/mo |
| Flexible history (on/off) | ✓ (lite↔solid) | ✗ | Configurable | ✗ | ✗ | N/A | N/A |
| Cross-file dedup | ✓ (CDC) | ✗ | ✗ | ✗ | ✗ | ✗ | ✗ |

---

## Quick start

```bash
# Initialize a new bit repository
bit init

# Add files (computes hashes, creates metadata)
bit add .

# Commit
bit commit -m "Add raw footage"

# Set up a remote (any rclone-supported backend, local/USB path, or git host)
bit remote add origin gdrive:Projects/footage
# or: bit remote add backup E:\Backup
# or: bit remote add github git@github.com:user/repo.git  # metadata only

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
bit --remote origin verify     # Do remote files match metadata?
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

Network shares work the same way. Under Git Bash / MINGW, use forward slashes for UNC paths (the shell mangles backslashes before bit sees them):

```bash
bit remote add nas //server/share/project
# bit normalizes and displays: \\server\share\project
```

---

## bit for everyday use

You don't need to be a developer to use bit. If you've ever struggled with keeping files in sync across devices, bit solves that.

You've got a folder of mp3 lectures. Or video projects. Or a dataset you've been building for months. You put it on Dropbox, maybe copy it to a USB drive too. Life is good.

Then one day you notice a file is missing. Did you delete it? Did Dropbox delete it? Was it the laptop or the desktop? You're not sure, so you check your USB backup — but wait, is that copy even up to date? You copied it... last month? Two months ago? Which direction did you sync last time?

You find a file called `lecture-07 (Conflicted Copy 2025-02-03).mp3`. Great. Which one is real?

bit answers these questions:

```
$ bit status
On branch main
Your branch is ahead of 'origin/main' by 2 commits.

$ bit verify
Verifying 847 files...
[OK] All files match metadata.

$ bit --remote origin verify
Verifying 847 files...
[ERROR] Missing: lectures/lecture-07.mp3
1 issue found.
```

No more guessing. bit-lite is all most people need — it tracks the *state* of each copy without storing old versions. Think of it as a flight controller for your data:

- "Your USB drive is 3 commits behind your laptop — you should push."
- "The Google Drive copy is missing 2 files — something went wrong."
- "Your NAS has everything. Your laptop matches. You're good."

When you outgrow that and want full history, `bit config core.mode solid` turns on binary versioning without changing your workflow.

---

## When to use bit

✓ Large binary files — video, audio, datasets, models, game assets
✓ Multiple copies across machines, drives, and cloud storage
✓ "Which copy is current?" — you need the answer, fast
✓ Teams sharing files via cloud storage or shared drives
✓ USB-drive workflows where drive letters change
✓ Binary history without terabytes of storage (bit-solid)
✓ GitHub/GitLab PRs and collaboration (via metadata-only remotes)

## When NOT to use bit

✗ Source code — just use Git

---

## Install

```bash
# From source (requires GHC + cabal)
git clone https://github.com/david-hoze/bit.git
cd bit
cabal install
```

---

## Status

`bit` is under active development. The core workflow — init, add, commit, push, pull, merge, verify, fsck — works today. Both bit-lite (metadata tracking) and bit-solid (content-addressed binary history) are operational, with content-defined chunking for bandwidth-efficient sync.

`bit` is currently fully tested against Google Drive; other backends are supported through rclone but not yet verified.

For the full design, see [docs/spec.md](docs/spec.md). Check out the [tutorials](docs/tutorials) folder.

## License

MIT