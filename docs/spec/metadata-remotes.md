# Metadata-Only Remotes

## Problem Statement

When a user wants to share project history and file metadata through a native git host (GitHub, GitLab, a bare git repo on a server or filesystem), they cannot currently do so — bit only understands remotes that can hold both metadata and content. But a native git endpoint has nowhere to store binary blobs or CAS objects. The user must choose between a full bit remote (which requires rclone-compatible storage) and no remote at all.

Metadata-only remotes solve this: they sync the `.bit/index` history — file names, hashes, sizes, commit messages, the full DAG — without transferring any content. The remote becomes a pure metadata mirror.

**Typical setup:**

```
bit remote add origin gdrive:Projects/foo          # full — content lives here
bit remote add github git@github.com:user/foo.git  # metadata only (auto-detected)
```

Push to `origin` syncs everything. Push to `github` syncs only history. Collaborators clone from GitHub to see the project structure and history, then add the Google Drive remote to get actual content.

---

## Layout: `metadata`

Metadata-only is a third layout alongside `full` and `bare`:

```
Layout      What the remote stores
──────────  ──────────────────────────────────────────────────
full        Metadata bundle + CAS blobs + human-readable files
bare        Metadata bundle + CAS blobs
metadata    Metadata only (bundle or native git objects)
```

The `layout: metadata` value is valid for any remote type — git, cloud, filesystem, or device.

---

## New Remote Type: `RemoteGit`

A new `RemoteType` value covers native git endpoints — hosts and repos that speak the git protocol but have no bit infrastructure:

```haskell
data RemoteType = RemoteFilesystem | RemoteDevice | RemoteCloud | RemoteGit
  deriving (Show, Eq)
```

`RemoteGit` remotes are always `layout: metadata`. They cannot hold content.

`isFilesystemType` returns `False` for `RemoteGit` — there is no bit repo at the remote to talk to via filesystem conventions.

---

## Auto-Detection

When the user runs `bit remote add <name> <url>`, bit classifies the URL. The existing `classifyRemotePath` is extended with git-native detection **before** the rclone/filesystem check:

```
Input URL                              Classification
─────────────────────────────────────  ────────────────────────────
git@github.com:user/foo.git            RemoteGit (SSH git host)
https://github.com/user/foo.git        RemoteGit (HTTPS git host)
ssh://git@server/repo.git              RemoteGit (SSH URL)
/path/to/repo.git   (bare, no .bit)    RemoteGit (filesystem bare git)
/path/to/repo/.git  (non-bare, no .bit) RemoteGit (filesystem git)
gdrive:Projects/foo                    RemoteCloud (rclone)
/path/to/repo       (has .bit/)        RemoteFilesystem (bit repo)
/path/to/repo       (new, no .bit)     RemoteFilesystem (will be initialized)
```

### Detection Rules

**URL-based detection** (no filesystem access needed):

1. URL ends with `.git` and is not a local path → `RemoteGit`.
2. URL matches known git host patterns (`git@*`, `ssh://git@*`, `https://github.com/*`, `https://gitlab.com/*`, `https://bitbucket.org/*`, etc.) → `RemoteGit`.
3. URL uses `ssh://` scheme → `RemoteGit`.

**Filesystem detection** (local/network paths only):

4. Path has `.bit/` directory → `RemoteFilesystem` (existing bit repo).
5. Path ends with `.git` or contains a bare git repo (`HEAD` file exists at path root) or has a `.git/` directory, **and** has no `.bit/` → `RemoteGit`.
6. Path does not exist or is empty → `RemoteFilesystem` (new remote, will be initialized by first push).

**Fallthrough**: existing `classifyRemotePath` logic (rclone check, etc.)

Rule 4 takes priority over rule 5: a directory that has **both** `.bit/` and `.git/` is a bit repo, not a pure git repo. This handles the case where someone manually runs `git init` alongside bit.

### Detection Messages

Auto-detection must be communicated to the user:

```
$ bit remote add github git@github.com:user/foo.git
Remote 'github' added (git@github.com:user/foo.git, metadata only).
```

```
$ bit remote add archive /mnt/server/project.git
Remote 'archive' added (/mnt/server/project.git, metadata only).
```

For non-git remotes where the user explicitly requests metadata-only:

```
$ bit remote add backup gdrive:Backup/foo --metadata-only
Remote 'backup' added (gdrive:Backup/foo, metadata only).
```

---

## Remote Config Format

### Git-native remote

```
# .bit/remotes/github
type: git
target: git@github.com:user/foo.git
layout: metadata
```

### Cloud metadata-only remote

```
# .bit/remotes/backup
type: cloud
target: gdrive:Backup/foo
layout: metadata
```

### Filesystem metadata-only remote

```
# .bit/remotes/archive
type: filesystem
layout: metadata
```

(Filesystem remotes store their URL only in git config, not in the remote file — same as existing filesystem remotes.)

---

## Git Remote Registration in `.bit/index`

All remotes are registered as named git remotes inside `.bit/index/.git`. Metadata-only remotes follow the same pattern, but what the git remote points to depends on remote type:

```
Type        Git remote URL in .bit/index/.git/config
──────────  ─────────────────────────────────────────────────────
git         The actual remote URL (git@github.com:user/foo.git)
cloud       .git/bundles/<sanitized-name>.bundle (existing pattern)
filesystem  /path/to/remote/.bit/index (existing pattern for bit
            repos) OR the bare git repo path directly (for
            metadata-only filesystem remotes)
device      Resolved device path + .bit/index (existing pattern)
```

For `RemoteGit` remotes, the git remote in `.bit/index` points directly at the native git endpoint. `git push`/`git fetch` inside `.bit/index` talk to GitHub (or wherever) with no intermediate bundle.

For `RemoteCloud` with `layout: metadata`, the existing bundle mechanism is used — rclone uploads/downloads `.bit/bit.bundle` on the remote, and the local git remote points to the local bundle file. Same as bare/full cloud, just without the file sync step.

For `RemoteFilesystem` with `layout: metadata` pointing at a bare git repo, the git remote in `.bit/index` points directly at that bare repo path. Git fetch/push work natively.

---

## Transport

### Summary Table

```
Type        Layout      Metadata transport              File sync
──────────  ──────────  ─────────────────────────────   ─────────
git         metadata    git push/fetch (native)         None
cloud       metadata    Bundle via rclone               None
cloud       bare        Bundle via rclone               CAS only
cloud       full        Bundle via rclone               CAS + readable
filesystem  metadata    git push/fetch (native)         None
filesystem  full        git fetch + ff-only merge       Working tree
device      metadata    git push/fetch (native)         None
device      full        git fetch + ff-only merge       Working tree
```

### Git Transport Seam

A new seam implementation handles `RemoteGit` remotes (and filesystem/device metadata-only remotes):

```haskell
mkGitSeam :: Remote -> PushSeam
mkGitSeam remote = PushSeam
    { ptFetchHistory = do
        -- git fetch <name> inside .bit/index
        code <- Git.fetchRemote (remoteName remote)
        mHash <- Git.getRemoteTrackingHash (remoteName remote)
        pure mHash
    , ptPushMetadata = do
        -- git push <name> main inside .bit/index
        void $ Git.pushRemote (remoteName remote)
    }

mkGitPullSeam :: Remote -> PullSeam
mkGitPullSeam remote = PullSeam
    { psFetchMetadata = do
        code <- Git.fetchRemote (remoteName remote)
        pure (code == ExitSuccess)
    , psVerifyRemote = \_ -> pure ()  -- Nothing to verify
    }
```

For cloud metadata-only remotes, the existing `mkCloudSeam` / `mkCloudPullSeam` are reused — only the file sync step is skipped.

### Seam Selection

The seam is chosen based on remote type and layout:

```haskell
selectPushSeam :: Remote -> RemoteType -> RemoteLayout -> PushSeam
selectPushSeam remote remoteType layout = case (remoteType, layout) of
    (RemoteGit, _)               -> mkGitSeam remote
    (_, LayoutMetadata)          -> case remoteType of
        RemoteCloud              -> mkCloudSeam remote  -- bundle, no file sync
        _                        -> mkGitSeam remote    -- native git
    (RemoteCloud, _)             -> mkCloudSeam remote
    _                            -> mkFilesystemSeam remote
```

---

## Push Flow

Push to a metadata-only remote:

1. **Skip verification.** No content is transferred, so proof of possession does not apply.
2. **Skip file sync entirely.** `syncRemoteFiles` is not called.
3. **Push metadata** via the appropriate transport:
    - `RemoteGit`: `git push <name> main` inside `.bit/index`.
    - Cloud `layout: metadata`: Upload bundle via rclone, skip file sync.
    - Filesystem/device `layout: metadata`: `git push` to the remote's git repo.
4. **Update tracking ref.** `refs/remotes/<n>/main` updated to HEAD.

### Behavior Imitates Git

Push operates on committed state only. Uncommitted changes in the working directory are irrelevant — `git push` pushes commits, not the working tree. No special "clean state" check is needed beyond what git itself enforces.

If the remote is ahead (non-fast-forward), push fails with the same ancestry error as any other remote. `--force` overrides, same as existing behavior.

---

## Pull Flow

Pull from a metadata-only remote:

1. **Fetch metadata** via the appropriate transport:
    - `RemoteGit`: `git fetch <name>` inside `.bit/index`.
    - Cloud `layout: metadata`: Download bundle via rclone, `git fetch` from bundle.
    - Filesystem/device `layout: metadata`: `git fetch` from remote git repo.
2. **Merge/checkout** to update `.bit/index/`. Same merge logic as any other pull — fast-forward, three-way merge, conflict resolution all apply.
3. **Update tracking ref.**
4. **Stop.** No `applyMergeToWorkingDir`. No file download. The working directory is untouched.

After pulling from a metadata-only remote, the local repo has updated metadata — `bit log` shows the new history, `bit status` reflects the merged state — but no binary content is fetched. If the user wants actual files, they pull from a content-bearing remote (`full` or `bare` layout).

### Conflict Resolution

Merge conflicts are resolved in `.bit/index` the same way as any pull. Since metadata files are either text content (for text files) or hash/size pointers (for binary files), conflicts are handled by the existing interactive conflict resolver. The working directory is not affected.

---

## Proof of Possession

Not applicable for metadata-only remotes. No content is transferred in either direction, so there is nothing to verify.

```
Layout      Push verification        Pull verification
──────────  ────────────────────     ──────────────────────
full        Verify local content     Verify remote content
bare        Not needed (CAS)         Not needed (CAS)
metadata    Not needed (no content)  Not needed (no content)
```

---

## Remote State Classification

The existing `classifyRemoteState` (depth-2 rclone listing) does not apply to `RemoteGit` remotes — there is no rclone listing for a GitHub repo.

For `RemoteGit`:

- **First push detection**: `git push` will fail or succeed based on whether the remote repo exists and has history. Use git's native error reporting.
- **Empty remote**: `git ls-remote` returns no refs → remote is empty, first push proceeds.
- **Non-empty remote**: `git ls-remote` returns refs → ancestry check applies.

For cloud `layout: metadata`, `classifyRemoteState` still runs (it checks for `.bit/bit.bundle`), but the result only affects metadata operations — no file-related states matter.

---

## CLI

### `bit remote add`

```bash
# Auto-detected as metadata-only (git host):
bit remote add github git@github.com:user/foo.git
# → Remote 'github' added (git@github.com:user/foo.git, metadata only).

# Auto-detected as metadata-only (bare git repo on filesystem):
bit remote add archive /mnt/server/project.git
# → Remote 'archive' added (/mnt/server/project.git, metadata only).

# Explicit metadata-only for cloud:
bit remote add backup gdrive:Backup/foo --metadata-only
# → Remote 'backup' added (gdrive:Backup/foo, metadata only).

# Explicit metadata-only for filesystem (bit repo that you only want metadata from):
bit remote add ref /mnt/nas/project --metadata-only
# → Remote 'ref' added (/mnt/nas/project, metadata only).
```

**`--metadata-only` flag**: Explicitly sets `layout: metadata` for any remote type. This is the only way to get metadata-only on a cloud or existing-bit-repo remote (auto-detection doesn't apply — those look like normal cloud/filesystem remotes).

**`--bare` and `--metadata-only` are mutually exclusive.** Both set the layout; specifying both is an error:

```
$ bit remote add x gdrive:foo --bare --metadata-only
error: --bare and --metadata-only are mutually exclusive.
```

**`--metadata-only` on a git-detected remote is redundant but accepted silently** — it's already metadata-only by detection.

### `bit remote show`

```
$ bit remote show github
* remote github
  Fetch URL: git@github.com:user/foo.git
  Push  URL: git@github.com:user/foo.git
  Type: git
  Layout: metadata

$ bit remote show backup
* remote backup
  Fetch URL: gdrive:Backup/foo
  Push  URL: gdrive:Backup/foo
  Type: cloud
  Layout: metadata
```

### `bit remote -v`

```
$ bit remote -v
origin → gdrive:Projects/foo (cloud, Layout: full)
github → git@github.com:user/foo.git (git, metadata only)
backup → gdrive:Backup/foo (cloud, metadata only)
```

---

## `RemoteLayout` Extension

```haskell
data RemoteLayout = LayoutFull | LayoutBare | LayoutMetadata
  deriving (Show, Eq)
```

`readRemoteLayout` is extended:

```haskell
case getVal "layout: " of
    Just "bare"     -> pure LayoutBare
    Just "metadata" -> pure LayoutMetadata
    _               -> pure LayoutFull
```

`writeRemoteFile` is extended:

```haskell
layoutLine = case mLayout of
    Just LayoutBare     -> "\nlayout: bare"
    Just LayoutMetadata -> "\nlayout: metadata"
    _                   -> "\nlayout: full"
```

For `RemoteGit`, `writeRemoteFile` always writes `layout: metadata` regardless of user input.

---

## `isFilesystemType` Extension

```haskell
isFilesystemType :: RemoteType -> Bool
isFilesystemType RemoteCloud = False
isFilesystemType RemoteGit   = False
isFilesystemType _           = True
```

`RemoteGit` returns `False` because there is no bit repo at the remote — no `.bit/index` to `git fetch` from, no filesystem seam. The git transport seam handles it instead.

---

## Layout Immutability

Same rule as full/bare: **layout is set at creation time and cannot be changed.** There is no conversion between metadata, bare, and full. To change layout, remove the remote and add a new one.

---

## `--remote` (Remote-Targeted Commands)

Remote-targeted commands (`bit --remote <n> add .`, `bit --remote <n> status`, etc.) are **not supported for metadata-only remotes.** These commands operate on an ephemeral workspace built from the remote's content — a metadata-only remote has no content to scan.

```
$ bit --remote github add .
error: Remote 'github' is metadata-only. Remote-targeted commands require
a remote with content (layout: full or bare).
```

---

## Multi-Remote Workflows

### Content + Metadata Mirror

```bash
bit init
bit remote add origin gdrive:Projects/foo
bit remote add github git@github.com:user/foo.git

# Work locally...
bit add .
bit commit -m "Initial project"

# Push content to Google Drive:
bit push origin

# Push metadata to GitHub:
bit push github
```

### Collaborator Workflow

```bash
# Collaborator starts from GitHub (metadata only):
bit init
bit remote add github git@github.com:user/foo.git
bit pull github
# → Local repo now has full history and file metadata, but no actual files.

# Add the content remote:
bit remote add origin gdrive:Projects/foo
bit pull origin
# → Now local repo has actual files too.
```

### Metadata-Only Cloud Backup

```bash
# Cheap metadata backup (no storage costs for large files):
bit remote add meta-backup gdrive:MetaBackup/foo --metadata-only
bit push meta-backup
# → Only the bundle is uploaded. No CAS, no file sync.
```

---

## What We Deliberately Do NOT Do

- **No content fallback on pull.** Pulling from a metadata-only remote does not attempt to find content from other remotes. If the user wants content, they pull from a content remote explicitly. Automatic multi-remote content resolution would add complexity with unclear semantics.
    
- **No partial metadata sync.** Metadata-only means the full history. There is no "sync metadata for some files but not others." The `.bit/index` is a git repo; it syncs as a unit.
    
- **No layout conversion.** Same as full↔bare: no conversion to/from metadata. Create a new remote.
    
- **No content operations on metadata-only remotes.** `bit verify <remote>`, `bit repair <remote>`, `bit --remote <n> add .` — all operations that touch content — are rejected for metadata-only remotes with a clear error message.