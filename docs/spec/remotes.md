# Remotes

## Remote Types and Device Resolution

bit supports four kinds of remotes:

- **Cloud remotes**: rclone-based (e.g., `gdrive:Projects/foo`). Identified by URL. Uses bundle + rclone sync.
- **Filesystem remotes**: Local/network paths (USB drives, network shares, local directories). Creates a **full bit repository** at the remote location.
- **Device remotes**: Removable/network drives identified by UUID + hardware serial (survives drive letter changes).
- **Git remotes**: Native git endpoints (GitHub, GitLab, Bitbucket, bare git repos). Metadata only -- syncs commit history without binary file content. See [Metadata-Only Remotes](metadata-remotes.md).

**UNC paths**: Under MINGW / Git Bash, UNC paths must use forward slashes (`//server/share/path`). The shell strips leading backslashes before bit receives the argument. bit normalizes forward-slash UNC paths and displays the canonical backslash form.

### Remote Config Format

Remote configs stored in `.bit/remotes/<name>` with typed format:
- `type: filesystem` (path lives only in git config as named remote)
- `type: cloud\ntarget: gdrive:Projects/foo` (rclone path for transport)
- `type: device\ntarget: black_usb:Backup` (device identity for resolution)
- `type: git\ntarget: git@github.com:user/repo.git\nlayout: metadata`

### Cloud Remote Layout

Cloud remotes have a `layout` field that controls what is stored on the cloud:

- **`layout: full`** (default, backward compatible): Files at human-readable paths mirroring the working tree AND in CAS. Users can browse files in Google Drive / S3 / etc.
- **`layout: bare`**: Files only in CAS layout (`cas/<prefix>/<hash>`). Opaque to humans -- no browsable file tree.
- **`layout: metadata`**: Only git history is synced -- no binary file content is transferred. Used automatically for git remotes and optionally for cloud/filesystem remotes via `--metadata-only`.

**`bit remote add --bare`**: Sets `layout: bare`. Only valid for cloud remotes. Without `--bare`, cloud remotes default to `layout: full`.

**`bit remote add --metadata-only`**: Sets `layout: metadata`. Valid for cloud and filesystem remotes. `--bare` and `--metadata-only` are mutually exclusive.

**No cloud remote layout conversion.** Switching a remote between layouts is not supported. To change layout, create a new remote with the desired layout and push to it.

### Device Add Fallback

When adding a removable or network path, bit may prompt for a device name and write `.bit-store` at the volume root. If writing `.bit-store` fails (e.g. permission denied), the remote is added as path-only (RemoteFilesystem) -- device identity is not persisted.

### Git Remote Registration

All remote types get a **named git remote** inside `.bit/index/.git/`:

| Remote type | git remote URL | Example |
|---|---|---|
| Filesystem | `<remotePath>/.bit/index` | `/mnt/usb/project/.bit/index` |
| Cloud | `.git/bundles/<name>.bundle` (relative) | `.git/bundles/gdrive.bundle` |
| Device | `<resolved-path>/.bit/index` (updated at operation time) | `/mnt/usb/.bit/index` |
| Git | Direct URL to git endpoint | `git@github.com:user/repo.git` |

**Cloud vs filesystem/device**: For cloud, the git remote URL and bit's actual target serve different purposes. The git remote points to a local bundle file; bit's rclone target is in `.bit/remotes/<name>`. Rclone downloads the remote's bundle to the local file; then git fetches from that file. For filesystem/device, git talks directly to the remote repo -- no bundle staging.

---

## Remote State Classification

Before push or fetch, the implementation classifies the remote with `classifyRemoteState`: it lists the remote at **depth 2**, then interprets the result purely:

- **StateEmpty** -- No items. Push will create and initialize the remote. Fetch/pull abort with "Remote is empty. Run 'bit push' first."
- **StateValidBit** -- Any of: path `.bit/bit.bundle`, path `.bit/index`, any path with prefix `.bit/`, or name `.bit`. A `cas/` directory alongside `.bit/` also indicates a valid bit remote. Push and fetch proceed.
- **StateNonBitOccupied** -- Otherwise; up to 3 paths are shown. Push and fetch abort with "The remote path is not empty and not a bit repository."
- **StateNetworkError** -- Network error during classification.

---

## Upstream Tracking (Git-Standard Behavior)

**IMPORTANT**: bit follows git's upstream tracking conventions exactly:

1. **`bit remote add <name> <url>` does NOT set upstream** -- unlike old bit behavior, adding a remote (even "origin") does not auto-configure `branch.main.remote`. This matches git.

2. **`bit push -u <remote>` sets upstream** -- the `-u` / `--set-upstream` flag pushes and configures `branch.main.remote = <remote>` in one operation.

3. **Commands accept explicit remote names**:
   - `bit push <remote>` -- push to named remote (no tracking change)
   - `bit pull <remote>` -- pull from named remote (no tracking change)
   - `bit fetch <remote>` -- fetch from named remote (no tracking change)

4. **Default remote selection**:
   - If `branch.main.remote` is set, it's used as the default
   - If not set, **`bit push` requires explicit remote** (no fallback)
   - If not set, **`bit pull` requires explicit remote** (no fallback)
   - If not set and "origin" exists, **`bit fetch` uses it as fallback** (git-standard -- fetch is read-only)
   - If neither upstream nor "origin", commands fail with error suggesting `bit push <remote>` or `bit push -u <remote>`

5. **First pull does NOT set upstream**: When pulling for the first time (unborn branch), `checkoutRemoteAsMain` uses `git checkout -B main --no-track`. Users must use `bit push -u <remote>` to explicitly configure tracking.

6. **Internal git remote vs upstream tracking**: bit's internal git repo has a remote named "origin" (used for fetching refs from bundles), but this is distinct from upstream tracking config (`branch.main.remote`). The internal remote is set up automatically; upstream tracking is never automatic.
