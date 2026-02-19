# Tutorial: Filesystem Remotes

Filesystem remotes let you sync to USB drives, network shares, NAS devices, or any local directory. Unlike cloud remotes, bit creates a **complete bit repository** at the remote — anyone who plugs in the USB drive can run bit commands directly.

## Quick Start

```bash
bit remote add usb /mnt/usb/my-project
bit push -u usb
```

On the USB drive, you now have a full bit repo. Plug it into another machine and:

```bash
bit init
bit remote add usb /mnt/usb/my-project
bit pull usb
```

## Adding a Filesystem Remote

```bash
bit remote add backup /mnt/nas/projects/my-project
```

The path can be:

- A local directory: `/home/user/backup/my-project`
- A USB drive: `/mnt/usb/my-project` or `E:\Backup\my-project`
- A network share: `//server/share/my-project` (UNC path)
- A NAS path: `/mnt/nas/projects/my-project`

bit auto-detects the path type. For removable drives, bit may prompt for a device name and write a `.bit-store` identity file at the volume root — this lets bit find the drive even when the drive letter changes.

## Pushing

```bash
bit push backup
```

On first push, bit creates a full bit repository at the remote path:

```
Pushing to 'backup'...
Initializing remote repository...
Syncing files: 5/12 files, 8.2 GB / 18.3 GB (45%)
Synced 12 files (18.3 GB).
```

Subsequent pushes only transfer changes.

### How It Works

Filesystem push uses **native git** — no bundles, no rclone indirection. bit has the remote run `git fetch` from your local repo, then `git merge --ff-only`. Files are synced via rclone (which handles local-to-local copies just like cloud transfers).

This means:

- The remote is a real git repo — `bit log`, `bit status`, etc. work there
- No bundle serialization overhead
- git handles the metadata transport natively

## Pulling

```bash
bit pull backup
```

```
Pulling from 'backup'...
Updated: abc1234..def5678
Syncing files: 2/2 files, 240.0 MB / 240.0 MB (100%)
Synced 2 files (240.0 MB).
```

Pulling from a filesystem remote uses `git fetch` directly — git talks repo-to-repo.

## Using the Remote Directly

Because filesystem remotes are full bit repos, you can work directly on the remote:

```bash
cd /mnt/usb/my-project
bit status          # see the state of files on the drive
bit log             # view history
bit verify          # check file integrity
```

This is useful for verifying a backup or checking what's on a USB drive without copying everything locally.

## UNC Paths (Network Shares)

On Windows under Git Bash / MSYS2, use forward slashes for UNC paths:

```bash
bit remote add nas //server/share/projects/my-project
```

bit normalizes this internally and displays it as `\\server\share\projects\my-project`.

## Device Identity

When you add a removable drive, bit may ask for a device name:

```
Enter a name for this device: black_usb
```

bit writes a `.bit-store` file at the drive root with a unique UUID. This means:

- If the drive letter changes (e.g., `E:` becomes `F:`), bit can still find the remote
- Multiple repos on the same drive each store their config in `.bit/remotes/<name>` but share the device identity

If writing `.bit-store` fails (e.g., permission denied on a read-only volume), the remote is added as a plain path — it still works, but won't survive drive letter changes.

## Verification

Filesystem remotes are verified the same way as local files — bit reads and hashes every file on the remote:

```bash
bit --remote backup verify
```

```
Verifying remote 'backup' files...
Collecting files... 47 found.
Checking cache... 45 cached, 2 need hashing (120.5 MB).
Comparing against committed metadata...
[OK] All 47 files match metadata.
```

The bandwidth detection and size-only fallback applies — if the remote is slow (e.g., a USB 2.0 drive), bit will offer to skip full hashing.

## Repair Across Remotes

If a file is corrupted locally, bit can repair it from the filesystem remote:

```bash
bit repair
```

```
Searching 1 source(s): 'backup'
Repairing 1 file(s)...
  (1/1) data/model.bin from 'backup' — 120.0 MB / 120.0 MB (100%)
  [REPAIRED] data/model.bin
```

And vice versa — if the USB drive has corruption, repair it from local:

```bash
bit --remote backup repair
```

## Multiple Machine Workflow

Filesystem remotes are a natural hub for multiple machines:

```
Machine A ──push──→ USB Drive ←──pull── Machine B
                        ↑
                        └──pull── Machine C
```

Each machine pushes to and pulls from the USB drive:

```bash
# Machine A
bit push usb

# Machine B
bit pull usb
# ... make changes ...
bit push usb

# Machine A (later)
bit pull usb
```

Conflicts are handled the same way as cloud remotes — three-way merge, with `--accept-remote` and `--manual-merge` as escape hatches. See [Conflict Resolution](conflict-resolution.md).
