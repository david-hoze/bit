# Tutorial: Cloud Remotes

Cloud remotes let you push and pull files to any storage backend that rclone supports — Google Drive, S3, Dropbox, OneDrive, Backblaze B2, and dozens more. The remote is just a folder of files; no server software is needed.

## Prerequisites

1. [rclone](https://rclone.org/) installed and configured with at least one remote (e.g., `rclone config` to set up `gdrive:`)
2. A bit repository (`bit init`)

## Quick Start

```bash
bit remote add origin gdrive:Projects/my-project
bit push -u origin
```

Your files are now on Google Drive. To pull them back on another machine:

```bash
bit init
bit remote add origin gdrive:Projects/my-project
bit pull origin
```

## Adding a Cloud Remote

```bash
bit remote add origin gdrive:Projects/my-project
```

```
Remote 'origin' added.
```

The remote name (`origin`) is just a label — you can name it anything. The URL (`gdrive:Projects/my-project`) is any rclone path.

Common rclone paths:

| Backend | Example |
|---------|---------|
| Google Drive | `gdrive:Projects/my-project` |
| AWS S3 | `s3:my-bucket/my-project` |
| Dropbox | `dropbox:Projects/my-project` |
| OneDrive | `onedrive:Projects/my-project` |
| Backblaze B2 | `b2:my-bucket/my-project` |

## Pushing

```bash
bit push origin
```

On the first push, bit creates the remote directory structure and uploads everything:

```
Pushing to 'origin'...
Syncing files: 3/12 files, 5.1 GB / 18.3 GB (28%)
Synced 12 files (18.3 GB).
```

Subsequent pushes only upload what changed. bit computes the minimal set of operations from git metadata diffs — no remote scanning needed:

```
Pushing to 'origin'...
Syncing files: 1/1 files, 120.0 MB / 120.0 MB (100%)
Synced 1 files (120.0 MB).
```

Renames are handled efficiently — `rclone moveto` instead of delete + re-upload.

### Setting Upstream

Use `-u` to set the default remote so you can just type `bit push` next time:

```bash
bit push -u origin
```

After this, `bit push` and `bit pull` use `origin` automatically.

## Pulling

```bash
bit pull origin
```

```
Pulling from 'origin'...
Updated: abc1234..def5678
Syncing files: 2/2 files, 240.0 MB / 240.0 MB (100%)
Synced 2 files (240.0 MB).
```

If there are no changes:

```
Already up to date.
```

### First Pull on a New Machine

```bash
bit init
bit remote add origin gdrive:Projects/my-project
bit pull origin
```

bit fetches the metadata bundle (small — just git history), then downloads all the binary files. Text files are extracted directly from the metadata.

## Fetching (Metadata Only)

```bash
bit fetch origin
```

Downloads only the metadata bundle without syncing any files. Useful for checking what's changed before pulling:

```bash
bit fetch origin
bit status          # shows ahead/behind
bit log origin/main # see remote history
```

## Checking Remote Status

```bash
bit remote show origin
```

```
  Remote: origin
  Type: cloud
  Target: gdrive:Projects/my-project
  Layout: full
  HEAD branch: main
    main pushes to main (up to date)
```

## What Gets Stored on the Cloud

With the default `full` layout, the cloud folder mirrors your project:

```
gdrive:Projects/my-project/
├── .bit/
│   └── bit.bundle          # Metadata (git history)
├── cas/                     # Content-addressed backup
│   └── a1/
│       └── a1b2c3d4...
├── lectures/
│   ├── lecture-01.mp3       # Actual files, browsable
│   └── lecture-02.mp3
└── data/
    └── dataset.bin
```

You can open Google Drive and browse your files directly. The `cas/` directory is an automated backup of file content by hash.

For opaque backup without browsable files, see [Bare Remotes](bare-remotes.md).

## Verification

bit automatically verifies files before pushing and pulling:

- **Before push**: checks that your local files match your committed metadata (you can't push claims you can't back up)
- **Before pull**: checks that the remote files match the remote metadata (you won't pull corrupted data)

If verification fails, bit tells you exactly what's wrong and suggests fixes. See [Verify and Repair](verify-and-repair.md) for details.

## Multiple Remotes

You can push to multiple cloud destinations:

```bash
bit remote add gdrive gdrive:Projects/my-project
bit remote add s3backup s3:my-bucket/my-project

bit push -u gdrive      # primary remote
bit push s3backup        # backup
```

Or combine cloud with [filesystem remotes](filesystem-remotes.md) and [metadata-only remotes](metadata-remotes.md):

```bash
bit remote add gdrive gdrive:Projects/my-project     # content + metadata
bit remote add github git@github.com:user/project.git # metadata only
bit remote add usb /mnt/usb/my-project                # filesystem backup

bit push gdrive
bit push github
bit push usb
```

## Troubleshooting

**"The remote path is not empty and not a bit repository"**

The cloud folder already has files that aren't from bit. Either use a different path, or use `bit push --force` to overwrite (destructive).

**"Working tree does not match metadata"**

Your local files have changed since the last commit. Either `bit add` + `bit commit` to update, or `bit restore` to revert.

**"Remote is empty. Run 'bit push' first."**

You tried to pull or fetch from a remote that hasn't been pushed to yet.
