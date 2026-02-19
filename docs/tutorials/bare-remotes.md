# Tutorial: Bare Cloud Remotes

Bare remotes store files as content-addressed blobs — no human-readable file tree on the cloud. This is for pure backup where you don't need to browse files on Google Drive or S3.

## When to Use Bare Remotes

- **Backup storage**: you only need to push and pull, not browse on the web
- **Deduplication**: with [content-defined chunking](../spec/cdc-spec.md), only changed chunks are uploaded
- **Privacy**: the cloud folder is opaque — just hashed blobs, not recognizable filenames

If you want to browse your files directly on Google Drive, use the default `full` layout instead (see [Cloud Remotes](cloud-remotes.md)).

## Quick Start

```bash
bit remote add backup gdrive:Backup/my-project --bare
bit push -u backup
```

## Adding a Bare Remote

The `--bare` flag sets the layout:

```bash
bit remote add backup gdrive:Backup/my-project --bare
```

```
Remote 'backup' added.
```

Without `--bare`, cloud remotes default to `full` layout. The `--bare` flag is only valid for cloud remotes — filesystem remotes are always full repos.

## What Gets Stored

```
gdrive:Backup/my-project/
├── .bit/
│   └── bit.bundle          # Metadata (git history)
└── cas/
    ├── a1/
    │   └── a1b2c3d4...     # Content blob (keyed by hash)
    ├── f8/
    │   └── f8e9a0b1...
    └── ...
```

No `lectures/` or `data/` directories — just hashed blobs in `cas/`. The metadata bundle contains the file names, directory structure, and history — but the actual content is only accessible via bit.

## Push and Pull

Push and pull work identically to full remotes:

```bash
bit push backup
bit pull backup
```

The difference is internal:

- **Push**: uploads content into `cas/<prefix>/<hash>`, then pushes the metadata bundle. No readable file tree is created.
- **Pull**: reads metadata to determine needed hashes, downloads blobs from `cas/<prefix>/<hash>`, and places them at the correct working tree paths.

## No Verification Needed

Bare remotes use content-addressed storage — each blob's filename *is* its hash. If `cas/a1/a1b2c3d4...` exists, the content is correct by construction. There's no mutable path that can drift, so bit skips the verification step that full remotes require.

This makes bare remote operations slightly faster than full remotes, since there's no pre-push or pre-pull integrity check.

## Checking Remote Status

```bash
bit remote show backup
```

```
  Remote: backup
  Type: cloud
  Target: gdrive:Backup/my-project
  Layout: bare
  HEAD branch: main
    main pushes to main (up to date)
```

## Combining with Full Remotes

A common setup: full remote for daily use, bare remote for backup.

```bash
bit remote add origin gdrive:Projects/my-project          # full (default)
bit remote add backup gdrive:Backup/my-project --bare     # bare

bit push -u origin     # daily push — files browsable on Drive
bit push backup        # periodic backup — opaque but space-efficient
```

## Layout Is Permanent

You can't convert between `full` and `bare` after creation. If you want to change the layout, create a new remote:

```bash
bit remote add backup-v2 gdrive:Backup/my-project-v2 --bare
bit push backup-v2
```

## Content-Defined Chunking

Bare remotes benefit the most from content-defined chunking (CDC). When enabled, large binary files are split into content-determined chunks. Modifying a small region of a 2 GB video only uploads the changed chunks — not the entire file.

CDC is enabled by default when in solid mode:

```bash
bit config core.mode solid      # CAS is required for CDC; CDC is on by default
```

See [Modes and CAS](modes-and-cas.md) for more on solid mode.
