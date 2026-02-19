# Tutorial: Modes and CAS

bit has two modes that control how file content is stored: **lite** (default) and **solid**. This tutorial explains the difference and when to switch.

## The Two Modes

### Lite Mode (Default)

In lite mode, `bit add` only writes metadata (hash + size) to `.bit/index/`. The actual binary content lives exclusively in the working tree — there is exactly one copy of each file.

- Simple, space-efficient
- Old versions are gone when you overwrite a file
- No binary history, no time travel

### Solid Mode

In solid mode, `bit add` writes metadata AND copies the file's content into `.bit/cas/` (the content-addressed store). Every version of every binary file is preserved.

- Full binary history
- Restore any previous version of any file
- Uses more disk space (one copy per version)
- Required for content-defined chunking (CDC)

## Checking the Current Mode

```bash
bit config core.mode
```

```
lite
```

If nothing is printed, the repo is in lite mode (the default).

## Switching to Solid Mode

```bash
bit config core.mode solid
```

```
Mode set to solid. bit add will now store file content in .bit/cas/.
hint: Run 'bit cas backfill' to store current files for existing commits.
```

From this point forward, every `bit add` stores the file content in `.bit/cas/`. Existing history has no CAS backing yet — only new additions are stored.

## Switching Back to Lite

```bash
bit config core.mode lite
```

```
Mode set to lite. bit add will no longer store file content in .bit/cas/.
Existing CAS data is preserved.
```

`bit add` stops writing to the CAS. The existing `.bit/cas/` directory is preserved with all its data — nothing is deleted. You can switch back to solid at any time.

## The CAS

The content-addressed store lives at `.bit/cas/`. Files are stored by hash:

```
.bit/cas/
├── a1/
│   └── a1b2c3d4e5f67890...    # content of file with this hash
├── f8/
│   └── f8e9a0b1c2d3e4f5...
└── ...
```

The first two hex characters of the hash form a subdirectory (for filesystem performance). The filename is the full hash.

### CAS Reads Are Mode-Independent

Regardless of the current mode, any operation that needs old file content checks `.bit/cas/` as a fallback. This means:

- If you were in solid mode and stored versions, then switch to lite, those stored versions are still accessible
- `bit restore` can recover files from CAS even in lite mode
- Verification checks both the working tree and CAS

## Backfilling History

When you switch to solid mode, existing commits have no CAS backing. The `bit cas backfill` command stores any files that are currently in the working tree for historical commits:

```bash
bit config core.mode solid
bit cas backfill
```

```
Scanning historical commits...
Stored 47 files from 12 commits.
```

This only stores files that currently exist in the working tree. If a file was modified since a historical commit, only the current version is stored (the old version is gone — that's the nature of lite mode).

Backfill is optional — the CAS is simply incomplete for older commits, and that's fine.

## Restoring Files from CAS

When a file has been stored in CAS, you can restore previous versions:

```bash
bit restore --source=HEAD~3 data/model.bin
```

If the blob for that version exists in CAS, bit copies it to the working tree. If not (because that commit predates solid mode), bit reports "no content available for this version."

## Verification with CAS

When bit verifies for push to full-layout remotes (the "proof of possession" check), it checks both the working tree and the CAS:

1. Does the working tree file match the metadata? -> verified
2. Does `.bit/cas/` contain a blob for that hash? -> verified
3. Neither? -> verification fails

This means solid-mode repos have an easier time passing verification — even if a working tree file is missing, the CAS copy satisfies the proof of possession rule.

## Mode Is Local-Only

The mode is stored in `.bit/config` and is **not** tracked by git. Different clones of the same project can run in different modes:

- A laptop might use lite to save space
- A NAS might use solid for full history
- A workstation might use solid with CDC for efficient cloud backup

Switching modes is instant, with no migration or data loss.

## Content-Defined Chunking (CDC)

When solid mode is active, you can enable content-defined chunking for efficient storage of large binary files:

```bash
bit config cdc.enabled true
```

CDC splits large files into content-determined chunks. When you modify a small part of a large file, only the affected chunks get new hashes — the rest are already stored. This dramatically reduces storage and upload time for incremental changes to large files.

CDC parameters:

```bash
bit config cdc.min-size 32768      # minimum chunk size (default: 32 KB)
bit config cdc.avg-size 131072     # target average chunk size (default: 128 KB)
bit config cdc.max-size 524288     # maximum chunk size (default: 512 KB)
```

CDC is most beneficial with [bare remotes](bare-remotes.md) — only changed chunks are uploaded.

## Summary

| | Lite | Solid |
|---|---|---|
| `bit add` writes to CAS | No | Yes |
| Binary history | No | Yes |
| Disk usage | Minimal | One copy per version |
| CDC available | No | Yes |
| Restore old versions | No | Yes (if stored) |
| Default | Yes | No |
