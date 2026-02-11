# Verify and Repair Tutorial

## Why Verification Matters

In Git, every blob is stored by its SHA-1 hash. If you have the metadata, you have
the content — they're the same thing.

bit is different. Binary files live in the working tree as regular files. The
metadata *claims* that `photos/vacation.jpg` has a certain hash and size — but the
file could have been silently corrupted by a bad disk sector, accidentally
overwritten, or partially transferred. Verification is how bit checks that reality
matches the claims.

## `bit verify`

Checks whether the files in your working tree match what you committed.

```
$ bit verify
[OK] All 47 files match metadata.
```

Every tracked file is hashed and compared against the committed metadata. If
anything doesn't match, bit tells you exactly what's wrong:

```
$ bit verify
[ERROR] Hash mismatch: data/model.bin
  Expected: md5:a1b2c3d4e5f6...
  Actual:   md5:9f8e7d6c5b4a...
Checked 47 files. 1 issues found. Run 'bit status' for details.
```

bit also checks for missing files — tracked files that no longer exist on disk.

### Automatic Verification

bit verifies automatically in:

- **`bit push`** verifies local files before pushing — you can't push metadata
  that doesn't match your actual files
- **`bit pull`** verifies remote files before pulling — you won't pull corrupted
  data from a remote

This is the **proof of possession** rule: you can't transfer metadata claims you
can't substantiate, i.e. provide the actual file.

## `bit verify --remote`

Same idea, but checks the files on the remote:

```
$ bit verify --remote
[OK] All 47 files match metadata.
```

For cloud remotes (Google Drive, S3, etc.), this is fast — cloud providers store
MD5 hashes as native file metadata, so bit can verify without downloading anything.

For filesystem remotes (USB drives, network shares), bit reads and hashes every
file on the remote, same as local verification.

## `bit remote repair`

When verification finds problems, repair fixes them. It checks both sides, then
copies healthy files to replace broken ones.

```
$ bit remote repair
Repairing against remote: gdrive (gdrive:MyBackup)

Verifying local files...
  47 files checked, 1 issues
Verifying remote files...
  47 files checked, 0 issues

Repairing 1 file(s)...
  [REPAIRED] data/model.bin

1 repaired, 0 failed, 0 unrepairable.
```

### How Repair Finds the Right File

Repair matches files by **content hash and size**, not by path. This means:

- If `photos/vacation.jpg` is corrupted locally, but the remote has an identical
  copy at `backup/vacation_copy.jpg`, it will be used as the repair source.
- Renamed files, moved files, and duplicates all serve as valid repair sources.

Two files with the same hash and size have identical content — it doesn't matter
what they're called or where they live.

### Repair Works Both Ways

Repair isn't one-directional. If a local file is broken, it copies from the remote.
If a remote file is broken, it copies from local. It fixes whatever it can on
both sides in a single run.

### When Files Can't Be Repaired

A file is **unrepairable** when the same content is broken on both sides:

```
$ bit remote repair
  [UNREPAIRABLE] data/model.bin

0 repaired, 0 failed, 2 unrepairable.
```

If this happens, you'll need to restore the file from another source (another
backup, the original file, etc.), then `bit add` and `bit commit` to update
the metadata.

## `bit fsck`

Runs `git fsck` on bit's internal metadata repository. This checks that the
metadata store itself is healthy — not the files, but the database that tracks
them. Rarely needed; it's the "something is really wrong with my disk" command.

```
$ bit fsck
```

## Putting It All Together

A typical workflow when something seems wrong:

```bash
# 1. Check if local files are healthy
bit verify

# 2. Check if remote files are healthy
bit verify --remote

# 3. If either side has issues, repair automatically
bit remote repair

# 4. Verify again to confirm everything is clean
bit verify
```
