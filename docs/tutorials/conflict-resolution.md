# Tutorial: Conflict Resolution

When you and someone else (or another machine) modify the same files, bit needs to reconcile the differences. This tutorial covers all the ways to handle conflicts and divergence.

## Normal Merge (Default)

When you pull and there are non-conflicting changes, bit merges automatically:

```bash
bit pull origin
```

```
Pulling from 'origin'...
Updated: abc1234..def5678
Syncing files: 2/2 files, 240.0 MB / 240.0 MB (100%)
Synced 2 files (240.0 MB).
```

bit uses `git merge --no-commit --no-ff` under the hood, then syncs the working directory to match the merged metadata.

## When Conflicts Happen

Conflicts arise when both sides modified the same binary file. bit detects this during pull:

```
$ bit pull origin
error: Remote files do not match remote metadata.
  Modified: data/model.bin (expected md5:a1b2..., got md5:c3d4...)
hint: Run 'bit --remote origin verify' to see all mismatches.
hint: Run 'bit pull --accept-remote' to accept the remote's actual file state.
hint: Run 'bit push --force' to overwrite remote with local state.
```

You have three options.

## Option 1: Accept Remote (`--accept-remote`)

Take whatever is on the remote as the truth. Your local files are overwritten with the remote's version:

```bash
bit pull --accept-remote
```

```
Pulling from 'origin' (accepting remote state)...
Syncing files: 3/3 files, 450.0 MB / 450.0 MB (100%)
Synced 3 files (450.0 MB).
```

This is the simplest option when the remote has the "right" version and you want to discard local changes.

**What happens:** bit force-checkouts the remote branch, then syncs the working directory to match. No merge commit — just a straight replacement.

## Option 2: Force Push (`--force`)

Overwrite the remote with your local state:

```bash
bit push --force
```

This skips the ancestry check and uploads your files regardless of what's on the remote. The remote's version is replaced with yours.

### Force with Lease

If you want to force-push but only if nobody else has pushed since your last fetch:

```bash
bit push --force-with-lease
```

This is safer — it fails if the remote has changed since you last fetched:

```
Remote has changed since last fetch! Someone else pushed to the remote.
Run 'bit fetch' to update your local view.
```

## Option 3: Manual Merge (`--manual-merge`)

Choose per-file which version to keep:

```bash
bit pull --manual-merge
```

```
Pulling from 'origin'...
3 file(s) diverge between local and remote:

  (1/3) data/model.bin
    Local:  md5:a1b2c3d4... (120.0 MB)
    Remote: md5:f8e9a0b1... (118.5 MB)
    Keep (l)ocal or take (r)emote? l

  (2/3) data/weights.bin
    Local:  md5:1234abcd... (45.2 MB)
    Remote: md5:5678efgh... (45.2 MB)
    Keep (l)ocal or take (r)emote? r

  (3/3) data/config.json
    Local:  md5:aaaa1111... (2.1 KB)
    Remote: md5:bbbb2222... (2.3 KB)
    Keep (l)ocal or take (r)emote? l
```

After resolving all files, bit creates a merge commit automatically.

## Git Text Conflicts

For text files (source code, configs, markdown), git handles conflicts the same way it always does — conflict markers in the file:

```
<<<<<<< HEAD
local change
=======
remote change
>>>>>>> refs/remotes/origin/main
```

Edit the file, then:

```bash
bit add conflicted-file.txt
bit merge --continue
```

## `merge --continue`

After resolving conflicts (either via the interactive prompt or by editing files manually):

```bash
bit merge --continue
```

```
Merge complete.
```

This creates the merge commit and syncs the working directory.

If you used `--manual-merge` and the conflict directories still exist, `merge --continue` cleans them up:

```
Merge remote (manual merge resolved)
Conflict directories cleaned up.
```

**Important:** `merge --continue` checks for conflict markers in metadata files before committing. If any are found, it aborts:

```
fatal: Metadata files contain conflict markers. Merge aborted.
```

## `merge --abort`

Changed your mind? Abort the merge entirely:

```bash
bit merge --abort
```

This runs `git merge --abort` and cleans up any conflict directories. Your working tree is restored to its pre-merge state.

## Deciding Which Option to Use

| Situation | Recommended |
|-----------|-------------|
| Remote has the right version | `bit pull --accept-remote` |
| Local has the right version | `bit push --force` |
| Need to pick per-file | `bit pull --manual-merge` |
| Want safety against concurrent pushers | `bit push --force-with-lease` |
| Simple non-conflicting changes | `bit pull` (default merge) |

## Workflow Tips

**Check before you push:** Use `bit fetch` + `bit status` to see if the remote has changed before attempting to push.

```bash
bit fetch origin
bit status
# shows "Your branch is ahead of 'origin/main' by 2 commits."
# or "Your branch and 'origin/main' have diverged."
```

**Don't panic:** Every conflict resolution option is safe — `--accept-remote` makes your local match the remote, `--force` makes the remote match your local, and `--manual-merge` lets you choose per-file. None of them delete git history.
