# Conflict Resolution

## Merge Conflict Resolution

Conflict resolution is structured as a traversal over a list of conflicts: `resolveAll` maps over the list with `resolveConflict`, so each conflict is visited exactly once with correct progress tracking (1/N, 2/N, ...). The decision logic (KeepLocal vs TakeRemote) is cleanly separated from the git checkout/merge mechanics.

**Critical**: After resolving all conflicts, the merge commit must **always** be created, regardless of whether the index has staged changes. When the user chooses "keep local" (`--ours`), `git checkout --ours` + `git add` restores HEAD's version -- the index becomes identical to HEAD. A naive `hasStagedChanges` check would skip the commit, leaving `MERGE_HEAD` dangling. Git's `commit` command always succeeds when `MERGE_HEAD` exists (it knows it's recording a merge), even if the tree is identical to HEAD's tree. Skipping the commit breaks the next push (ancestry check fails because HEAD was never advanced past the merge).

---

## `merge --continue` and `merge --abort`

Understanding the control flow here is important for maintainers; getting it wrong can lead to data loss.

### `merge --continue`

Has two branches:

1. **No git conflicts and no `.bit/conflicts` directory:** If `MERGE_HEAD` exists, create the merge commit ("Merge remote"), print "Merge complete.", then sync the working tree by calling **syncBinariesAfterMerge(remote, oldHead)** -- which diffs oldHead vs current HEAD and copies binaries from the remote (and text from the index) so the working tree matches the merged metadata. This is the path when the user resolved conflicts via the interactive (l)ocal/(r)emote prompt during pull.

2. **`.bit/conflicts` directory exists** (e.g. after `bit pull --manual-merge` with remote divergence): Before committing, **validateMetadataDir** is run on `.bit/index`. If any metadata file contains conflict markers, the merge is aborted with "fatal: Metadata files contain conflict markers. Merge aborted." If validation passes, the merge commit is created ("Merge remote (manual merge resolved)"), `.bit/conflicts` is removed, "Conflict directories cleaned up." is printed, then syncBinariesAfterMerge runs.

### `merge --abort`

Runs `git merge --abort`. If `.bit/conflicts` exists, it is removed and "Conflict directories cleaned up." is printed.

---

## Handling Remote Divergence

When remote files don't match remote metadata (detected via `bit --remote <name> verify` or during `bit pull`):

### Resolution Option 1: Accept Remote Reality (`--accept-remote`)

Force-checkout the remote branch so git puts the correct metadata in `.bit/index/`, then mirror the changes to the working directory. Architecturally identical to a normal pull -- just a force-checkout instead of a merge.

The flow:
1. Fetch remote bundle (git gets remote history)
2. Record current HEAD (for diff-based sync)
3. `git checkout -f -B main --no-track refs/remotes/origin/main` (force-checkout without setting upstream)
4. `applyMergeToWorkingDir` (diff old HEAD vs new HEAD, sync files)
5. Update tracking ref

**Important**: `--accept-remote` must NOT scan remote files via rclone and write metadata directly. Rclone cannot distinguish text from binary files, so text files would get `hash:/size:` metadata instead of their actual content.

### Resolution Option 2: Force Local (`bit push --force`)

Upload all local files, overwriting remote. Push metadata bundle. Requires confirmation.

### Resolution Option 3: Manual Merge (`--manual-merge`)

Interactive per-file conflict resolution:
- For each conflict, displays local hash/size vs remote hash/size
- User chooses (l)ocal or (r)emote for each file
- Resolution is applied via git checkout mechanics
- Supports `bit merge --continue` and `bit merge --abort`
