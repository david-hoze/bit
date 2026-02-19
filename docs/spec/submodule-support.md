## Submodule Support

bit supports two kinds of submodules: **git subrepos** (directories with a `.git/` directory) and **bit subrepos** (directories with a `.bit/` directory). Submodule operations are organized into two flows based on direction:

- **WorkingDir → Index** (`bit add <path>`): The user has a local subrepo. Move the child's git directory into the parent's `.bit/index/`, let git create the `160000` entry, rewrite the gitlink for the working directory.
- **Index → WorkingDir** (`bit submodule` commands): A git operation has populated `.bit/index/`. Replace the working directory with the index copy, rewriting the gitlink.

### Background: How Git Stores Submodules

Git represents a submodule as a tree entry with mode `160000` (a "gitlink") pointing to a commit hash. The submodule's actual `.git` data lives under the parent repo's `.git/modules/<path>/`, and the submodule's working tree has a `.git` file (not directory) containing a `gitdir:` pointer back to it:

```
extern/lib/.git  →  "gitdir: ../../.git/modules/extern/lib"
```

When `git add extern/lib` is run, git detects the `.git` presence inside `extern/lib/`, recognizes it as a nested repo, and creates the `160000` entry. `git submodule add` automates the process of cloning, wiring up the modules directory, creating the gitlink, and registering in `.gitmodules`.

### Two Flows

Submodule operations fall into two directions. Each direction has its own mechanism; they do not share code paths.

#### Flow 1: Working Directory → Index (`bit add <path>`)

The user has a local subrepo in the working directory and wants to register it with the parent. The working directory already has the correct content — the index needs to catch up.

**Git subrepo** (`a/sub/.git/` is a directory):

1. **Detect**: `a/sub/.git` is a directory
2. **Move**: `a/sub/.git/` → `.bit/index/a/sub/.git/`
3. **Git add**: `git -C .bit/index add a/sub` — git sees `.git/`, detects the nested repo, creates the `160000` entry, moves `.git/` into `.bit/index/.git/modules/a/sub/`, and writes a gitlink file at `.bit/index/a/sub/.git`
4. **Rewrite gitlink only**: Copy `.bit/index/a/sub/.git` (the gitlink file) to `a/sub/.git` with the path rewrite applied. The working directory already has the correct content; only the gitlink needs updating.

> **TODO**: Check if there is any scenario in local adding of a git subrepo where we do need to use `syncSubmoduleToWorkingDirectory`.

**Bit subrepo** (`a/sub/.bit/` exists):

1. **Detect**: `a/sub/.bit/` exists
2. **Move**: `a/sub/.bit/index/.git/` → `.bit/index/a/sub/.bit/index/.git/`
3. **Git add**: `git -C .bit/index add a/sub/.bit/index` — git sees `.git/`, creates the `160000` entry, moves `.git/` into `.bit/index/.git/modules/a/sub/.bit/index/`, writes a gitlink
4. **Rewrite gitlink only**: Copy `.bit/index/a/sub/.bit/index/.git` (the gitlink file) to `a/sub/.bit/index/.git` with the path rewrite applied. The working directory already has the correct content.

> **TODO**: Check if there is any scenario in local adding of a bit subrepo where we do need to use `syncSubmoduleToWorkingDirectory`.

Note: for bit subrepos, the submodule path registered in `.gitmodules` is `a/sub/.bit/index`, not `a/sub/`. The child's working directory files (`a/sub/*.mp4` etc.), CAS (`a/sub/.bit/cas/`), and config (`a/sub/.bit/config`) are untouched — they remain in the working directory. Only the child's git metadata is tracked by the parent.

#### Flow 2: Index → Working Directory (`bit submodule` commands)

The index has been populated by a git operation (clone, update, foreach) and the working directory needs to catch up. This is handled by `syncSubmoduleToWorkingDirectory`.

**`bit submodule add <url> <path>`:**

1. **Git submodule add**: `git -C .bit/index submodule add <url> <path>` — git clones into `.bit/index/<path>/`, sets up `.bit/index/.git/modules/<path>/`, creates the gitlink file
2. **Replace**: `syncSubmoduleToWorkingDirectory`

**`bit submodule update [--init]`:**

1. **Git submodule update**: `git -C .bit/index submodule update ...` — git populates `.bit/index/<path>/`
2. **Replace**: `syncSubmoduleToWorkingDirectory`

**`bit submodule deinit <path>`:**

1. **Git submodule deinit**: `git -C .bit/index submodule deinit ...` — git empties `.bit/index/<path>/`
2. **Replace**: `syncSubmoduleToWorkingDirectory` (working dir becomes empty)

**`bit submodule foreach <cmd>`:**

1. **Git submodule foreach**: `git -C .bit/index submodule foreach ...` — command runs in each submodule inside `.bit/index/`
2. **Replace**: `syncSubmoduleToWorkingDirectory`

After any Flow 2 operation, the submodule's source files live in the working directory where editors and tools expect them, and the gitlink stays in `.bit/index/<path>/.git` so the parent git sees the `160000` entry.

### Scanner Behavior

The scanner **entirely ignores** git and bit subrepos. When the scanner encounters a directory containing `.git/` (a git subrepo) or `.bit/` (a bit subrepo), it does not descend into it and does not copy any files. These directories are opaque boundaries.

This means:

- Flow 1 (`bit add` of a local subrepo) is not a scanner operation. It is handled by the `bit add` command directly: move the `.git` directory, run `git add`, rewrite the gitlink.
- Flow 2 (`bit submodule` commands) is handled by `syncSubmoduleToWorkingDirectory`. The scanner is not involved.
- For ongoing work, the user operates inside the subrepo independently (using `bit` or `git` commands within the child). The parent only sees the `160000` commit pointer, updated when the user runs `bit add <path>` or `bit add <path>/.bit/index` from the parent.

### Gitlink Path Rewriting

When a gitlink file is copied from `.bit/index/` to the working directory, its `gitdir:` path must be rewritten. Inside `.bit/index/`, the path resolves correctly (e.g., `gitdir: ../../.git/modules/a/sub`). In the working directory, the same relative path would resolve incorrectly because `.bit/index/` is one level deeper.

The rewrite rule: replace the relative `../.git/modules/` prefix with the equivalent path that routes through `.bit/index/`. For example:

```
In .bit/index/:     gitdir: ../../.git/modules/a/sub
In working dir:     gitdir: ../../.bit/index/.git/modules/a/sub
```

This is a text substitution on the single-line gitlink file: insert `.bit/index/` into the path so it resolves from the working directory through to the parent's git modules directory inside `.bit/index/`.

### `bit submodule` Command

`bit submodule` is a known bit command (not passthrough). Subcommands that modify the working tree (see Flow 2 above) delegate to `git -C .bit/index submodule ...` and then call `syncSubmoduleToWorkingDirectory`. Read-only subcommands (`status`, `sync`, etc.) delegate to `git -C .bit/index submodule ...` directly with no follow-up.

#### `syncSubmoduleToWorkingDirectory`

For each submodule path registered in `.bit/index/.gitmodules`:

1. **Delete** `<submodule-path>/` in the working directory entirely
2. **Replace** it with the contents of `.bit/index/<submodule-path>/`
3. Copy the gitlink file with the path rewrite applied

This is a destructive replacement, not a sync or merge. The index copy is the source of truth; the working directory copy is overwritten completely.

This single function handles all Flow 2 operations uniformly:

- After `submodule add`: index is populated → working directory is populated
- After `submodule update --init`: index is populated → working directory is populated
- After `submodule deinit`: index is empty → working directory becomes empty
- After `submodule foreach <cmd>`: index reflects command's changes → working directory matches

### Day-to-Day Workflow

**Working inside a submodule** (git subrepo):

```
cd a/sub/
# .git is a gitlink → resolves through .bit/index to parent's modules dir
# edit files, git add, git commit — works normally via the gitlink
```

**Working inside a submodule** (bit subrepo):

```
cd a/sub/
# This is an independent bit repo with its own .bit/
# bit add, bit commit — works normally within the child
# Large files managed by child's own CAS and remotes
```

**Updating the parent after child commits:**

```
cd /repo-root/
bit add a/sub          # (git subrepo) updates 160000 pointer
bit add a/sub/.bit/index  # (bit subrepo) updates 160000 pointer
bit commit -m "bump submodule"
```

**Cloning a parent with submodules:**

```
bit clone <url> project
cd project/
bit submodule update --init
# git populates .bit/index/<path>/, syncSubmoduleToWorkingDirectory
# copies files to working directory
# For bit subrepos: user runs bit init or bit clone inside <path>/ to
# set up the child's .bit/ structure, CAS, and remotes
```

### What Bit Does NOT Do

- **No submodule handling in the scanner**: The scanner does not descend into directories that contain `.git/` or `.bit/`. It does not parse `.gitmodules`, check for `160000` entries, or copy submodule content. Subrepo directories are opaque boundaries.
    
- **No automatic child `bit init`**: When cloning a parent that contains bit subrepo submodules, the user must explicitly initialize the child's `.bit/` structure. The parent only tracks the child's git metadata (`.bit/index/`), not the child's CAS, config, or remotes.
    
- **No `EntryKind` change**: Submodule directories do not need a new `Submodule` variant in `EntryKind`. The gitlink file is a small text file handled by git, not by the scanner's classification logic.
    
- **No CAS nesting**: The parent's CAS does not store the child's large files. Each bit repo manages its own CAS and remotes independently.
    

### Summary

| Operation                        | Flow               | What happens                                                                    |
| -------------------------------- | ------------------ | ------------------------------------------------------------------------------- |
| `bit add <path>` (git subrepo)   | WorkingDir → Index | Move `.git/` → index, `git add`, rewrite gitlink only                           |
| `bit add <path>` (bit subrepo)   | WorkingDir → Index | Move `.bit/index/.git/` → index, `git add .bit/index`, rewrite gitlink only     |
| `bit submodule add <url> <path>` | Index → WorkingDir | `git -C .bit/index submodule add` → `syncSubmoduleToWorkingDirectory`           |
| `bit submodule update --init`    | Index → WorkingDir | `git -C .bit/index submodule update --init` → `syncSubmoduleToWorkingDirectory` |
| `bit submodule deinit <path>`    | Index → WorkingDir | `git -C .bit/index submodule deinit` → `syncSubmoduleToWorkingDirectory`        |
| `bit submodule foreach <cmd>`    | Index → WorkingDir | `git -C .bit/index submodule foreach` → `syncSubmoduleToWorkingDirectory`       |
| Scanner                          | —                  | Ignores subrepo directories entirely (opaque boundary)                          |