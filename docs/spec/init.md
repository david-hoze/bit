# Init

## `bit init`

Creates `.bit/` with an internal Git repo in `.bit/index/.git`. Git repository initialized during `bit init`, with all git operations using the repo at `.bit/index/.git`. Working tree is `.bit/index` (not the project root).

### Git Configuration

- **Ignore rules**: User creates `.bitignore` in the project root. Before any command that uses working tree state (add, commit, status, diff, restore, checkout, merge, reset, mv), bit runs **syncBitignoreToIndex**: if `.bitignore` exists it is copied to `.bit/index/.gitignore` with normalization (line endings, trim, drop empty lines); if `.bitignore` does not exist, `.bit/index/.gitignore` is removed if present.
- **Attribute rules**: Same commands also run **syncGitattributesToIndex**: if `.gitattributes` exists in the project root, it is copied verbatim to `.bit/index/.gitattributes`; if not, `.bit/index/.gitattributes` is removed.

### Init Steps

**Init additionally:**
- Adds the repo's absolute path to `git config --global safe.directory` so git does not report "dubious ownership" when the repo lives on an external or USB drive.
- Sets `core.autocrlf=false` to prevent git from applying CRLF conversion to metadata files.
- Creates `.bit/index/.git/bundles` for per-remote bundle files.
- Creates `.bit/cas/` for the content-addressed store.
- These post-init config steps only run on fresh init -- re-init (`bit init` on an existing repo) skips them and just forwards to `git init`.

### Branch Naming

On fresh init, bit sets `init.defaultBranch=main` and renames `master` to `main` -- unless the user passed `--initial-branch`/`-b`, in which case git's own branch naming is respected and bit does not override it.

### Template Path Resolution

`--template=<path>` and `--template <path>` flags are resolved to absolute paths before forwarding to git. This is necessary because git runs inside `.bit/index/` via `-C`, so relative template paths would resolve from the wrong directory.

### Re-init

Running `bit init` on an existing repo always forwards to `git init` (including on `.bit` bitlink repos from `--separate-git-dir`). Git prints "Reinitialized existing Git repository" and handles format changes, gitdir moves, etc. bit propagates git's exit code and output verbatim.

**Re-init with `--separate-git-dir` (junction environment):** When re-initializing with `--separate-git-dir` and `BIT_GIT_JUNCTION=1`, bit normalizes the `.git` directory junction to a gitlink file before calling `git init`. This is necessary because git's `--separate-git-dir` expects to rename `.git` (a directory), but junctions are removed with `rmdir` not `rename`.

### Global Flags Before Init

`bit -c key=val init [dir]` and `bit --bare init [dir]` are dispatched correctly -- `-c` pairs and `--bare` are peeled from before the `init` subcommand and forwarded to git in the right position.

---

## Bare Init

`bit init --bare [dir]` passes through directly to `git init --bare`, then creates a `bit/cas/` subdirectory inside the resulting bare repo. bit does not create `.bit/index/` or any other bit-specific structure -- the bare repo is a standard git bare repo that git can discover and operate on natively.

Known bit commands (`add`, `commit`, `status`, etc.) are rejected in bare repos with "fatal: this operation must be run in a work tree" (exit 128). Unknown commands pass through to git, which discovers the bare repo naturally.

---

## Separated Init

`bit init --separate-git-dir <dir> [workdir]` places the git database and bit metadata at `<dir>`, writes a bitlink (`.bit` file with `bitdir:` pointer) and gitlink (`.git` file with `gitdir:` pointer) in the working directory. All operations transparently follow bitlinks. Re-init resolves bitlinks to find the real `.bit` directory.
