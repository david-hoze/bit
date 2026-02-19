# Git Compatibility

## Text Files: Exact Git Behavior

bit handles text files exactly like git does -- the content lives in `.bit/index` and goes through the same add/commit/diff/merge cycle. All of git's features work identically: submodules, separating the git directory, ignoring whitespace. Text classification is automatic: files under the configured size limit (default 1 MB) that contain valid UTF-8 and no NULL bytes are treated as text. Binary extensions (`.mp4`, `.zip`, `.exe`, etc.) are always binary regardless of content.

The underlying git repository stays completely intact inside `.bit/index` -- you can export back to pure git at any time via `bit export`. There is no lock-in.

---

## Git Executable Router

### Overview

The git router allows bit to transparently replace `git` on the system. When activated via `bit become-git`, a compiled Haskell executable (`bit-git-router`) is installed as `git` on the user's PATH. It dispatches commands to either real git (for `.git/` repos) or bit (for `.bit/` repos).

### Router Dispatch Logic

The router is a standalone executable with no bit module imports (fast startup, minimal dependencies):

1. **`git init`** -> always exec real git (preserves standard `git init` behavior). The router peels leading flags (`-c`, `-C`, `--bare`) to find the `init` subcommand.
2. **Walk up from CWD** looking for `.bit/` directory or `.bit` file (bitlink). If found -> exec `bit` with all args; otherwise -> exec real git.

Before calling bit, the router sets `BIT_REAL_GIT` to the real git path. This prevents recursion: `spawnGit` in `Bit/Git/Run.hs` checks this variable and uses the real git instead of the router.

### Finding Real Git

Priority order:
1. `BIT_REAL_GIT` environment variable
2. Config file `~/.bit-router/real-git` (written by `bit become-git`)
3. Search PATH excluding the router's own directory

### `bit become-git`

Installs the router:
1. Finds real git (`where git` / `which -a git`, filtering out `~/.bit-router/`)
2. Creates `~/.bit-router/` directory
3. Saves real git path to `~/.bit-router/real-git`
4. Copies `bit-git-router` executable as `git` (or `git.exe`) into `~/.bit-router/`
5. Adds `~/.bit-router/` to front of user's PATH:
   - **Windows**: modifies `HKCU\Environment\Path` registry key, broadcasts `WM_SETTINGCHANGE`
   - **Unix**: appends `export PATH=...` to `~/.bashrc` and `~/.profile`
6. Prints instructions to restart shell

### `bit become-bit`

Uninstalls the router:
1. Removes `~/.bit-router/` from PATH
2. Removes `~/.bit-router/` directory
3. Prints confirmation

### `git init` Asymmetry

`git init` always creates a standard git repo, never a bit repo. This is intentional:
- Users who want bit repos use `bit init`
- `git init` compatibility ensures existing scripts and tools work unchanged
- The router detects `init` by peeling global flags, not just checking `args[0]`

---

## Alias Expansion

bit expands git aliases before dispatching, matching git's behavior. When a command is not a known bit command, `tryAlias` queries git config for `alias.<name>`:

- **Inside a bit repo**: queries local config (`.bit/index/.git/config`) and global config
- **Outside a bit repo**: queries global config only

Alias types:
- **Simple aliases** (e.g. `alias.ci = commit`): expanded and re-dispatched through `runCommand`
- **Shell aliases** (starting with `!`): forwarded to git directly via `runGitGlobal`
- **No alias found**: passed through to git (see Passthrough below)

---

## Passthrough

Unknown commands (not handled by bit, no alias match) are forwarded to git:

- **Inside a bit repo with `.git` junction/gitlink**: uses `runGitGlobal` (no `-C`, no `-c` flags) -- when CWD has a `.git` directory (junction) or `.git` file (gitlink from `--separate-git-dir`), git's own repo discovery works natively. Using `-C .bit/index` would break commands like `git config -f <relative-path>`. **Subdirectory CWD restoration:** Before running git, passthrough restores CWD to the user's original directory. When CWD differs from root, `--work-tree=<root>` is added. For gitlink repos, `--git-dir=<abs-path>` is passed so relative paths in user arguments resolve correctly.
- **Inside a bit repo without `.git`**: uses `runGitRawAt` to target `.bit/index/` via `-C`
- **Inside a bare bit repo** (has `bit/` + `HEAD`): uses `runGitGlobal` (no `-C`) -- git discovers the bare repo naturally
- **Inside a git directory** (CWD has `HEAD` file + `refs/` directory but no `.bit/`): uses `runGitHere` -- lets git's own repo discovery work. This catches bare repos and users who `cd`'d into `.git/`.
- **Outside any repo**: uses `runGitGlobal` (no `-C`)

### `GIT_DIR` Bypass

When `GIT_DIR` is set to any non-empty value, bit passes through to git immediately via `runGitGlobal` without any command parsing, alias expansion, or `-C` insertion. This covers `GIT_DIR=/dev/null` (used by tools like `test_cmp`), `GIT_DIR=<path>`, and any other value. This check happens before all other dispatch.

### `--git-dir`/`--work-tree` Flag Bypass

When `--git-dir` or `--work-tree` appears as a leading global flag (before the subcommand), bit passes through to git immediately via `runGitGlobal`. Both `--git-dir=<path>` and `--git-dir <path>` forms are supported. Flags after the subcommand (e.g. `bit rev-parse --git-dir`) do not trigger bypass.

---

## `-C <dir>` Flag

When invoked as `bit -C <dir> <args...>`, bit intercepts the `-C` flag before any command dispatch:

- **`<dir>` has `.bit/`** (bit repo): `cd`s to `<dir>` and runs through normal bit dispatch
- **Otherwise** (plain git repo, bare repo, or nonexistent): runs `git -C <dir> <args...>`

Interception is necessary because without it, `-C` would be treated as an unknown command, and passthrough would prepend `-C .bit/index` -- producing `git -C .bit/index -C <dir> ...` (wrong directory).

---

## Init Dispatch Order

`init` is dispatched **before** repository discovery (`findBitRoot`). This is critical because `init` creates repos -- it must not be affected by a parent repo's `.bit/` directory. Without this, `cd existing-repo/subdir && bit init` would `setCurrentDirectory` to the parent root and re-initialize the parent.

Before matching `("init":rest)`, the dispatch peels git "global" flags: `-c key=val` pairs and `--bare`. This allows `bit -c init.defaultBranch=test init dir` and `bit --bare init dir` to work.

---

## Repo Discovery (`findBitRoot`)

`findBitRoot` walks up from CWD and returns a `BitRoot` sum type:

- **`NormalRoot FilePath`** -- directory has `.bit/` (standard bit repo)
- **`BareRoot FilePath`** -- directory has `bit/` + `HEAD` (bare bit repo)

Both are checked at each level, so a bare repo nested inside a normal repo's tree is detected at the correct (closest) level.

### `BIT_CEILING_DIRECTORIES` / `GIT_CEILING_DIRECTORIES`

`findBitRoot` respects both `BIT_CEILING_DIRECTORIES` and `GIT_CEILING_DIRECTORIES`. When either is set, `findBitRoot` stops walking up at the specified directory. Both variables use the platform path separator (`;` on Windows, `:` on Unix) and support multiple directories. The values from both are combined.

---

## GitHub and Remote Collaboration

With metadata-only remotes, bit works with GitHub, GitLab, and any native git host. The metadata-only remote syncs the `.bit/index` history using native git protocols. Collaborators see the full project structure and history; they add a content remote (Google Drive, S3, etc.) to get actual binary files.

```bash
bit init
bit remote add origin gdrive:Projects/foo            # full -- content lives here
bit remote add github git@github.com:user/foo.git    # metadata only (auto-detected)

bit add .
bit commit -m "initial commit"
bit push origin        # syncs everything (metadata + content)
bit push github        # syncs history only (metadata)
```

### Workflow Summary

| Command | Behavior |
|---|---|
| `bit become-git` | Activates the router -- `git` now dispatches to bit or git |
| `bit become-bit` | Deactivates the router -- restores system `git` |
| `git init` | Always creates a **git** repo |
| `bit init` | Always creates a **bit** repo |
| `git add/commit/...` | Routes to **git** or **bit** based on repo type |
| `git push/pull` | Routes to **git** or **bit** based on repo type |
