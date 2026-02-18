# Git Compatibility and the `git` Executable Router

## Importing Git Repos

For projects that mix binary assets with regular text files (scripts, configs, source code, markdown), bit can **import an existing git repo**. This is the recommended workflow for mixed-content projects.

When you import a git repo, the entire repository — history, branches, the full DAG — is transferred into `.bit/index`, and from that point on bit functions normally. Binary files get metadata-only tracking (hash + size); text files get their content stored directly in the index. Commits, diffs, logs, and merges all work as expected.

```bash
# Initialize bit in an existing git project
cd my-project
bit init          # creates .bit/, moves git history into .bit/index
```

## Text Files: Exact Git Behavior

We designed bit to **pass all of git's tests** for text file handling. Bit handles text files **exactly** like git does — the content lives in `.bit/index` and goes through the same add/commit/diff/merge cycle. All of git's features work exactly the same: submodules, separating the git directory into another folder, ignoring whitespace. Everything just works. We achieve this by cleverly passing through to git when we can, and handling the cases where bit has to meddle. This means you can use bit on scripts, configuration files, source code, documentation, or any text-based workflow **without** any behavioral differences.

Text classification is automatic: files under the configured size limit (default 1 MB) that contain valid UTF-8 and no NULL bytes are treated as text. Binary extensions (`.mp4`, `.zip`, `.exe`, etc.) are always binary regardless of content. The classification is configurable in `.bit/config`.

### Going Back

Even so, you can always export the git repo back with no changes. The underlying git repository stays completely intact inside `.bit/index` — the only additions are small metadata files (hash + size) for binary files, which now point to nothing. The repo itself is unchanged, meaning you can **easily** go back to pure git at any time. There is no lock-in.

## Replacing Git: The `git` Executable Router

For convenience, bit can **totally replace git** on your system. Bit ships a `git` executable that acts as a simple router:

- On **git repos** (directory has `.git/` but no `.bit/`), it calls the real `git`.
- On **bit repos** (directory has `.bit/`), it calls `bit`.

To activate this, run `bit become-git`. This automatically hides the system `git` from the path and installs bit's `git` router in its place. Every `git` command you run will be transparently dispatched to the right tool. To revert, run `bit become-bit` — the system `git` is restored to its original position.

### `git init` vs `bit init`

There is one deliberate asymmetry: **`git init` always initializes a git repo**, even when running through bit's router. This preserves the expectation that `git init` produces a standard git repository. To initialize a bit repo, use `bit init` once — after that, you can go back to using bit's `git` executable for everything:

```bash
# Initialize a bit repo (must use bit init)
bit init

# Activate the git router
bit become-git

# From here on, git commands are routed transparently
git add .
git commit -m "first commit"
git remote add origin git@github.com:user/project.git
git push -u origin main

# To go back to using system git directly
bit become-bit
```

### Workflow Summary

|Command|Behavior|
|---|---|
|`bit become-git`|Activates the router — `git` now dispatches to bit or git|
|`bit become-bit`|Deactivates the router — restores system `git`|
|`git init`|Always creates a **git** repo|
|`bit init`|Always creates a **bit** repo|
|`git add/commit/…`|Routes to **git** or **bit** based on repo type|
|`git push/pull`|Routes to **git** or **bit** based on repo type|

## GitHub and Remote Collaboration

With metadata-only remotes, bit functions normally with GitHub, GitLab, and any native git host. Pushing, pulling, cloning — all work through the standard git transport. The metadata-only remote syncs the `.bit/index` history (file names, hashes, sizes, commit messages, the full DAG) using native git protocols. Collaborators see the full project structure and history on GitHub; they add a content remote (Google Drive, S3, etc.) to get the actual binary files.

```bash
bit init
bit remote add origin gdrive:Projects/foo            # full — content lives here
bit remote add github git@github.com:user/foo.git    # metadata only (auto-detected)

bit add .
bit commit -m "initial commit"
bit push origin        # syncs everything (metadata + content)
bit push github        # syncs history only (metadata)
```

Or, after `bit become-git`:

```bash
git add .
git commit -m "initial commit"
git push origin
git push github
```

Both workflows are identical — the router dispatches to bit transparently.