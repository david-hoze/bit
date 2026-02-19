# Tutorial: Metadata-Only Remotes

Metadata-only remotes let you share project history through native git hosts (GitHub, GitLab, bare git repos) without uploading binary file content. Only commit history, file names, hashes, sizes, and the DAG structure are synced — the actual files stay where they are.

## When to use metadata-only remotes

- **GitHub/GitLab integration**: Share your project's structure and history on GitHub without uploading large binary files.
- **Lightweight backup of history**: Keep commit history in a bare git repo on a network share without duplicating terabytes of content.
- **Collaboration on metadata**: Multiple contributors can see what files exist, what changed, and when — without each needing all the files locally.
- **Hybrid workflows**: Use a metadata-only remote for history (GitHub) alongside a full remote (Google Drive) for content.

## Quick start: GitHub remote

```bash
# In an existing bit repo
bit remote add github git@github.com:youruser/yourproject.git

# bit auto-detects git URLs and sets up metadata-only
# Output: Remote 'github' added (git@github.com:youruser/yourproject.git, metadata only).

# Push history (no files uploaded — just git metadata)
bit push github

# On another machine
bit init
bit remote add github git@github.com:youruser/yourproject.git
bit pull github
# You now have the full commit history, but no binary files
```

## Quick start: bare git repo

```bash
# Create a bare git repo (e.g., on a network share)
git init --bare //server/share/project.git

# Add it as a remote — bit auto-detects bare git repos
bit remote add origin //server/share/project.git
# Output: Remote 'origin' added (.../project.git, metadata only).

# Push and pull work the same way
bit push origin
```

## Making a cloud remote metadata-only

You can also use `--metadata-only` with cloud remotes:

```bash
bit remote add backup gdrive:Backup/project --metadata-only
# Output: Remote 'backup' added (gdrive:Backup/project, metadata only).

# Push syncs only the git bundle — no file content uploaded
bit push backup
```

## How it works

When you add a git remote (or use `--metadata-only`), bit sets `layout: metadata` in the remote config at `.bit/remotes/<name>`:

```
type: git
target: git@github.com:youruser/yourproject.git
layout: metadata
```

On **push**, bit runs `git push <name> main` — standard git push. No rclone, no file sync, no CAS upload.

On **pull**, bit runs `git fetch <name>`, then merges the fetched history into your local branch. File sync is skipped — your working directory's binary files are untouched.

## Checking remote status

```bash
bit remote show
# github → git@github.com:youruser/yourproject.git (git, metadata only)
# origin → //server/share/project.git (metadata only)

bit remote show github
# Remote: github
# Type: git
# Target: git@github.com:youruser/yourproject.git
# Layout: metadata
```

## What's NOT synced

Metadata-only remotes explicitly skip:
- Binary file content (the actual large files)
- CAS (content-addressed storage) blobs
- Human-readable file tree on the remote

What IS synced:
- Git commits (author, date, message)
- File metadata (paths, hashes, sizes)
- Branch history and DAG structure

## Combining with full remotes

A common pattern is to use metadata-only for history sharing and a full remote for content:

```bash
# History on GitHub (free, fast, collaborative)
bit remote add github git@github.com:team/project.git

# Content on Google Drive (stores actual files)
bit remote add gdrive gdrive:Projects/footage

# Push to both
bit push github    # Just history — instant
bit push gdrive    # History + files — takes longer
```

Team members who only need to see what files exist and what changed can pull from GitHub. Those who need the actual files pull from Google Drive.

## Limitations

- **No file recovery from metadata-only remotes**: Since binary content isn't stored, you can't restore files from a metadata-only remote. Always keep at least one full remote for content backup.
- **Remote workspace commands not supported**: Commands like `bit @github add` or `bit --remote github status` are rejected for metadata-only remotes — there's no content to operate on.
- **`--bare` and `--metadata-only` are mutually exclusive**: `--bare` controls CAS layout for cloud remotes; `--metadata-only` skips content entirely. They serve different purposes and can't be combined.

## Git URL auto-detection

bit recognizes these patterns as git URLs and automatically sets up metadata-only:

| Pattern | Example |
|---------|---------|
| `git@host:path` | `git@github.com:user/repo.git` |
| `ssh://git@host/path` | `ssh://git@example.com/repo.git` |
| `https://github.com/...` | `https://github.com/user/repo.git` |
| `https://gitlab.com/...` | `https://gitlab.com/user/repo` |
| `https://bitbucket.org/...` | `https://bitbucket.org/user/repo` |
| URL ending in `.git` | `https://myserver.com/repos/project.git` |
| Local bare git repo | `/path/to/repo.git` (has HEAD, no .bit/) |
