# Tutorials

## Getting Started

- [Getting Started](getting-started.md) — Init a repo, add files, commit, branch, merge, and other everyday commands
- [Configuration](config.md) — Read and write repo-local config (modes, CDC parameters)

## Remotes and Sync

- [Cloud Remotes](cloud-remotes.md) — Push and pull binary files to cloud storage via rclone
- [Filesystem Remotes](filesystem-remotes.md) — Sync to USB drives, NAS, and local paths
- [Bare Remotes](bare-remotes.md) — CAS-only remotes for efficient storage without full file layout
- [Metadata Remotes](metadata-remotes.md) — Lightweight remotes that sync only git metadata
- [Remote Commands](remote-commands.md) — Run bit commands against a remote without a local clone

## Storage

- [Modes and CAS](modes-and-cas.md) — Lite vs solid mode, the content-addressed store, and restoring old versions

## Collaboration

- [Conflict Resolution](conflict-resolution.md) — Handle divergence: accept-remote, force push, manual merge
- [Submodules](submodules.md) — Nest git and bit repos as submodules

## Verification

- [Verify and Repair](verify-and-repair.md) — Check file integrity and repair from remotes

## Git Interop

- [Git Import](git-import.md) — Convert an existing git repo to bit
- [Git Export](git-export.md) — Convert a bit repo back to git
- [Git Router](git-router.md) — Use bit as a drop-in replacement for git
