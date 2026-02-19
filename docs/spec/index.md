# bit Specification

This directory contains the complete specification for bit, organized by feature area.

## Overview

- [Overview](overview.md) -- Context, vision, design philosophy, and comparisons with alternatives
- [Architecture](architecture.md) -- Directory structure, module layout, IO safety, performance, subdirectory support

## Core Features

- [Init](init.md) -- Repository initialization, bare init, separated repos, git configuration
- [File Tracking](file-tracking.md) -- Metadata file format, file handling, CLI command mapping
- [Modes and CAS](modes-and-cas.md) -- Lite/solid mode configuration, content-addressed store, config command, index invariant
- [Content-Defined Chunking](cdc-spec.md) -- FastCDC algorithm for efficient large-file deduplication

## Remotes and Sync

- [Remotes](remotes.md) -- Remote types, device resolution, remote state classification, upstream tracking
- [Push and Pull](push-pull.md) -- Sync architecture, transport strategies, unified push/pull, seam pattern
- [Remote Workspace](remote-workspace.md) -- Remote-targeted commands (`@<remote>` / `--remote`)
- [Metadata-Only Remotes](metadata-remotes.md) -- Git-native remotes, metadata-only layout

## Integrity and Conflicts

- [Verify and Repair](verify-repair.md) -- Proof of possession, verification, repair, fsck
- [Conflict Resolution](conflict-resolution.md) -- Merge conflicts, remote divergence handling

## Git Integration

- [Git Compatibility](git-compatibility.md) -- Git executable router, alias expansion, passthrough, repo discovery
- [Git Test Suite](git-test-suite.md) -- Git shim, junction mode, test results
- [Binary File Test Suite](../binary-test-suite.md) -- Binary-specific tests: classification, metadata, push/pull, merge, verify
- [Git Import](git-import.md) -- Converting git repos to bit repos
- [Git Export](git-export.md) -- Converting bit repos back to git repos

## Extensions

- [Submodule Support](submodule-support.md) -- Git and bit subrepos, two-flow architecture

## Reference

- [Design Decisions](design-decisions.md) -- Architectural choices, guardrails, deliberate non-features
- [Implementation Status](implementation-status.md) -- Current state, module map, test infrastructure
