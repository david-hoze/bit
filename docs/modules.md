# Module Reference

Every module in the project, grouped by architectural layer.

## Entry Point

| Module | Path | Purpose |
|--------|------|---------|
| *(main)* | `Bit.hs` | Sets UTF-8 encoding, delegates to `Bit.Commands.run` |

## Command Layer

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Commands` | `Bit/Commands.hs` | CLI argument parser and command dispatcher |
| `Bit.Help` | `Bit/Help.hs` | Help text and command documentation |

## Core Business Logic (`Bit.Core.*`)

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Core` | `Bit/Core.hs` | Facade re-exporting all core operations |
| `Bit.Core.Init` | `Bit/Core/Init.hs` | Repository initialization (local and remote) |
| `Bit.Core.Push` | `Bit/Core/Push.hs` | Push: bundle creation and file sync |
| `Bit.Core.Pull` | `Bit/Core/Pull.hs` | Pull: three-way merge and conflict handling |
| `Bit.Core.Fetch` | `Bit/Core/Fetch.hs` | Bundle fetching from cloud and filesystem remotes |
| `Bit.Core.Transport` | `Bit/Core/Transport.hs` | Sync action derivation from `git diff` and working-directory sync |
| `Bit.Core.Verify` | `Bit/Core/Verify.hs` | Verification and repair commands with progress reporting |
| `Bit.Core.RemoteManagement` | `Bit/Core/RemoteManagement.hs` | Remote add/show and device-name prompting |
| `Bit.Core.GitPassthrough` | `Bit/Core/GitPassthrough.hs` | Git command passthrough (add, commit, diff, log, merge, etc.) |
| `Bit.Core.Helpers` | `Bit/Core/Helpers.hs` | Shared helpers for Core modules (git queries, monadic utilities) |

## Domain Types and Data

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Types` | `Bit/Types.hs` | Core domain types: `Path`, `Hash`, `FileEntry`, `EntryKind`, `BitM` monad |
| `Bit.Path` | `Bit/Path.hs` | Path newtypes (`RemotePath`) for compile-time path semantics |
| `Bit.Remote` | `Bit/Remote.hs` | Opaque `Remote` type with smart constructor and resolution |
| `Bit.Plan` | `Bit/Plan.hs` | Sync action planning and swap detection |
| `Bit.Conflict` | `Bit/Conflict.hs` | Merge conflict resolution and conflict-marker detection |

## Infrastructure (`Bit.Internal.*`)

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Internal.Config` | `Bit/Internal/Config.hs` | Path constants and configuration (`bitDir`, `bitIndexPath`, bundle names) |
| `Bit.Internal.ConfigFile` | `Bit/Internal/ConfigFile.hs` | Text-file classification config reading |
| `Bit.Internal.Git` | `Bit/Internal/Git.hs` | Git command abstraction (**only** module that calls `git`) |
| `Bit.Internal.Transport` | `Bit/Internal/Transport.hs` | Rclone transport layer (**only** module that calls `rclone`) |
| `Bit.Internal.Metadata` | `Bit/Internal/Metadata.hs` | Metadata file serialization, parsing, and hash computation |

## Scanning and Verification

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Scan` | `Bit/Scan.hs` | Working-directory scanning, metadata file I/O, and file hashing |
| `Bit.Verify` | `Bit/Verify.hs` | Local and remote verification against metadata with concurrent hashing |
| `Bit.Fsck` | `Bit/Fsck.hs` | `git fsck` wrapper for metadata repository integrity checks |
| `Bit.Remote.Scan` | `Bit/Remote/Scan.hs` | Remote file listing via rclone JSON output |

## Device and Remote Workspace

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Device` | `Bit/Device.hs` | Device-identity-based remote resolution (UUID/serial) |
| `Bit.DevicePrompt` | `Bit/DevicePrompt.hs` | Interactive device-name prompting for filesystem remotes |
| `Bit.RemoteWorkspace` | `Bit/RemoteWorkspace.hs` | Direct remote repository operations (init, add, commit, status, log, ls-files) |

## Concurrency and I/O

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Concurrency` | `Bit/Concurrency.hs` | Bounded parallel execution with CPU-scaled limits |
| `Bit.ConcurrentIO` | `Bit/ConcurrentIO.hs` | Type-safe concurrent I/O newtype (strict operations only) |
| `Bit.ConcurrentFileIO` | `Bit/ConcurrentFileIO.hs` | Strict, concurrent-safe file I/O (no lazy I/O) |
| `Bit.AtomicWrite` | `Bit/AtomicWrite.hs` | Atomic file writes with Windows retry logic and directory locking |
| `Bit.Process` | `Bit/Process.hs` | Strict process output capture (prevents lazy-I/O handle issues) |

## Progress and Display

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Progress` | `Bit/Progress.hs` | Centralized progress reporting for terminal operations |
| `Bit.CopyProgress` | `Bit/CopyProgress.hs` | Progress reporting for rclone file copies via JSON log parsing |

## Utilities

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Platform` | `Bit/Platform.hs` | Platform-safe directory/file wrappers handling UNC paths on Windows |
| `Bit.Utils` | `Bit/Utils.hs` | Path helpers, formatting, and atomic-write re-exports |

## Test Executables

| Module | Path | Purpose |
|--------|------|---------|
| *(main)* | `test/RunCliTests.hs` | Runs CLI integration test suite |
| *(main)* | `test/RunCliTestsFast.hs` | Runs fast subset of CLI tests |
| *(main)* | `test/DevicePromptTests.hs` | Unit tests for device prompting |
| *(main)* | `test/PlanSpec.hs` | Unit/property tests for sync planning |
| *(main)* | `test/MergeSpec.hs` | Unit tests for merge logic |
| *(main)* | `test/LintTestFiles.hs` | Lints test file structure |

## Scripts

| Module | Path | Purpose |
|--------|------|---------|
| *(main)* | `scripts/GenerateLiterate.hs` | Generates literate documentation from source |
