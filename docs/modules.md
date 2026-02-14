# Module Reference

Every module in the project, grouped by architectural layer.

## Entry Point

| Module | Path | Purpose |
|--------|------|---------|
| *(main)* | `Bit.hs` | Sets UTF-8 encoding, delegates to `Bit.Commands.run` |

## Command Layer

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Commands` | `Bit/Commands.hs` | CLI argument parser, `findBitRoot` repo discovery, prefix computation, command dispatcher |
| `Bit.Help` | `Bit/Help.hs` | Help text and command documentation |

## Core Business Logic (`Bit.Core.*`)

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Core` | `Bit/Core.hs` | Facade re-exporting all core operations |
| `Bit.Core.Init` | `Bit/Core/Init.hs` | Repository initialization (local and remote) |
| `Bit.Core.Push` | `Bit/Core/Push.hs` | Push: bundle creation and file sync |
| `Bit.Core.Pull` | `Bit/Core/Pull.hs` | Pull: three-way merge and conflict handling |
| `Bit.Core.Fetch` | `Bit/Core/Fetch.hs` | Bundle fetching from cloud and filesystem remotes |
| `Bit.Core.Verify` | `Bit/Core/Verify.hs` | Verification and repair commands with progress reporting |
| `Bit.Core.RemoteManagement` | `Bit/Core/RemoteManagement.hs` | Remote add/show and device-name prompting |
| `Bit.Core.Conflict` | `Bit/Core/Conflict.hs` | Merge conflict resolution and conflict-marker detection |
| `Bit.Core.Helpers` | `Bit/Core/Helpers.hs` | Shared helpers for Core modules (git queries, monadic utilities) |

## Git Boundary (`Bit.Git.*`)

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Git.Run` | `Bit/Git/Run.hs` | Git command abstraction (**only** module that calls `git`); `runGitRawIn`/`runGitWithOutputIn` for subdirectory prefix dispatch |
| `Bit.Git.Passthrough` | `Bit/Git/Passthrough.hs` | Git command passthrough (add, commit, diff, log, merge, etc.); prefix-aware for subdirectory support |

## Rclone Boundary (`Bit.Rclone.*`)

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Rclone.Run` | `Bit/Rclone/Run.hs` | Rclone transport layer (**only** module that calls `rclone`) |
| `Bit.Rclone.Sync` | `Bit/Rclone/Sync.hs` | Sync action derivation from `git diff` and working-directory sync |
| `Bit.Rclone.Progress` | `Bit/Rclone/Progress.hs` | Progress reporting for rclone file copies via JSON log parsing |

## Domain Types and Data

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Types` | `Bit/Types.hs` | Core domain types: `Path`, `Hash`, `FileEntry`, `EntryKind`, `BitEnv` (with `envPrefix`), `BitM` monad |
| `Bit.Path` | `Bit/Path.hs` | Path newtypes (`RemotePath`) for compile-time path semantics |
| `Bit.Remote` | `Bit/Remote.hs` | Opaque `Remote` type with smart constructor and resolution |
| `Bit.Domain.Plan` | `Bit/Domain/Plan.hs` | Sync action planning and swap detection |

## Configuration (`Bit.Config.*`)

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Config.Paths` | `Bit/Config/Paths.hs` | Path constants and configuration (`bitDir`, `bitIndexPath`, bundle names) |
| `Bit.Config.File` | `Bit/Config/File.hs` | Text-file classification config reading |
| `Bit.Config.Metadata` | `Bit/Config/Metadata.hs` | Metadata file serialization, parsing, and hash computation |

## Scanning and Verification (`Bit.Scan.*`)

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Scan.Local` | `Bit/Scan/Local.hs` | Working-directory scanning, metadata file I/O, and file hashing |
| `Bit.Scan.Verify` | `Bit/Scan/Verify.hs` | Local and remote verification against metadata with concurrent hashing |
| `Bit.Scan.Fsck` | `Bit/Scan/Fsck.hs` | `git fsck` wrapper for metadata repository integrity checks |
| `Bit.Scan.Remote` | `Bit/Scan/Remote.hs` | Remote file listing via rclone JSON output |

## Device and Remote Workspace (`Bit.Device.*`)

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Device.Identity` | `Bit/Device/Identity.hs` | Device-identity-based remote resolution (UUID/serial) |
| `Bit.Device.Prompt` | `Bit/Device/Prompt.hs` | Interactive device-name prompting for filesystem remotes |
| `Bit.Device.RemoteWorkspace` | `Bit/Device/RemoteWorkspace.hs` | Direct remote repository operations (init, add, commit, status, log, ls-files) |

## Concurrency and I/O (`Bit.IO.*`)

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.IO.Concurrency` | `Bit/IO/Concurrency.hs` | Bounded parallel execution with CPU-scaled limits |
| `Bit.IO.ConcurrentIO` | `Bit/IO/ConcurrentIO.hs` | Type-safe concurrent I/O newtype (strict operations only) |
| `Bit.IO.ConcurrentFileIO` | `Bit/IO/ConcurrentFileIO.hs` | Strict, concurrent-safe file I/O (no lazy I/O) |
| `Bit.IO.AtomicWrite` | `Bit/IO/AtomicWrite.hs` | Atomic file writes with Windows retry logic and directory locking |
| `Bit.IO.Process` | `Bit/IO/Process.hs` | Strict process output capture (prevents lazy-I/O handle issues) |
| `Bit.IO.Platform` | `Bit/IO/Platform.hs` | Platform-safe directory/file wrappers handling UNC paths on Windows |

## Progress and Display

| Module | Path | Purpose |
|--------|------|---------|
| `Bit.Progress.Report` | `Bit/Progress/Report.hs` | Centralized progress reporting for terminal operations |

## Utilities

| Module | Path | Purpose |
|--------|------|---------|
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
