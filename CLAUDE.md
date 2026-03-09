# Claude Code Guidelines for bit

## Collaboration Mode

When the user says **`collaborate <name>`**, immediately:

1. Read `CLAUDE_COLLAB.md` and follow all instructions there.
2. Run `claude-collab init --name <name>` using the name they provided.
3. Start a background message listener (`claude-collab read $HASH --wait --timeout 600`).

Then proceed with whatever task the user gives you, following the collaboration protocol for all file edits and commits.

## Design Philosophy

**bit = Git(metadata) + rclone(sync).** Use git and rclone as primitives, never reimplement what they already do.

- **Git bundles** for serializing history over dumb cloud storage — one file, full DAG, `git fetch` unpacks it
- **Git native remotes** for filesystem peers — `git fetch /path/to/.bit/index` and `git merge`, like two ordinary repos
- **`git diff --name-status`** for deriving sync actions — no remote file scan, no directory diff algorithm
- **`rclone copy --files-from`** for bulk file transfer — one subprocess, works identically for local and cloud
- **`git merge --no-commit --no-ff`** for three-way merge — git handles the hard work, bit reads the result

**The golden rule:** when you find yourself writing logic that git or rclone already handles, stop and wire into the existing tool instead.

### Layer Contract

```
Commands.hs → Core/*.hs ──→ Rclone/Sync.hs → Rclone/Run.hs → rclone (only here!)
                    │
                    ├──→ Git/Passthrough.hs → Git/Run.hs → git (only here!)
                    │
                    ├──→ Scan/*.hs → Git/Run.hs, Rclone/Run.hs
                    │
                    └──→ Device/Identity.hs
```

- **Bit/Git/Run.hs** — the ONLY place that calls `git`. Use `Git.runGitRaw`, `Git.runGitRawAt`, or `Git.runGitAt`.
- **Bit/Rclone/Run.hs** — the ONLY place that calls `rclone`. Exposes `copyToRemote`, `copyFromRemote`, etc.
- **Bit/Core/*.hs** — all business logic. Never calls `readProcessWithExitCode` directly.
- Push and pull use seam types (`PushSeam`, `PullSeam`) to share one code path across cloud and filesystem remotes — only the metadata transport differs.

### Key Invariants

- **Proof of possession**: a repo must not transfer metadata it cannot back up with actual content. Push verifies only the diff set (files being synced), not the entire working tree. Pull verifies the remote's readable tree before downloading.
- **Index mirror**: `.bit/index/` is a perfect mirror of the working directory — every tracked file is copied byte-for-byte, except binary files which are replaced with metadata stubs. This includes `.gitattributes` and all other dotfiles. Git is the sole authority over `.bit/index/`; after any git operation that changes HEAD, `.bit/index/` is correct by definition.
- `.bit/index/` files are mutable working state — every scan overwrites them. To read what the user **committed**, use git (`git diff`, `git show HEAD:<path>`), not the filesystem files.
- **CAS is only populated by `bit add` in solid mode.** Push never stages files to local CAS — it only uploads CAS blobs that already exist. In lite mode with no CAS data: full-layout remotes sync readable copies, bare-layout remotes have nothing to upload (bare requires solid mode to transfer binary files).
- **Imported remotes are always metadata-only.** `bit import` converts a git repo — all its remotes are git remotes with no bit binary data. Every imported remote is registered as `type: git, layout: metadata`.

### Mode × Layout Matrix

| Mode   | Full layout remote | Bare layout remote |
|--------|--------------------|--------------------|
| **Lite**   | Push uploads readable copies (no CAS) | **No binary sync** — no CAS, no readable paths |
| **Solid**  | Push uploads CAS blobs + readable copies | Push uploads CAS blobs |

Bare remotes store files exclusively in CAS (`cas/<prefix>/<hash>`). Without solid mode populating local CAS, there is nothing to upload — bare+lite is effectively metadata-only for binary files.

## Build Tool

This project uses **cabal** exclusively. Never use stack.

```bash
cabal build          # Build
cabal install        # Install
cabal test           # Run tests
```

### Troubleshooting: `cabal build` says "Up to date" after editing source

`cabal install` builds from an sdist tarball and caches the result in the package store by hash. If the tarball hash matches a previous build, cabal skips recompilation entirely — even if the source files on disk have changed. Symptoms: `cabal build` says "Up to date", `cabal install` goes straight from "Wrote tarball" to "Copying" with no "Building" step.

**Fix**: Use `cabal exec -- ghc` to build directly from source files, bypassing the store:

```bash
cabal exec -- ghc --make -fforce-recomp -O -o bit-new.exe Bit.hs
cp bit-new.exe "$(where bit)"
```

This compiles all modules from the working directory (not the tarball) and produces a fresh binary. Copy it over the installed `bit.exe`.

**Alternative**: Delete the store entries and dist-newstyle, then `cabal install`:

```bash
rm -rf dist-newstyle
rm -rf "$(cabal path --store-dir)/ghc-9.6.7/bit-0.1.0.0-"*
cabal install --overwrite-policy=always
```

### Troubleshooting: Bash returns exit code 1 with no output

On Windows (MSYS2/PortableGit), bash can enter a state where all commands silently fail with exit code 1. The error is `write error: Bad file descriptor` — bash can't write to stdout because the pipe from the parent process is broken.

**Diagnosis**: Run bash via cmd and capture stderr to a file:
```cmd
cmd //c "C:\Users\natanh\tools\PortableGit\usr\bin\bash.exe -c "echo hello" 2>C:\Users\natanh\bash_err.txt & type C:\Users\natanh\bash_err.txt"
```
If you see `write error: Bad file descriptor`, this is the issue.

**Workaround**: Use a batch file wrapper that redirects bash output to a file, then `type` the file:
```bat
@echo off
C:\Users\natanh\tools\PortableGit\usr\bin\bash.exe --norc --noprofile -c "%*" 1>C:\Users\natanh\bout.txt 2>C:\Users\natanh\berr.txt
set EXITCODE=%ERRORLEVEL%
type C:\Users\natanh\bout.txt
type C:\Users\natanh\berr.txt 1>&2
exit /b %EXITCODE%
```
Save as `C:\Users\natanh\run.bat` and invoke via `cmd //c "C:\Users\natanh\run.bat <bash command>"`.

**Root cause**: Unknown — may be triggered by disk-full conditions or long-running MSYS2 sessions. Restarting the terminal sometimes fixes it; the batch file workaround always works.

### Working with multiple agents (parallel workers)

When multiple Claude Code agents are working in this repo, follow `CLAUDE_COLLAB.md` for coordination — file claims, shared commits, resource reservations, and messaging.

When another worker is also modifying code and using the global `bit.exe`, avoid interfering by building into a local `dev-bin/` directory and prepending it to PATH:

```bash
# Build into a separate binary (not bit.exe)
cabal exec -- ghc --make -fforce-recomp -O -o bit-dev.exe Bit.hs
cabal exec -- ghc --make -fforce-recomp -O -o bit-git-router-dev.exe BitGitRouter.hs

# Copy to dev-bin/ as bit.exe and bit-git-router.exe
mkdir -p dev-bin
cp bit-dev.exe dev-bin/bit.exe
cp bit-git-router-dev.exe dev-bin/bit-git-router.exe

# Prepend to PATH in every shell command (shell state doesn't persist between calls)
export PATH="/c/Users/natanh/repos/bit/dev-bin:$PATH"
```

Since shell state doesn't persist between Bash tool calls, **every command** must start with the `export PATH=...` prefix. For shelltest runs:

```bash
export PATH="/c/Users/natanh/repos/bit/dev-bin:$PATH" && cd /c/Users/natanh/repos/bit && shelltest test/cli/some.test
```

The `dev-bin/` directory is gitignored. After changes are validated, install globally with `cabal install --overwrite-policy=always` or copy `dev-bin/bit.exe` over the global one.

## Haskell Coding Standards

Consult these docs when modifying `.hs` files:
- `docs/haskell-type-safety.md` — newtypes, sum types, totality
- `docs/idiomatic-haskell.md` — combinators, do-notation, DRY

### Type Safety Checklist
- Pattern match on sum types, never compare with `==`
- No partial functions: `head`, `tail`, `fromJust`, `read`, `!!`, `Map.!`
- Use `newtype` not `type` for domain concepts
- Use custom sum types instead of `Bool` parameters

### Idiomatic Style
- `traverse_ f mx` not `maybe (pure ()) f mx`
- `void action` not `_ <- action`
- `pure` not `return`
- `foldl'` never lazy `foldl`
- Consolidate multiple `liftIO` calls into `liftIO $ do`

### Key Project Types
- `BitM` = `ReaderT BitEnv IO` — main monad
- `Path` — newtype over FilePath, unwrap with `unPath`
- `Remote` — opaque type, use `remoteName`/`remoteUrl`
- `FileEntry` — tracked file with `Path` + `EntryKind`

## Testing

### Test vs Code Correctness
When tests fail, analyze which is wrong — the test or the implementation. Fix the wrong one, not the convenient one.

### CLI Tests
- Follow `.cursor/rules/creating-cli-tests.mdc` for format, directory naming, cleanup, and output matching
- Use shelltest Format 3 syntax
- Each directive (`<<<`, `>>>`, `>>>2`, `>>>=`) appears at most once per test case
- All test directories under `test\cli\output\`
- Use `timeout /t 1 >nul &` before `rmdir` on Windows
- **Use `&&` (not `&`) after `cd`** — `cd dir & bit cmd` runs `bit cmd` even if `cd` fails (cmd.exe quirk), which leaks into the parent repo. Always: `cd test\cli\output\work_dir && bit cmd`

### Running Tests

**Run order matters:** Always run CLI and binary suites **before** the git suite.
The git suite spawns hundreds of concurrent processes that cause I/O contention on
Windows, which can make binary tests flaky. Run them sequentially:

1. CLI tests (~2 min)
2. Binary tests (~1 min)
3. Git suite with 4 team agents (~30 min)

```bash
cabal install --overwrite-policy=always
bash test/cli/run.sh                                                # 1. CLI suite
cd test/t && bash run-tests.sh                                       # 2. Binary suite
# 3. Git suite — see "Git Test Suite" section below
```

For single tests:
```bash
shelltest test/cli/specific.test --debug                       # Single CLI test
cd test/t && bash t0001-binary-add-commit.sh                   # Single binary test
```

### Binary File Test Suite
- Tests bit's binary file handling: classification, metadata, push/pull, merge, stash, verify, reset, cherry-pick, rebase, revert, grep, diff/rename, attributes, format-patch/am
- Location: `test/t/` — 14 bash scripts, 222 tests
- Consult `docs/binary-test-suite.md` for details and how to add new tests

### Git Test Suite
- Consult `docs/git-test-suite.md` for setup, environment variables, and how the router works
- Consult `docs/git-test-suite-efficiency.md` for batch splits, skip reference, and the throttled runner
- One-time setup: `extern/git-shim/setup.sh` then `bit become-git --init`
- **Always use the throttled runner** (`extern/run-throttled-suite.sh`) — never raw for-loops.
  It handles special timeouts, skip prefixes, load-aware throttling, trash cleanup, and result tracking.
- **Full suite** — use 4 parallel background Bash tasks (NOT agents) with the batch split.
  Clean trash directories first, then launch each batch with `run_in_background: true`:
  ```bash
  # Clean first
  rm -rf extern/git/t/trash-directory.* 2>/dev/null

  # Launch 4 background Bash tasks (run_in_background: true, timeout: 600000):
  cd /path/to/bit/extern && RESULTS_FILE=git-suite-batch-a.txt bash run-throttled-suite.sh t0*.sh t1*.sh t2*.sh
  cd /path/to/bit/extern && RESULTS_FILE=git-suite-batch-b.txt bash run-throttled-suite.sh t3*.sh t4*.sh
  cd /path/to/bit/extern && RESULTS_FILE=git-suite-batch-c.txt bash run-throttled-suite.sh t5*.sh
  cd /path/to/bit/extern && RESULTS_FILE=git-suite-batch-d.txt bash run-throttled-suite.sh t6*.sh t7*.sh t8*.sh t9*.sh
  ```
  **Do NOT use Agent tool** — use the Bash tool with `run_in_background: true` for each batch.
  Agents add unnecessary overhead; background Bash tasks run the same commands directly.
  The throttled runner's semaphore (`MAX_SLOTS=2`) ensures at most 2 heavy tests run concurrently
  across all 4 batches, preventing I/O contention that causes false timeouts on Windows.
  Lightweight tests (<25 test cases) run freely without throttling.
- **Rerunning timeouts** — collect timed-out scripts from results files and rerun with 2
  background Bash tasks (not 4 — all timeout scripts are heavy, so match to `MAX_SLOTS=2`):
  ```bash
  cd /path/to/bit/extern && DEFAULT_TIMEOUT=600 RESULTS_FILE=rerun-1.txt bash run-throttled-suite.sh <scripts...>
  cd /path/to/bit/extern && DEFAULT_TIMEOUT=600 RESULTS_FILE=rerun-2.txt bash run-throttled-suite.sh <scripts...>
  ```
- **Single test** — run directly:
  ```bash
  cd extern/git/t
  BIT_GIT_JUNCTION=1 GIT_TEST_INSTALLED=/c/Users/natanh/repos/bit/extern/git-shim \
      bash t0001-init.sh --verbose 2>&1 | tee ../../t0001-results-latest.txt
  ```
- Use `--run=N` to run a single test number within a script
- **Always save results to a file** (avoids re-running expensive tests)
- Analyze results from the saved file, not by re-running

### Cloud Remote Tests
- `gdrive-test` is a configured rclone remote for cloud tests (`test/cli/gdrive-remote.test`)
- Cloud tests are included in the parallel test runner — do NOT exclude them

### Network / Slow-Device Tests
- See `docs/network-test-benchmark.md` for full analysis
- **Never run bit/git directly on a slow device** (UNC, USB, NAS) — git's many small I/Os are 10-50x slower over high-latency links
- **Use `rclone copy` to stage a bit repo** on the device instead of `bit push` for test setup — batches all `.bit/index/.git/` files in one pass
- **Use `rclone purge`** for cleanup instead of `rmdir /s /q` — faster over network paths
- **Only test transport-layer operations** (push, verify, repair) against slow devices — merge, conflict resolution, status, and multi-repo workflows are already covered by fast local filesystem tests
- **Env vars**: `BIT_TEST_NETWORK_BASE` (path to slow device) and `BIT_TEST_NETWORK_TYPE` (label: `RDP`, `USB`, `NAS`, etc.) — set in shell profile
- **When documenting network test results**, always include the network type from `BIT_TEST_NETWORK_TYPE` (visible in the first test case output). Tag results sections/tables by network type so different transport benchmarks are distinguishable.

## Commit Messages

- Always `git pull` before committing
- Imperative mood: "Add feature" not "Added feature"
- Concise: under 72 characters
- Examples: `Add safe.directory config for USB drives`, `Fix memory leak in pipeline`

## Implementation Reference

Consult `docs/spec/implementation-status.md` before making structural changes. It contains the current type table and module map — use it to stay consistent with existing types, avoid duplicating definitions, and orient yourself in the codebase. Update it when you add or rename types, modules, or significant interfaces.

## After Implementation

1. Build: `cabal install --overwrite-policy=always`
2. Add tests that test the feature, follow `.cursor/rules/creating-cli-tests.mdc`
3. Test the feature and fix if necessary, follow `.cursor/rules/testing-principles.mdc`
4. Run CLI suite, then binary suite, then git suite (in that order — see "Running Tests")
5. Update `docs/spec/` accordingly (see `docs/spec/index.md` for file layout)
6. Review the entire implementation process — if you introduced bugs, follow `.cursor/rules/review.mdc`
7. Give a commit message, follow `.cursor/rules/commit-messages.mdc`
