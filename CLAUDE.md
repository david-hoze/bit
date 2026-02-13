# Claude Code Guidelines for bit

## Build Tool

This project uses **cabal** exclusively. Never use stack.

```bash
cabal build          # Build
cabal install        # Install
cabal test           # Run tests
```

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

### Architecture Boundaries
- Never call `git` via `rawSystem` or `readProcessWithExitCode` outside `Internal/Git.hs`
- Never call `rclone` outside `Internal/Transport.hs`
- Use `Git.runGitRaw`, `Git.runGitRawAt`, or `Git.runGitAt` instead
- `.bit/index/` files are mutable working state — every scan overwrites them. To read what the user **committed**, use git (`git diff`, `git show HEAD:<path>`), not the filesystem files.

### Key Project Types
- `BitM` = `ReaderT BitEnv IO` — main monad
- `Path` — newtype over FilePath, unwrap with `unPath`
- `Remote` — opaque type, use `remoteName`/`remoteUrl`
- `FileEntry` — tracked file with `Path` + `EntryKind`

## Testing

### Test vs Code Correctness
When tests fail, analyze which is wrong — the test or the implementation. Fix the wrong one, not the convenient one.

### CLI Tests
- Use shelltest Format 3 syntax
- Each directive (`<<<`, `>>>`, `>>>2`, `>>>=`) appears at most once per test case
- All test directories under `test\cli\output\`
- Use `timeout /t 1 >nul &` before `rmdir` on Windows

### Running Tests
```bash
cabal install --overwrite-policy=always
shelltest test/cli/specific.test --debug                       # Single test
powershell -ExecutionPolicy Bypass -File test/cli/run-parallel.ps1   # Full suite (parallel, including cloud)
```

### Cloud Remote Tests
- `gdrive-test` is a configured rclone remote for cloud tests (`test/cli/gdrive-remote.test`)
- Cloud tests are included in the parallel test runner — do NOT exclude them

## Commit Messages

- Always `git pull` before committing
- Imperative mood: "Add feature" not "Added feature"
- Concise: under 72 characters
- Examples: `Add safe.directory config for USB drives`, `Fix memory leak in pipeline`

## After Implementation

1. Build: `cabal install --overwrite-policy=always`
2. Add tests following `test/cli/` patterns
3. Update `docs/spec.md` if behavior changed
4. Provide a commit message (do not offer to commit — the user will commit)
