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
shelltest test/cli/specific.test --debug    # Single test
shelltest test/cli --debug                  # Full suite
```

## Commit Messages

- Imperative mood: "Add feature" not "Added feature"
- Concise: under 72 characters
- Examples: `Add safe.directory config for USB drives`, `Fix memory leak in pipeline`

## After Implementation

1. Build: `cabal install --overwrite-policy=always`
2. Add tests following `test/cli/` patterns
3. Update `docs/spec.md` if behavior changed
4. Provide commit message
