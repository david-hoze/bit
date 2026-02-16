# Layout × Mode test coverage

| Layout | Mode  | Add | Push | Pull | Merge | Conflicts | Test file |
|--------|-------|-----|------|------|-------|-----------|-----------|
| **Full** | lite  | ✓   | ✓    | ✓    | ✓     | ✓ (--manual-merge) | layout-mode-matrix.test |
| **Full** | solid | ✓   | ✓    | ✓    | ✓     | ✓ (--manual-merge) | layout-mode-matrix.test |
| **Bare** | lite  | ✓   | ✓    | ✓    | —     | —     | bare-remote.test (add/show), bare-push-pull.test (push/pull) |
| **Bare** | solid | ✓   | ✓    | ✓    | —     | —     | bare-remote.test (add/show), bare-push-pull.test (push/pull) |

- **bare-remote.test**: `bit remote add` (full default, `--bare`), `bit remote show` (Remote/Type/Target/Layout), usage, filesystem + `--bare` warning. No push/pull.
- **layout-mode-matrix.test**: Full layout only (filesystem remote). Lite and solid: init, config core.mode, add remote, add files, commit, push, second repo fetch/pull (checkout), merge (both sides commit then pull), manual-merge (divergence/conflict path).
- **bare-push-pull.test**: Requires rclone remote **gdrive-test**. Bare + lite: push, pull. Bare + solid: push, pull. Uses `gdrive-test:bit-test-bare`.
- **proof-of-possession.test**: Full (filesystem) verification on push/pull, --accept-remote, --manual-merge bypass.
- **filesystem-remote-direct.test**, **filesystem-manual-merge.test**, **merge-local.test**, **gdrive-remote.test**: Additional full layout flows (direct at remote, manual merge, merge scenarios, cloud full).

**Checkout**: First pull performs checkout (and sync); layout-mode-matrix and bare-push-pull verify by checking file content after pull.

**Run bare-push-pull.test**: `shelltest test/cli/bare-push-pull.test` (with `gdrive-test` configured). Not included in cli-fast (no cloud).

**e2e-insane.test**: Full user workflows (filesystem remotes only). Laptop/PC: init, add, commit, verify, push, pull; lite/solid mode switching; repair (local + remote); missing file recovery via repair; remote verify after corrupt; two remotes; push verification (missing file blocks push); divergence and pull --manual-merge. Included in cli-fast.
