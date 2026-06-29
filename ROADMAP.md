# bit roadmap

Direction distilled from a competitive/adoption research pass (2026). See
[`docs/bit-vs-lore.md`](docs/bit-vs-lore.md) for the deep Lore comparison.

## Positioning — what bit is and isn't

**The moat is the architecture, not the algorithms.** bit's content-defined
chunking (FastCDC), BLAKE3 hashing, and content-addressed store with GC are now
*table stakes* — HuggingFace Xet, Epic's Lore, and the open-source tool `dits`
all independently use the same primitives. Continuing to invest there buys
parity, not advantage.

bit's genuine, defensible differentiator is **serverless: "git + rclone, any
backend, zero server-side software."** It is the explicit opposite of Lore
(centralized by design) and structurally distinct from Xet (global cross-user
dedup via a central service). Lead with serverless / offline / any-backend /
git-compatible-by-construction.

**Deliberate non-goals** (taking these would lose the edge): a centralized
server-of-record; global cross-user dedup (needs a coordinating service); and
sparse *workspaces* that don't materialize real files. bit can still do
cross-repo dedup within a shared CAS bucket.

## Audiences

- **ML / research-data** → alternative to **DVC** (which struggles past
  ~hundreds of thousands of objects): pitch offline, any-backend.
- **Indie / small game studios** → alternative to **Perforce** (expensive, no
  offline) and **Git LFS** (tedious `.gitattributes`, weak locking): pitch free,
  serverless, real file locking.

## Now (quick wins)

- [x] **LICENSE = MIT** — most-adopted permissive license; required for the repo
      to be legally open source.
- [x] **CI** (GitHub Actions, Linux): build + unit gate, plus binary & non-cloud
      CLI suites. Reproducible builds pinned via `cabal.project.freeze`.
- [ ] **Tagged `v*` releases with prebuilt binaries** (this is the single biggest
      adoption lever): cross-platform archives attached to a GitHub Release. See
      `.github/workflows/release.yml`.
- [ ] **A reproducible bit-vs-Git-LFS benchmark** on an evolving-binary workload
      (storage saved, push/pull speedup). No bit-specific numbers exist yet;
      every comparison claim needs this to stand on.

## Next (growth)

- [ ] **Comparison pages** vs Git LFS / Xet / Lore / dits / Perforce — emphasize
      zero-infrastructure, offline, any-backend.
- [ ] **Launch**: Show HN (Tue–Thu, with a prepared first comment), cross-post
      r/programming + r/selfhosted, Lobsters. Pick one lead audience per launch.
- [ ] **Packaging**: Homebrew tap, Scoop manifest, Nix flake (none exist yet).
- [ ] **CONTRIBUTING.md** and a short docs site / landing page.

## Later (deeper features, by value × philosophy-fit)

- [ ] **Offline-robust sparse / partial checkout** — fetch only-needed blobs from
      any rclone backend *without a server*. This is the exact Git weakness Lore
      documents ("sparse+partial … experimental … fails offline"); bit's
      serverless design can turn it into a strength. Highest-value; feasibility
      within "never reimplement git" is the key open question.
- [ ] **Mature the binary-asset locking** (`bit lock`/`unlock`/`locks` + push
      enforcement already shipped) — a top game-artist demand. Invest in UX and a
      GUI/IDE path; consider the race-free variant backed by a git locks-ref.
- [ ] **`bit cas verify` / scrub** — re-hash CAS blobs against their
      content-addressed filenames to detect bit-rot.
- [ ] **Commit/content signing** via git's native GPG/SSH signing.
- [ ] **Partial / shallow history** via git's `--filter`/`--depth` for long-lived
      repos.

## Code-health follow-ups

- [ ] Generalize the FastCDC byte-identical gate into broader QuickCheck
      properties + fuzzing over random byte streams (esp. the bounded-buffer
      streaming chunker).
- [ ] Extend CI to a Windows/macOS matrix (bit has heavy Windows-specific paths).
- [ ] Consider a static musl/Alpine Linux binary for distribution portability
      (glibc binaries couple to the build host's libc version).
- [x] **macOS (Apple Silicon) release binary** — fixed in v0.1.1. `blake3-0.3`
      auto-references a NEON impl it never compiles, failing to link on arm64.
      `crypton` has no BLAKE3, so the package was made to build by forcing the
      portable path with `ghc-options: -optc-DBLAKE3_USE_NEON=0` (cabal
      `cc-options` did not reach the store-built dependency; `ghc-options` does).

## Open questions (from the research)

1. What chunk-dedup ratio and push/pull speedup does bit *actually* achieve vs
   Git LFS? (Needs the benchmark above.)
2. Can offline sparse/partial checkout cooperate with bit's index-mirror model,
   or does bit need its own sparse layer?
3. What's the realistic effort to build/distribute reliably on macOS and Windows?
4. Which lead audience (ML vs game-dev) converts best at launch?
