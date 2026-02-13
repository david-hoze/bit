# Verify Parallelism Benchmark

Measured wall-clock and hashing time for `bit verify` with sequential vs parallel file hashing.

## Setup

- Platform: Windows 10 (MINGW64)
- Date: 2026-02-13
- Files: random binary data, created with `dd if=/dev/urandom`
- Each configuration: 3 runs, cold cache (`.bit/cache/` cleared between runs)
- Sequential: `bit verify --sequential` (single-threaded hashing)
- Parallel: `bit verify` (auto-detect core count, `Parallel 0`)

## Results

### 500 files, 28 MB

| Mode | Hashing time | Wall clock (avg) |
|---|---|---|
| Sequential | 1,000 ms | 2.42 s |
| Parallel | 193 ms | 1.38 s |

Hashing speedup: **5.2x**. Wall-clock speedup: **1.75x**.

### 1000 files, 1 GB

| Mode | Hashing time | Wall clock (avg) |
|---|---|---|
| Sequential | 6,300 ms | 8.0 s |
| Parallel | 642 ms | 2.25 s |

Hashing speedup: **9.8x**. Wall-clock speedup: **3.6x**.

## Analysis

- Parallelism scales well with dataset size — hashing speedup goes from 5x to 10x as data grows from 28 MB to 1 GB.
- Fixed overhead (~1.5 s) comes from git operations (`read-tree`, `diff`, metadata comparison) that are not parallelized. This dominates on small datasets but becomes negligible on large ones.
- With warm cache (all files cached), both modes take ~1.5 s — confirming the bottleneck shifts entirely to git operations when no hashing is needed.
