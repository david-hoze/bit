# Large-file testing on memory-constrained machines (WSL OOM hazard)

## TL;DR

**Do not create or copy multi-GB test files on a machine whose total RAM is
comparable to the file size.** On WSL2 specifically this can hang or crash the
whole VM — not just the test process. Size large-file fixtures to a fraction of
available RAM, and cap subprocess heaps with the GHC RTS so a regression aborts
cleanly instead of taking the machine down.

## What happened

While validating the streaming `chunkFile` rewrite (see
[`bit-vs-lore.md`](bit-vs-lore.md)), we built a **4 GB** fixture file on a WSL2
VM with only **3.7 GB of total RAM**, via:

```bash
head -c 67108864 /dev/urandom > seed.bin     # 64 MiB seed
for i in $(seq 1 64); do cat seed.bin >> big.bin; done   # 64 × 64 MiB = 4 GiB
```

Writing a file larger than RAM floods the Linux **page cache**: every written
page is retained as reclaimable cache. WSL2's dynamic-memory manager does not
always reclaim or balloon fast enough under that pressure, so the VM ran out of
usable memory and became unresponsive. **The crash was caused by writing the
fixture, before the chunker ever ran** — the act of materializing a
larger-than-RAM file was enough.

A secondary, smaller-scale repeat of the same lesson: the first run of the smoke
test then hit `Heap exhausted` at a 512 MB cap — but that was a **lazy-MD5 space
leak in the test harness itself** (accumulating `MD5.update` thunks retained
every buffer read), not in `chunkFile`. Forcing the context strictly (`!ctx`)
fixed it. The chunker's own residency stayed at ~2.5 MB.

## Why a multi-GB file was unnecessary anyway

The property under test is *"peak memory is bounded (~2×maxSize) regardless of
file size."* You prove that by showing **residency ≪ file size and structurally
independent of it**, which a 1 GB file demonstrates just as conclusively as a
4 GB one — at a fraction of the risk. The actual result on a 1 GiB file:

```
file size: 1073741824 bytes
chunks produced: 10176
tiling OK: chunks cover [0, fileSize) contiguously
HASH MATCH: reassembled stream matches whole file
   2,565,928 bytes maximum residency   # ~2.5 MB for a 1 GB file
```

## Rules for large-file tests

1. **Cap fixture size to a fraction of available RAM.** Check first with
   `free -h`. On a 3–4 GB VM, keep large-file fixtures at ~1 GB. Never approach
   total RAM. More size buys no extra signal for a bounded-memory test.

2. **Cap the subprocess heap with the GHC RTS.** Build with `-rtsopts` and run
   with `+RTS -s -M512m`. If the code under test regressed to loading the whole
   file, it aborts with `Heap exhausted` instead of dragging the machine into an
   OOM hang. `-s` also prints `maximum residency`, which *is* the measurement.

3. **Force strict accumulation in measurement helpers.** A lazy fold over a
   large stream (e.g. `MD5.update` without a bang) silently retains the whole
   stream and will OOM the *harness*, masquerading as a failure of the code
   under test. Use `!ctx` / `let !x = ...`.

4. **Clean up immediately.** Delete multi-hundred-MB fixtures as soon as the test
   finishes; don't leave them in the scratch dir across turns.

5. **`ghc --make` drops `.o`/`.hi` into the source tree.** When compiling a
   standalone test program against the in-repo modules, remove the stray object
   files afterwards (`find Bit test/single-workflows -name '*.o' -o -name '*.hi'
   | xargs rm -f`) so they don't shadow a later cabal build.

## The smoke test

The reusable program lives at
[`test/single-workflows/cdc-largefile-smoke.hs`](../test/single-workflows/cdc-largefile-smoke.hs).
It chunks a file, verifies the chunks tile it contiguously, and checks that a
whole-file MD5 equals an MD5 streamed back from the chunk slices. Build and run:

```bash
cabal exec -- ghc --make -O -rtsopts -o /tmp/cdc-smoke \
    test/single-workflows/cdc-largefile-smoke.hs
/tmp/cdc-smoke <path-to-~1GB-file> +RTS -s -M512m
```
