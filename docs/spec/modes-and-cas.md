# Modes and CAS

## Mode Configuration (lite vs solid)

The mode is stored in `.bit/config` using git-style INI format under the `[core]` section:

```ini
[core]
    mode = lite
```
or
```ini
[core]
    mode = solid
```

**Setting the mode:** Use `bit config`:

```
bit config core.mode solid    # switch to solid
bit config core.mode lite     # switch to lite
bit config core.mode          # prints current mode
```

When switching to solid, bit prints a hint:
```
$ bit config core.mode solid
Mode set to solid. bit add will now store file content in .bit/cas/.
hint: Run 'bit cas backfill' to store current files for existing commits.
```

When switching to lite:
```
$ bit config core.mode lite
Mode set to lite. bit add will no longer store file content in .bit/cas/.
Existing CAS data is preserved.
```

**Default:** `lite`. If `.bit/config` does not exist or does not contain a `core.mode` key, the repo operates in lite mode.

**What the mode controls:** A single behavior -- whether `bit add` copies file content into `.bit/cas/` in addition to writing metadata to `.bit/index/`.

- **lite**: `bit add` writes metadata only. No CAS writes.
- **solid**: `bit add` writes metadata AND copies the blob (keyed by its hash) into `.bit/cas/`. CAS blobs serve as repair sources (via `bit repair`) and enable push even after the working tree file has been repaired from CAS.

### Switching Modes

**lite -> solid**: `bit config core.mode solid`. From this point forward, `bit add` populates the CAS. Existing history has no CAS backing -- the CAS is simply incomplete for older commits. An optional `bit cas backfill` command can walk historical commits and store any blobs that are currently present in the working tree, but it is not required.

**solid -> lite**: `bit config core.mode lite`. `bit add` stops writing to the CAS. The existing `.bit/cas/` directory is preserved with all its data -- nothing is deleted.

**CAS reads are mode-independent.** Regardless of the current mode, any operation that needs old file content (e.g. `bit restore` from a historical commit) checks `.bit/cas/` as a fallback. If the requested blob exists in CAS, it is used. If not, the operation fails with "no content available for this version." The mode only gates *writes* -- reads always consult the CAS if data is present.

**The mode is local-only.** It is not committed or tracked in git. Different clones of the same project can run in different modes -- a laptop might use lite to save space while a NAS uses solid for full history.

**CAS garbage collection:** `bit cas gc` (future) can prune blobs that are not referenced by any reachable commit. The safe default is to never delete CAS data automatically.

---

## Content-Defined Chunking (CDC)

In solid mode, `bit add` splits large binary files into content-determined chunks using the FastCDC algorithm. Only files at or above `cdc.min-size` are chunked; smaller files are stored as whole blobs. CDC is enabled by default; set `cdc.enabled = false` to disable.

See [CDC Spec](cdc-spec.md) for the full specification of the chunking algorithm, manifest format, and integration with push/pull.

**Backward compatibility**: A CAS containing a mix of whole-blob and chunked files works correctly. Repos that explicitly set `cdc.enabled = false` store all files as whole blobs.

---

## `bit config` Command

`bit config` reads and writes `.bit/config`, which uses git-style INI format with `section.key` keys (matching `git config` conventions). Every key must contain at least one dot.

```
bit config <key>              # get -- prints value
bit config <key> <value>      # set -- writes value
bit config --list             # dump all key=value pairs
```

**Validation:** Each key has its own validation. `core.mode` only accepts `lite` or `solid`. Unknown keys are rejected. This prevents typos from silently creating garbage config entries.

**Current config keys:**

| Key | Values | Default | Description |
|-----|--------|---------|-------------|
| `core.mode` | `lite`, `solid` | `lite` | Whether `bit add` writes to CAS |
| `cdc.enabled` | `true`, `false` | `true` (enabled) | Enable content-defined chunking for large binaries |
| `cdc.min-size` | positive integer | `32768` | Minimum chunk size in bytes |
| `cdc.avg-size` | positive integer | `131072` | Target average chunk size in bytes |
| `cdc.max-size` | positive integer | `524288` | Maximum chunk size in bytes |

---

## The Index Invariant

**Git is the sole authority over `.bit/index/`.** After any git operation that changes HEAD (merge, checkout, commit), `.bit/index/` is correct by definition. The only remaining work is mirroring those changes onto the actual working directory (downloading binaries from remote, copying text files from the index).

This invariant applies uniformly across all pull/merge paths:

- `git merge` -> determines correct metadata -> we sync actual files
- `git checkout` (first pull, `--accept-remote`) -> determines correct metadata -> we sync actual files

No path should write metadata files to `.bit/index/` directly and then commit. Scanning the remote via rclone and writing the result to the index bypasses git and **will** produce wrong metadata (rclone cannot distinguish text from binary, so text files get `hash:/size:` metadata instead of their actual content).
