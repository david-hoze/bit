# Tutorial: Configuration

`bit config` reads and writes bit's repo-local configuration file at `.bit/config`. It uses git-style INI format with `section.key` keys.

## Getting a Value

```bash
bit config core.mode
```

```
lite
```

If the key doesn't exist, nothing is printed.

## Setting a Value

```bash
bit config core.mode solid
```

```
Mode set to solid. bit add will now store file content in .bit/cas/.
hint: Run 'bit cas backfill' to store current files for existing commits.
```

## Listing All Config

```bash
bit config --list
```

```
core.mode=solid
cdc.enabled=true
```

## Available Config Keys

| Key | Values | Default | Description |
|-----|--------|---------|-------------|
| `core.mode` | `lite`, `solid` | `lite` | Whether `bit add` writes to the content-addressed store |
| `cdc.enabled` | `true`, `false` | `true` (enabled) | Enable content-defined chunking for large binaries |
| `cdc.min-size` | positive integer | `32768` | Minimum chunk size in bytes (32 KB) |
| `cdc.avg-size` | positive integer | `131072` | Target average chunk size (128 KB) |
| `cdc.max-size` | positive integer | `524288` | Maximum chunk size (512 KB) |

## Validation

Each key has its own validation. Invalid values are rejected immediately:

```bash
bit config core.mode fast
```

```
error: Invalid value 'fast' for core.mode. Must be 'lite' or 'solid'.
```

Unknown keys are also rejected — this prevents typos from creating garbage config entries:

```bash
bit config core.mod lite
```

```
error: Unknown config key 'core.mod'.
```

## The Config File

The config file at `.bit/config` uses git-style INI format:

```ini
[core]
    mode = solid

[cdc]
    enabled = true
    min-size = 32768
    avg-size = 131072
    max-size = 524288
```

You can edit this file directly, but using `bit config` is recommended because it validates values.

## Config Is Local-Only

The config file is **not** tracked by git. It's local to each clone. This means:

- Different machines can have different modes (lite on laptop, solid on NAS)
- Config changes don't create git commits
- Cloning a repo starts with default config (lite mode, CDC disabled)

## Common Workflows

### Enable solid mode (CDC is on by default)

```bash
bit config core.mode solid
# CDC chunking is enabled by default — no need to set cdc.enabled
```

### Check current mode

```bash
bit config core.mode
```

### Switch back to lite mode

```bash
bit config core.mode lite
```

The CAS data from solid mode is preserved — you can switch back to solid later without losing anything.
