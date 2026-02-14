# Compiling Git's test-tool on Windows

This guide explains how to compile Git's `test-tool` helper binary from the git
submodule using [w64devkit](https://github.com/skeeto/w64devkit), a portable C
development kit for Windows that requires no installation.

## Prerequisites

| Tool | Location | Notes |
|------|----------|-------|
| w64devkit | `C:\Users\natanh\tools\w64devkit\` | Portable GCC toolchain, just extract the zip |
| zlib | `C:\Users\natanh\tools\zlib-1.3.1\` | Built from source (see below) |
| Git submodule | `extern/git/` | Pinned at v2.47.0 |

## One-time setup: Build zlib

w64devkit does not include zlib. Download and build it once:

```bash
# Download zlib source
cd /c/Users/natanh/tools
curl -L -o zlib.tar.gz https://zlib.net/zlib-1.3.1.tar.gz
tar xzf zlib.tar.gz

# Build with w64devkit's gcc
cd zlib-1.3.1
PATH="/c/Users/natanh/tools/w64devkit/bin:$PATH" make -f win32/Makefile.gcc

# Create the include/lib structure that git's Makefile expects
mkdir -p include lib
cp zlib.h zconf.h include/
cp libz.a lib/
```

## Building test-tool

From Git Bash (not w64devkit's shell — see "Why Git Bash?" below):

```bash
cd /c/Users/natanh/repos/workers/worker2/bit/extern/git

PATH="/c/Users/natanh/tools/w64devkit/bin:$PATH" make -j$(nproc) \
  uname_S=MINGW \
  NO_OPENSSL=1 \
  NO_CURL=1 \
  NO_EXPAT=1 \
  NO_GETTEXT=1 \
  NO_PERL=1 \
  NO_PYTHON=1 \
  NO_TCLTK=1 \
  NO_ICONV=1 \
  USE_LIBPCRE= \
  USE_NED_ALLOCATOR= \
  ZLIB_PATH=/c/Users/natanh/tools/zlib-1.3.1 \
  CFLAGS="-O2" \
  t/helper/test-tool.exe
```

The binary lands at `extern/git/t/helper/test-tool.exe` (~3.2 MB).

### Verify it works

```bash
extern/git/t/helper/test-tool.exe
# Should print: usage: test-tool <toolname> [args] followed by subcommand list
```

### Clean rebuild

```bash
cd extern/git
make clean
# Then re-run the build command above
```

## Explanation of build flags

### Why `uname_S=MINGW`?

Git's Makefile auto-detects the platform via `uname -s`. When w64devkit's `sh`
is on PATH, `uname -s` returns `Windows_NT` instead of `MINGW64_NT-...`, which
breaks the MINGW detection in `config.mak.uname`. Forcing `uname_S=MINGW`
activates the correct Windows/MinGW compat layer (NO_SYSLOG, socket shims,
.exe suffix, etc.).

**Run from Git Bash** so that make itself uses Git Bash's shell, and only
w64devkit's `gcc`/`make` binaries are added via PATH prepend.

### Disabled dependencies

| Flag | Why |
|------|-----|
| `NO_OPENSSL=1` | Not needed for test-tool; avoids OpenSSL dependency |
| `NO_CURL=1` | No HTTP transport needed |
| `NO_EXPAT=1` | No XML parsing needed |
| `NO_GETTEXT=1` | No internationalization needed |
| `NO_PERL=1` | No Perl scripts needed |
| `NO_PYTHON=1` | No Python scripts needed |
| `NO_TCLTK=1` | No GUI needed |
| `NO_ICONV=1` | No character encoding conversion needed |
| `USE_LIBPCRE=` | Overrides MINGW default that enables pcre2 (not available) |
| `USE_NED_ALLOCATOR=` | Overrides MINGW default (not available in w64devkit) |

### zlib

`ZLIB_PATH` tells the Makefile where to find zlib. It expects:
- `$ZLIB_PATH/include/zlib.h`
- `$ZLIB_PATH/lib/libz.a`

## Updating the git submodule

```bash
cd extern/git
git fetch
git checkout v2.48.0   # or whatever new version
cd ../..
git add extern/git
git commit -m "Update git submodule to v2.48.0"
```

After updating, rebuild test-tool with the same command (run `make clean` first).
