#!/bin/bash
# Set up the git test infrastructure for running git's test suite against bit.
# Run once after cloning or after updating the extern/git submodule.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GIT_DIR="$(cd "$SCRIPT_DIR/../git" && pwd)"

# 1. Create GIT-BUILD-OPTIONS stub
cat > "$GIT_DIR/GIT-BUILD-OPTIONS" <<EOF
PERL_PATH='/usr/bin/perl'
SHELL_PATH='/usr/bin/bash'
TEST_SHELL_PATH='/usr/bin/bash'
NO_PERL=
NO_PYTHON=
NO_UNIX_SOCKETS=
PAGER_ENV='LESS=FRX LV=-c'
X=''
TAR='tar'
GIT_TEST_TEMPLATE_DIR='$GIT_DIR/templates/blt'
EOF

# 2. Create templates/blt directory with default template files
# The test harness sets GIT_TEMPLATE_DIR to this directory.
# Tests expect standard template files (e.g. info/exclude) to exist.
mkdir -p "$GIT_DIR/templates/blt/info"
mkdir -p "$GIT_DIR/templates/blt/hooks"
cat > "$GIT_DIR/templates/blt/info/exclude" <<'TMPL'
# git ls-files --others --exclude-from=.git/info/exclude
# Lines that start with '#' are comments.
# For a project-specific exclude file see gitignore(5).
TMPL
cat > "$GIT_DIR/templates/blt/description" <<'TMPL'
Unnamed repository; edit this file 'description' to name the repository.
TMPL

# 3. Ensure test-tool exists
# The test harness requires t/helper/test-tool. If a compiled test-tool.exe
# exists (built via docs/compiling-git-test-tool.md), skip stub creation.
# Otherwise, create a minimal bash stub for basic harness startup.
mkdir -p "$GIT_DIR/t/helper"
if [ -f "$GIT_DIR/t/helper/test-tool.exe" ] && [ "$(wc -c < "$GIT_DIR/t/helper/test-tool.exe")" -gt 10000 ]; then
    echo "Using compiled test-tool.exe ($(wc -c < "$GIT_DIR/t/helper/test-tool.exe") bytes)."
    # Remove any extensionless stub that could shadow the .exe on MSYS2
    if [ -f "$GIT_DIR/t/helper/test-tool" ] && [ ! "$GIT_DIR/t/helper/test-tool" -ef "$GIT_DIR/t/helper/test-tool.exe" ]; then
        rm -f "$GIT_DIR/t/helper/test-tool"
    fi
else
    echo "No compiled test-tool.exe found — creating bash stub (limited)."
    echo "Build the real one with: docs/compiling-git-test-tool.md"
    cat > "$GIT_DIR/t/helper/test-tool" <<'STUB'
#!/bin/bash
case "$1" in
    path-utils)
        case "$2" in
            file-size) wc -c < "$3" | tr -d ' ' ;;
            *) echo 0 ;;
        esac
        ;;
    date)
        case "$2" in
            is64bit|time_t-is64bit) exit 0 ;;
            *) echo "unknown" ;;
        esac
        ;;
    env-helper)
        shift
        val=""
        while [ $# -gt 0 ]; do
            case "$1" in
                --type=*|--default=*) shift ;;
                --exit-code) shift ;;
                *) val="${!1}"; shift ;;
            esac
        done
        case "$val" in
            true|1|yes) exit 0 ;;
            *) exit 1 ;;
        esac
        ;;
    *) exit 1 ;;
esac
STUB
    chmod +x "$GIT_DIR/t/helper/test-tool"
fi

echo "Git test infrastructure ready."
echo "Run tests with:"
echo "  cd extern/git/t"
echo "  GIT_TEST_INSTALLED=$SCRIPT_DIR bash t0001-init.sh --verbose"
