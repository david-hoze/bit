#!/bin/bash
# Set up the git test infrastructure for running git's test suite against bit.
# Run once after cloning or after updating the extern/git submodule.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GIT_DIR="$(cd "$SCRIPT_DIR/../git" && pwd)"

# 1. Create GIT-BUILD-OPTIONS stub
cat > "$GIT_DIR/GIT-BUILD-OPTIONS" <<'EOF'
PERL_PATH='/usr/bin/perl'
SHELL_PATH='/usr/bin/bash'
TEST_SHELL_PATH='/usr/bin/bash'
NO_PERL=
NO_PYTHON=
NO_UNIX_SOCKETS=
PAGER_ENV='LESS=FRX LV=-c'
DC_SHA1=
X=''
EOF

# 2. Create templates/blt directory (test harness requires it)
mkdir -p "$GIT_DIR/templates/blt"

echo "Git test infrastructure ready."
echo "Run tests with:"
echo "  cd extern/git/t"
echo "  GIT_TEST_INSTALLED=$SCRIPT_DIR bash t0001-init.sh --verbose"
