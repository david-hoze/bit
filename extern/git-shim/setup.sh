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

echo "Git test infrastructure ready."
echo "Run tests with:"
echo "  cd extern/git/t"
echo "  GIT_TEST_INSTALLED=$SCRIPT_DIR bash t0001-init.sh --verbose"
