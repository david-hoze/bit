#!/bin/bash
# Run full git test suite with parallelism via xargs
# Usage: bash extern/run-suite.sh [jobs] (default: 8)
set -e

JOBS="${1:-8}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SHIM_DIR="$SCRIPT_DIR/git-shim"
TEST_DIR="$SCRIPT_DIR/git/t"
RESULTS_DIR=$(mktemp -d)

echo "Shim dir: $SHIM_DIR"
echo "Test dir: $TEST_DIR"
echo "Results dir: $RESULTS_DIR"
echo "Parallel jobs: $JOBS"
echo "Starting test suite run..."

# Write a helper script that xargs will call
HELPER="$RESULTS_DIR/run-one.sh"
cat > "$HELPER" <<SCRIPT
#!/bin/bash
t="\$1"
name="\${t%.sh}"
cd "$TEST_DIR"
if GIT_TEST_INSTALLED="$SHIM_DIR" bash "\$t" > /dev/null 2>&1; then
    echo "PASS \$name"
    echo "PASS" > "$RESULTS_DIR/\$name"
else
    echo "FAIL \$name"
    echo "FAIL" > "$RESULTS_DIR/\$name"
fi
SCRIPT
chmod +x "$HELPER"

# Run tests in parallel
cd "$TEST_DIR"
ls t[0-9]*.sh | xargs -P "$JOBS" -I{} bash "$HELPER" {}

# Tally results
passed=$(grep -rl PASS "$RESULTS_DIR" | grep -v run-one | wc -l)
failed=$(grep -rl FAIL "$RESULTS_DIR" | wc -l)
total=$((passed + failed))
fail_list=$(grep -rl FAIL "$RESULTS_DIR" | xargs -I{} basename {} | sort | tr '\n' ' ')

echo ""
echo "=== FINAL RESULTS ==="
echo "Total: $total  Passed: $passed  Failed: $failed"
echo "Pass rate: $(echo "scale=1; $passed * 100 / $total" | bc)%"
echo ""
echo "Failing suites ($failed):"
echo "$fail_list" | tr ' ' '\n' | grep -v '^$'

rm -rf "$RESULTS_DIR"
