#!/bin/bash
# Run the full git test suite against bit via the shim, with parallelism
# Usage: bash run-parallel-tests.sh [JOBS]
# Default: 4 parallel jobs

JOBS=${1:-4}
SHIM_DIR="/c/Users/natanh/repos/bit/extern/git-shim"
RESULTS_DIR="/c/Users/natanh/repos/bit/extern/test-results"
SUMMARY="/c/Users/natanh/repos/bit/extern/test-summary-$(date +%Y-%m-%d).txt"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

cd "$SCRIPT_DIR/git/t" || exit 1

mkdir -p "$RESULTS_DIR"

echo "Git test suite run: $(date)" > "$SUMMARY"
echo "Parallel jobs: $JOBS" >> "$SUMMARY"
echo "========================================" >> "$SUMMARY"

# Collect all test scripts
tests=(t[0-9]*.sh)
total=${#tests[@]}
echo "Total test suites: $total" >> "$SUMMARY"
echo "========================================" >> "$SUMMARY"

# Run tests in parallel
# xargs calls run-one-test.sh directly (no extra sh -c wrapper)
# BIT_GIT_JUNCTION=1 tells the router to always route to bit (junction mode)
export BIT_GIT_JUNCTION=1
export GIT_TEST_INSTALLED="$SHIM_DIR"
export RESULTS_DIR
printf '%s\n' "${tests[@]}" | xargs -P "$JOBS" -I {} "$SCRIPT_DIR/run-one-test.sh" {} | tee -a "$SUMMARY"

echo "========================================" >> "$SUMMARY"
pass=$(grep -c "^PASS " "$SUMMARY")
fail=$(grep -c "^FAIL " "$SUMMARY")
echo "Total: $total  Pass: $pass  Fail: $fail" >> "$SUMMARY"
echo "Done: $(date)" >> "$SUMMARY"

echo ""
echo "Total: $total  Pass: $pass  Fail: $fail"
echo "Summary: $SUMMARY"
echo "Individual results: $RESULTS_DIR/"
