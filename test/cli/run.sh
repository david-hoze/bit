#!/usr/bin/env bash
# Cross-platform parallel test runner for bit CLI tests (bash-based).
# Usage: bash test/cli/run.sh [test-file ...]
#
# Runs 000-cleanup.test first, then all other .test files in parallel.
# Tests that share a cloud remote (gdrive-test) are serialized via flock.
#
# Requires: shelltest (shelltestrunner) on PATH.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

# Prevent findBitRoot from walking past test output dirs into the parent repo
export BIT_CEILING_DIRECTORIES="$PROJECT_ROOT/test/cli/output"

# Detect shell for shelltest
SHELL_EXE="$(command -v bash 2>/dev/null || echo /bin/sh)"
SHELLTEST_OPTS="--shell=$SHELL_EXE"

# Tests that share the gdrive-test rclone remote — serialized via lockfile
GDRIVE_TESTS="gdrive-remote.test multi-remote-sync.test bare-push-pull.test fetch-output.test verify-bare-remote.test"
GDRIVE_LOCK="/tmp/bit-test-gdrive.lock"

# Temp dir for per-test results
RESULTS_DIR=$(mktemp -d)
trap 'rm -rf "$RESULTS_DIR"; rmdir "$GDRIVE_LOCK" 2>/dev/null || true' EXIT

# Run global cleanup first
echo "Running global cleanup..."
shelltest $SHELLTEST_OPTS "$SCRIPT_DIR/000-cleanup.test" >/dev/null 2>&1 || {
    echo "FATAL: cleanup failed"
    exit 1
}

# Collect test files
if [ $# -gt 0 ]; then
    TEST_FILES=("$@")
else
    TEST_FILES=()
    for f in "$SCRIPT_DIR"/*.test; do
        name="$(basename "$f")"
        [ "$name" = "000-cleanup.test" ] && continue
        TEST_FILES+=("$f")
    done
fi

# Run a single test, writing results to RESULTS_DIR
run_one() {
    local test_file="$1"
    local name
    name="$(basename "$test_file")"
    local result_file="$RESULTS_DIR/$name"
    local needs_lock=false

    for gt in $GDRIVE_TESTS; do
        if [ "$name" = "$gt" ]; then
            needs_lock=true
            break
        fi
    done

    local output exit_code=0
    if $needs_lock; then
        # Serialize gdrive tests: flock on Linux, mkdir spinlock on macOS
        if command -v flock >/dev/null 2>&1; then
            output=$(flock "$GDRIVE_LOCK" shelltest $SHELLTEST_OPTS "$test_file" 2>&1) || exit_code=$?
        else
            while ! mkdir "$GDRIVE_LOCK" 2>/dev/null; do sleep 1; done
            output=$(shelltest $SHELLTEST_OPTS "$test_file" 2>&1) || exit_code=$?
            rmdir "$GDRIVE_LOCK" 2>/dev/null || true
        fi
    else
        output=$(shelltest $SHELLTEST_OPTS "$test_file" 2>&1) || exit_code=$?
    fi

    local pass=0 fail=0
    pass=$(echo "$output" | sed -n 's/.*Passed[[:space:]]*\([0-9]*\).*/\1/p' | head -1)
    fail=$(echo "$output" | sed -n 's/.*Failed[[:space:]]*\([0-9]*\).*/\1/p' | head -1)
    pass=${pass:-0}
    fail=${fail:-0}

    # Write result file: pass fail exit_code
    echo "$pass $fail $exit_code" > "$result_file"
    if [ "$fail" -gt 0 ] || [ "$exit_code" -ne 0 ]; then
        echo "$output" >> "$result_file"
    fi
}

echo "Running ${#TEST_FILES[@]} test files in parallel..."

# Launch all tests in background
pids=()
for test_file in "${TEST_FILES[@]}"; do
    run_one "$test_file" &
    pids+=($!)
done

# Wait for all
for pid in "${pids[@]}"; do
    wait "$pid" 2>/dev/null || true
done

# Collect results
TOTAL_PASS=0
TOTAL_FAIL=0
FAILED_NAMES=()

for test_file in "${TEST_FILES[@]}"; do
    name="$(basename "$test_file")"
    result_file="$RESULTS_DIR/$name"
    if [ -f "$result_file" ]; then
        read -r pass fail exit_code < "$result_file"
        TOTAL_PASS=$((TOTAL_PASS + pass))
        TOTAL_FAIL=$((TOTAL_FAIL + fail))
        if [ "$fail" -gt 0 ] || [ "$exit_code" -ne 0 ]; then
            printf "  FAIL  %-45s (%d passed, %d failed)\n" "$name" "$pass" "$fail"
            FAILED_NAMES+=("$name")
        else
            printf "  OK    %-45s (%d passed)\n" "$name" "$pass"
        fi
    else
        printf "  FAIL  %-45s (no result)\n" "$name"
        FAILED_NAMES+=("$name")
    fi
done

echo ""
echo "Total: $TOTAL_PASS passed, $TOTAL_FAIL failed"

if [ ${#FAILED_NAMES[@]} -gt 0 ]; then
    echo ""
    echo "=== FAILURE DETAILS ==="
    for name in "${FAILED_NAMES[@]}"; do
        result_file="$RESULTS_DIR/$name"
        echo ""
        echo "--- $name ---"
        if [ -f "$result_file" ]; then
            tail -n +2 "$result_file"
        fi
    done
    exit 1
fi
exit 0
