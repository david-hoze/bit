#!/bin/bash
#
# Run git test suite with load-aware throttling.
#
# Designed for multi-agent parallel execution: multiple agents can each run
# this script with different test ranges. Lightweight tests (<25 test cases)
# run freely in parallel. Heavy tests acquire a shared semaphore slot and
# also wait for system load to drop, preventing I/O contention on Windows.
#
# Usage:
#   bash run-throttled-suite.sh t0*.sh t1*.sh t2*.sh   # Batch A
#   bash run-throttled-suite.sh t3*.sh t4*.sh           # Batch B
#   bash run-throttled-suite.sh t5*.sh                  # Batch C
#   bash run-throttled-suite.sh t6*.sh t7*.sh t8*.sh t9*.sh  # Batch D
#
# Environment:
#   MAX_SLOTS=2          Max concurrent heavy tests (default: 2)
#   DEFAULT_TIMEOUT=300  Default per-script timeout in seconds (default: 300)
#   HEAVY_THRESHOLD=25   Test count above which a script is "heavy" (default: 25)
#   MAX_LOAD=4           Load average threshold for heavy test gating (default: 4)
#   RESULTS_FILE=...     Output file for results (default: stdout summary only)

set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SHIM_DIR="$SCRIPT_DIR/git-shim"
TEST_DIR="$SCRIPT_DIR/git/t"

# Configurable parameters
MAX_SLOTS="${MAX_SLOTS:-2}"
DEFAULT_TIMEOUT="${DEFAULT_TIMEOUT:-300}"
HEAVY_THRESHOLD="${HEAVY_THRESHOLD:-25}"
MAX_LOAD="${MAX_LOAD:-4}"
RESULTS_FILE="${RESULTS_FILE:-}"
SLOT_DIR="/tmp/git-test-slots"

# Special per-script timeouts (genuinely slow, not contention)
declare -A SPECIAL_TIMEOUTS=(
    [t0027-auto-crlf.sh]=900
    [t1092-sparse-checkout-compatibility.sh]=2400
    [t1517-outside-repo.sh]=1200
    [t3432-rebase-fast-forward.sh]=600
    [t5510-fetch.sh]=600
    [t5516-fetch-push.sh]=600
)

# Skip prefixes (always skip on Windows — svn/p4/cvs)
SKIP_PREFIXES="t91 t94 t98"

# --- Semaphore functions ---

mkdir -p "$SLOT_DIR"

acquire_slot() {
    while true; do
        for i in $(seq 1 "$MAX_SLOTS"); do
            if mkdir "$SLOT_DIR/slot-$i" 2>/dev/null; then
                ACQUIRED_SLOT="$SLOT_DIR/slot-$i"
                # Write PID for stale-slot detection
                echo $$ > "$ACQUIRED_SLOT/pid" 2>/dev/null
                return
            fi
        done
        sleep 2
    done
}

release_slot() {
    if [ -n "${ACQUIRED_SLOT:-}" ]; then
        rm -f "$ACQUIRED_SLOT/pid" 2>/dev/null
        rmdir "$ACQUIRED_SLOT" 2>/dev/null
        ACQUIRED_SLOT=""
    fi
}

# Clean stale slots (from crashed agents)
clean_stale_slots() {
    for slot in "$SLOT_DIR"/slot-*; do
        [ -d "$slot" ] || continue
        local pid_file="$slot/pid"
        if [ -f "$pid_file" ]; then
            local pid=$(cat "$pid_file" 2>/dev/null)
            if [ -n "$pid" ] && ! kill -0 "$pid" 2>/dev/null; then
                rm -f "$pid_file" 2>/dev/null
                rmdir "$slot" 2>/dev/null
            fi
        fi
    done
}

# --- Load detection ---

is_loaded() {
    # Try /proc/loadavg first (works on most MSYS2/Cygwin)
    if [ -f /proc/loadavg ]; then
        local load=$(cut -d' ' -f1 /proc/loadavg)
        local load_x10=$(printf '%.0f' "$(echo "$load * 10" | bc 2>/dev/null)" 2>/dev/null || echo 0)
        local max_x10=$((MAX_LOAD * 10))
        [ "$load_x10" -gt "$max_x10" ] && return 0
    fi
    return 1
}

wait_for_load() {
    local waited=0
    while is_loaded; do
        if [ "$waited" -eq 0 ]; then
            echo "  [throttle] load > $MAX_LOAD, waiting..."
        fi
        sleep 5
        waited=$((waited + 1))
        # Don't wait forever — cap at 2 minutes
        if [ "$waited" -gt 24 ]; then
            echo "  [throttle] gave up waiting after 2 min, proceeding"
            return
        fi
    done
}

# --- Test classification ---

count_tests() {
    local n
    n=$(grep -c 'test_expect_success\|test_expect_failure' "$1" 2>/dev/null || echo 0)
    echo "${n//[^0-9]/}"
}

is_heavy() {
    local count=$(count_tests "$1")
    [ "$count" -gt "$HEAVY_THRESHOLD" ]
}

get_timeout() {
    local script="$1"
    if [ -n "${SPECIAL_TIMEOUTS[$script]+x}" ]; then
        echo "${SPECIAL_TIMEOUTS[$script]}"
    else
        echo "$DEFAULT_TIMEOUT"
    fi
}

should_skip() {
    local script="$1"
    for prefix in $SKIP_PREFIXES; do
        case "$script" in
            ${prefix}*) return 0 ;;
        esac
    done
    return 1
}

# --- Main ---

cd "$TEST_DIR" || { echo "Cannot cd to $TEST_DIR"; exit 1; }

# Resolve glob patterns from arguments
scripts=()
for pattern in "$@"; do
    for f in $pattern; do
        [ -f "$f" ] && scripts+=("$f")
    done
done

if [ ${#scripts[@]} -eq 0 ]; then
    echo "No test scripts found matching: $*"
    exit 1
fi

# Clean stale slots from previous crashed runs
clean_stale_slots

# Counters
passed=0
failed=0
timed_out=0
skipped=0
bailed=0
total=0
failed_names=""
timeout_names=""
bail_names=""

# Trap to release slot on unexpected exit
trap 'release_slot' EXIT

echo "=== Throttled test suite runner ==="
echo "Scripts: ${#scripts[@]}"
echo "Heavy threshold: >${HEAVY_THRESHOLD} test cases"
echo "Max concurrent heavy: $MAX_SLOTS"
echo "Max load for heavy: $MAX_LOAD"
echo "Default timeout: ${DEFAULT_TIMEOUT}s"
echo ""

for f in "${scripts[@]}"; do
    total=$((total + 1))
    name="${f%.sh}"

    # Skip known-skip prefixes
    if should_skip "$f"; then
        skipped=$((skipped + 1))
        echo "SKIP $name (excluded prefix)"
        continue
    fi

    t=$(get_timeout "$f")
    test_count=$(count_tests "$f")
    heavy=""

    if is_heavy "$f"; then
        heavy=" [HEAVY:${test_count}t]"
        # Wait for system load to drop
        wait_for_load
        # Acquire semaphore slot
        acquire_slot
    fi

    echo -n "$name (${test_count}t, ${t}s${heavy}): "

    # Clean trash directory before running (synchronous, prevents bail-outs)
    trash="trash directory.$name"
    if [ -d "$trash" ]; then
        cmd //c "rmdir /s /q \"C:\\Users\\natanh\\repos\\bit\\extern\\git\\t\\$trash\"" 2>/dev/null
    fi

    # Run the test
    result=$(BIT_GIT_JUNCTION=1 GIT_TEST_INSTALLED="$SHIM_DIR" \
        timeout "$t" bash "$f" 2>&1 | tail -1 | tr -d '\r')
    exit_code=${PIPESTATUS[0]:-$?}

    # Release slot if heavy
    if [ -n "$heavy" ]; then
        release_slot
    fi

    # Classify result
    if [ $exit_code -eq 124 ] || [ $exit_code -eq 143 ]; then
        timed_out=$((timed_out + 1))
        timeout_names="$timeout_names $name"
        echo "TIMEOUT (${t}s)"
    elif echo "$result" | grep -q "^Bail out!"; then
        bailed=$((bailed + 1))
        bail_names="$bail_names $name"
        echo "BAIL: $result"
    elif echo "$result" | grep -q "^# passed all"; then
        passed=$((passed + 1))
        echo "PASS ($result)"
    elif echo "$result" | grep -q "^1\.\." && [ $exit_code -eq 0 ]; then
        passed=$((passed + 1))
        echo "PASS ($result)"
    elif [ $exit_code -eq 0 ]; then
        # Exit 0 but no clear pass indicator — likely all-skip
        passed=$((passed + 1))
        echo "PASS/SKIP ($result)"
    else
        failed=$((failed + 1))
        failed_names="$failed_names $name"
        echo "FAIL (exit $exit_code): $result"
    fi

    # Clean trash directory after run (background, best-effort)
    if [ -d "$trash" ]; then
        cmd //c "rmdir /s /q \"C:\\Users\\natanh\\repos\\bit\\extern\\git\\t\\$trash\"" 2>/dev/null &
    fi
done

# Summary
echo ""
echo "=== RESULTS ==="
echo "Total: $total"
echo "Passed: $passed"
echo "Failed: $failed"
echo "Timeout: $timed_out"
echo "Bail out: $bailed"
echo "Skipped: $skipped"
echo ""
if [ -n "$failed_names" ]; then
    echo "FAILED:$failed_names"
fi
if [ -n "$timeout_names" ]; then
    echo "TIMEOUT:$timeout_names"
fi
if [ -n "$bail_names" ]; then
    echo "BAIL OUT:$bail_names"
fi

# Save to file if requested
if [ -n "$RESULTS_FILE" ]; then
    {
        echo "Total: $total"
        echo "Passed: $passed"
        echo "Failed: $failed"
        echo "Timeout: $timed_out"
        echo "Bail out: $bailed"
        echo "Skipped: $skipped"
        [ -n "$failed_names" ] && echo "FAILED:$failed_names"
        [ -n "$timeout_names" ] && echo "TIMEOUT:$timeout_names"
        [ -n "$bail_names" ] && echo "BAIL OUT:$bail_names"
    } > "$RESULTS_FILE"
    echo ""
    echo "Results saved to $RESULTS_FILE"
fi

# Exit with failure count
exit $failed
