#!/bin/bash
# Run the full git test suite against bit via the shim
# Results saved to extern/test-results-YYYY-MM-DD.txt

cd "$(dirname "$0")/git/t" || exit 1

SHIM_DIR="/c/Users/natanh/repos/bit/extern/git-shim"
export BIT_GIT_JUNCTION=1
RESULTS_FILE="/c/Users/natanh/repos/bit/extern/test-results-$(date +%Y-%m-%d).txt"

echo "Git test suite run: $(date)" > "$RESULTS_FILE"
echo "Shim: $SHIM_DIR" >> "$RESULTS_FILE"
echo "bit version: $(GIT_TEST_INSTALLED="$SHIM_DIR" bit --version 2>&1)" >> "$RESULTS_FILE"
echo "========================================" >> "$RESULTS_FILE"

pass=0
fail=0
total=0

for t in t[0-9]*.sh; do
    total=$((total + 1))
    echo "Running $t ..." >&2
    # Run with a 120-second timeout per test suite
    timeout 120 bash -c "BIT_GIT_JUNCTION=1 GIT_TEST_INSTALLED='$SHIM_DIR' bash '$t' 2>&1" > /tmp/test-output.txt 2>&1
    rc=$?

    if [ $rc -eq 0 ]; then
        pass=$((pass + 1))
        echo "PASS: $t" >> "$RESULTS_FILE"
    elif [ $rc -eq 124 ]; then
        fail=$((fail + 1))
        echo "TIMEOUT: $t" >> "$RESULTS_FILE"
    else
        fail=$((fail + 1))
        echo "FAIL: $t (exit $rc)" >> "$RESULTS_FILE"
        # Save last 5 lines of output for failed tests
        tail -5 /tmp/test-output.txt | sed "s/^/  /" >> "$RESULTS_FILE"
    fi

    # Progress every 50 tests
    if [ $((total % 50)) -eq 0 ]; then
        echo "Progress: $total tests ($pass pass, $fail fail)" >&2
    fi
done

echo "========================================" >> "$RESULTS_FILE"
echo "Total: $total  Pass: $pass  Fail: $fail" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"
echo "Done: $(date)" >> "$RESULTS_FILE"

echo "Results saved to $RESULTS_FILE"
echo "Total: $total  Pass: $pass  Fail: $fail"
