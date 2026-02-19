#!/bin/bash
#
# Run all bit binary-file tests.
#
# Usage:
#   ./run-tests.sh              # Run all tests
#   ./run-tests.sh t0001*.sh    # Run specific test(s)
#   TEST_NO_CLEANUP=1 ./run-tests.sh  # Keep trash dirs for debugging
#   NO_COLOR=1 ./run-tests.sh         # Disable colors

cd "$(dirname "$0")" || exit 1

passed=0
failed=0
total=0

if [ $# -gt 0 ]; then
	tests="$@"
else
	tests=$(ls t[0-9]*.sh 2>/dev/null)
fi

if [ -z "$tests" ]; then
	echo "No test scripts found."
	exit 1
fi

for t in $tests; do
	echo ""
	echo "=== $t ==="
	bash "$t"
	if [ $? -eq 0 ]; then
		passed=$((passed + 1))
	else
		failed=$((failed + 1))
	fi
	total=$((total + 1))
done

echo ""
echo "=============================="
echo "Results: $passed/$total scripts passed"
if [ $failed -gt 0 ]; then
	echo "FAILED: $failed script(s)"
	exit 1
else
	echo "All tests passed!"
	exit 0
fi
