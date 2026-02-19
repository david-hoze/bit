#!/bin/bash
#
# Minimal test framework for bit binary-file tests.
# Inspired by Git's t/test-lib.sh but self-contained.
#
# Usage in test scripts:
#   test_description='what this test does'
#   . ./test-lib.sh
#   test_expect_success 'name' 'commands'
#   test_done

# --- Configuration ---
# Where bit lives — defaults to whatever is in PATH
BIT=${BIT:-bit}

# Trash directory for test isolation
TEST_NAME=$(basename "$0" .sh)
TRASH_DIR="$PWD/trash-$TEST_NAME"

# Colors (disabled if NO_COLOR is set or stdout is not a terminal)
if [ -z "$NO_COLOR" ] && [ -t 1 ]; then
	GREEN=$'\033[32m'
	RED=$'\033[31m'
	YELLOW=$'\033[33m'
	RESET=$'\033[0m'
else
	GREEN=''
	RED=''
	YELLOW=''
	RESET=''
fi

# Counters
test_count=0
test_success=0
test_failure=0
test_skip=0

# --- Cleanup helpers ---
_cleanup_hooks=""

test_when_finished () {
	_cleanup_hooks="$1; $_cleanup_hooks"
}

_run_cleanup () {
	if [ -n "$_cleanup_hooks" ]; then
		eval "$_cleanup_hooks" 2>/dev/null
		_cleanup_hooks=""
	fi
}

# --- Binary content helpers (from Git's test-lib-functions.sh) ---

# Convert 'Q' to NUL byte
q_to_nul () {
	tr 'Q' '\000'
}

# Convert NUL byte to 'Q'
nul_to_q () {
	tr '\000' 'Q'
}

# Convert LF to NUL
lf_to_nul () {
	tr '\012' '\000'
}

# Generate N bytes of deterministic pseudo-random binary content
# Usage: generate_binary <seed> <size_bytes>
generate_binary () {
	local seed="$1"
	local size="$2"
	# Use printf with repeated pattern + head to get exact size
	python3 -c "
import hashlib, sys
data = b''
block = b'$seed'
while len(data) < $size:
    block = hashlib.md5(block).digest()
    data += block
sys.stdout.buffer.write(data[:$size])
" 2>/dev/null || {
		# Fallback if python3 not available: use dd with /dev/urandom
		dd if=/dev/urandom bs=1 count="$size" 2>/dev/null
	}
}

# Compare two files byte-for-byte
test_cmp_bin () {
	cmp "$1" "$2"
}

# Compare two text files
test_cmp () {
	diff -u "$1" "$2"
}

# Assert that a file is empty
test_must_be_empty () {
	if [ -s "$1" ]; then
		echo "error: '$1' is not empty:"
		cat "$1"
		return 1
	fi
}

# Assert that a command fails (exit non-zero)
test_must_fail () {
	"$@"
	local status=$?
	if [ $status -eq 0 ]; then
		echo "error: command succeeded but should have failed: $*"
		return 1
	fi
	return 0
}

# --- Bit-specific helpers ---

# Verify that a file in .bit/index/ has binary metadata (hash: + size:)
verify_binary_metadata () {
	local repo="$1"
	local filepath="$2"
	local index_path="$repo/.bit/index/$filepath"
	if [ ! -f "$index_path" ]; then
		echo "error: metadata file does not exist: $index_path"
		return 1
	fi
	if ! grep -q '^hash: ' "$index_path"; then
		echo "error: metadata missing 'hash:' line in $index_path"
		cat "$index_path"
		return 1
	fi
	if ! grep -q '^size: ' "$index_path"; then
		echo "error: metadata missing 'size:' line in $index_path"
		cat "$index_path"
		return 1
	fi
	return 0
}

# Verify that a file in .bit/index/ has text content (no hash:/size: lines)
verify_text_metadata () {
	local repo="$1"
	local filepath="$2"
	local index_path="$repo/.bit/index/$filepath"
	if [ ! -f "$index_path" ]; then
		echo "error: metadata file does not exist: $index_path"
		return 1
	fi
	if grep -q '^hash: ' "$index_path"; then
		echo "error: text file should not have 'hash:' in metadata: $index_path"
		cat "$index_path"
		return 1
	fi
	return 0
}

# Extract the hash value from binary metadata
get_metadata_hash () {
	local repo="$1"
	local filepath="$2"
	grep '^hash: ' "$repo/.bit/index/$filepath" | sed 's/^hash: //'
}

# Extract the size value from binary metadata
get_metadata_size () {
	local repo="$1"
	local filepath="$2"
	grep '^size: ' "$repo/.bit/index/$filepath" | sed 's/^size: //' | tr -d '\r'
}

# Create a fresh bit repo in a given directory
create_bit_repo () {
	local dir="$1"
	mkdir -p "$dir" &&
	(cd "$dir" && $BIT init) >/dev/null 2>&1
}

# --- Test framework ---

test_expect_success () {
	local name="$1"
	local commands="$2"
	test_count=$((test_count + 1))

	# Run in subshell to isolate failures
	(
		cd "$TRASH_DIR" &&
		eval "$commands"
	)
	local status=$?

	_run_cleanup

	if [ $status -eq 0 ]; then
		test_success=$((test_success + 1))
		echo "${GREEN}ok $test_count - $name${RESET}"
	else
		test_failure=$((test_failure + 1))
		echo "${RED}FAIL $test_count - $name${RESET}"
	fi
}

test_expect_failure () {
	local name="$1"
	local commands="$2"
	test_count=$((test_count + 1))

	(
		cd "$TRASH_DIR" &&
		eval "$commands"
	)
	local status=$?

	_run_cleanup

	if [ $status -ne 0 ]; then
		test_skip=$((test_skip + 1))
		echo "${YELLOW}TODO $test_count - $name (expected failure)${RESET}"
	else
		test_success=$((test_success + 1))
		echo "${GREEN}ok $test_count - $name (unexpected success!)${RESET}"
	fi
}

test_done () {
	echo ""
	echo "# $test_description"
	echo "# passed: $test_success / $test_count"
	if [ $test_failure -gt 0 ]; then
		echo "# ${RED}FAILED: $test_failure${RESET}"
	fi
	if [ $test_skip -gt 0 ]; then
		echo "# TODO: $test_skip"
	fi

	# Clean up trash directory
	if [ $test_failure -eq 0 ] && [ -z "$TEST_NO_CLEANUP" ]; then
		rm -rf "$TRASH_DIR"
	fi

	if [ $test_failure -gt 0 ]; then
		exit 1
	fi
	exit 0
}

# --- Setup ---

# Remove stale trash directory and create fresh one
rm -rf "$TRASH_DIR"
mkdir -p "$TRASH_DIR"

echo "# Running: $test_description"
