#!/bin/bash
#
# t0006-binary-stash.sh — Test bit stash with binary files
#
# Adapted from Git's t3903-stash.sh (binary stash sections).
# Tests that stashing and unstashing binary file changes preserves
# metadata and content through bit.

test_description='bit stash with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with committed binary' '
	create_bit_repo repo &&
	printf "committed\0binary" >repo/data.bin &&
	echo "committed text" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Initial commit")
'

# ======================================================================
# STASH MODIFIED BINARY
# ======================================================================

test_expect_success 'modify binary file' '
	printf "modified\0binary\0for\0stash" >repo/data.bin &&
	(cd repo && $BIT add data.bin)
'

test_expect_success 'stash saves binary changes' '
	committed_hash=$(cd repo && git -C .bit/index show HEAD:data.bin | grep "^hash: " | sed "s/^hash: //") &&
	(cd repo && $BIT stash) &&
	current_hash=$(get_metadata_hash repo data.bin) &&
	test "$current_hash" = "$committed_hash"
'

test_expect_success 'stash pop restores binary changes' '
	(cd repo && $BIT stash pop) &&
	verify_binary_metadata repo data.bin &&
	current_hash=$(get_metadata_hash repo data.bin) &&
	test "$current_hash" != "$committed_hash"
'

test_expect_success 'commit stashed-then-popped binary' '
	(cd repo && $BIT add data.bin && $BIT commit -m "Commit after stash pop")
'

# ======================================================================
# STASH NEW BINARY FILE
# ======================================================================

test_expect_success 'stash new (untracked-included) binary' '
	printf "new\0stash\0binary" >repo/new-stash.bin &&
	(cd repo && $BIT add new-stash.bin) &&
	verify_binary_metadata repo new-stash.bin &&
	(cd repo && $BIT stash)
'

test_expect_success 'new binary is gone after stash' '
	test ! -f repo/.bit/index/new-stash.bin
'

test_expect_success 'stash pop brings back new binary' '
	(cd repo && $BIT stash pop) &&
	verify_binary_metadata repo new-stash.bin
'

test_expect_success 'commit new binary after stash cycle' '
	(cd repo && $BIT add new-stash.bin && $BIT commit -m "Add new-stash.bin after stash")
'

# ======================================================================
# STASH MULTIPLE BINARY CHANGES
# ======================================================================

test_expect_success 'stash multiple binary file changes' '
	printf "alpha\0mod" >repo/data.bin &&
	printf "beta\0mod" >repo/new-stash.bin &&
	(cd repo && $BIT add data.bin new-stash.bin) &&
	(cd repo && $BIT stash)
'

test_expect_success 'stash pop restores all binary changes' '
	(cd repo && $BIT stash pop) &&
	verify_binary_metadata repo data.bin &&
	verify_binary_metadata repo new-stash.bin
'

test_expect_success 'commit multiple stashed binaries' '
	(cd repo && $BIT add . && $BIT commit -m "Multi-binary stash cycle")
'

# ======================================================================
# STASH MIXED BINARY + TEXT
# ======================================================================

test_expect_success 'stash mixed binary and text changes' '
	printf "mixed\0binary" >repo/data.bin &&
	echo "mixed text" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT stash)
'

test_expect_success 'stash pop restores both types' '
	(cd repo && $BIT stash pop) &&
	verify_binary_metadata repo data.bin &&
	verify_text_metadata repo readme.txt &&
	grep -q "mixed text" repo/.bit/index/readme.txt
'

test_done
