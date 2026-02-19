#!/bin/bash
#
# t0011-binary-grep.sh — Test bit grep with binary files
#
# Adapted from Git's t7815-grep-binary.sh.
# Tests that grep correctly handles binary files (which in bit contain
# metadata, not actual binary content).

test_description='bit grep with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with binary and text files' '
	create_bit_repo repo &&
	printf "binary\0content\0here" >repo/data.bin &&
	echo "searchable text content" >repo/readme.txt &&
	echo "another searchable line" >repo/notes.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Initial commit")
'

# ======================================================================
# GREP IN TEXT FILES (baseline)
# ======================================================================

test_expect_success 'grep finds text in text files' '
	(cd repo && $BIT grep "searchable") >grep_output 2>&1 &&
	grep -q "readme.txt" grep_output &&
	grep -q "notes.txt" grep_output
'

# ======================================================================
# GREP AND BINARY METADATA
# ======================================================================

test_expect_success 'grep for hash finds binary metadata' '
	(cd repo && $BIT grep "hash:") >grep_output 2>&1 &&
	grep -q "data.bin" grep_output
'

test_expect_success 'grep for size finds binary metadata' '
	(cd repo && $BIT grep "size:") >grep_output 2>&1 &&
	grep -q "data.bin" grep_output
'

test_expect_success 'grep for md5 finds binary metadata' '
	(cd repo && $BIT grep "md5:") >grep_output 2>&1 &&
	grep -q "data.bin" grep_output
'

# ======================================================================
# GREP WITH FILE PATTERNS
# ======================================================================

test_expect_success 'grep limited to text files excludes binary metadata' '
	(cd repo && $BIT grep "searchable" -- "*.txt") >grep_output 2>&1 &&
	grep -q "readme.txt" grep_output &&
	! grep -q "data.bin" grep_output
'

test_expect_success 'grep limited to binary metadata files' '
	(cd repo && $BIT grep "hash:" -- "*.bin") >grep_output 2>&1 &&
	grep -q "data.bin" grep_output
'

# ======================================================================
# GREP ACROSS COMMITS
# ======================================================================

test_expect_success 'grep in specific revision' '
	printf "updated\0binary" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Update binary") &&
	(cd repo && $BIT grep "hash:" HEAD~1 -- data.bin) >grep_old 2>&1 &&
	(cd repo && $BIT grep "hash:" HEAD -- data.bin) >grep_new 2>&1 &&
	! test_cmp grep_old grep_new
'

# ======================================================================
# GREP COUNT AND LIST
# ======================================================================

test_expect_success 'grep -c counts matches' '
	(cd repo && $BIT grep -c "hash:") >count_output 2>&1 &&
	grep -q "data.bin" count_output
'

test_expect_success 'grep -l lists matching files' '
	(cd repo && $BIT grep -l "searchable") >list_output 2>&1 &&
	grep -q "readme.txt" list_output &&
	grep -q "notes.txt" list_output
'

test_done
