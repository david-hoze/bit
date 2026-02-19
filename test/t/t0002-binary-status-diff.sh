#!/bin/bash
#
# t0002-binary-status-diff.sh — Test bit status and diff with binary files
#
# Adapted from Git's t4012-diff-binary.sh, t4031-diff-rewrite-binary.sh.
# Tests that bit status/diff correctly report binary file changes.

test_description='bit status and diff with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with binary and text files' '
	create_bit_repo repo &&
	echo "text content" >repo/readme.txt &&
	printf "binary\0content" >repo/data.bin &&
	printf "\0\0\0\0" >repo/zeroes.bin &&
	(cd repo && $BIT add . && $BIT commit -m "Initial commit")
'

# ======================================================================
# STATUS WITH UNMODIFIED FILES
# ======================================================================

test_expect_success 'status clean after commit' '
	(cd repo && $BIT status) | grep -q "nothing to commit\|working tree clean"
'

# ======================================================================
# STATUS WITH MODIFIED BINARY
# ======================================================================

test_expect_success 'status shows modified binary file' '
	printf "new\0binary\0content" >repo/data.bin &&
	(cd repo && $BIT status) | grep -q "modified\|data.bin"
'

test_expect_success 'add modified binary and status shows staged' '
	(cd repo && $BIT add data.bin) &&
	verify_binary_metadata repo data.bin &&
	(cd repo && $BIT status) | grep -q "data.bin"
'

test_expect_success 'commit modified binary' '
	(cd repo && $BIT commit -m "Modify data.bin")
'

# ======================================================================
# STATUS WITH DELETED BINARY
# ======================================================================

test_expect_success 'delete binary via bit rm' '
	(cd repo && $BIT rm zeroes.bin) &&
	(cd repo && $BIT status) | grep -q "deleted\|zeroes.bin"
'

test_expect_success 'commit binary deletion' '
	(cd repo && $BIT commit -m "Delete zeroes.bin")
'

# ======================================================================
# STATUS WITH NEW BINARY (UNTRACKED)
# ======================================================================

test_expect_success 'status shows untracked binary file' '
	printf "\xFF\xFE\x00\x01" >repo/new-binary.dat &&
	(cd repo && $BIT status) | grep -q "new-binary.dat\|Untracked"
'

test_expect_success 'add and commit new binary' '
	(cd repo && $BIT add new-binary.dat && $BIT commit -m "Add new-binary.dat")
'

# ======================================================================
# DIFF WITH BINARY FILES (git passthrough)
# ======================================================================

test_expect_success 'diff shows binary change indicator' '
	printf "changed\0" >repo/new-binary.dat &&
	(cd repo && $BIT add new-binary.dat) &&
	(cd repo && $BIT diff --cached) >diff_output 2>&1 &&
	grep -q "Binary\|hash:\|differ\|new-binary" diff_output
'

test_expect_success 'diff --stat with binary file' '
	(cd repo && $BIT diff --cached --stat) >stat_output 2>&1 &&
	grep -q "new-binary" stat_output
'

test_expect_success 'commit diffed binary' '
	(cd repo && $BIT commit -m "Modify new-binary.dat")
'

# ======================================================================
# STATUS MIXED: BINARY + TEXT CHANGES
# ======================================================================

test_expect_success 'simultaneous binary and text changes' '
	echo "updated text" >repo/readme.txt &&
	printf "updated\0binary" >repo/new-binary.dat &&
	(cd repo && $BIT status) >status_output &&
	grep -q "readme.txt" status_output &&
	grep -q "new-binary.dat" status_output
'

test_expect_success 'add and commit mixed changes' '
	(cd repo && $BIT add . && $BIT commit -m "Update text and binary together")
'

# ======================================================================
# RENAME DETECTION
# ======================================================================

test_expect_success 'rename binary file shows in status' '
	mv repo/new-binary.dat repo/renamed-binary.dat &&
	(cd repo && $BIT add . ) &&
	(cd repo && $BIT status) | grep -q "renamed\|renamed-binary\|new-binary\|deleted"
'

test_expect_success 'renamed binary has correct metadata' '
	verify_binary_metadata repo renamed-binary.dat
'

test_expect_success 'commit renamed binary' '
	(cd repo && $BIT commit -m "Rename new-binary.dat to renamed-binary.dat")
'

# ======================================================================
# BINARY FILE REPLACED WITH TEXT AND VICE VERSA
# ======================================================================

test_expect_success 'replace .bin extension file with text content — stays binary by extension' '
	echo "now I am text" >repo/data.bin &&
	(cd repo && $BIT add data.bin) &&
	verify_binary_metadata repo data.bin
'

test_expect_success 'replace .txt text file with binary content (NUL bytes)' '
	printf "now\0I\0am\0binary" >repo/readme.txt &&
	(cd repo && $BIT add readme.txt) &&
	verify_binary_metadata repo readme.txt
'

test_expect_success 'replace .dat text file with NUL — classified by content' '
	echo "was text" >repo/was-text.dat &&
	(cd repo && $BIT add was-text.dat && $BIT commit -m "Add text .dat") &&
	verify_text_metadata repo was-text.dat &&
	printf "now\0binary" >repo/was-text.dat &&
	(cd repo && $BIT add was-text.dat) &&
	verify_binary_metadata repo was-text.dat
'

test_expect_success 'commit type-change files' '
	(cd repo && $BIT commit -m "Binary/text type changes")
'

test_done
