#!/bin/bash
#
# t0012-binary-diff-rename.sh — Test diff and rename detection with binary files
#
# Adapted from Git's t4012-diff-binary.sh, t4043-diff-rename-binary.sh,
# t4031-diff-rewrite-binary.sh. Tests that diff correctly detects changes,
# renames, and rewrites in binary metadata files.

test_description='bit diff and rename detection with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with binary files' '
	create_bit_repo repo &&
	printf "alpha\0binary\0content" >repo/alpha.bin &&
	printf "beta\0binary\0content" >repo/beta.bin &&
	echo "text file" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Initial commit")
'

# ======================================================================
# DIFF WITH BINARY CHANGES
# ======================================================================

test_expect_success 'diff detects modified binary' '
	printf "alpha\0modified" >repo/alpha.bin &&
	(cd repo && $BIT add alpha.bin) &&
	(cd repo && $BIT diff --cached --name-only) >diff_output &&
	grep -q "alpha.bin" diff_output
'

test_expect_success 'diff --stat shows binary in stats' '
	(cd repo && $BIT diff --cached --stat) >stat_output &&
	grep -q "alpha.bin" stat_output
'

test_expect_success 'diff --numstat with binary' '
	(cd repo && $BIT diff --cached --numstat) >numstat_output &&
	grep -q "alpha.bin" numstat_output
'

test_expect_success 'commit modified binary' '
	(cd repo && $BIT commit -m "Modify alpha.bin")
'

# ======================================================================
# RENAME DETECTION
# ======================================================================

test_expect_success 'mv binary file — diff detects rename' '
	(cd repo && $BIT mv beta.bin gamma.bin) &&
	(cd repo && $BIT diff --cached --name-status) >rename_output &&
	grep -q "gamma.bin" rename_output
'

test_expect_success 'renamed binary has correct metadata' '
	verify_binary_metadata repo gamma.bin
'

test_expect_success 'commit rename' '
	(cd repo && $BIT commit -m "Rename beta to gamma")
'

test_expect_success 'old name not tracked by git after commit' '
	! (cd repo && git -C .bit/index ls-files --error-unmatch beta.bin 2>/dev/null)
'

# ======================================================================
# RENAME + MODIFY
# ======================================================================

test_expect_success 'rename and modify binary simultaneously' '
	printf "gamma\0totally\0new\0content" >repo/gamma.bin &&
	(cd repo && $BIT add gamma.bin) &&
	(cd repo && $BIT mv gamma.bin delta.bin) &&
	verify_binary_metadata repo delta.bin
'

test_expect_success 'commit rename+modify' '
	(cd repo && $BIT commit -m "Rename gamma to delta with new content")
'

# ======================================================================
# MOVE TO SUBDIRECTORY
# ======================================================================

test_expect_success 'move binary to subdirectory' '
	mkdir -p repo/subdir &&
	(cd repo && $BIT mv delta.bin subdir/delta.bin) &&
	verify_binary_metadata repo subdir/delta.bin &&
	test ! -f repo/.bit/index/delta.bin
'

test_expect_success 'commit move to subdir' '
	(cd repo && $BIT commit -m "Move delta to subdir")
'

# ======================================================================
# COPY DETECTION
# ======================================================================

test_expect_success 'copy binary file' '
	cp repo/alpha.bin repo/alpha-copy.bin &&
	(cd repo && $BIT add alpha-copy.bin) &&
	verify_binary_metadata repo alpha-copy.bin
'

test_expect_success 'diff -C detects copy' '
	(cd repo && $BIT diff --cached -C --name-status) >copy_output &&
	grep -q "alpha-copy.bin" copy_output
'

test_expect_success 'commit copy' '
	(cd repo && $BIT commit -m "Copy alpha to alpha-copy")
'

# ======================================================================
# DIFF BETWEEN BRANCHES
# ======================================================================

test_expect_success 'diff between branches with binary changes' '
	(cd repo && git -C .bit/index checkout -b diff-branch) &&
	printf "branch\0binary\0data" >repo/alpha.bin &&
	(cd repo && $BIT add alpha.bin && $BIT commit -m "Branch binary change") &&
	(cd repo && $BIT diff main..diff-branch --name-only) >branch_diff &&
	grep -q "alpha.bin" branch_diff
'

test_expect_success 'diff --stat between branches' '
	(cd repo && $BIT diff main..diff-branch --stat) >branch_stat &&
	grep -q "alpha.bin" branch_stat
'

test_done
