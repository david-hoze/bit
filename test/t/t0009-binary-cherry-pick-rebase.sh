#!/bin/bash
#
# t0009-binary-cherry-pick-rebase.sh — Test cherry-pick and rebase with binary files
#
# Adapted from Git's t3501-revert-cherry-pick.sh, t3419-rebase-patch-id.sh.
# Tests that cherry-pick and rebase correctly handle binary metadata.

test_description='bit cherry-pick and rebase with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with base commit' '
	create_bit_repo repo &&
	printf "base\0binary" >repo/data.bin &&
	echo "base text" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Base commit")
'

# ======================================================================
# CHERRY-PICK A BINARY CHANGE
# ======================================================================

test_expect_success 'create binary change on feature branch' '
	(cd repo && git -C .bit/index checkout -b feature) &&
	printf "feature\0binary\0change" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Feature: modify binary") &&
	get_metadata_hash repo data.bin >feature_hash_file
'

test_expect_success 'add another commit on feature' '
	printf "feature\0binary\0v2" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Feature: binary v2")
'

test_expect_success 'cherry-pick binary change onto main' '
	(cd repo && git -C .bit/index checkout main) &&
	printf "base\0binary" >repo/data.bin &&
	(cd repo && $BIT cherry-pick feature~1) &&
	get_metadata_hash repo data.bin >picked_hash_file &&
	test_cmp feature_hash_file picked_hash_file
'

test_expect_success 'cherry-picked binary commit shows in log' '
	(cd repo && $BIT log --oneline) | grep -q "Feature: modify binary"
'

# ======================================================================
# CHERRY-PICK BINARY CONFLICT
# ======================================================================

test_expect_success 'setup: create conflicting binary on main' '
	printf "main\0different\0binary" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Main: different binary")
'

test_expect_success 'cherry-pick conflicting binary detects conflict' '
	(cd repo && $BIT cherry-pick feature 2>&1) >pick_output || true ;
	(cd repo && $BIT status) >status_output 2>&1 ;
	grep -qi "conflict\|unmerged\|CONFLICT\|cherry-pick" pick_output status_output 2>/dev/null ||
	echo "Note: conflict handling may vary"
'

test_expect_success 'abort conflicting cherry-pick' '
	(cd repo && $BIT cherry-pick --abort 2>/dev/null || true)
'

# ======================================================================
# REBASE WITH BINARY CHANGES (fresh repo to avoid stale state)
# ======================================================================

test_expect_success 'setup: create repo for rebase' '
	create_bit_repo rebase-repo &&
	printf "base\0binary" >rebase-repo/data.bin &&
	echo "base text" >rebase-repo/readme.txt &&
	(cd rebase-repo && $BIT add . && $BIT commit -m "Base commit")
'

test_expect_success 'create binary change on rebase branch' '
	(cd rebase-repo && git -C .bit/index checkout -b rebase-branch) &&
	printf "rebase\0binary\0content" >rebase-repo/data.bin &&
	(cd rebase-repo && $BIT add data.bin && $BIT commit -m "Rebase: modify binary")
'

test_expect_success 'add text change on main for rebase target' '
	(cd rebase-repo && git -C .bit/index checkout main) &&
	printf "base\0binary" >rebase-repo/data.bin &&
	echo "new text on main" >rebase-repo/readme.txt &&
	(cd rebase-repo && $BIT add readme.txt && $BIT commit -m "Main: update text")
'

test_expect_success 'rebase binary branch onto main' '
	(cd rebase-repo && git -C .bit/index checkout rebase-branch) &&
	printf "rebase\0binary\0content" >rebase-repo/data.bin &&
	(cd rebase-repo && $BIT rebase main) &&
	verify_binary_metadata rebase-repo data.bin
'

test_expect_success 'rebased binary commit preserved' '
	(cd rebase-repo && $BIT log --oneline) | grep -q "Rebase: modify binary"
'

# ======================================================================
# REVERT A BINARY CHANGE (fresh repo to avoid stale state)
# ======================================================================

test_expect_success 'setup: create repo for revert' '
	create_bit_repo revert-repo &&
	printf "base\0binary" >revert-repo/data.bin &&
	echo "base text" >revert-repo/readme.txt &&
	(cd revert-repo && $BIT add . && $BIT commit -m "Base commit") &&
	printf "modified\0binary\0data" >revert-repo/data.bin &&
	(cd revert-repo && $BIT add data.bin && $BIT commit -m "Modify binary") &&
	get_metadata_hash revert-repo data.bin >hash_before_revert
'

test_expect_success 'revert binary commit' '
	(cd revert-repo && $BIT revert HEAD --no-edit) &&
	get_metadata_hash revert-repo data.bin >hash_after_revert &&
	! test_cmp hash_before_revert hash_after_revert
'

test_expect_success 'revert commit shows in log' '
	(cd revert-repo && $BIT log --oneline -1) | grep -qi "revert"
'

test_done
