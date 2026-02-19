#!/bin/bash
#
# t0005-binary-checkout-restore.sh — Test checkout/restore with binary files
#
# Adapted from Git's t7201-co.sh. Tests that switching branches and restoring
# binary files works correctly through bit.

test_description='bit checkout and restore with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with binary on main' '
	create_bit_repo repo &&
	printf "main\0binary\0v1" >repo/data.bin &&
	echo "main text v1" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Initial on main")
'

# ======================================================================
# BRANCH WITH BINARY CHANGES
# ======================================================================

test_expect_success 'create branch and modify binary' '
	(cd repo && git -C .bit/index checkout -b feature) &&
	printf "feature\0binary\0v2" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Modify binary on feature")
'

test_expect_success 'binary metadata differs between branches' '
	get_metadata_hash repo data.bin >feature_hash_file &&
	(cd repo && git -C .bit/index checkout main) &&
	get_metadata_hash repo data.bin >main_hash_file &&
	! test_cmp feature_hash_file main_hash_file
'

test_expect_success 'switch back to feature — binary metadata restored' '
	(cd repo && git -C .bit/index checkout feature) &&
	get_metadata_hash repo data.bin >current_hash_file &&
	test_cmp feature_hash_file current_hash_file
'

# ======================================================================
# CHECKOUT WITH BINARY FILE ADDED ON BRANCH
# ======================================================================

test_expect_success 'add binary only on feature branch' '
	(cd repo && git -C .bit/index checkout -b binary-only) &&
	printf "branch\0only\0binary" >repo/branch-only.bin &&
	(cd repo && $BIT add branch-only.bin && $BIT commit -m "Add branch-only.bin")
'

test_expect_success 'switch to main — branch-only binary not in metadata' '
	(cd repo && git -C .bit/index checkout main) &&
	test ! -f repo/.bit/index/branch-only.bin
'

test_expect_success 'switch back — branch-only binary reappears in metadata' '
	(cd repo && git -C .bit/index checkout binary-only) &&
	verify_binary_metadata repo branch-only.bin
'

# ======================================================================
# CHECKOUT WITH BINARY DELETED ON BRANCH
# ======================================================================

test_expect_success 'setup: go to main and add a deletable binary' '
	(cd repo && git -C .bit/index checkout main) &&
	printf "delete\0me" >repo/deletable.bin &&
	(cd repo && $BIT add deletable.bin && $BIT commit -m "Add deletable.bin")
'

test_expect_success 'delete binary on branch using bit rm' '
	(cd repo && git -C .bit/index checkout -b delete-branch) &&
	(cd repo && $BIT rm deletable.bin) &&
	(cd repo && $BIT commit -m "Delete deletable.bin")
'

test_expect_success 'switch to main — deleted binary reappears' '
	(cd repo && git -C .bit/index checkout main) &&
	verify_binary_metadata repo deletable.bin
'

test_expect_success 'switch to delete-branch — binary is gone' '
	(cd repo && git -C .bit/index checkout delete-branch) &&
	test ! -f repo/.bit/index/deletable.bin
'

test_done
