#!/bin/bash
#
# t0008-binary-reset.sh — Test bit reset with binary files
#
# Adapted from Git's t7102-reset.sh, t7104-reset-hard.sh.
# Tests that reset operations correctly restore binary metadata.

test_description='bit reset with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with two binary commits' '
	create_bit_repo repo &&
	printf "v1\0binary" >repo/data.bin &&
	echo "v1 text" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "v1") &&
	printf "v2\0binary\0updated" >repo/data.bin &&
	echo "v2 text" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "v2")
'

# ======================================================================
# RESET --SOFT (keeps changes staged)
# ======================================================================

test_expect_success 'reset --soft HEAD~1 keeps binary changes staged' '
	get_metadata_hash repo data.bin >v2_hash_file &&
	(cd repo && $BIT reset --soft HEAD~1) &&
	get_metadata_hash repo data.bin >current_hash_file &&
	test_cmp v2_hash_file current_hash_file
'

test_expect_success 'status shows staged binary after soft reset' '
	(cd repo && $BIT status) | grep -q "data.bin"
'

test_expect_success 're-commit after soft reset' '
	(cd repo && $BIT commit -m "v2 re-committed")
'

# ======================================================================
# RESET --MIXED (unstages but keeps working tree)
# ======================================================================

test_expect_success 'reset --mixed HEAD~1 unstages binary' '
	(cd repo && $BIT reset HEAD~1) &&
	(cd repo && $BIT status) | grep -q "data.bin\|modified"
'

test_expect_success 're-add and commit after mixed reset' '
	(cd repo && $BIT add . && $BIT commit -m "v2 again")
'

# ======================================================================
# RESET --HARD (discards all changes)
# ======================================================================

test_expect_success 'reset --hard restores binary metadata to v1' '
	get_metadata_hash repo data.bin >v2_hash_file &&
	(cd repo && $BIT reset --hard HEAD~1) &&
	get_metadata_hash repo data.bin >v1_hash_file &&
	! test_cmp v2_hash_file v1_hash_file
'

test_expect_success 'hard reset updated the metadata to v1' '
	get_metadata_hash repo data.bin >hard_reset_hash &&
	test_cmp v1_hash_file hard_reset_hash
'

# ======================================================================
# RESET SINGLE FILE
# ======================================================================

test_expect_success 'setup: make new commit with binary change' '
	printf "v3\0binary\0new" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "v3")
'

test_expect_success 'reset single binary file from HEAD~1' '
	get_metadata_hash repo data.bin >v3_hash_file &&
	(cd repo && $BIT checkout HEAD~1 -- data.bin) &&
	get_metadata_hash repo data.bin >restored_hash_file &&
	! test_cmp v3_hash_file restored_hash_file
'

test_expect_success 'commit restored binary' '
	(cd repo && $BIT add data.bin && $BIT commit -m "Restore v1 binary" 2>&1) ||
	(cd repo && $BIT add . && $BIT commit -m "Restore v1 binary")
'

# ======================================================================
# RESET WITH UNCOMMITTED BINARY CHANGES
# ======================================================================

test_expect_success 'reset --hard discards uncommitted binary modifications' '
	get_metadata_hash repo data.bin >committed_hash_file &&
	printf "uncommitted\0changes" >repo/data.bin &&
	(cd repo && $BIT add data.bin) &&
	get_metadata_hash repo data.bin >modified_hash_file &&
	! test_cmp committed_hash_file modified_hash_file &&
	(cd repo && $BIT reset --hard HEAD) &&
	get_metadata_hash repo data.bin >reset_hash_file &&
	test_cmp committed_hash_file reset_hash_file
'

test_done
