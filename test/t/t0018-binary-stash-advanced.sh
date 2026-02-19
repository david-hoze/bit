#!/bin/bash
#
# t0018-binary-stash-advanced.sh — Advanced stash operations with binary files
#
# Adapted from Git's t3903-stash.sh.
# Tests --staged stash, path-restricted stash, and stash with binary in subdirs.

test_description='bit stash advanced operations with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with binary in subdir' '
	create_bit_repo repo &&
	mkdir -p repo/subdir &&
	printf "root\0binary" >repo/root.bin &&
	printf "sub\0binary" >repo/subdir/nested.bin &&
	echo "text file" >repo/readme.txt &&
	echo "sub text" >repo/subdir/notes.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Initial commit")
'

# ======================================================================
# STASH --STAGED WITH BINARY
# ======================================================================

test_expect_success 'modify and stage binary' '
	printf "staged\0binary\0change" >repo/root.bin &&
	(cd repo && $BIT add root.bin) &&
	get_metadata_hash repo root.bin >staged_hash
'

test_expect_success 'stash --staged stashes only staged binary change' '
	(cd repo && $BIT stash --staged) &&
	get_metadata_hash repo root.bin >after_stash_hash &&
	! test_cmp staged_hash after_stash_hash
'

test_expect_success 'stash pop restores staged binary' '
	(cd repo && $BIT stash pop) &&
	get_metadata_hash repo root.bin >popped_hash &&
	test_cmp staged_hash popped_hash
'

test_expect_success 'commit staged change' '
	(cd repo && $BIT add root.bin && $BIT commit -m "Staged change committed")
'

# ======================================================================
# STASH WITH PATH RESTRICTION (subdir)
# ======================================================================

test_expect_success 'modify binary in subdir and root' '
	printf "modified\0root" >repo/root.bin &&
	printf "modified\0sub" >repo/subdir/nested.bin &&
	(cd repo && $BIT add .)
'

test_expect_success 'stash push -- subdir/ stashes only subdir binary' '
	get_metadata_hash repo root.bin >root_before_stash &&
	get_metadata_hash repo subdir/nested.bin >sub_before_stash &&
	(cd repo && $BIT stash push -- subdir/) &&
	get_metadata_hash repo root.bin >root_after_stash &&
	test_cmp root_before_stash root_after_stash
'

test_expect_success 'stash pop restores subdir binary' '
	(cd repo && $BIT stash pop) &&
	get_metadata_hash repo subdir/nested.bin >sub_after_pop &&
	test_cmp sub_before_stash sub_after_pop
'

test_expect_success 'commit all changes' '
	(cd repo && $BIT add . && $BIT commit -m "All modified")
'

# ======================================================================
# STASH LIST AND SHOW WITH BINARY
# ======================================================================

test_expect_success 'modify binary and stash' '
	printf "for\0list\0test" >repo/root.bin &&
	(cd repo && $BIT add root.bin && $BIT stash)
'

test_expect_success 'stash list shows the stash' '
	(cd repo && $BIT stash list) >stash_list 2>&1 &&
	test -s stash_list
'

test_expect_success 'stash show mentions binary file' '
	(cd repo && $BIT stash show) >stash_show 2>&1 &&
	grep -q "root.bin" stash_show
'

test_expect_success 'stash drop cleans up' '
	(cd repo && $BIT stash drop)
'

# ======================================================================
# MULTIPLE STASHES WITH BINARY
# ======================================================================

test_expect_success 'create first stash with binary change' '
	printf "stash1\0binary" >repo/root.bin &&
	(cd repo && $BIT add root.bin && $BIT stash)
'

test_expect_success 'create second stash with different binary change' '
	printf "stash2\0binary" >repo/root.bin &&
	(cd repo && $BIT add root.bin && $BIT stash)
'

test_expect_success 'stash list shows both stashes' '
	(cd repo && $BIT stash list) >stash_list2 2>&1 &&
	count=$(wc -l <stash_list2) &&
	test "$count" -ge 2
'

test_expect_success 'pop latest stash and verify' '
	(cd repo && $BIT stash pop) &&
	verify_binary_metadata repo root.bin
'

test_expect_success 'drop remaining stash' '
	(cd repo && $BIT add root.bin && $BIT commit -m "Popped stash 2") &&
	(cd repo && $BIT stash drop)
'

test_done
