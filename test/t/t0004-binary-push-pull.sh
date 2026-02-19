#!/bin/bash
#
# t0004-binary-push-pull.sh — Test bit push and pull with binary files
#
# Tests the full push/pull cycle between two bit repos using a filesystem
# remote, verifying that binary file content and metadata survive the
# round-trip. Adapted from Git's push/pull tests (t5400, t5500 series)
# focused on binary content integrity.

test_description='bit push and pull with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create remote and two repos' '
	mkdir -p remote &&
	create_bit_repo repo-a &&
	create_bit_repo repo-b &&
	(cd repo-a && $BIT remote add origin ../remote) &&
	(cd repo-b && $BIT remote add origin ../remote)
'

# ======================================================================
# PUSH BINARY FILE FROM REPO-A
# ======================================================================

test_expect_success 'create and push binary file' '
	printf "push\0test\0binary" >repo-a/data.bin &&
	(cd repo-a && $BIT add data.bin && $BIT commit -m "Add data.bin") &&
	verify_binary_metadata repo-a data.bin &&
	(cd repo-a && $BIT push -u origin)
'

test_expect_success 'push multiple binary files' '
	printf "\xFF\xFE\x00\x01\x02\x03" >repo-a/raw.bin &&
	printf "another\0binary\0file" >repo-a/extra.bin &&
	mkdir -p repo-a/subdir &&
	printf "nested\0binary" >repo-a/subdir/deep.bin &&
	(cd repo-a && $BIT add . && $BIT commit -m "Add multiple binaries") &&
	(cd repo-a && $BIT push)
'

# ======================================================================
# PULL BINARY FILES INTO REPO-B
# ======================================================================

test_expect_success 'pull binary files into repo-b' '
	(cd repo-b && $BIT pull origin)
'

test_expect_success 'pulled binary files exist' '
	test -f repo-b/data.bin &&
	test -f repo-b/raw.bin &&
	test -f repo-b/extra.bin &&
	test -f repo-b/subdir/deep.bin
'

test_expect_success 'pulled binary metadata matches' '
	hash_a=$(get_metadata_hash repo-a data.bin) &&
	hash_b=$(get_metadata_hash repo-b data.bin) &&
	test "$hash_a" = "$hash_b" &&
	size_a=$(get_metadata_size repo-a data.bin) &&
	size_b=$(get_metadata_size repo-b data.bin) &&
	test "$size_a" = "$size_b"
'

test_expect_success 'pulled binary content matches' '
	test_cmp_bin repo-a/data.bin repo-b/data.bin &&
	test_cmp_bin repo-a/raw.bin repo-b/raw.bin &&
	test_cmp_bin repo-a/extra.bin repo-b/extra.bin &&
	test_cmp_bin repo-a/subdir/deep.bin repo-b/subdir/deep.bin
'

# ======================================================================
# MODIFY AND RE-PUSH
# ======================================================================

test_expect_success 'modify binary in repo-a and push' '
	printf "modified\0push\0content" >repo-a/data.bin &&
	(cd repo-a && $BIT add data.bin && $BIT commit -m "Modify data.bin") &&
	(cd repo-a && $BIT push)
'

test_expect_success 'pull modified binary into repo-b' '
	(cd repo-b && $BIT pull origin)
'

test_expect_success 'modified binary content matches after pull' '
	test_cmp_bin repo-a/data.bin repo-b/data.bin
'

test_expect_success 'modified binary metadata matches after pull' '
	hash_a=$(get_metadata_hash repo-a data.bin) &&
	hash_b=$(get_metadata_hash repo-b data.bin) &&
	test "$hash_a" = "$hash_b"
'

# ======================================================================
# DELETE BINARY AND PUSH
# ======================================================================

test_expect_success 'delete binary in repo-a and push' '
	(cd repo-a && $BIT rm extra.bin && $BIT commit -m "Delete extra.bin") &&
	(cd repo-a && $BIT push)
'

test_expect_success 'pull deletion into repo-b' '
	(cd repo-b && $BIT pull origin)
'

test_expect_success 'deleted binary is gone in repo-b' '
	test ! -f repo-b/extra.bin
'

# ======================================================================
# PUSH FROM REPO-B (REVERSE DIRECTION)
# ======================================================================

test_expect_success 'repo-b creates binary and pushes' '
	printf "from-b\0binary" >repo-b/from-b.bin &&
	(cd repo-b && $BIT add from-b.bin && $BIT commit -m "Add from-b.bin") &&
	(cd repo-b && $BIT push origin)
'

test_expect_success 'repo-a pulls binary from repo-b' '
	(cd repo-a && $BIT pull origin)
'

test_expect_success 'binary from repo-b exists in repo-a' '
	test -f repo-a/from-b.bin &&
	test_cmp_bin repo-a/from-b.bin repo-b/from-b.bin
'

# ======================================================================
# LARGE BINARY PUSH/PULL
# ======================================================================

test_expect_success 'push large binary (64KB)' '
	generate_binary "large-push" 65536 >repo-a/large.bin &&
	(cd repo-a && $BIT add large.bin && $BIT commit -m "Add large binary") &&
	(cd repo-a && $BIT push)
'

test_expect_success 'pull large binary' '
	(cd repo-b && $BIT pull origin) &&
	test_cmp_bin repo-a/large.bin repo-b/large.bin
'

# ======================================================================
# MIXED TEXT + BINARY PUSH/PULL
# ======================================================================

test_expect_success 'push mixed text and binary changes' '
	echo "new text content" >repo-a/notes.txt &&
	printf "new\0binary\0data" >repo-a/mixed.bin &&
	(cd repo-a && $BIT add . && $BIT commit -m "Add text and binary together") &&
	(cd repo-a && $BIT push)
'

test_expect_success 'pull mixed changes' '
	(cd repo-b && $BIT pull origin) &&
	grep -q "new text content" repo-b/notes.txt &&
	test_cmp_bin repo-a/mixed.bin repo-b/mixed.bin
'

test_done
