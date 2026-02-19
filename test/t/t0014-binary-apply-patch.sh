#!/bin/bash
#
# t0014-binary-apply-patch.sh — Test applying patches with binary file changes
#
# Adapted from Git's t4103-apply-binary.sh, t4108-apply-threeway.sh.
# Tests that git format-patch / git am works when binary metadata changes.

test_description='bit apply and patch with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with binary file' '
	create_bit_repo repo &&
	printf "original\0binary" >repo/data.bin &&
	echo "original text" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Initial commit")
'

# ======================================================================
# FORMAT-PATCH WITH BINARY METADATA
# ======================================================================

test_expect_success 'create commit with binary change' '
	printf "modified\0binary\0content" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Modify binary")
'

test_expect_success 'format-patch generates patch for binary change' '
	(cd repo && $BIT format-patch -1 --stdout) >patch_file &&
	grep -q "data.bin" patch_file
'

# ======================================================================
# AM WITH BINARY METADATA (format-patch round-trip)
# ======================================================================

test_expect_success 'reset to before binary change' '
	(cd repo && $BIT reset --hard HEAD~1) &&
	printf "original\0binary" >repo/data.bin
'

test_expect_success 'am applies patch with binary metadata change' '
	patch_abs="$(pwd)/patch_file" &&
	(cd repo && $BIT am "$patch_abs") &&
	verify_binary_metadata repo data.bin
'

test_expect_success 'am preserved commit message' '
	(cd repo && $BIT log --oneline -1) | grep -q "Modify binary"
'

# ======================================================================
# AM WITH MULTI-FILE PATCH
# ======================================================================

test_expect_success 'setup: create another binary change' '
	printf "v3\0binary\0final" >repo/data.bin &&
	echo "v3 text" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Update to v3")
'

test_expect_success 'format-patch to directory' '
	mkdir -p patches &&
	(cd repo && $BIT format-patch -1 -o "$(cd .. && pwd)/patches")
'

test_expect_success 'reset and am the patch from directory' '
	(cd repo && $BIT reset --hard HEAD~1) &&
	printf "modified\0binary\0content" >repo/data.bin &&
	patch_abs="$(pwd)/patches" &&
	patch_file=$(ls "$patch_abs"/*.patch) &&
	(cd repo && $BIT am "$patch_file") &&
	verify_binary_metadata repo data.bin
'

test_expect_success 'am preserved commit message from directory' '
	(cd repo && $BIT log --oneline -1) | grep -q "Update to v3"
'

# ======================================================================
# FORMAT-PATCH WITH BINARY ADD AND DELETE
# ======================================================================

test_expect_success 'format-patch for new binary file' '
	printf "brand\0new\0binary" >repo/new-file.bin &&
	(cd repo && $BIT add new-file.bin && $BIT commit -m "Add new-file.bin") &&
	(cd repo && $BIT format-patch -1 --stdout) >add_patch &&
	grep -q "new-file.bin" add_patch
'

test_expect_success 'format-patch for deleted binary file' '
	(cd repo && $BIT rm new-file.bin && $BIT commit -m "Remove new-file.bin") &&
	(cd repo && $BIT format-patch -1 --stdout) >delete_patch &&
	grep -q "new-file.bin" delete_patch
'

# ======================================================================
# DIFF PATCH BETWEEN BRANCHES
# ======================================================================

test_expect_success 'create branch with binary changes' '
	(cd repo && $BIT checkout -b patch-branch) &&
	printf "patched\0binary" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Patch branch binary")
'

test_expect_success 'generate diff between branches' '
	(cd repo && $BIT diff main..patch-branch) >branch_diff 2>&1 &&
	grep -q "data.bin" branch_diff
'

test_done
