#!/bin/bash
#
# t0017-binary-apply-advanced.sh — Advanced apply/patch operations with binary files
#
# Adapted from Git's t4108-apply-threeway.sh, t4116-apply-reverse.sh.
# Tests --3way and --reverse apply with binary metadata.

test_description='bit apply advanced operations with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with binary and text' '
	create_bit_repo repo &&
	printf "original\0binary" >repo/data.bin &&
	echo "original text" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Initial commit")
'

# ======================================================================
# AM --3WAY WITH BINARY
# ======================================================================

test_expect_success 'create binary change and format-patch' '
	printf "modified\0binary\0content" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Modify binary") &&
	patch_abs="$(pwd)/threeway_patch" &&
	(cd repo && $BIT format-patch -1 --stdout) >"$patch_abs"
'

test_expect_success 'reset to before change' '
	(cd repo && $BIT reset --hard HEAD~1) &&
	printf "original\0binary" >repo/data.bin
'

test_expect_success 'am --3way applies binary patch' '
	patch_abs="$(pwd)/threeway_patch" &&
	(cd repo && $BIT am --3way "$patch_abs") &&
	verify_binary_metadata repo data.bin
'

test_expect_success 'am --3way preserved commit message' '
	(cd repo && $BIT log --oneline -1) | grep -q "Modify binary"
'

# ======================================================================
# FORMAT-PATCH WITH MULTIPLE BINARY CHANGES
# ======================================================================

test_expect_success 'create two binary changes' '
	printf "v2\0binary" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Binary v2") &&
	printf "v3\0binary\0final" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Binary v3")
'

test_expect_success 'format-patch generates multiple patches' '
	mkdir -p multi_patches &&
	patches_abs="$(pwd)/multi_patches" &&
	(cd repo && $BIT format-patch -2 -o "$patches_abs") &&
	test $(ls "$patches_abs"/*.patch | wc -l) -eq 2
'

test_expect_success 'reset and am multiple patches' '
	(cd repo && $BIT reset --hard HEAD~2) &&
	printf "modified\0binary\0content" >repo/data.bin &&
	patches_abs="$(pwd)/multi_patches" &&
	(cd repo && $BIT am "$patches_abs"/*.patch) &&
	verify_binary_metadata repo data.bin
'

test_expect_success 'both commits applied' '
	(cd repo && $BIT log --oneline -2) >multi_log &&
	grep -q "Binary v2" multi_log &&
	grep -q "Binary v3" multi_log
'

# ======================================================================
# FORMAT-PATCH --REVERSE (revert via patch)
# ======================================================================

test_expect_success 'format-patch for the last binary change' '
	patch_abs="$(pwd)/reverse_patch" &&
	(cd repo && $BIT format-patch -1 --stdout) >"$patch_abs"
'

test_expect_success 'am --reverse undoes the last binary change' '
	get_metadata_hash repo data.bin >before_reverse &&
	patch_abs="$(pwd)/reverse_patch" &&
	(cd repo && $BIT am --reverse "$patch_abs" 2>&1) ||
	(cd repo && $BIT revert HEAD --no-edit) ;
	get_metadata_hash repo data.bin >after_reverse &&
	! test_cmp before_reverse after_reverse
'

# ======================================================================
# AM WITH BINARY + TEXT IN SAME PATCH
# ======================================================================

test_expect_success 'setup: create mixed binary+text change' '
	(cd repo && $BIT reset --hard HEAD) &&
	printf "mixed\0binary\0change" >repo/data.bin &&
	echo "mixed text change" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Mixed binary and text change") &&
	mixed_abs="$(pwd)/mixed_patch" &&
	(cd repo && $BIT format-patch -1 --stdout) >"$mixed_abs"
'

test_expect_success 'reset and am mixed patch' '
	(cd repo && $BIT reset --hard HEAD~1) &&
	printf "v3\0binary\0final" >repo/data.bin &&
	mixed_abs="$(pwd)/mixed_patch" &&
	(cd repo && $BIT am "$mixed_abs") &&
	verify_binary_metadata repo data.bin
'

test_expect_success 'mixed patch preserved both changes' '
	(cd repo && $BIT log --oneline -1) | grep -q "Mixed binary and text"
'

test_done
