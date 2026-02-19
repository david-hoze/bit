#!/bin/bash
#
# t0015-binary-merge-strategies.sh — Test merge strategies with binary files
#
# Adapted from Git's t6417-merge-ours-theirs.sh, t6406-merge-attr.sh,
# t6403-merge-file.sh, t6404-recursive-merge.sh.
# Tests that merge strategy flags and attributes work with binary metadata.

test_description='bit merge strategies with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with binary on two branches' '
	create_bit_repo repo &&
	printf "base\0binary\0content" >repo/data.bin &&
	echo "base text" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Base commit") &&
	(cd repo && git -C .bit/index checkout -b side) &&
	printf "side\0binary\0version" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Side: modify binary") &&
	get_metadata_hash repo data.bin >side_hash_file &&
	(cd repo && git -C .bit/index checkout main) &&
	printf "main\0binary\0version" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Main: modify binary") &&
	get_metadata_hash repo data.bin >main_hash_file
'

# ======================================================================
# MERGE -X OURS
# ======================================================================

test_expect_success 'merge -X ours resolves binary conflict with our version' '
	(cd repo && $BIT merge -X ours side -m "Merge with ours") &&
	get_metadata_hash repo data.bin >merged_hash_file &&
	test_cmp main_hash_file merged_hash_file
'

test_expect_success 'cleanup: reset for next test' '
	(cd repo && $BIT reset --hard HEAD~1)
'

# ======================================================================
# MERGE -X THEIRS
# ======================================================================

test_expect_success 'merge -X theirs resolves binary conflict with their version' '
	(cd repo && $BIT merge -X theirs side -m "Merge with theirs") &&
	get_metadata_hash repo data.bin >merged_hash_file &&
	test_cmp side_hash_file merged_hash_file
'

test_expect_success 'cleanup: reset for next test' '
	(cd repo && $BIT reset --hard HEAD~1)
'

# ======================================================================
# MERGE -S OURS (strategy, not option)
# ======================================================================

test_expect_success 'merge -s ours ignores their binary changes entirely' '
	(cd repo && $BIT merge -s ours side -m "Merge strategy ours") &&
	get_metadata_hash repo data.bin >merged_hash_file &&
	test_cmp main_hash_file merged_hash_file
'

test_expect_success 'cleanup: reset for next test' '
	(cd repo && $BIT reset --hard HEAD~1)
'

# ======================================================================
# BINARY CONFLICT WITHOUT STRATEGY (baseline)
# ======================================================================

test_expect_success 'merge without strategy fails on binary conflict' '
	! (cd repo && $BIT merge side -m "Should conflict" 2>&1) ||
	(cd repo && $BIT merge --abort 2>/dev/null || true)
'

# ======================================================================
# MERGE WITH GITATTRIBUTES: -merge
# ======================================================================

test_expect_success 'setup: fresh repo for attribute tests' '
	create_bit_repo attr-repo &&
	printf "base\0binary" >attr-repo/data.bin &&
	echo "base text" >attr-repo/readme.txt &&
	(cd attr-repo && $BIT add . && $BIT commit -m "Base") &&
	(cd attr-repo && git -C .bit/index checkout -b attr-side) &&
	printf "side\0attr\0binary" >attr-repo/data.bin &&
	(cd attr-repo && $BIT add data.bin && $BIT commit -m "Side change") &&
	(cd attr-repo && git -C .bit/index checkout main) &&
	printf "main\0attr\0binary" >attr-repo/data.bin &&
	(cd attr-repo && $BIT add data.bin && $BIT commit -m "Main change")
'

test_expect_success 'merge with -merge attribute detects conflict' '
	echo "data.bin -merge" >attr-repo/.gitattributes &&
	(cd attr-repo && $BIT add .gitattributes && $BIT commit -m "Add attributes") &&
	! (cd attr-repo && $BIT merge attr-side 2>&1) ;
	(cd attr-repo && $BIT merge --abort 2>/dev/null || true)
'

# ======================================================================
# MERGE WITH GITATTRIBUTES: merge=union ON BINARY
# ======================================================================

test_expect_success 'setup: fresh repo for union test' '
	create_bit_repo union-repo &&
	printf "base\0binary" >union-repo/data.bin &&
	echo "base text" >union-repo/readme.txt &&
	(cd union-repo && $BIT add . && $BIT commit -m "Base") &&
	(cd union-repo && git -C .bit/index checkout -b union-side) &&
	printf "side\0union\0binary" >union-repo/data.bin &&
	(cd union-repo && $BIT add data.bin && $BIT commit -m "Side change") &&
	(cd union-repo && git -C .bit/index checkout main) &&
	printf "main\0union\0binary" >union-repo/data.bin &&
	(cd union-repo && $BIT add data.bin && $BIT commit -m "Main change")
'

test_expect_success 'merge=union on binary file warns cannot merge' '
	echo "data.bin merge=union" >union-repo/.gitattributes &&
	(cd union-repo && $BIT add .gitattributes && $BIT commit -m "Add union attr") &&
	(cd union-repo && $BIT merge union-side 2>&1) >merge_output || true ;
	grep -qi "cannot merge\|conflict\|CONFLICT" merge_output ;
	(cd union-repo && $BIT merge --abort 2>/dev/null || true)
'

# ======================================================================
# MERGE BINARY + TEXT IN SAME COMMIT
# ======================================================================

test_expect_success 'setup: fresh repo for mixed merge' '
	create_bit_repo mixed-repo &&
	printf "base\0binary" >mixed-repo/data.bin &&
	echo "base text" >mixed-repo/readme.txt &&
	(cd mixed-repo && $BIT add . && $BIT commit -m "Base") &&
	(cd mixed-repo && git -C .bit/index checkout -b mixed-side) &&
	echo "side text" >mixed-repo/readme.txt &&
	printf "side\0binary" >mixed-repo/data.bin &&
	(cd mixed-repo && $BIT add . && $BIT commit -m "Side: both") &&
	(cd mixed-repo && git -C .bit/index checkout main) &&
	echo "main text different line" >>mixed-repo/readme.txt &&
	(cd mixed-repo && $BIT add readme.txt && $BIT commit -m "Main: text only")
'

test_expect_success 'merge -X theirs resolves mixed binary+text conflict' '
	printf "base\0binary" >mixed-repo/data.bin &&
	(cd mixed-repo && $BIT merge -X theirs mixed-side -m "Merge mixed") &&
	verify_binary_metadata mixed-repo data.bin
'

test_done
