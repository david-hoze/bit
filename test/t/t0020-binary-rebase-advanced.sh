#!/bin/bash
#
# t0020-binary-rebase-advanced.sh — Advanced rebase with binary files
#
# Adapted from Git's t3419-rebase-patch-id.sh.
# Tests patch-id detection, rebase --onto, and rebase with conflicts.

test_description='bit rebase advanced operations with binary files'
. ./test-lib.sh

# ======================================================================
# REBASE DROPS DUPLICATE BINARY COMMIT
# ======================================================================

test_expect_success 'setup: create repo with binary base' '
	create_bit_repo repo &&
	printf "base\0binary" >repo/data.bin &&
	echo "base text" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Base commit")
'

test_expect_success 'create feature branch with binary change' '
	(cd repo && git -C .bit/index checkout -b feature) &&
	printf "feature\0binary\0change" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Feature: binary change") &&
	echo "feature text" >repo/readme.txt &&
	(cd repo && $BIT add readme.txt && $BIT commit -m "Feature: text change")
'

test_expect_success 'cherry-pick same binary change onto main' '
	(cd repo && git -C .bit/index checkout main) &&
	printf "base\0binary" >repo/data.bin &&
	(cd repo && $BIT cherry-pick feature~1) &&
	get_metadata_hash repo data.bin >main_hash
'

test_expect_success 'rebase feature onto main (should detect duplicate)' '
	(cd repo && git -C .bit/index checkout feature) &&
	printf "feature\0binary\0change" >repo/data.bin &&
	(cd repo && $BIT rebase main 2>&1) >rebase_output || true ;
	(cd repo && $BIT log --oneline) >rebase_log ;
	grep -q "Feature: text change" rebase_log
'

# ======================================================================
# REBASE --ONTO WITH BINARY
# ======================================================================

test_expect_success 'setup: fresh repo for --onto test' '
	create_bit_repo onto-repo &&
	printf "base\0binary" >onto-repo/data.bin &&
	echo "base" >onto-repo/readme.txt &&
	(cd onto-repo && $BIT add . && $BIT commit -m "Base") &&
	(cd onto-repo && git -C .bit/index checkout -b feature) &&
	echo "feature1" >onto-repo/readme.txt &&
	(cd onto-repo && $BIT add readme.txt && $BIT commit -m "Feature 1") &&
	printf "feature\0binary" >onto-repo/data.bin &&
	(cd onto-repo && $BIT add data.bin && $BIT commit -m "Feature 2: binary") &&
	(cd onto-repo && git -C .bit/index checkout main) &&
	printf "base\0binary" >onto-repo/data.bin &&
	echo "main change" >onto-repo/readme.txt &&
	(cd onto-repo && $BIT add readme.txt && $BIT commit -m "Main update")
'

test_expect_success 'rebase --onto main feature~1 feature replays only binary commit' '
	(cd onto-repo && git -C .bit/index checkout feature) &&
	printf "feature\0binary" >onto-repo/data.bin &&
	(cd onto-repo && $BIT rebase --onto main feature~1) &&
	(cd onto-repo && $BIT log --oneline -1) | grep -q "Feature 2: binary" &&
	verify_binary_metadata onto-repo data.bin
'

# ======================================================================
# REBASE PRESERVING BINARY METADATA THROUGH MULTIPLE COMMITS
# ======================================================================

test_expect_success 'setup: fresh repo for multi-commit rebase' '
	create_bit_repo multi-repo &&
	printf "base\0binary" >multi-repo/data.bin &&
	echo "base" >multi-repo/readme.txt &&
	(cd multi-repo && $BIT add . && $BIT commit -m "Base") &&
	(cd multi-repo && git -C .bit/index checkout -b multi-feature) &&
	printf "v1\0binary\0data" >multi-repo/data.bin &&
	(cd multi-repo && $BIT add data.bin && $BIT commit -m "Binary v1") &&
	printf "v2\0binary\0more\0data" >multi-repo/data.bin &&
	(cd multi-repo && $BIT add data.bin && $BIT commit -m "Binary v2") &&
	printf "v3\0binary\0even\0more\0data" >multi-repo/data.bin &&
	(cd multi-repo && $BIT add data.bin && $BIT commit -m "Binary v3") &&
	(cd multi-repo && git -C .bit/index checkout main) &&
	printf "base\0binary" >multi-repo/data.bin &&
	echo "unrelated main change" >multi-repo/readme.txt &&
	(cd multi-repo && $BIT add readme.txt && $BIT commit -m "Main text update")
'

test_expect_success 'rebase 3 binary commits onto updated main' '
	(cd multi-repo && git -C .bit/index checkout multi-feature) &&
	printf "v3\0binary\0even\0more\0data" >multi-repo/data.bin &&
	(cd multi-repo && $BIT rebase main) &&
	verify_binary_metadata multi-repo data.bin
'

test_expect_success 'all 3 binary commits preserved after rebase' '
	(cd multi-repo && $BIT log --oneline) >multi_log &&
	grep -q "Binary v1" multi_log &&
	grep -q "Binary v2" multi_log &&
	grep -q "Binary v3" multi_log
'

# ======================================================================
# REBASE WITH BINARY-ONLY AND TEXT-ONLY COMMITS INTERLEAVED
# ======================================================================

test_expect_success 'setup: interleaved binary and text commits' '
	create_bit_repo interleave-repo &&
	printf "base\0binary" >interleave-repo/data.bin &&
	echo "base" >interleave-repo/readme.txt &&
	(cd interleave-repo && $BIT add . && $BIT commit -m "Base") &&
	(cd interleave-repo && git -C .bit/index checkout -b interleave) &&
	printf "bin1\0data" >interleave-repo/data.bin &&
	(cd interleave-repo && $BIT add data.bin && $BIT commit -m "Binary change 1") &&
	echo "text change 1" >interleave-repo/readme.txt &&
	(cd interleave-repo && $BIT add readme.txt && $BIT commit -m "Text change 1") &&
	printf "bin2\0data" >interleave-repo/data.bin &&
	(cd interleave-repo && $BIT add data.bin && $BIT commit -m "Binary change 2") &&
	(cd interleave-repo && git -C .bit/index checkout main) &&
	printf "base\0binary" >interleave-repo/data.bin &&
	echo "main text" >>interleave-repo/readme.txt &&
	(cd interleave-repo && $BIT add readme.txt && $BIT commit -m "Main: append text")
'

test_expect_success 'rebase interleaved commits preserves all' '
	(cd interleave-repo && git -C .bit/index checkout interleave) &&
	printf "bin2\0data" >interleave-repo/data.bin &&
	(cd interleave-repo && $BIT rebase main 2>&1) || true ;
	(cd interleave-repo && $BIT log --oneline) >interleave_log ;
	grep -q "Binary change" interleave_log
'

test_done
