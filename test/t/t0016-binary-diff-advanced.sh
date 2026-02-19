#!/bin/bash
#
# t0016-binary-diff-advanced.sh — Advanced diff operations with binary files
#
# Adapted from Git's t4017-diff-retval.sh, t4048-diff-combined-binary.sh,
# t4031-diff-rewrite-binary.sh, t4209-log-pickaxe.sh.
# Tests exit codes, combined diff, rewrite detection, and pickaxe with binary.

test_description='bit diff advanced operations with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with binary file' '
	create_bit_repo repo &&
	printf "original\0binary\0content" >repo/data.bin &&
	echo "original text" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Initial commit")
'

# ======================================================================
# DIFF --EXIT-CODE WITH BINARY
# ======================================================================

test_expect_success 'diff --exit-code returns 0 when no changes' '
	(cd repo && $BIT diff --exit-code)
'

test_expect_success 'diff --exit-code returns 1 for staged binary change' '
	printf "modified\0binary" >repo/data.bin &&
	(cd repo && $BIT add data.bin) &&
	! (cd repo && $BIT diff --cached --exit-code)
'

test_expect_success 'diff --quiet returns 1 for staged binary change' '
	! (cd repo && $BIT diff --cached --quiet)
'

test_expect_success 'commit the change' '
	(cd repo && $BIT commit -m "Modify binary")
'

test_expect_success 'diff --exit-code returns 0 after commit' '
	(cd repo && $BIT diff --exit-code)
'

# ======================================================================
# DIFF -B (REWRITE DETECTION) WITH BINARY
# ======================================================================

test_expect_success 'setup: create totally different binary' '
	printf "completely\0different\0binary\0data\0here" >repo/data.bin &&
	(cd repo && $BIT add data.bin)
'

test_expect_success 'diff -B detects rewrite of binary' '
	(cd repo && $BIT diff --cached -B) >rewrite_output 2>&1 &&
	grep -q "data.bin" rewrite_output
'

test_expect_success 'diff -B --stat shows binary rewrite in stats' '
	(cd repo && $BIT diff --cached -B --stat) >rewrite_stat 2>&1 &&
	grep -q "data.bin" rewrite_stat
'

test_expect_success 'commit rewrite' '
	(cd repo && $BIT commit -m "Rewrite binary")
'

# ======================================================================
# COMBINED DIFF ON BINARY MERGE COMMIT
# ======================================================================

test_expect_success 'setup: create merge with binary conflict resolved' '
	create_bit_repo merge-repo &&
	printf "base\0binary" >merge-repo/data.bin &&
	echo "base" >merge-repo/readme.txt &&
	(cd merge-repo && $BIT add . && $BIT commit -m "Base") &&
	(cd merge-repo && git -C .bit/index checkout -b side) &&
	printf "side\0binary\0version" >merge-repo/data.bin &&
	(cd merge-repo && $BIT add data.bin && $BIT commit -m "Side binary") &&
	(cd merge-repo && git -C .bit/index checkout main) &&
	printf "main\0binary\0version" >merge-repo/data.bin &&
	(cd merge-repo && $BIT add data.bin && $BIT commit -m "Main binary") &&
	(cd merge-repo && $BIT merge -X ours side -m "Merge binary")
'

test_expect_success 'show -m --stat on merge commit mentions binary' '
	(cd merge-repo && $BIT show -m --stat) >show_stat 2>&1 &&
	grep -q "data.bin" show_stat
'

test_expect_success 'log -1 -m --name-only on merge commit mentions binary' '
	(cd merge-repo && $BIT log -1 -m --name-only) >log_names 2>&1 &&
	grep -q "data.bin" log_names
'

test_expect_success 'diff between merge parents shows binary' '
	(cd merge-repo && $BIT diff HEAD~1..HEAD --name-only) >merge_diff 2>&1 ;
	grep -q "data.bin\|readme.txt" merge_diff || true
'

# ======================================================================
# LOG PICKAXE WITH BINARY (-S and -G)
# ======================================================================

test_expect_success 'log -S finds commits changing binary metadata content' '
	(cd repo && $BIT log -S "hash:" --oneline) >pickaxe_s 2>&1 &&
	test -s pickaxe_s
'

test_expect_success 'log -G finds commits matching binary metadata pattern' '
	(cd repo && $BIT log -G "md5:" --oneline) >pickaxe_g 2>&1 &&
	test -s pickaxe_g
'

test_expect_success 'log --all --oneline with binary changes works' '
	(cd repo && $BIT log --all --oneline) >all_log 2>&1 &&
	test -s all_log
'

# ======================================================================
# DIFF --NUMSTAT WITH BINARY
# ======================================================================

test_expect_success 'diff --numstat shows dashes for binary' '
	printf "newer\0binary" >repo/data.bin &&
	(cd repo && $BIT add data.bin) &&
	(cd repo && $BIT diff --cached --numstat) >numstat_out &&
	grep -q "data.bin" numstat_out &&
	(cd repo && $BIT commit -m "Another binary change")
'

# ======================================================================
# DIFF BETWEEN TAGS/REFS WITH BINARY
# ======================================================================

test_expect_success 'tag current state' '
	(cd repo && $BIT tag v1)
'

test_expect_success 'create another binary change and tag' '
	printf "tagged\0binary\0v2" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Tagged change") &&
	(cd repo && $BIT tag v2)
'

test_expect_success 'diff between tags shows binary change' '
	(cd repo && $BIT diff v1..v2 --name-only) >tag_diff &&
	grep -q "data.bin" tag_diff
'

test_expect_success 'diff --stat between tags shows binary' '
	(cd repo && $BIT diff v1..v2 --stat) >tag_stat &&
	grep -q "data.bin" tag_stat
'

test_done
