#!/bin/bash
#
# t0010-binary-log-show.sh — Test bit log and show with binary files
#
# Adapted from Git's t4202-log.sh, t4209-log-pickaxe.sh.
# Tests that log/show correctly display binary file changes.

test_description='bit log and show with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with binary history' '
	create_bit_repo repo &&
	printf "v1\0binary" >repo/data.bin &&
	echo "v1 text" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Add binary v1") &&
	printf "v2\0binary\0updated" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Update binary to v2") &&
	printf "v3\0final" >repo/data.bin &&
	echo "v3 text" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Update all to v3")
'

# ======================================================================
# LOG
# ======================================================================

test_expect_success 'log --oneline shows all commits' '
	(cd repo && $BIT log --oneline) >log_output &&
	grep -q "Add binary v1" log_output &&
	grep -q "Update binary to v2" log_output &&
	grep -q "Update all to v3" log_output
'

test_expect_success 'log with path filter shows only binary commits' '
	(cd repo && $BIT log --oneline -- data.bin) >log_output &&
	test "$(wc -l <log_output | tr -d " ")" = "3"
'

test_expect_success 'log -p shows diff for binary changes' '
	(cd repo && $BIT log -p -- data.bin) >log_p_output 2>&1 &&
	grep -q "data.bin" log_p_output
'

test_expect_success 'log --stat shows binary file in stats' '
	(cd repo && $BIT log --stat -- data.bin) >log_stat_output &&
	grep -q "data.bin" log_stat_output
'

test_expect_success 'log --follow tracks binary renames' '
	mv repo/data.bin repo/renamed.bin &&
	(cd repo && $BIT add . && $BIT commit -m "Rename binary") &&
	(cd repo && $BIT log --oneline --follow -- renamed.bin) >follow_output &&
	grep -q "Rename binary" follow_output &&
	grep -q "Update all to v3" follow_output
'

# ======================================================================
# SHOW
# ======================================================================

test_expect_success 'show HEAD displays binary change' '
	(cd repo && $BIT show HEAD) >show_output 2>&1 &&
	grep -q "renamed.bin\|data.bin" show_output
'

test_expect_success 'show specific commit with binary' '
	commit=$(cd repo && $BIT log --oneline | grep "Update binary to v2" | cut -d" " -f1) &&
	(cd repo && $BIT show "$commit") >show_output 2>&1 &&
	grep -q "data.bin" show_output
'

test_expect_success 'show --stat with binary' '
	(cd repo && $BIT show --stat HEAD~1) >stat_output &&
	grep -q "data.bin\|renamed.bin" stat_output
'

# ======================================================================
# DIFF BETWEEN COMMITS
# ======================================================================

test_expect_success 'diff between two commits with binary' '
	(cd repo && $BIT diff HEAD~3..HEAD~1 -- data.bin) >diff_output 2>&1 &&
	grep -q "data.bin" diff_output
'

test_expect_success 'diff --name-only with binary' '
	(cd repo && $BIT diff --name-only HEAD~3..HEAD~1) >names_output &&
	grep -q "data.bin" names_output
'

test_expect_success 'diff --name-status with binary' '
	(cd repo && $BIT diff --name-status HEAD~2..HEAD~1) >namestatus_output &&
	grep -q "data.bin" namestatus_output
'

test_done
