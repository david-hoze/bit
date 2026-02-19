#!/bin/bash
#
# t0019-binary-grep-advanced.sh — Advanced grep operations with binary files
#
# Adapted from Git's t7815-grep-binary.sh, t7816-grep-binary-pattern.sh.
# Tests -I flag, diff attribute interaction, and pattern matching edge cases.

test_description='bit grep advanced operations with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with binary and text files' '
	create_bit_repo repo &&
	printf "binary\0content\0here" >repo/data.bin &&
	echo "searchable text content" >repo/readme.txt &&
	echo "another line with pattern" >repo/notes.txt &&
	printf "text with no nul bytes" >repo/plain.dat &&
	(cd repo && $BIT add . && $BIT commit -m "Initial commit")
'

# ======================================================================
# GREP -I (SKIP BINARY FILES)
# ======================================================================

test_expect_success 'grep -I skips binary metadata files' '
	(cd repo && $BIT grep -I "content") >grep_I_output 2>&1 ;
	grep -q "readme.txt" grep_I_output &&
	! grep -q "data.bin" grep_I_output
'

test_expect_success 'grep without -I shows binary metadata matches' '
	(cd repo && $BIT grep "hash:") >grep_normal 2>&1 &&
	grep -q "data.bin" grep_normal
'

# ======================================================================
# GREP WITH INVERTED MATCH
# ======================================================================

test_expect_success 'grep -v inverts match' '
	(cd repo && $BIT grep -v "searchable" -- "*.txt") >grep_v_output 2>&1 &&
	grep -q "notes.txt" grep_v_output
'

# ======================================================================
# GREP -w (WORD MATCH)
# ======================================================================

test_expect_success 'grep -w matches whole words' '
	(cd repo && $BIT grep -w "text") >grep_w_output 2>&1 &&
	grep -q "readme.txt" grep_w_output
'

# ======================================================================
# GREP -i (CASE INSENSITIVE)
# ======================================================================

test_expect_success 'grep -i finds case-insensitive matches' '
	(cd repo && $BIT grep -i "SEARCHABLE") >grep_i_output 2>&1 &&
	grep -q "readme.txt" grep_i_output
'

# ======================================================================
# GREP WITH MULTIPLE PATTERNS
# ======================================================================

test_expect_success 'grep -e pattern1 -e pattern2 (OR)' '
	(cd repo && $BIT grep -e "searchable" -e "pattern") >grep_or 2>&1 &&
	grep -q "readme.txt" grep_or &&
	grep -q "notes.txt" grep_or
'

# ======================================================================
# GREP --AND WITH BINARY METADATA
# ======================================================================

test_expect_success 'grep for hash and size in same file' '
	(cd repo && $BIT grep -l "hash:") >hash_files 2>&1 &&
	(cd repo && $BIT grep -l "size:") >size_files 2>&1 &&
	grep -q "data.bin" hash_files &&
	grep -q "data.bin" size_files
'

# ======================================================================
# GREP IN SPECIFIC COMMIT RANGE
# ======================================================================

test_expect_success 'modify binary and commit' '
	printf "updated\0binary\0v2" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Update binary")
'

test_expect_success 'grep in HEAD~1 finds old metadata' '
	(cd repo && $BIT grep "hash:" HEAD~1 -- data.bin) >grep_old 2>&1 &&
	grep -q "data.bin" grep_old
'

test_expect_success 'grep in HEAD finds new metadata' '
	(cd repo && $BIT grep "hash:" HEAD -- data.bin) >grep_new 2>&1 &&
	grep -q "data.bin" grep_new
'

test_expect_success 'old and new metadata differ' '
	! test_cmp grep_old grep_new
'

# ======================================================================
# GREP --CACHED VS WORKING TREE
# ======================================================================

test_expect_success 'modify binary but do not stage' '
	printf "unstaged\0change" >repo/data.bin
'

test_expect_success 'grep --cached searches index, not working tree' '
	(cd repo && $BIT grep --cached "hash:" -- data.bin) >grep_cached 2>&1 &&
	grep -q "data.bin" grep_cached
'

test_expect_success 'cleanup: restore and commit' '
	(cd repo && $BIT add data.bin && $BIT commit -m "Stage the change")
'

# ======================================================================
# GREP WITH --NAME-ONLY
# ======================================================================

test_expect_success 'grep --name-only lists just filenames' '
	(cd repo && $BIT grep --name-only "hash:") >grep_names 2>&1 &&
	grep -q "data.bin" grep_names &&
	! grep -q ":" grep_names
'

test_done
