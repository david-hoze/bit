#!/bin/bash
#
# t0007-binary-verify.sh — Test bit verify with binary files
#
# Bit-specific test: bit verify checks that working tree files match
# their committed metadata. This tests the binary-specific paths:
# hash mismatches, missing files, corrupted content.

test_description='bit verify with binary files'
. ./test-lib.sh

# ======================================================================
# SETUP
# ======================================================================

test_expect_success 'setup: create repo with committed binary files' '
	create_bit_repo repo &&
	printf "verify\0test\0binary" >repo/data.bin &&
	printf "second\0binary\0file" >repo/extra.bin &&
	echo "text file" >repo/readme.txt &&
	(cd repo && $BIT add . && $BIT commit -m "Initial commit")
'

# ======================================================================
# VERIFY CLEAN STATE
# ======================================================================

test_expect_success 'verify passes on clean repo' '
	(cd repo && $BIT verify) >verify_output 2>&1 ;
	grep -qi "clean\|no issues\|ok\|All files verified\|0 issues" verify_output
'

# ======================================================================
# VERIFY DETECTS CORRUPTED BINARY
# ======================================================================

test_expect_success 'corrupt binary file content' '
	printf "corrupted\0content" >repo/data.bin
'

test_expect_success 'verify detects binary corruption' '
	(cd repo && $BIT verify) >verify_output 2>&1 ;
	grep -qi "mismatch\|issue\|error\|corrupt\|data.bin" verify_output
'

test_expect_success 'restore binary to correct content' '
	printf "verify\0test\0binary" >repo/data.bin
'

# ======================================================================
# VERIFY DETECTS MISSING BINARY
# ======================================================================

test_expect_success 'delete binary file' '
	rm repo/extra.bin
'

test_expect_success 'verify detects missing binary' '
	(cd repo && $BIT verify) >verify_output 2>&1 ;
	grep -qi "missing\|issue\|error\|extra.bin" verify_output
'

test_expect_success 'restore missing binary' '
	printf "second\0binary\0file" >repo/extra.bin
'

# ======================================================================
# VERIFY DETECTS WRONG SIZE
# ======================================================================

test_expect_success 'change binary file size' '
	printf "short" >repo/data.bin
'

test_expect_success 'verify detects size change' '
	(cd repo && $BIT verify) >verify_output 2>&1 ;
	grep -qi "mismatch\|issue\|error\|data.bin" verify_output
'

test_expect_success 'restore correct content' '
	printf "verify\0test\0binary" >repo/data.bin
'

# ======================================================================
# VERIFY AFTER MODIFICATION CYCLE
# ======================================================================

test_expect_success 'modify, add, commit — verify passes again' '
	printf "updated\0content" >repo/data.bin &&
	(cd repo && $BIT add data.bin && $BIT commit -m "Update data.bin") &&
	(cd repo && $BIT verify) >verify_output 2>&1 ;
	grep -qi "clean\|no issues\|ok\|verified\|0 issues" verify_output
'

test_done
