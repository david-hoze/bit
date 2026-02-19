#!/bin/bash
#
# t0001-binary-add-commit.sh — Test bit add and commit with binary files
#
# Adapted from Git's t3700-add.sh, t7501-commit-basic-functionality.sh,
# and t4012-diff-binary.sh. Tests that bit correctly classifies binary files,
# creates proper metadata (hash: + size:), and commits them.

test_description='bit add and commit with binary files'
. ./test-lib.sh

# ======================================================================
# BASIC BINARY ADD
# ======================================================================

test_expect_success 'setup: create bit repo' '
	create_bit_repo repo
'

test_expect_success 'add binary file with NUL bytes' '
	printf "hello\0world" >repo/binary.dat &&
	(cd repo && $BIT add binary.dat) &&
	verify_binary_metadata repo binary.dat
'

test_expect_success 'binary metadata has correct size' '
	local_size=$(wc -c <repo/binary.dat | tr -d " ") &&
	meta_size=$(get_metadata_size repo binary.dat) &&
	test "$local_size" = "$meta_size"
'

test_expect_success 'commit binary file' '
	(cd repo && $BIT commit -m "Add binary.dat") &&
	(cd repo && $BIT log --oneline | grep -q "Add binary.dat")
'

test_expect_success 'status is clean after commit' '
	(cd repo && $BIT status) | grep -q "nothing to commit\|working tree clean"
'

# ======================================================================
# BINARY DETECTION: NUL BYTE IN CONTENT
# ======================================================================

test_expect_success 'file with single NUL byte is classified as binary' '
	printf "\0" >repo/single-nul.bin &&
	(cd repo && $BIT add single-nul.bin) &&
	verify_binary_metadata repo single-nul.bin
'

test_expect_success 'file with NUL in middle is classified as binary' '
	printf "text\0more text\nand lines\n" >repo/mid-nul.dat &&
	(cd repo && $BIT add mid-nul.dat) &&
	verify_binary_metadata repo mid-nul.dat
'

test_expect_success 'file with many NUL bytes is classified as binary' '
	echo "AAQBBQCCQDDQEEQFFQ" | q_to_nul >repo/many-nuls.dat &&
	(cd repo && $BIT add many-nuls.dat) &&
	verify_binary_metadata repo many-nuls.dat
'

# ======================================================================
# TEXT FILES — VERIFY THEY ARE NOT TREATED AS BINARY
# ======================================================================

test_expect_success 'plain text file is NOT classified as binary' '
	echo "just plain text" >repo/readme.txt &&
	(cd repo && $BIT add readme.txt) &&
	verify_text_metadata repo readme.txt
'

test_expect_success 'text file with special chars but no NUL is text' '
	printf "line1\nline2\ttab\n" >repo/special.txt &&
	(cd repo && $BIT add special.txt) &&
	verify_text_metadata repo special.txt
'

# ======================================================================
# BINARY FILE BY EXTENSION
# ======================================================================

test_expect_success 'file with .png extension is classified as binary' '
	echo "not really a png" >repo/image.png &&
	(cd repo && $BIT add image.png) &&
	verify_binary_metadata repo image.png
'

test_expect_success 'file with .jpg extension is classified as binary' '
	echo "not really a jpg" >repo/photo.jpg &&
	(cd repo && $BIT add photo.jpg) &&
	verify_binary_metadata repo photo.jpg
'

test_expect_success 'file with .pdf extension is classified as binary' '
	echo "not really a pdf" >repo/doc.pdf &&
	(cd repo && $BIT add doc.pdf) &&
	verify_binary_metadata repo doc.pdf
'

test_expect_success 'file with .zip extension is classified as binary' '
	echo "not really a zip" >repo/archive.zip &&
	(cd repo && $BIT add archive.zip) &&
	verify_binary_metadata repo archive.zip
'

test_expect_success 'file with .exe extension is classified as binary' '
	echo "not really an exe" >repo/program.exe &&
	(cd repo && $BIT add program.exe) &&
	verify_binary_metadata repo program.exe
'

# ======================================================================
# MODIFY BINARY FILE AND RE-ADD
# ======================================================================

test_expect_success 'commit all files so far' '
	(cd repo && $BIT commit -m "Add various binary and text files")
'

test_expect_success 'modify binary file — hash changes on re-add' '
	old_hash=$(get_metadata_hash repo binary.dat) &&
	printf "different\0content" >repo/binary.dat &&
	(cd repo && $BIT add binary.dat) &&
	new_hash=$(get_metadata_hash repo binary.dat) &&
	test "$old_hash" != "$new_hash"
'

test_expect_success 'modify binary file — size changes on re-add' '
	old_size=$(get_metadata_size repo binary.dat) &&
	printf "a\0b\0c\0d\0e\0f\0g\0h\0i\0j\0k\0l\0m" >repo/binary.dat &&
	(cd repo && $BIT add binary.dat) &&
	new_size=$(get_metadata_size repo binary.dat) &&
	test "$old_size" != "$new_size"
'

test_expect_success 'commit modified binary' '
	(cd repo && $BIT commit -m "Modify binary.dat")
'

# ======================================================================
# ADD MULTIPLE BINARY FILES AT ONCE
# ======================================================================

test_expect_success 'add multiple binary files with bit add .' '
	printf "\0alpha" >repo/alpha.bin &&
	printf "\0beta" >repo/beta.bin &&
	printf "\0gamma" >repo/gamma.bin &&
	(cd repo && $BIT add .) &&
	verify_binary_metadata repo alpha.bin &&
	verify_binary_metadata repo beta.bin &&
	verify_binary_metadata repo gamma.bin
'

test_expect_success 'commit multiple binary files' '
	(cd repo && $BIT commit -m "Add alpha, beta, gamma binaries")
'

# ======================================================================
# BINARY FILES IN SUBDIRECTORIES
# ======================================================================

test_expect_success 'add binary file in subdirectory' '
	mkdir -p repo/data/raw &&
	printf "sub\0dir\0binary" >repo/data/raw/dataset.bin &&
	(cd repo && $BIT add data/raw/dataset.bin) &&
	verify_binary_metadata repo data/raw/dataset.bin
'

test_expect_success 'commit subdirectory binary' '
	(cd repo && $BIT commit -m "Add data/raw/dataset.bin")
'

# ======================================================================
# LARGE BINARY FILE
# ======================================================================

test_expect_success 'add large binary file (64KB)' '
	generate_binary "seed42" 65536 >repo/large.bin &&
	(cd repo && $BIT add large.bin) &&
	verify_binary_metadata repo large.bin &&
	meta_size=$(get_metadata_size repo large.bin) &&
	test "$meta_size" = "65536"
'

test_expect_success 'commit large binary' '
	(cd repo && $BIT commit -m "Add large.bin")
'

# ======================================================================
# EMPTY BINARY (EDGE CASE)
# ======================================================================

test_expect_success 'add empty file (zero bytes)' '
	: >repo/empty.dat &&
	(cd repo && $BIT add empty.dat)
'

test_expect_success 'commit empty file' '
	(cd repo && $BIT commit -m "Add empty.dat")
'

# ======================================================================
# VERIFY FULL HISTORY
# ======================================================================

test_expect_success 'log shows all commits' '
	(cd repo && $BIT log --oneline) >log_output &&
	grep -q "Add binary.dat" log_output &&
	grep -q "Modify binary.dat" log_output &&
	grep -q "Add alpha, beta, gamma" log_output &&
	grep -q "Add data/raw/dataset.bin" log_output &&
	grep -q "Add large.bin" log_output
'

test_done
