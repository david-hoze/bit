#!/bin/bash
#
# t0013-binary-attributes.sh — Test .gitattributes interaction with binary files
#
# Adapted from Git's t0003-attributes.sh, t0020-crlf.sh, t4006-diff-mode.sh.
# Tests that bit's binary classification interacts correctly with
# .gitattributes and that files >1MB are always classified as binary.

test_description='bit binary classification edge cases and attributes'
. ./test-lib.sh

# ======================================================================
# SIZE-BASED CLASSIFICATION (>1MB = binary)
# ======================================================================

test_expect_success 'setup: create repo' '
	create_bit_repo repo
'

test_expect_success 'file over 1MB is classified as binary regardless of content' '
	python3 -c "print(\"a\" * 1048577)" >repo/large-text.dat 2>/dev/null ||
	dd if=/dev/zero bs=1 count=1048577 2>/dev/null | tr "\000" "a" >repo/large-text.dat &&
	(cd repo && $BIT add large-text.dat) &&
	verify_binary_metadata repo large-text.dat
'

test_expect_success 'file under 1MB with only text content is text' '
	python3 -c "print(\"a\" * 100000)" >repo/small-text.dat 2>/dev/null ||
	dd if=/dev/zero bs=1 count=100000 2>/dev/null | tr "\000" "a" >repo/small-text.dat &&
	(cd repo && $BIT add small-text.dat) &&
	verify_text_metadata repo small-text.dat
'

test_expect_success 'commit size-based files' '
	(cd repo && $BIT commit -m "Add large and small files")
'

# ======================================================================
# EXTENSION-BASED VS CONTENT-BASED CLASSIFICATION
# ======================================================================

test_expect_success 'text content with .bin extension is binary (extension wins)' '
	echo "plain text" >repo/misleading.bin &&
	(cd repo && $BIT add misleading.bin) &&
	verify_binary_metadata repo misleading.bin
'

test_expect_success 'text content with .png extension is binary (extension wins)' '
	echo "not a real png" >repo/fake.png &&
	(cd repo && $BIT add fake.png) &&
	verify_binary_metadata repo fake.png
'

test_expect_success 'NUL content with .txt extension is binary (content wins)' '
	printf "has\0nul\0bytes" >repo/sneaky.txt &&
	(cd repo && $BIT add sneaky.txt) &&
	verify_binary_metadata repo sneaky.txt
'

test_expect_success 'text content with .dat extension is text (no special extension)' '
	echo "just data" >repo/plain.dat &&
	(cd repo && $BIT add plain.dat) &&
	verify_text_metadata repo plain.dat
'

test_expect_success 'commit classification files' '
	(cd repo && $BIT commit -m "Add classification test files")
'

# ======================================================================
# ALL KNOWN BINARY EXTENSIONS
# ======================================================================

test_expect_success '.mp4 extension is binary' '
	echo "x" >repo/video.mp4 && (cd repo && $BIT add video.mp4) &&
	verify_binary_metadata repo video.mp4
'

test_expect_success '.dll extension is binary' '
	echo "x" >repo/lib.dll && (cd repo && $BIT add lib.dll) &&
	verify_binary_metadata repo lib.dll
'

test_expect_success '.so extension is binary' '
	echo "x" >repo/lib.so && (cd repo && $BIT add lib.so) &&
	verify_binary_metadata repo lib.so
'

test_expect_success '.gif extension is binary' '
	echo "x" >repo/anim.gif && (cd repo && $BIT add anim.gif) &&
	verify_binary_metadata repo anim.gif
'

test_expect_success '.gz extension is binary' '
	echo "x" >repo/archive.gz && (cd repo && $BIT add archive.gz) &&
	verify_binary_metadata repo archive.gz
'

test_expect_success '.tar extension is binary' '
	echo "x" >repo/archive.tar && (cd repo && $BIT add archive.tar) &&
	verify_binary_metadata repo archive.tar
'

test_expect_success '.7z extension is binary' '
	echo "x" >repo/archive.7z && (cd repo && $BIT add archive.7z) &&
	verify_binary_metadata repo archive.7z
'

test_expect_success '.iso extension is binary' '
	echo "x" >repo/disk.iso && (cd repo && $BIT add disk.iso) &&
	verify_binary_metadata repo disk.iso
'

test_expect_success 'commit all extension tests' '
	(cd repo && $BIT commit -m "All binary extensions")
'

# ======================================================================
# NON-UTF8 CONTENT DETECTION
# ======================================================================

test_expect_success 'file with invalid UTF-8 is classified as binary' '
	printf "\xff\xfe\x80\x81\x82" >repo/invalid-utf8.dat &&
	(cd repo && $BIT add invalid-utf8.dat) &&
	verify_binary_metadata repo invalid-utf8.dat
'

test_expect_success 'file with valid UTF-8 multibyte is text' '
	printf "caf\xc3\xa9\n\xe4\xb8\xad\xe6\x96\x87\n" >repo/unicode.txt &&
	(cd repo && $BIT add unicode.txt) &&
	verify_text_metadata repo unicode.txt
'

test_expect_success 'commit encoding tests' '
	(cd repo && $BIT commit -m "Encoding classification tests")
'

# ======================================================================
# MIXED BINARY AND TEXT IN SAME COMMIT
# ======================================================================

test_expect_success 'batch add with mixed types preserves classification' '
	echo "text1" >repo/batch-text.txt &&
	printf "binary\0batch" >repo/batch-binary.dat &&
	echo "text2" >repo/batch-text2.md &&
	printf "\xff" >repo/batch-binary2.dat &&
	(cd repo && $BIT add .) &&
	verify_text_metadata repo batch-text.txt &&
	verify_binary_metadata repo batch-binary.dat &&
	verify_text_metadata repo batch-text2.md &&
	verify_binary_metadata repo batch-binary2.dat
'

test_expect_success 'commit batch' '
	(cd repo && $BIT commit -m "Batch mixed types")
'

test_done
