#!/bin/bash
# Multi-remote binary file test: push binary files to both remotes, verify divergence.
#
# Tests:
#   1. Create repo with a binary file, push to MinIO + filesystem
#   2. Verify both remotes have the binary
#   3. Edit the binary, push to only one remote
#   4. Verify divergence (one has new binary, other has old)
#
# Prerequisites:
#   - rclone remote "minio" configured (local MinIO, S3-compatible)

set -e

WORKDIR="$HOME/mr-binary-test"
USBDIR="$HOME/mr-binary-usb"
BUCKET="minio:mr-binary"

echo "============================================"
echo "Multi-remote binary file test"
echo "============================================"
echo ""

echo "0. Clean up previous runs"
rm -rf "$WORKDIR" "$USBDIR"
rclone purge "$BUCKET" 2>/dev/null || true
echo "   Done."

echo ""
echo "1. Initialize repo and create binary file"
mkdir -p "$WORKDIR" "$USBDIR" && cd "$WORKDIR"
bit init
# Create a real binary file (not just text with a .bin extension)
dd if=/dev/urandom of=image.bin bs=1024 count=64 2>/dev/null
echo "some text" > notes.txt
bit add image.bin notes.txt
bit commit -m "Initial: binary + text"
echo "   PASS: repo initialized with binary (64KB) and text file"

echo ""
echo "2. Add two remotes and push to both"
bit remote add cloud "$BUCKET"
bit remote add usb "$USBDIR"
bit push -u cloud
bit push usb
echo "   PASS: pushed to both remotes"

echo ""
echo "3. Verify both remotes"
bit --remote cloud verify
bit --remote usb verify
echo "   PASS: both remotes verify clean"

echo ""
echo "4. Check binary exists on both remotes"
CLOUD_SIZE=$(rclone size "$BUCKET/image.bin" --json 2>/dev/null | grep -o '"bytes":[0-9]*' | grep -o '[0-9]*')
USB_SIZE=$(stat -c%s "$USBDIR/image.bin" 2>/dev/null || wc -c < "$USBDIR/image.bin")
echo "   Cloud binary size: $CLOUD_SIZE"
echo "   USB binary size:   $USB_SIZE"
if [ -n "$CLOUD_SIZE" ] && [ "$CLOUD_SIZE" -gt 0 ]; then
    echo "   PASS: cloud has binary"
else
    echo "   FAIL: cloud missing binary!"
    exit 1
fi
if [ -n "$USB_SIZE" ] && [ "$USB_SIZE" -gt 0 ]; then
    echo "   PASS: USB has binary"
else
    echo "   FAIL: USB missing binary!"
    exit 1
fi

echo ""
echo "5. Record v1 hash from metadata"
V1_HASH=$(grep 'hash:' "$WORKDIR/.bit/index/image.bin" | head -1)
echo "   v1: $V1_HASH"

echo ""
echo "6. Edit binary (overwrite first 1KB with different random data)"
cd "$WORKDIR"
dd if=/dev/urandom of=image.bin bs=1024 count=1 conv=notrunc 2>/dev/null
bit add image.bin
bit commit -m "Edit binary: modified first 1KB"
echo "   PASS: binary edited and committed"

echo ""
echo "7. Record v2 hash"
V2_HASH=$(grep 'hash:' "$WORKDIR/.bit/index/image.bin" | head -1)
echo "   v2: $V2_HASH"
if [ "$V1_HASH" != "$V2_HASH" ]; then
    echo "   PASS: hashes differ (edit detected)"
else
    echo "   FAIL: hashes identical after edit!"
    exit 1
fi

echo ""
echo "8. Push edited binary to ONLY cloud"
bit push cloud
echo "   PASS: pushed to cloud only"

echo ""
echo "9. Verify divergence"
# Cloud should have v2 hash in its metadata
CLOUD_COMMITS=$(git -C "$WORKDIR/.bit/index" rev-list --count cloud/main)
USB_COMMITS=$(git -C "$USBDIR/.bit/index" rev-list --count HEAD)
echo "   Cloud commits: $CLOUD_COMMITS"
echo "   USB commits:   $USB_COMMITS"
if [ "$CLOUD_COMMITS" = "2" ] && [ "$USB_COMMITS" = "1" ]; then
    echo "   PASS: commit counts confirm divergence"
else
    echo "   FAIL: unexpected commit counts"
    exit 1
fi

# USB should still have old binary (same size but different content)
USB_HASH_CHECK=$(grep 'hash:' "$USBDIR/.bit/index/image.bin" | head -1)
echo "   USB metadata hash: $USB_HASH_CHECK"
if [ "$USB_HASH_CHECK" = "$V1_HASH" ]; then
    echo "   PASS: USB still has v1 metadata"
else
    echo "   FAIL: USB metadata changed unexpectedly!"
    exit 1
fi

echo ""
echo "10. Sync USB and verify both match"
cd "$WORKDIR"
bit push usb
bit --remote cloud verify
bit --remote usb verify
USB_HASH_FINAL=$(grep 'hash:' "$USBDIR/.bit/index/image.bin" | head -1)
if [ "$USB_HASH_FINAL" = "$V2_HASH" ]; then
    echo "   PASS: USB now has v2 metadata"
else
    echo "   FAIL: USB metadata not updated!"
    exit 1
fi

echo ""
echo "11. Cleanup"
cd ~
rm -rf "$WORKDIR" "$USBDIR"
rclone purge "$BUCKET" 2>/dev/null || true
echo "   Done."

echo ""
echo "============================================"
echo "Summary:"
echo "  Binary push to both remotes:    PASS"
echo "  Binary exists on both:          PASS"
echo "  Binary edit detected:           PASS"
echo "  Selective push (cloud only):    PASS"
echo "  Divergence confirmed:           PASS"
echo "  Sync and verify:                PASS"
echo "============================================"
