#!/bin/bash
# Multi-remote sync test: push to MinIO (cloud) and filesystem (USB) independently.
#
# Tests bit's multi-remote capability:
#   1. Create a repo, add two remotes (MinIO + local folder)
#   2. Push to both, verify both have correct files
#   3. Edit a file, push to only ONE remote
#   4. Verify the remotes have diverged
#   5. Push to the other remote, verify both are back in sync
#
# Prerequisites:
#   - rclone remote "minio" configured (local MinIO, S3-compatible)

set -e

WORKDIR="$HOME/multi-remote-test"
USBDIR="$HOME/multi-remote-usb"
BUCKET="minio:multi-remote-sync"

echo "============================================"
echo "Multi-remote sync test"
echo "============================================"
echo ""

echo "0. Clean up previous runs"
rm -rf "$WORKDIR" "$USBDIR"
rclone purge "$BUCKET" 2>/dev/null || true
echo "   Done."

echo ""
echo "1. Initialize repo and create files"
mkdir -p "$WORKDIR" && cd "$WORKDIR"
bit init
echo "Hello from multi-remote test" > readme.txt
echo "Binary payload data" > data.bin
bit add readme.txt data.bin
bit commit -m "Initial files"
echo "   PASS: repo initialized with 2 files"

echo ""
echo "2. Add two remotes: cloud (MinIO) and usb (filesystem)"
mkdir -p "$USBDIR"
bit remote add cloud "$BUCKET"
bit remote add usb "$USBDIR"
echo "   PASS: two remotes added"

echo ""
echo "3. Push to both remotes"
bit push -u cloud
echo "   Cloud push done."
bit push usb
echo "   USB push done."
echo "   PASS: pushed to both remotes"

echo ""
echo "4. Verify both remotes"
bit --remote cloud verify
bit --remote usb verify
echo "   PASS: both remotes verify clean"

echo ""
echo "5. Check file content on both remotes"
CLOUD_CONTENT=$(rclone cat "$BUCKET/readme.txt")
USB_CONTENT=$(cat "$USBDIR/readme.txt")
echo "   Cloud: $CLOUD_CONTENT"
echo "   USB:   $USB_CONTENT"
if echo "$CLOUD_CONTENT" | grep -q "Hello from multi-remote test"; then
    echo "   PASS: cloud has correct content"
else
    echo "   FAIL: cloud content mismatch!"
    exit 1
fi
if echo "$USB_CONTENT" | grep -q "Hello from multi-remote test"; then
    echo "   PASS: USB has correct content"
else
    echo "   FAIL: USB content mismatch!"
    exit 1
fi

echo ""
echo "6. Edit file and push to ONLY cloud"
cd "$WORKDIR"
echo "Updated content - version 2" > readme.txt
bit add readme.txt
bit commit -m "Update readme"
bit push cloud
echo "   PASS: pushed update to cloud only"

echo ""
echo "7. Verify remotes have DIVERGED"
CLOUD_CONTENT_V2=$(rclone cat "$BUCKET/readme.txt")
USB_CONTENT_V2=$(cat "$USBDIR/readme.txt")
echo "   Cloud: $CLOUD_CONTENT_V2"
echo "   USB:   $USB_CONTENT_V2"

if echo "$CLOUD_CONTENT_V2" | grep -q "Updated content - version 2"; then
    echo "   PASS: cloud has updated content"
else
    echo "   FAIL: cloud was not updated!"
    exit 1
fi

if echo "$USB_CONTENT_V2" | grep -q "Hello from multi-remote test"; then
    echo "   PASS: USB still has old content (not pushed)"
else
    echo "   FAIL: USB was unexpectedly updated!"
    exit 1
fi

CLOUD_COMMITS=$(git -C "$WORKDIR/.bit/index" rev-list --count cloud/main)
USB_COMMITS=$(git -C "$USBDIR/.bit/index" rev-list --count HEAD)
echo "   Cloud commits: $CLOUD_COMMITS"
echo "   USB commits:   $USB_COMMITS"
if [ "$CLOUD_COMMITS" = "2" ] && [ "$USB_COMMITS" = "1" ]; then
    echo "   PASS: commit counts confirm divergence (cloud=2, usb=1)"
else
    echo "   FAIL: unexpected commit counts (cloud=$CLOUD_COMMITS, usb=$USB_COMMITS)"
    exit 1
fi

echo ""
echo "8. Push to USB to bring it up to date"
cd "$WORKDIR"
bit push usb
echo "   PASS: pushed update to USB"

echo ""
echo "9. Verify both remotes are back in sync"
CLOUD_FINAL=$(rclone cat "$BUCKET/readme.txt")
USB_FINAL=$(cat "$USBDIR/readme.txt")
echo "   Cloud: $CLOUD_FINAL"
echo "   USB:   $USB_FINAL"

if echo "$CLOUD_FINAL" | grep -q "Updated content - version 2"; then
    echo "   PASS: cloud has v2"
else
    echo "   FAIL: cloud content wrong!"
    exit 1
fi

if echo "$USB_FINAL" | grep -q "Updated content - version 2"; then
    echo "   PASS: USB now has v2"
else
    echo "   FAIL: USB was not updated!"
    exit 1
fi

bit --remote cloud verify
bit --remote usb verify
echo "   PASS: both remotes verify clean after sync"

echo ""
echo "10. Cleanup"
cd ~
rm -rf "$WORKDIR" "$USBDIR"
rclone purge "$BUCKET" 2>/dev/null || true
echo "   Done."

echo ""
echo "============================================"
echo "Summary:"
echo "  Initial push to both remotes:  PASS"
echo "  Both remotes verify clean:     PASS"
echo "  Selective push (cloud only):   PASS"
echo "  Divergence detected:           PASS"
echo "  Sync USB after divergence:     PASS"
echo "  Both remotes verify after sync: PASS"
echo "============================================"
