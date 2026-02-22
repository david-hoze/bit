#!/bin/bash
# Multi-remote many-files test: push 50+ files, edit a subset, verify partial divergence.
#
# Tests:
#   1. Create repo with 50 files, push to MinIO + filesystem
#   2. Edit 10 of the 50 files, push to only one remote
#   3. Verify: the other remote still has old versions of those 10 files
#   4. Verify: the other 40 files are identical on both remotes
#
# Prerequisites:
#   - rclone remote "minio" configured (local MinIO, S3-compatible)

set -e

WORKDIR="$HOME/mr-many-test"
USBDIR="$HOME/mr-many-usb"
BUCKET="minio:mr-many"

echo "============================================"
echo "Multi-remote many-files test"
echo "============================================"
echo ""

echo "0. Clean up previous runs"
rm -rf "$WORKDIR" "$USBDIR"
rclone purge "$BUCKET" 2>/dev/null || true
echo "   Done."

echo ""
echo "1. Initialize repo and create 50 files"
mkdir -p "$WORKDIR" "$USBDIR" && cd "$WORKDIR"
bit init

for i in $(seq -w 1 50); do
    echo "original content of file $i" > "file-$i.txt"
done
bit add .
bit commit -m "Initial: 50 files"
echo "   PASS: repo initialized with 50 files"

echo ""
echo "2. Push to both remotes"
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
echo "4. Edit 10 files (file-01 through file-10)"
cd "$WORKDIR"
for i in $(seq -w 1 10); do
    echo "UPDATED content of file $i" > "file-$i.txt"
done
bit add .
bit commit -m "Update 10 files"
echo "   PASS: 10 files edited and committed"

echo ""
echo "5. Push to ONLY cloud"
bit push cloud
echo "   PASS: pushed to cloud only"

echo ""
echo "6. Verify divergence: edited files differ, others match"
EDITED_DIVERGED=0
UNEDITED_MATCH=0

for i in $(seq -w 1 10); do
    USB_CONTENT=$(cat "$USBDIR/file-$i.txt")
    if echo "$USB_CONTENT" | grep -q "original content"; then
        EDITED_DIVERGED=$((EDITED_DIVERGED + 1))
    else
        echo "   FAIL: USB file-$i.txt was unexpectedly updated!"
        exit 1
    fi
done
echo "   $EDITED_DIVERGED/10 edited files still have old content on USB"

for i in $(seq -w 11 50); do
    USB_CONTENT=$(cat "$USBDIR/file-$i.txt")
    CLOUD_CONTENT=$(rclone cat "$BUCKET/file-$i.txt" 2>/dev/null)
    if echo "$USB_CONTENT" | grep -q "original content" && echo "$CLOUD_CONTENT" | grep -q "original content"; then
        UNEDITED_MATCH=$((UNEDITED_MATCH + 1))
    else
        echo "   FAIL: file-$i.txt content mismatch!"
        exit 1
    fi
done
echo "   $UNEDITED_MATCH/40 unedited files match on both remotes"

if [ "$EDITED_DIVERGED" = "10" ] && [ "$UNEDITED_MATCH" = "40" ]; then
    echo "   PASS: partial divergence confirmed"
else
    echo "   FAIL: unexpected divergence pattern"
    exit 1
fi

echo ""
echo "7. Verify cloud has updated content for edited files"
for i in $(seq -w 1 10); do
    CLOUD_CONTENT=$(rclone cat "$BUCKET/file-$i.txt" 2>/dev/null)
    if echo "$CLOUD_CONTENT" | grep -q "UPDATED content"; then
        : # ok
    else
        echo "   FAIL: cloud file-$i.txt not updated!"
        exit 1
    fi
done
echo "   PASS: cloud has updated content for all 10 edited files"

echo ""
echo "8. Sync USB and verify"
cd "$WORKDIR"
bit push usb
for i in $(seq -w 1 10); do
    USB_CONTENT=$(cat "$USBDIR/file-$i.txt")
    if echo "$USB_CONTENT" | grep -q "UPDATED content"; then
        : # ok
    else
        echo "   FAIL: USB file-$i.txt not updated after sync!"
        exit 1
    fi
done
echo "   PASS: USB now has updated content for all 10 files"
bit --remote usb verify
echo "   PASS: USB verify clean after sync"

echo ""
echo "9. Cleanup"
cd ~
rm -rf "$WORKDIR" "$USBDIR"
rclone purge "$BUCKET" 2>/dev/null || true
echo "   Done."

echo ""
echo "============================================"
echo "Summary:"
echo "  50 files to both remotes:          PASS"
echo "  Edit 10, push to cloud only:       PASS"
echo "  10 edited files diverged:          PASS"
echo "  40 unedited files match:           PASS"
echo "  Cloud has updated content:         PASS"
echo "  Sync USB, all verified:            PASS"
echo "============================================"
