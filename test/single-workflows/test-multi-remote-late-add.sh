#!/bin/bash
# Multi-remote late-add test: add a second remote after pushing to the first.
#
# Tests:
#   1. Create repo, add remote A (MinIO), push several commits
#   2. Add remote B (filesystem) later
#   3. Push to B — verify B gets FULL history, not just latest
#
# Prerequisites:
#   - rclone remote "minio" configured (local MinIO, S3-compatible)

set -e

WORKDIR="$HOME/mr-lateadd-test"
USBDIR="$HOME/mr-lateadd-usb"
BUCKET="minio:mr-lateadd"

echo "============================================"
echo "Multi-remote late-add test"
echo "============================================"
echo ""

echo "0. Clean up previous runs"
rm -rf "$WORKDIR" "$USBDIR"
rclone purge "$BUCKET" 2>/dev/null || true
echo "   Done."

echo ""
echo "1. Initialize repo, add cloud remote, make 3 commits"
mkdir -p "$WORKDIR" "$USBDIR" && cd "$WORKDIR"
bit init
bit remote add cloud "$BUCKET"

echo "file one" > one.txt
bit add one.txt
bit commit -m "Commit 1: add one.txt"
bit push -u cloud
echo "   Commit 1 pushed to cloud."

echo "file two" > two.txt
bit add two.txt
bit commit -m "Commit 2: add two.txt"
bit push cloud
echo "   Commit 2 pushed to cloud."

echo "file three" > three.txt
bit add three.txt
bit commit -m "Commit 3: add three.txt"
bit push cloud
echo "   Commit 3 pushed to cloud."

LOCAL_COMMITS=$(git -C "$WORKDIR/.bit/index" rev-list --count HEAD)
echo "   Local has $LOCAL_COMMITS commits"
echo "   PASS: 3 commits pushed to cloud"

echo ""
echo "2. Add USB remote AFTER the 3 commits"
bit remote add usb "$USBDIR"
echo "   PASS: USB remote added late"

echo ""
echo "3. Push to USB (should get full history)"
bit push usb
echo "   PASS: pushed to USB"

echo ""
echo "4. Verify USB has full history (all 3 commits)"
USB_COMMITS=$(git -C "$USBDIR/.bit/index" rev-list --count HEAD)
echo "   USB commits: $USB_COMMITS"
if [ "$USB_COMMITS" = "3" ]; then
    echo "   PASS: USB has all 3 commits (full history)"
else
    echo "   FAIL: USB has $USB_COMMITS commits, expected 3!"
    exit 1
fi

echo ""
echo "5. Verify USB has all 3 files"
for f in one.txt two.txt three.txt; do
    if [ -f "$USBDIR/$f" ]; then
        echo "   PASS: USB has $f"
    else
        echo "   FAIL: USB missing $f!"
        exit 1
    fi
done

echo ""
echo "6. Verify USB remote integrity"
cd "$WORKDIR"
bit --remote usb verify
echo "   PASS: USB verify clean"

echo ""
echo "7. Verify cloud also still clean"
bit --remote cloud verify
echo "   PASS: cloud verify clean"

echo ""
echo "8. Cleanup"
cd ~
rm -rf "$WORKDIR" "$USBDIR"
rclone purge "$BUCKET" 2>/dev/null || true
echo "   Done."

echo ""
echo "============================================"
echo "Summary:"
echo "  3 commits to cloud:               PASS"
echo "  Add USB remote late:               PASS"
echo "  Push to USB gets full history:     PASS"
echo "  USB has all 3 files:               PASS"
echo "  Both remotes verify clean:         PASS"
echo "============================================"
