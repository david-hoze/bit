#!/bin/bash
# Multi-remote delete-and-diverge test.
#
# Tests:
#   1. Create repo with files, push to MinIO + filesystem
#   2. Delete a file, push to only one remote
#   3. Verify: one remote has deletion, other still has the file
#   4. Clone from each remote into separate repos, verify state
#
# Prerequisites:
#   - rclone remote "minio" configured (local MinIO, S3-compatible)

set -e

WORKDIR="$HOME/mr-delete-test"
USBDIR="$HOME/mr-delete-usb"
CLONE_CLOUD="$HOME/mr-delete-clone-cloud"
CLONE_USB="$HOME/mr-delete-clone-usb"
BUCKET="minio:mr-delete"

echo "============================================"
echo "Multi-remote delete-and-diverge test"
echo "============================================"
echo ""

echo "0. Clean up previous runs"
rm -rf "$WORKDIR" "$USBDIR" "$CLONE_CLOUD" "$CLONE_USB"
rclone purge "$BUCKET" 2>/dev/null || true
echo "   Done."

echo ""
echo "1. Initialize repo with 3 files"
mkdir -p "$WORKDIR" "$USBDIR" && cd "$WORKDIR"
bit init
echo "keep me" > keeper.txt
echo "delete me" > doomed.txt
echo "also keep" > another.txt
bit add keeper.txt doomed.txt another.txt
bit commit -m "Initial: three files"
echo "   PASS: repo initialized with 3 files"

echo ""
echo "2. Push to both remotes"
bit remote add cloud "$BUCKET"
bit remote add usb "$USBDIR"
bit push -u cloud
bit push usb
echo "   PASS: pushed to both remotes"

echo ""
echo "3. Verify both remotes have all 3 files"
bit --remote cloud verify
bit --remote usb verify
if [ -f "$USBDIR/doomed.txt" ]; then
    echo "   PASS: USB has doomed.txt"
else
    echo "   FAIL: USB missing doomed.txt!"
    exit 1
fi
CLOUD_DOOMED=$(rclone cat "$BUCKET/doomed.txt" 2>/dev/null)
if echo "$CLOUD_DOOMED" | grep -q "delete me"; then
    echo "   PASS: cloud has doomed.txt"
else
    echo "   FAIL: cloud missing doomed.txt!"
    exit 1
fi

echo ""
echo "4. Delete file and push to ONLY cloud"
cd "$WORKDIR"
bit rm doomed.txt
bit commit -m "Delete doomed.txt"
bit push cloud
echo "   PASS: deleted doomed.txt, pushed to cloud only"

echo ""
echo "5. Verify divergence"
# Cloud should NOT have doomed.txt in working tree anymore
CLOUD_FILES=$(rclone ls "$BUCKET" 2>/dev/null | grep -c "doomed.txt" || true)
echo "   Cloud 'doomed.txt' file count: $CLOUD_FILES"

# USB should still have doomed.txt
if [ -f "$USBDIR/doomed.txt" ]; then
    echo "   PASS: USB still has doomed.txt"
else
    echo "   FAIL: USB lost doomed.txt unexpectedly!"
    exit 1
fi

# Check commit counts
CLOUD_COMMITS=$(git -C "$WORKDIR/.bit/index" rev-list --count cloud/main)
USB_COMMITS=$(git -C "$USBDIR/.bit/index" rev-list --count HEAD)
echo "   Cloud commits: $CLOUD_COMMITS  USB commits: $USB_COMMITS"
if [ "$CLOUD_COMMITS" = "2" ] && [ "$USB_COMMITS" = "1" ]; then
    echo "   PASS: commit counts confirm divergence"
else
    echo "   FAIL: unexpected commit counts"
    exit 1
fi

echo ""
echo "6. Pull from cloud into a fresh clone"
mkdir -p "$CLONE_CLOUD" && cd "$CLONE_CLOUD"
bit init
bit remote add cloud "$BUCKET"
bit pull cloud
echo "   Pull from cloud done."

# Clone from cloud should NOT have doomed.txt
if [ -f "$CLONE_CLOUD/doomed.txt" ]; then
    echo "   FAIL: cloud clone still has doomed.txt!"
    exit 1
else
    echo "   PASS: cloud clone does NOT have doomed.txt (deletion propagated)"
fi
if [ -f "$CLONE_CLOUD/keeper.txt" ]; then
    echo "   PASS: cloud clone has keeper.txt"
else
    echo "   FAIL: cloud clone missing keeper.txt!"
    exit 1
fi

echo ""
echo "7. Pull from USB into a fresh clone"
mkdir -p "$CLONE_USB" && cd "$CLONE_USB"
bit init
bit remote add usb "$USBDIR"
bit pull usb
echo "   Pull from USB done."

# Clone from USB SHOULD still have doomed.txt (USB not updated)
if [ -f "$CLONE_USB/doomed.txt" ]; then
    echo "   PASS: USB clone still has doomed.txt (USB was not updated)"
else
    echo "   FAIL: USB clone missing doomed.txt!"
    exit 1
fi
if [ -f "$CLONE_USB/keeper.txt" ]; then
    echo "   PASS: USB clone has keeper.txt"
else
    echo "   FAIL: USB clone missing keeper.txt!"
    exit 1
fi

echo ""
echo "8. Now push deletion to USB and verify"
cd "$WORKDIR"
bit push usb
echo "   Pushed to USB."

# USB should now also reflect the deletion
if [ -f "$USBDIR/doomed.txt" ]; then
    echo "   FAIL: USB still has doomed.txt after push!"
    exit 1
else
    echo "   PASS: USB no longer has doomed.txt (deletion propagated)"
fi

echo ""
echo "9. Cleanup"
cd ~
rm -rf "$WORKDIR" "$USBDIR" "$CLONE_CLOUD" "$CLONE_USB"
rclone purge "$BUCKET" 2>/dev/null || true
echo "   Done."

echo ""
echo "============================================"
echo "Summary:"
echo "  Initial push (3 files to both):     PASS"
echo "  Delete + push to cloud only:        PASS"
echo "  Divergence (USB still has file):    PASS"
echo "  Cloud clone: deletion propagated:   PASS"
echo "  USB clone: file still present:      PASS"
echo "  Push deletion to USB:               PASS"
echo "============================================"
