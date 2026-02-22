#!/bin/bash
# Multi-remote three-way test: MinIO + two local folders, each diverged differently.
#
# Tests:
#   1. Create repo with files, push to 3 remotes (MinIO, usb1, usb2)
#   2. Make edit A, push to only MinIO
#   3. Make edit B, push to only usb1
#   4. Verify all 3 remotes have different states
#   5. Sync all remotes to latest
#
# Prerequisites:
#   - rclone remote "minio" configured (local MinIO, S3-compatible)

set -e

WORKDIR="$HOME/mr-three-test"
USB1DIR="$HOME/mr-three-usb1"
USB2DIR="$HOME/mr-three-usb2"
BUCKET="minio:mr-three"

echo "============================================"
echo "Multi-remote three-way divergence test"
echo "============================================"
echo ""

echo "0. Clean up previous runs"
rm -rf "$WORKDIR" "$USB1DIR" "$USB2DIR"
rclone purge "$BUCKET" 2>/dev/null || true
echo "   Done."

echo ""
echo "1. Initialize repo with files"
mkdir -p "$WORKDIR" "$USB1DIR" "$USB2DIR" && cd "$WORKDIR"
bit init
echo "version 1" > shared.txt
echo "data A" > alpha.txt
echo "data B" > beta.txt
bit add shared.txt alpha.txt beta.txt
bit commit -m "Initial: three files"
echo "   PASS: repo initialized"

echo ""
echo "2. Add three remotes and push to all"
bit remote add cloud "$BUCKET"
bit remote add usb1 "$USB1DIR"
bit remote add usb2 "$USB2DIR"
bit push -u cloud
bit push usb1
bit push usb2
echo "   PASS: pushed to all 3 remotes"

echo ""
echo "3. Verify all 3 remotes"
bit --remote cloud verify
bit --remote usb1 verify
bit --remote usb2 verify
echo "   PASS: all 3 remotes verify clean"

echo ""
echo "4. Edit alpha.txt, push to ONLY cloud"
cd "$WORKDIR"
echo "alpha UPDATED" > alpha.txt
bit add alpha.txt
bit commit -m "Update alpha"
bit push cloud
echo "   PASS: alpha edit pushed to cloud only"

echo ""
echo "5. Edit beta.txt, push to ONLY usb1"
cd "$WORKDIR"
echo "beta UPDATED" > beta.txt
bit add beta.txt
bit commit -m "Update beta"
bit push usb1
echo "   PASS: beta edit pushed to usb1 only"

echo ""
echo "6. Verify all 3 remotes have DIFFERENT states"
# Cloud: has alpha update (commit 2) but NOT beta update (commit 3)
CLOUD_COMMITS=$(git -C "$WORKDIR/.bit/index" rev-list --count cloud/main)
USB1_COMMITS=$(git -C "$USB1DIR/.bit/index" rev-list --count HEAD)
USB2_COMMITS=$(git -C "$USB2DIR/.bit/index" rev-list --count HEAD)
echo "   Cloud commits: $CLOUD_COMMITS (should be 2)"
echo "   USB1 commits:  $USB1_COMMITS (should be 3)"
echo "   USB2 commits:  $USB2_COMMITS (should be 1)"

if [ "$CLOUD_COMMITS" = "2" ]; then
    echo "   PASS: cloud has 2 commits (initial + alpha edit)"
else
    echo "   FAIL: cloud has $CLOUD_COMMITS commits, expected 2"
    exit 1
fi

if [ "$USB1_COMMITS" = "3" ]; then
    echo "   PASS: usb1 has 3 commits (initial + alpha + beta edits)"
else
    echo "   FAIL: usb1 has $USB1_COMMITS commits, expected 3"
    exit 1
fi

if [ "$USB2_COMMITS" = "1" ]; then
    echo "   PASS: usb2 has 1 commit (initial only)"
else
    echo "   FAIL: usb2 has $USB2_COMMITS commits, expected 1"
    exit 1
fi

echo ""
echo "7. Verify file content on each remote"
# Cloud: alpha updated, beta original
CLOUD_ALPHA=$(rclone cat "$BUCKET/alpha.txt" 2>/dev/null)
CLOUD_BETA=$(rclone cat "$BUCKET/beta.txt" 2>/dev/null)
if echo "$CLOUD_ALPHA" | grep -q "UPDATED"; then
    echo "   PASS: cloud alpha.txt is updated"
else
    echo "   FAIL: cloud alpha.txt not updated!"
    exit 1
fi
if echo "$CLOUD_BETA" | grep -q "data B"; then
    echo "   PASS: cloud beta.txt is original"
else
    echo "   FAIL: cloud beta.txt changed unexpectedly!"
    exit 1
fi

# USB1: both alpha and beta updated
USB1_ALPHA=$(cat "$USB1DIR/alpha.txt")
USB1_BETA=$(cat "$USB1DIR/beta.txt")
if echo "$USB1_ALPHA" | grep -q "UPDATED"; then
    echo "   PASS: usb1 alpha.txt is updated"
else
    echo "   FAIL: usb1 alpha.txt not updated!"
    exit 1
fi
if echo "$USB1_BETA" | grep -q "UPDATED"; then
    echo "   PASS: usb1 beta.txt is updated"
else
    echo "   FAIL: usb1 beta.txt not updated!"
    exit 1
fi

# USB2: both alpha and beta original
USB2_ALPHA=$(cat "$USB2DIR/alpha.txt")
USB2_BETA=$(cat "$USB2DIR/beta.txt")
if echo "$USB2_ALPHA" | grep -q "data A"; then
    echo "   PASS: usb2 alpha.txt is original"
else
    echo "   FAIL: usb2 alpha.txt changed unexpectedly!"
    exit 1
fi
if echo "$USB2_BETA" | grep -q "data B"; then
    echo "   PASS: usb2 beta.txt is original"
else
    echo "   FAIL: usb2 beta.txt changed unexpectedly!"
    exit 1
fi

echo ""
echo "8. Sync all remotes to latest (3 commits)"
cd "$WORKDIR"
bit push cloud
bit push usb2
echo "   PASS: all remotes synced"

echo ""
echo "9. Verify all 3 remotes match"
CLOUD_FINAL=$(git -C "$WORKDIR/.bit/index" rev-list --count cloud/main)
USB1_FINAL=$(git -C "$USB1DIR/.bit/index" rev-list --count HEAD)
USB2_FINAL=$(git -C "$USB2DIR/.bit/index" rev-list --count HEAD)
echo "   Cloud: $CLOUD_FINAL  USB1: $USB1_FINAL  USB2: $USB2_FINAL"
if [ "$CLOUD_FINAL" = "3" ] && [ "$USB1_FINAL" = "3" ] && [ "$USB2_FINAL" = "3" ]; then
    echo "   PASS: all 3 remotes have 3 commits"
else
    echo "   FAIL: commit counts don't match"
    exit 1
fi

bit --remote cloud verify
bit --remote usb1 verify
bit --remote usb2 verify
echo "   PASS: all 3 remotes verify clean"

echo ""
echo "10. Cleanup"
cd ~
rm -rf "$WORKDIR" "$USB1DIR" "$USB2DIR"
rclone purge "$BUCKET" 2>/dev/null || true
echo "   Done."

echo ""
echo "============================================"
echo "Summary:"
echo "  Push to 3 remotes:                 PASS"
echo "  Selective push (cloud only):       PASS"
echo "  Selective push (usb1 only):        PASS"
echo "  Three-way divergence confirmed:    PASS"
echo "  File content verified per remote:  PASS"
echo "  Sync all to latest:                PASS"
echo "============================================"
