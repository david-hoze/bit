#!/bin/bash
# Rollback workflow test: revert a bad edit and restore from remote CAS.
#
# Tests that bit revert correctly reassembles CDC-chunked files from CAS.
#
# Prerequisites:
#   - rclone remote "minio" configured (local MinIO)
#   - ~/blender-4.3-splash.blend exists (227MB)

set -e

BLEND="$HOME/blender-4.3-splash.blend"

if [ ! -f "$BLEND" ]; then
    echo "ERROR: $BLEND not found"
    exit 1
fi

echo "============================================"
echo "Rollback workflow test"
echo "============================================"
echo ""

echo "0. Clean up"
cd ~
rm -rf rollback-test rollback-clone
rclone purge minio:rollback 2>/dev/null || true
rclone mkdir minio:rollback

echo ""
echo "1. Initialize and push original (v1)"
mkdir rollback-test && cd ~/rollback-test
cp "$BLEND" .
bit init
bit config core.mode solid
bit add .
bit commit -m "v1: original splash file"
bit remote add minio minio:rollback --bare
bit push -u minio

echo ""
echo "2. Record v1 hash"
echo "--- v1 metadata ---"
V1_HASH=$(grep 'hash:' .bit/index/blender-4.3-splash.blend | head -1)
echo "$V1_HASH"

echo ""
echo "3. Make a bad edit (1KB garbage at 2MB offset)"
dd if=/dev/urandom of=blender-4.3-splash.blend bs=1024 count=1 seek=2048 conv=notrunc
bit add .
bit commit -m "v2: bad edit — broke the scene"
bit push

echo ""
echo "4. Record v2 hash"
echo "--- v2 metadata (bad) ---"
V2_HASH=$(grep 'hash:' .bit/index/blender-4.3-splash.blend | head -1)
echo "$V2_HASH"

echo ""
echo "5. Verify v1 != v2"
if [ "$V1_HASH" != "$V2_HASH" ]; then
    echo "PASS: hashes differ (edit was detected)"
else
    echo "FAIL: hashes identical!"
    exit 1
fi

echo ""
echo "6. Rollback via revert"
bit revert HEAD --no-edit

echo ""
echo "--- v3 metadata (should match v1) ---"
V3_HASH=$(grep 'hash:' .bit/index/blender-4.3-splash.blend | head -1)
echo "$V3_HASH"
if [ "$V1_HASH" = "$V3_HASH" ]; then
    echo "PASS: metadata reverted to v1"
else
    echo "FAIL: metadata not reverted!"
    exit 1
fi

echo ""
echo "7. Verify file content matches metadata (CDC reassembly from CAS)"
bit verify
echo "PASS: verify passed after revert"

echo ""
echo "8. Cross-check: fresh clone from remote"
cd ~
mkdir rollback-clone && cd ~/rollback-clone
bit init
bit config core.mode solid
bit remote add minio minio:rollback --bare
bit pull minio

echo ""
echo "9. Verify clone integrity"
bit verify
CLONE_HASH=$(grep 'hash:' .bit/index/blender-4.3-splash.blend | head -1)
echo "--- clone metadata ---"
echo "$CLONE_HASH"

echo ""
echo "10. Cleanup"
cd ~
rm -rf rollback-test rollback-clone
rclone purge minio:rollback 2>/dev/null || true

echo ""
echo "============================================"
echo "Summary:"
echo "  v1 hash: $V1_HASH"
echo "  v2 hash: $V2_HASH"
echo "  v3 (reverted) hash: $V3_HASH"
echo "  clone hash: $CLONE_HASH"
echo ""
echo "  Metadata revert: PASS"
echo "  File content revert (CDC reassembly): PASS"
echo "  Clone integrity: PASS"
echo "============================================"
