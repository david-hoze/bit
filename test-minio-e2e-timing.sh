#!/bin/bash
# End-to-end timing test: CDC push/pull with dedup + parallel transfers.
#
# Times each phase of the push/pull lifecycle with a 227MB binary file:
#   1. First push (all chunks)
#   2. Incremental push after 6-byte edit (dedup skips unchanged chunks)
#   3. Incremental push after 4KB edit
#   4. Fresh clone (all chunks downloaded)
#   5. Incremental pull (dedup skips cached chunks)
#   6. Verify integrity
#
# Prerequisites:
#   - An rclone remote configured (default: minio; pass remote name as $1)
#   - ~/blender-4.3-splash.blend exists (227MB)
#
# Usage:
#   bash test-minio-e2e-timing.sh              # local MinIO
#   bash test-minio-e2e-timing.sh gdrive-test  # Google Drive

set -e

REMOTE="${1:-minio}"
BUCKET="${2:-bit-e2e-timing}"
BLEND="$HOME/blender-4.3-splash.blend"

if [ ! -f "$BLEND" ]; then
    echo "ERROR: $BLEND not found"
    exit 1
fi

echo "============================================"
echo "End-to-end CDC timing test"
echo "Remote: $REMOTE:$BUCKET"
echo "============================================"
echo ""

echo "0. Clean up"
cd ~
rm -rf e2e-timing-a e2e-timing-b
rclone purge "$REMOTE:$BUCKET" 2>/dev/null || true
rclone mkdir "$REMOTE:$BUCKET"

echo ""
echo "1. Initialize and first push"
mkdir e2e-timing-a && cd ~/e2e-timing-a
cp "$BLEND" .
bit init
bit config core.mode solid
bit add .
bit commit -m "Initial: 227MB Blender splash file"
bit remote add origin "$REMOTE:$BUCKET" --bare

echo "--- TIMING: First push (all chunks) ---"
time bit push -u origin
echo ""

echo "2. Verify remote"
bit --remote origin verify
echo ""

echo "3. Small edit (6 bytes at 1MB offset)"
printf 'EDITED' | dd of=blender-4.3-splash.blend bs=1 seek=1000000 conv=notrunc
bit add .
bit commit -m "Small edit"

echo "--- TIMING: Incremental push (6B edit, dedup + parallel) ---"
time bit push
echo ""

echo "4. Larger edit (4KB at 50MB offset)"
dd if=/dev/urandom of=patch.bin bs=4096 count=1 2>/dev/null
dd if=patch.bin of=blender-4.3-splash.blend bs=1 seek=50000000 conv=notrunc 2>/dev/null
rm -f patch.bin
bit add .
bit commit -m "4KB edit at 50MB"

echo "--- TIMING: Incremental push (4KB edit, dedup + parallel) ---"
time bit push
echo ""

echo "5. Fresh clone"
cd ~
mkdir e2e-timing-b && cd ~/e2e-timing-b
bit init
bit config core.mode solid
bit remote add origin "$REMOTE:$BUCKET" --bare

echo "--- TIMING: Fresh pull (all chunks, parallel downloads) ---"
time bit pull origin
echo ""

echo "6. Incremental pull"
cd ~/e2e-timing-a
printf 'PULL!!' | dd of=blender-4.3-splash.blend bs=1 seek=2000000 conv=notrunc
bit add .
bit commit -m "Another small edit"
bit push

cd ~/e2e-timing-b
echo "--- TIMING: Incremental pull (dedup + parallel) ---"
time bit pull origin
echo ""

echo "7. Verify integrity"
bit verify
echo ""

echo "8. Cleanup"
cd ~
rm -rf e2e-timing-a e2e-timing-b
rclone purge "$REMOTE:$BUCKET" 2>/dev/null || true

echo "============================================"
echo "Done."
echo "============================================"
