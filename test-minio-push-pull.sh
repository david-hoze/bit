#!/bin/bash
set -e  # stop on first error

BLEND="$HOME/blender-4.3-splash.blend"

echo "0. Clean up"
cd ~
rm -rf blender-test blender-test-2
rclone purge minio:blender 2>/dev/null || true
rclone mkdir minio:blender

echo "1. Initialize and first push"
mkdir blender-test && cd ~/blender-test
cp "$BLEND" .
bit init
bit config core.mode solid
bit add .
bit commit -m "Initial: 227MB Blender splash file"
bit remote add minio minio:blender --bare
bit push -u minio

echo "2. Verify remote"
bit --remote minio verify

echo "3. Small edit (6 bytes at 1MB offset)"
printf 'EDITED' | dd of=blender-4.3-splash.blend bs=1 seek=1000000 conv=notrunc
bit add .
bit commit -m "Small edit: moved an object"
bit push

echo "4. Rename test"
mv blender-4.3-splash.blend scene-final.blend
bit add .
bit commit -m "Rename for delivery"
bit push

echo "5. Fresh clone test"
cd ~
mkdir blender-test-2 && cd ~/blender-test-2
bit init
bit config core.mode solid
bit remote add minio minio:blender --bare
bit pull minio

echo "6. Verify integrity"
bit verify
ls -la
