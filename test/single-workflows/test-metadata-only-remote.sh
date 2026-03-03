#!/bin/bash
set -e

BLEND="$HOME/blender-4.3-splash.blend"

echo "0. Clean up"
cd ~
rm -rf metadata-test metadata-test-2 metadata-git-remote
rclone purge minio:metadata 2>/dev/null || true
rclone mkdir minio:metadata

echo "1. Create a bare git repo (simulates GitHub)"
git init --bare --initial-branch=main ~/metadata-git-remote

echo "2. Machine A: initialize with both remotes"
mkdir metadata-test && cd ~/metadata-test
cp "$BLEND" .
bit init
bit config core.mode solid
bit add .
bit commit -m "Initial: 227MB Blender splash"

bit remote add github ~/metadata-git-remote --metadata-only
bit remote add storage minio:metadata --bare

echo "3. Push to both"
bit push -u storage
bit push github

echo "4. Verify: git remote has metadata, not content"
echo "--- git repo size (should be tiny, just metadata) ---"
du -sh ~/metadata-git-remote
echo "--- MinIO has the actual content ---"
bit --remote storage verify

echo "5. Check git log on the bare repo (should show commits)"
git -C ~/metadata-git-remote log --oneline

echo "6. Edit and push both"
printf 'UPDATED' | dd of=blender-4.3-splash.blend bs=1 seek=3000000 conv=notrunc
bit add .
bit commit -m "Updated scene lighting"
bit push storage
bit push github

echo "7. Git log shows both commits"
git -C ~/metadata-git-remote log --oneline

echo "8. Machine B: clone from both remotes"
cd ~
mkdir metadata-test-2 && cd ~/metadata-test-2
bit init
bit config core.mode solid
bit remote add github ~/metadata-git-remote --metadata-only
bit remote add storage minio:metadata --bare

echo "9. Pull metadata first, then content"
bit pull github
echo "--- Metadata is here, but files are missing ---"
bit status
ls -la

echo "10. Hydrate: download actual files from storage"
bit hydrate storage

echo "11. Verify everything"
bit verify
ls -la blender-4.3-splash.blend

echo "12. Check: file should be the updated version (with UPDATED edit)"
echo "--- hash should match Machine A's latest ---"
cat .bit/index/blender-4.3-splash.blend
