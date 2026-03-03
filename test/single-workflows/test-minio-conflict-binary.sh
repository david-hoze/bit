#!/bin/bash
set -e

BLEND="$HOME/blender-4.3-splash.blend"

echo "0. Clean up"
cd ~
rm -rf conflict-a conflict-b
rclone purge minio:conflict 2>/dev/null || true
rclone mkdir minio:conflict

echo "1. Machine A: initialize and push"
mkdir conflict-a && cd ~/conflict-a
cp "$BLEND" .
bit init
bit config core.mode solid
bit add .
bit commit -m "Initial commit"
bit remote add origin minio:conflict --bare
bit push -u origin

echo "2. Machine B: clone from the same remote"
cd ~
mkdir conflict-b && cd ~/conflict-b
bit init
bit config core.mode solid
bit remote add origin minio:conflict --bare
bit pull origin

echo "--- Both machines now have identical files ---"

echo "3. Machine A: edit near the beginning"
cd ~/conflict-a
printf 'AAAAAAA' | dd of=blender-4.3-splash.blend bs=1 seek=1000000 conv=notrunc
bit add .
bit commit -m "Machine A: edited intro section"
bit push

echo "4. Machine B: edit near the end (without pulling A's changes)"
cd ~/conflict-b
printf 'BBBBBBB' | dd of=blender-4.3-splash.blend bs=1 seek=200000000 conv=notrunc
bit add .
bit commit -m "Machine B: edited outro section"

echo "5. Machine B: try to push — should be rejected"
if bit push origin; then
    echo ">>> Push unexpectedly succeeded"
else
    echo ">>> Push rejected as expected — need to pull first"
fi

echo "6. Machine B: pull — should detect conflict"
echo "l" | bit pull origin

echo "7. Machine B: check status after conflict resolution"
bit status

echo "8. Machine B: push the merge"
bit push origin

echo "9. Machine A: pull the merge"
cd ~/conflict-a
bit pull

echo "10. Verify both machines match"
echo "--- Machine A ---"
cd ~/conflict-a
cat .bit/index/blender-4.3-splash.blend
echo "--- Machine B ---"
cd ~/conflict-b
cat .bit/index/blender-4.3-splash.blend

echo "11. Remote is clean"
cd ~/conflict-a
bit --remote origin verify
