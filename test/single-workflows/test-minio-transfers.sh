#!/bin/bash
# A/B test: --transfers 4 (old default) vs --transfers 32 (new) for CAS chunks.
#
# Sets up a repo with the 227MB Blender file, then times the raw rclone
# upload of all ~1,370 CAS chunks at both parallelism levels.
# This isolates the transfer speed from dedup query / metadata overhead.
#
# Prerequisites:
#   - An rclone remote configured (default: minio; pass remote name as $1)
#   - ~/blender-4.3-splash.blend exists (227MB)
#
# Usage:
#   bash test-minio-transfers.sh              # local MinIO (low latency)
#   bash test-minio-transfers.sh gdrive-test  # Google Drive (high latency)

set -e

REMOTE="${1:-minio}"
BLEND="$HOME/blender-4.3-splash.blend"

if [ ! -f "$BLEND" ]; then
    echo "ERROR: $BLEND not found"
    exit 1
fi

echo "============================================"
echo "CAS parallel transfers A/B test"
echo "Remote: $REMOTE"
echo "============================================"
echo ""

# ---- SETUP: create repo, populate CAS locally ----
echo "0. Setup: create repo and populate CAS with chunks"
cd ~
rm -rf transfers-test
rclone purge "$REMOTE:xfer-a" 2>/dev/null || true
rclone purge "$REMOTE:xfer-b" 2>/dev/null || true

mkdir transfers-test && cd ~/transfers-test
bit init
bit config core.mode solid
cp "$BLEND" .
bit add .
bit commit -m "Initial"

BITDIR="$PWD/.bit"
CASDIR="$BITDIR/cas"
NCHUNKS=$(find "$CASDIR" -type f | wc -l)
echo "  CAS contains $NCHUNKS files (chunks + manifests)"
echo ""

# Build the files-from list (relative to .bit/)
TMPFILE=$(mktemp)
find "$CASDIR" -type f | while read f; do
    echo "${f#$BITDIR/}" | sed 's|\\|/|g'
done > "$TMPFILE"
NFILES=$(wc -l < "$TMPFILE")
echo "  files-from list: $NFILES entries"
echo ""

# ---- TEST A: --transfers 4 (old default) ----
echo "============================================"
echo "TEST A: rclone copy --transfers 4 (old default)"
echo "============================================"
rclone mkdir "$REMOTE:xfer-a"
echo "--- TIMING ---"
time rclone copy "$BITDIR" "$REMOTE:xfer-a" \
    --files-from "$TMPFILE" \
    --transfers 4 \
    --retries 3 \
    --low-level-retries 10 \
    --no-traverse \
    -q
echo ""

# ---- TEST B: --transfers 32 (new) ----
echo "============================================"
echo "TEST B: rclone copy --transfers 32 (new)"
echo "============================================"
rclone mkdir "$REMOTE:xfer-b"
echo "--- TIMING ---"
time rclone copy "$BITDIR" "$REMOTE:xfer-b" \
    --files-from "$TMPFILE" \
    --transfers 32 \
    --retries 3 \
    --low-level-retries 10 \
    --no-traverse \
    -q
echo ""

# ---- VERIFY: same number of files on both remotes ----
echo "============================================"
echo "Verification"
echo "============================================"
COUNT_A=$(rclone lsf --recursive "$REMOTE:xfer-a/cas" | wc -l)
COUNT_B=$(rclone lsf --recursive "$REMOTE:xfer-b/cas" | wc -l)
echo "  Remote A (transfers=4):  $COUNT_A files"
echo "  Remote B (transfers=32): $COUNT_B files"
if [ "$COUNT_A" -eq "$COUNT_B" ]; then
    echo "  PASS: counts match"
else
    echo "  FAIL: counts differ!"
fi
echo ""

# ---- CLEANUP ----
echo "Cleanup..."
rm -f "$TMPFILE"
cd ~
rm -rf transfers-test
rclone purge "$REMOTE:xfer-a" 2>/dev/null || true
rclone purge "$REMOTE:xfer-b" 2>/dev/null || true

echo ""
echo "============================================"
echo "Summary: $NFILES CAS files uploaded to $REMOTE"
echo "  transfers=4  — latency-bound (~files * RTT / 4)"
echo "  transfers=32 — latency-bound (~files * RTT / 32)"
echo "============================================"
