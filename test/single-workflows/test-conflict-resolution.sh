#!/bin/bash
# Conflict resolution test: two machines edit the same file, resolve, sync.
#
# Tests the full conflict lifecycle:
#   1. Machine A creates repo, pushes to filesystem remote
#   2. Machine B pulls from remote (simulates second machine)
#   3. Machine A edits shared.txt, commits, pushes
#   4. Machine B edits shared.txt differently, commits, tries to push (should fail)
#   5. Machine B pulls — conflict detected, picks remote version via stdin
#   6. Machine B re-applies its edit on top, commits, pushes
#   7. Machine A pulls — gets the combined version
#
# Prerequisites: none (uses filesystem remote only)

set -e

MACHINE_A="$HOME/conflict-test-a"
MACHINE_B="$HOME/conflict-test-b"
REMOTE_DIR="$HOME/conflict-test-remote"

# Track pass/fail for summary
STEPS_PASSED=0
STEPS_FAILED=0

pass() { echo "   PASS: $1"; STEPS_PASSED=$((STEPS_PASSED + 1)); }
fail() { echo "   FAIL: $1"; STEPS_FAILED=$((STEPS_FAILED + 1)); }

echo "============================================"
echo "Conflict resolution test"
echo "============================================"
echo ""

echo "0. Clean up previous runs"
rm -rf "$MACHINE_A" "$MACHINE_B" "$REMOTE_DIR"
echo "   Done."

# ---- Step 1: Machine A creates repo and pushes ----
echo ""
echo "1. Machine A: initialize repo with shared file, push to remote"
mkdir -p "$MACHINE_A" "$REMOTE_DIR" && cd "$MACHINE_A"
bit init
echo "line 1: original content" > shared.txt
echo "line 2: base version" >> shared.txt
echo "line 3: unchanged" >> shared.txt
echo "untouched file" > other.txt
bit add shared.txt other.txt
bit commit -m "Initial: shared file with base content"
bit remote add origin "$REMOTE_DIR"
bit push -u origin
pass "Machine A initialized and pushed to remote"

# ---- Step 2: Machine B pulls initial state ----
echo ""
echo "2. Machine B: pull from remote (simulates second machine)"
mkdir -p "$MACHINE_B" && cd "$MACHINE_B"
bit init
bit remote add origin "$REMOTE_DIR"
bit pull origin

if grep -q "original content" "$MACHINE_B/shared.txt"; then
    pass "Machine B has shared.txt from remote"
else
    fail "Machine B missing shared.txt!"
    exit 1
fi

if grep -q "untouched file" "$MACHINE_B/other.txt"; then
    pass "Machine B has other.txt from remote"
else
    fail "Machine B missing other.txt!"
    exit 1
fi

# ---- Step 3: Machine A edits and pushes ----
echo ""
echo "3. Machine A: edit shared.txt and push"
cd "$MACHINE_A"
cat > shared.txt <<'EOF'
line 1: edited by Machine A
line 2: base version
line 3: unchanged
EOF
bit add shared.txt
bit commit -m "Machine A edits line 1"
bit push origin
pass "Machine A's edit pushed to remote"

# ---- Step 4: Machine B edits the same file differently ----
echo ""
echo "4. Machine B: edit shared.txt differently, commit"
cd "$MACHINE_B"
cat > shared.txt <<'EOF'
line 1: original content
line 2: edited by Machine B
line 3: unchanged
EOF
bit add shared.txt
bit commit -m "Machine B edits line 2"
pass "Machine B committed divergent edit"

# ---- Step 5: Machine B tries to push (should fail) ----
echo ""
echo "5. Machine B: try to push (should fail — remote has diverged)"
cd "$MACHINE_B"
set +e
PUSH_OUTPUT=$(bit push origin 2>&1)
PUSH_EXIT=$?
set -e
echo "   Push exit code: $PUSH_EXIT"
echo "   Push output:"
echo "$PUSH_OUTPUT" | sed 's/^/      /'
if [ $PUSH_EXIT -ne 0 ]; then
    pass "push correctly rejected (remote has diverged)"
else
    fail "push should have been rejected but succeeded"
fi

# ---- Step 6: Machine B pulls with conflict resolution via stdin ----
echo ""
echo "6. Machine B: pull from remote (resolve conflict by accepting remote via stdin)"
cd "$MACHINE_B"
set +e
# Pipe "r" to stdin to select remote version for the conflict
PULL_OUTPUT=$(echo "r" | bit pull origin 2>&1)
PULL_EXIT=$?
set -e
echo "   Pull exit code: $PULL_EXIT"
echo "   Pull output:"
echo "$PULL_OUTPUT" | sed 's/^/      /'

# Check what happened
echo ""
echo "   shared.txt after pull:"
cat "$MACHINE_B/shared.txt" | sed 's/^/      /'

if echo "$PULL_OUTPUT" | grep -qi "conflict\|CONFLICT"; then
    pass "conflict was detected during pull"
else
    # Could be auto-merged (non-overlapping edits on different lines)
    echo "   INFO: no conflict markers in output (may have auto-merged)"
fi

# Verify the pull completed (remote version should be accepted, or auto-merged)
if grep -q "edited by Machine A" "$MACHINE_B/shared.txt"; then
    pass "Machine A's edit is present after pull"
else
    fail "Machine A's edit missing after pull"
fi

# ---- Step 7: Machine B combines both edits and pushes ----
echo ""
echo "7. Machine B: apply combined edits, commit, and push"
cd "$MACHINE_B"
cat > shared.txt <<'EOF'
line 1: edited by Machine A
line 2: edited by Machine B
line 3: unchanged
EOF
bit add shared.txt
bit commit -m "Combine both edits after conflict resolution"
pass "Machine B committed combined edits"

set +e
PUSH2_OUTPUT=$(bit push origin 2>&1)
PUSH2_EXIT=$?
set -e
echo "   Push exit code: $PUSH2_EXIT"
echo "   Push output:"
echo "$PUSH2_OUTPUT" | sed 's/^/      /'

if [ $PUSH2_EXIT -eq 0 ]; then
    pass "Machine B pushed resolved version"
else
    fail "Machine B could not push resolved version"
    exit 1
fi

# ---- Step 8: Machine A pulls the resolved version ----
echo ""
echo "8. Machine A: pull resolved version from remote"
cd "$MACHINE_A"
set +e
PULLA_OUTPUT=$(bit pull origin 2>&1)
PULLA_EXIT=$?
set -e
echo "   Pull exit code: $PULLA_EXIT"
echo "   Pull output:"
echo "$PULLA_OUTPUT" | sed 's/^/      /'

if [ $PULLA_EXIT -eq 0 ]; then
    pass "Machine A pulled successfully"
else
    fail "Machine A pull failed"
fi

# ---- Step 9: Verify final state ----
echo ""
echo "9. Verify final state"

FINAL_A=$(cat "$MACHINE_A/shared.txt")
FINAL_B=$(cat "$MACHINE_B/shared.txt")

echo "   Machine A shared.txt:"
echo "$FINAL_A" | sed 's/^/      /'
echo "   Machine B shared.txt:"
echo "$FINAL_B" | sed 's/^/      /'

if echo "$FINAL_A" | grep -q "edited by Machine A" && echo "$FINAL_A" | grep -q "edited by Machine B"; then
    pass "Machine A has both edits"
else
    fail "Machine A does not have both edits"
fi

if [ "$FINAL_A" = "$FINAL_B" ]; then
    pass "both machines have identical shared.txt"
else
    fail "machines have different shared.txt"
fi

# Verify other.txt untouched
OTHER_A=$(cat "$MACHINE_A/other.txt")
OTHER_B=$(cat "$MACHINE_B/other.txt")
if echo "$OTHER_A" | grep -q "untouched file" && echo "$OTHER_B" | grep -q "untouched file"; then
    pass "other.txt unchanged on both machines"
else
    fail "other.txt was modified unexpectedly"
fi

# ---- Cleanup ----
echo ""
echo "10. Cleanup"
cd ~
rm -rf "$MACHINE_A" "$MACHINE_B" "$REMOTE_DIR"
echo "   Done."

echo ""
echo "============================================"
echo "Summary: $STEPS_PASSED passed, $STEPS_FAILED failed"
echo "============================================"

if [ $STEPS_FAILED -gt 0 ]; then
    exit 1
fi
