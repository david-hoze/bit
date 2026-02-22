#!/bin/bash
# SessionStart hook: auto-register with claude-collab
# Uses a short hash of the session ID as the agent name.

set -euo pipefail

INPUT=$(cat)
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')

if [ -z "$SESSION_ID" ]; then
    exit 0
fi

# Derive a short name from session ID
SHORT_ID=$(echo "$SESSION_ID" | cut -c1-8)
AGENT_NAME="agent-$SHORT_ID"

claude-collab init --name "$AGENT_NAME" 2>/dev/null || true

exit 0
