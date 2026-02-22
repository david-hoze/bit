#!/bin/bash
# SessionEnd hook: clean up claude-collab agent registration
# Unclaims all files and removes the agent from the registry.

set -euo pipefail

COLLAB_DIR="${CLAUDE_PROJECT_DIR:-.}/.claude-collab"
if [ ! -d "$COLLAB_DIR" ]; then
    exit 0
fi

# Clean up all registered agents for this session
for AGENT_HASH in $(ls "$COLLAB_DIR/agents/" 2>/dev/null); do
    claude-collab cleanup "$AGENT_HASH" 2>/dev/null || true
done

exit 0
