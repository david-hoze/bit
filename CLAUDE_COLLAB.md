# Multi-Agent Coordination

You are one of multiple Claude Code agents working in this repository.
A coordination tool `claude-collab` is available in your PATH.

## Automatic registration

A SessionStart hook (`session-start-init.sh`) automatically runs `claude-collab init` when your session begins, using a name derived from your session ID (e.g. `agent-a3f8b201`). You do NOT need to register manually.

## Messaging

Use **Claude Code's native messaging system** to communicate with other agents. Do NOT use `claude-collab send` or `claude-collab read` — those are deprecated.

## Shared working directory

All agents run in the same repository directory. When another agent commits or pushes, you already have the changes on disk — do NOT run `git pull` or `git fetch`. Just read the files directly. The channel message telling you about a push is informational; you're already up to date.

**Builds are additive, not contradictory.** Because all agents edit source files in the same directory, any build compiles everyone's changes — not just yours. If agent A edits `Foo.hs` and agent B edits `Bar.hs`, then either agent running `cabal exec -- ghc --make` produces a binary with both fixes. This means overwriting a shared binary (like `extern/git-shim/bit.exe`) with your build is fine — the other agent's changes are already included in your binary. You are not replacing their work, you are adding to it.

**Coordinate builds when binaries are locked.** If you need to build and install but the shared binary is locked (another agent is mid-test), message the other agent and ask them to pause so you can build. Don't wait silently for locks to clear — direct communication is faster.

## Hooks — automatic init, claim, and cleanup

Three hooks in `.claude/hooks/` automate the collaboration workflow:

- **`session-start-init.sh`** (SessionStart) — automatically runs `claude-collab init` to register the agent.
- **`pre-edit-claim.sh`** (PreToolUse on Edit|Write) — automatically runs `claude-collab files claim` before every file edit. You do NOT need to manually claim files.
- **`session-end-cleanup.sh`** (SessionEnd) — automatically runs `claude-collab cleanup` when your session ends. You do NOT need to manually clean up.

**What you still do manually:**
- `claude-collab commit $HASH -m "message"` when you're done with your feature — commit deliberately, not after every edit

## The one rule

**Commit through the tool.** Run `claude-collab commit $HASH -m "message"` instead of raw git. NEVER run `git add`, `git commit`, or `git checkout` directly.

## The workflow: edit → commit

```
# ... edit files ...                          # 1. Edit (claim is automatic via hook)
claude-collab commit $HASH -m "message"       # 2. Commit when feature is done (stages, commits, and unclaims)
```

`commit` automatically unclaims the committed files — you do NOT need to run `files unclaim` afterward. Never unclaim files without committing first, or your changes will be untracked dirty files that no agent owns.

## When a claim is rejected

If `files claim` fails because another agent has the file:

1. Message the other agent using Claude Code's native messaging to negotiate who edits what, or whether to co-claim.
2. Once agreed, co-claim:
```
claude-collab files claim $HASH <file> --shared
```

## Committing shared files

When you're done with your part of a co-claimed file, just run `commit` as normal.

- If the other agent isn't done yet, your files will be **staged** (git add) and you're free to work on other things.
- You can keep claiming and committing new files even while waiting — only the already-staged files are held back.
- When the last agent runs `commit`, the actual git commit happens with everyone's changes included.

You don't need to wait. You don't need to coordinate the commit timing. Just `commit` when you're done and move on.

## Shared resources

Some operations are physically exclusive — only one agent can run them at a time.
Run `claude-collab reservations` to check what's available.

Before running tests, builds, or package installs:
```
claude-collab reserve $HASH test
npm test
claude-collab release $HASH test
```

If the resource is busy, the command waits until it's free. Always release when done.

If you need to release and immediately re-reserve (e.g., running tests again), use `--renew` to do it atomically:
```
claude-collab reserve $HASH test --renew
```
This avoids a race condition where another agent grabs the resource between your `release` and `reserve`.

## Sharing test and build results

After running tests or a build, message other agents with the result so they can skip redundant work.

Before reserving `test` or `build` yourself, check if another agent recently posted a test or build result. If no files have changed since that result, skip the run and use the existing result.

If you *have* changed relevant files since the last posted result, run it yourself and share the new result.

## Helping each other

Messaging isn't just for conflict resolution — it's how agents delegate work and make use of each other's time. There are two situations where this matters:

### Delegating responsibility

If you discover a problem that belongs to another agent — a test broke because of their changes, a type error in a file they own, a regression in their feature — tell them directly via native messaging.

Don't fix other agents' bugs silently. The agent who owns the code has the context to fix it properly. If you spot the issue, describe it clearly — what's failing, why you think it's related to their work, and which files are involved. Then move on with your own task.

If you receive a message about your own changes breaking something, prioritize it. You broke it, you fix it.

### Requesting help from idle agents

If you're stuck on something outside your feature scope — a flaky test you didn't cause, a build configuration issue, a global refactor that blocks your progress — message other agents for help.

This is most useful when another agent is idle or between tasks. Don't expect an immediate response from an agent that's mid-task.

### Picking up work

**Act immediately — do not ask the user for permission.** The user told you to collaborate precisely so you would work autonomously with other agents. When another agent hands you work, delegates a task, or requests help — just do it. Acknowledge the message, claim the files, and start working. Sitting idle waiting for user confirmation defeats the purpose of collaboration.

When you finish your assigned task, don't immediately clean up. Check for messages from other agents — look for help requests, handover messages, or fix requests. If you see something you can handle, acknowledge it and start working immediately.

**When to pick up extra work:**
- You are idle or done with your assigned task.
- Another agent hands you work or asks for help.
- The request is well-scoped — you can understand what needs doing from the message alone.

**When not to:**
- You're mid-task and context-switching would be costly.
- The request is vague or open-ended.
- The user is actively giving you different instructions.

If you can't help, say so briefly — "can't right now, mid-task" is better than silence.

## When you're done

Don't clean up immediately if other agents are still working — check for messages first and see if there are any help requests you can handle. Once everything is settled, you're done.
