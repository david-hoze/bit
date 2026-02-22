# Multi-Agent Coordination

You are one of multiple Claude Code agents working in this repository.
A coordination tool `claude-collab` is available in your PATH.

## First thing — every session

Register yourself and start listening for messages:

```
claude-collab init --name my-feature
```

The `--name` flag gives your agent a human-readable alias. Other agents (and you) can use it instead of the hash in all commands — `claude-collab read my-feature` instead of `claude-collab read a3f8b201`.

The hash is printed in the JSON output. Save it: `HASH=<your-hash>`

You can also supply your own hash: `claude-collab init a3f8b201 --name my-feature`

### Start a message listener

Immediately after init, start a background task that waits for incoming messages:

```
claude-collab read $HASH --wait
```

Run this as a **background task** so it doesn't block your work. When a message arrives, it returns instantly (fsnotify-based, no polling delay). Read the result, act on it, then start a new `--wait` in the background.

**This is mandatory.** Every agent must always have a `--wait` listener running. Without it, you won't see messages from other agents until you manually poll with `claude-collab read $HASH`.

The `--wait` flag:
- Returns immediately if there are already unread messages
- Blocks until a new message from another agent arrives (your own sends are filtered out)
- Times out after 60 seconds by default (use `--timeout N` to change)
- On timeout with no messages, returns `{"messages":[],...}` — just restart the listener

**Pattern: wait → read output → act → wait again**

```
# 1. Start background wait
claude-collab read $HASH --wait --timeout 600  # (run in background)

# 2. When the background task completes, ALWAYS read the output file
# 3. Act on any messages (reply, fix bugs, pick up work)
# 4. Start a new background wait
claude-collab read $HASH --wait --timeout 600  # (run in background)
```

Keep this loop running for the entire session.

**CRITICAL: Always read the output file.** When the background listener completes (whether by message or timeout), you MUST read the output file before restarting the listener. Messages are delivered in the output — if you skip reading it and just restart, you will miss messages. Never blindly restart the listener without checking.

**CRITICAL: Never stop listening.** You must have a background `--wait` listener running at ALL times during your session. When a listener completes (message or timeout), immediately read the output and start a new one. Do not let the listener lapse — if you get busy with other work and forget to restart it, you will miss messages from other agents. Treat the listener like a heartbeat: if it's not running, you're unreachable.

## Shared working directory

All agents run in the same repository directory. When another agent commits or pushes, you already have the changes on disk — do NOT run `git pull` or `git fetch`. Just read the files directly. The channel message telling you about a push is informational; you're already up to date.

**Builds are additive, not contradictory.** Because all agents edit source files in the same directory, any build compiles everyone's changes — not just yours. If agent A edits `Foo.hs` and agent B edits `Bar.hs`, then either agent running `cabal exec -- ghc --make` produces a binary with both fixes. This means overwriting a shared binary (like `extern/git-shim/bit.exe`) with your build is fine — the other agent's changes are already included in your binary. You are not replacing their work, you are adding to it.

**Coordinate builds when binaries are locked.** If you need to build and install but the shared binary is locked (another agent is mid-test), message the other agent and ask them to pause so you can build. Don't wait silently for locks to clear — direct communication is faster.

## The two rules

1. **Claim before editing.** Run `claude-collab files claim $HASH <file>` before editing any file. NEVER use the Edit or Write tool on a file you haven't claimed. Not even "just a quick fix." Claim first, always.
2. **Commit through the tool.** Run `claude-collab commit $HASH -m "message"` instead of raw git. NEVER run `git add`, `git commit`, or `git checkout` directly.

**IMPORTANT — Edit/Write gate:** Every time you are about to call the Edit or Write tool on any file, stop and ask yourself: "Have I already run `claude-collab files claim` on this file in this session?" If not, you must run the claim command BEFORE the edit. This applies from the moment you run `claude-collab init` — not just when you "feel like" you're collaborating. Initializing means you are in collaboration mode for the rest of the session, and every file edit must go through the claim-edit-commit workflow. There are no exceptions, no "quick fixes," and no "I'll claim it later."

## The workflow: claim → edit → commit

**This order is strict. Do not skip or reorder steps.**

```
claude-collab files claim $HASH <file>       # 1. Claim FIRST
# ... edit the file ...                       # 2. Edit ONLY AFTER claiming
claude-collab commit $HASH -m "message"       # 3. Commit (stages, commits, and unclaims)
```

**Common mistake:** editing a file first, then claiming it right before commit. This is wrong — another agent may have been editing the same file concurrently. The claim must happen BEFORE the first edit, not before the commit.

`commit` automatically unclaims the committed files — you do NOT need to run `files unclaim` afterward. Never unclaim files without committing first, or your changes will be untracked dirty files that no agent owns.

## When a claim is rejected

If `files claim` fails because another agent has the file:

1. Send a message:
```
claude-collab send $HASH "I need to edit <file> — what parts are you changing?"
```
2. Your background `--wait` listener will pick up the response.
3. Negotiate: agree on who edits what, or whether to co-claim.
4. Once agreed, co-claim:
```
claude-collab files claim $HASH <file> --shared
```

## Committing shared files

When you're done with your part of a co-claimed file, just run `commit` as normal.

- If the other agent isn't done yet, your files will be **staged** (git add) and you're free to work on other things.
- You can keep claiming and committing new files even while waiting — only the already-staged files are held back.
- When the last agent runs `commit`, the actual git commit happens with everyone's changes included.
- You'll see a channel message when the commit goes through.

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

After running tests or a build, broadcast the result so other agents can skip redundant work:

```
claude-collab send $HASH "test-result: PASS (all 47 specs)" --type status claude-collab send $HASH "build-result: OK (binary at dist/claude-collab)" --type status
```

Before reserving `test` or `build` yourself, read the channel:

```
claude-collab read $HASH
```

If another agent posted a recent `test-result` or `build-result` **and** you haven't changed any files that would affect the outcome since that message, skip the run. Use the existing result. Don't waste time re-running a test suite that already passed against the current state of the code.

If you *have* changed relevant files since the last posted result, run it yourself and post the new result.

## Helping each other

The channel isn't just for conflict resolution — it's how agents delegate work and make use of each other's time. There are two situations where this matters:

### Delegating responsibility

If you discover a problem that belongs to another agent — a test broke because of their changes, a type error in a file they own, a regression in their feature — tell them directly:

```
claude-collab send $HASH "[needs-fix] @<other-hash> Tests in auth.spec.ts are failing — looks like the session refactor broke the token validation path. This is in your claimed files." --type chat
```

Don't fix other agents' bugs silently. The agent who owns the code has the context to fix it properly. If you spot the issue, describe it clearly — what's failing, why you think it's related to their work, and which files are involved. Then move on with your own task.

If you receive a `[needs-fix]` message about your own changes, prioritize it. You broke it, you fix it.

### Requesting help from idle agents

If you're stuck on something outside your feature scope — a flaky test you didn't cause, a build configuration issue, a global refactor that blocks your progress — ask for help:

```
claude-collab send $HASH "[help-wanted] The lint config is rejecting my new files because of a missing rule. Can someone update .eslintrc? I don't want to claim it mid-feature." --type chat
```

This is most useful when another agent is idle or between tasks. Don't expect an immediate response from an agent that's mid-task.

### Picking up work

**Act immediately — do not ask the user for permission.** The user told you to collaborate precisely so you would work autonomously with other agents. When another agent hands you work, delegates a task, or sends a `[help-wanted]` request — just do it. Acknowledge the message, claim the files, and start working. Sitting idle waiting for user confirmation defeats the purpose of collaboration.

When you finish your assigned task, don't immediately clean up. Check the channel:

```
claude-collab read $HASH
```

Look for `[help-wanted]` messages, `[handover]` messages, or `[needs-fix]` messages. If you see something you can handle:

```
claude-collab send $HASH "Picking up the eslint fix." --type chat
```

Then claim the relevant files and start working immediately.

**When to pick up extra work:**
- You are idle or done with your assigned task.
- Another agent hands you work or asks for help.
- The request is well-scoped — you can understand what needs doing from the message alone.

**When not to:**
- You're mid-task and context-switching would be costly.
- The request is vague or open-ended.
- The user is actively giving you different instructions.

If you can't help, say so briefly — "can't right now, mid-task" is better than silence.

## Status heartbeats

Every few minutes of active work, post a brief status so other agents know what you're doing:

```
claude-collab send $HASH "Working on auth middleware, editing src/auth.ts and src/session.ts" --type status
```

This helps other agents make informed decisions: whether to send you a `[needs-fix]`, whether your test results are still relevant, and whether a file you haven't claimed yet is about to be touched.

## Staying responsive

Your background `--wait` listener handles message delivery. When it returns a message, read it and respond promptly — especially `[needs-fix]` messages about your own changes. Always restart the listener after handling a message.

If your listener timed out or you suspect you missed something, a quick `claude-collab read $HASH` will catch up on any unread messages.

## When you're done

Don't clean up immediately if other agents are still working — check the channel first and see if there are any `[help-wanted]` requests you can handle. Once everything is settled:
