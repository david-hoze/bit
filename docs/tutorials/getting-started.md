# Getting Started with bit

This tutorial walks you through creating a bit repository from scratch, tracking files, and using the basic commands. If you've used git before, everything will feel familiar.

## Quick Start

```bash
mkdir my-project && cd my-project
bit init
# ... create some files ...
bit add .
bit commit -m "First commit"
```

## Creating a Repository

```bash
bit init
```

You'll see:

```
Initialized empty bit repository in /path/to/my-project/.bit/index/.git/
```

This creates a `.bit/` directory with bit's internal structure. Your working files live in the project root as usual — bit stays out of the way.

## Adding Files

Put some files in your project — documents, videos, images, source code, anything:

```bash
echo "# My Project" > README.md
cp ~/Desktop/presentation.pptx .
cp ~/Videos/demo.mp4 .
```

Now tell bit to track them:

```bash
bit add .
```

```
Scanning files...
Collecting files... 3 found.
Writing metadata: 3 files.
```

bit classifies each file automatically:

- **Text files** (README.md, source code, scripts): content is stored directly in the index, exactly like git. All git features work identically for text files.
- **Binary files** (videos, images, datasets): only a small metadata file (hash + size) is stored in the index. The actual content stays in the working tree.

You can also add specific files:

```bash
bit add README.md
bit add demo.mp4
```

## Committing

```bash
bit commit -m "Add project files"
```

```
[main abc1234] Add project files
 3 files changed, 3 insertions(+)
```

This works exactly like `git commit`. Your files are now tracked.

## Checking Status

```bash
bit status
```

When everything is committed:

```
On branch main
nothing to commit, working tree clean
```

After modifying a file:

```
On branch main
Changes not staged for commit:
  (use "bit add <file>..." to update what will be committed)

        modified:   README.md
```

bit detects both text file changes (content changed) and binary file changes (hash or size changed).

## Viewing Diffs

```bash
bit diff
```

For text files, you see the actual content diff — same as `git diff`:

```diff
diff --git a/README.txt b/README.txt
index e69de29..cb983c4 100644
--- a/README.txt
+++ b/README.txt
@@ -0,0 +1,3 @@
+# My Project
+
++This is a demo project.
```

For binary files, bit shows the hash and size changes:

```diff
diff --git a/demo.mp4 b/demo.mp4
index c0e968b..0c2daab 100644
--- a/215007.pdf
+++ b/215007.pdf
@@ -1,2 +1,2 @@
-hash: md5:0bb411bceef41732b8a4181c383048a7
-size: 4063862
+hash: md5:f83dc78da425281f2848ae561040df06
+size: 4094004
```

To see staged changes:

```bash
bit diff --staged
```

## Viewing History

```bash
bit log
```

```
commit abc1234 (HEAD -> main)
Author: You <you@example.com>
Date:   Wed Feb 19 2026

    Add project files
```

All of git's log options work: `bit log --oneline`, `bit log --graph`, `bit log -p`, etc.

## Other Familiar Commands

These all work exactly like their git counterparts:

```bash
bit restore README.md          # discard changes to a file
bit restore --staged README.md # unstage a file
bit reset                      # unstage everything
bit rm old-file.txt            # stop tracking and delete
bit mv old.mp4 new.mp4         # rename a tracked file
bit branch feature             # create a branch
bit checkout feature           # switch branches
bit merge feature              # merge branches
```

## Ignoring Files

Create a `.bitignore` file in the project root (same syntax as `.gitignore`):

```
*.tmp
build/
.DS_Store
```

bit syncs this to `.bit/index/.gitignore` automatically before any command that needs it.

If you already have a `.gitignore`, it works too — bit copies `.gitattributes` from the project root into the index.

## Working from Subdirectories

bit commands work from any subdirectory, just like git:

```bash
cd src/
bit add .          # adds files in src/ only
bit status         # shows status relative to src/
bit diff           # shows changes in src/ only
```

## What's Next

- [Cloud Remotes](cloud-remotes.md) — push and pull to Google Drive, S3, or any rclone backend
- [Filesystem Remotes](filesystem-remotes.md) — sync to USB drives, NAS, or local directories
- [Modes and CAS](modes-and-cas.md) — enable full binary history with solid mode
- [Git Import](git-import.md) — convert an existing git repo to bit
