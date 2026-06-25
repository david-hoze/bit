# Launch kit

Ready-to-post drafts and tactics for launching bit. Research-backed venue/timing
guidance is in the "Tactics" section at the bottom. Edit the drafts to sound
like you — authenticity beats polish on these platforms.

## Positioning (the hook)

> **bit — version control for large files using Git + rclone, no server required.**
> Git tracks tiny metadata stubs; rclone syncs the actual bytes to any backend
> (Google Drive, S3, USB, NAS). No pointer files, no special server, no vendor
> lock-in.

**Lead audience for the first launch:** the general developer / self-hoster /
indie crowd, framed around *serverless + any-backend + no-LFS*. It's the
broadest hook and the most HN-native (clever architecture, self-hostable,
scratch-your-own-itch). Game-dev and ML are strong *secondary* angles — save
targeted posts (r/gamedev, r/MachineLearning) for follow-up launches so each
post can speak that audience's language.

---

## Show HN

**Title** (pick one; keep it plain, no hype — HN rewards substance):

- `Show HN: Bit – Git for large files using rclone, no server required`
- `Show HN: Bit – serverless version control for binary assets (Git + rclone)`
- `Show HN: Bit – version control for large files that syncs to any storage`

**First comment** (post within ~5 minutes of submitting — this is the most
important artifact; it's your context, not marketing):

> Hi HN — I built bit because every option for versioning large/binary files
> annoyed me in a different way: Git LFS needs a server and ties you to a host,
> Perforce is expensive and offline-hostile, git-annex is powerful but complex,
> and DVC is really aimed at ML pipelines.
>
> bit takes a deliberately lazy approach — it doesn't reinvent version control.
> Git already does history, branches, merges, and diffs, so bit lets Git track
> tiny metadata stubs (hash + size) for your binaries and uses rclone to move
> the actual bytes to wherever you want: Google Drive, S3, a USB drive, a NAS —
> anything rclone speaks. There's no server to run and no lock-in; any folder
> rclone can reach is a valid remote.
>
> Things that fell out of the design:
> - Your working tree stays normal — no pointer files, no smudge/clean filters.
> - It's git-compatible by construction (you can even route `git` through it).
> - Content-defined chunking (FastCDC) means a small edit in a multi-GB file
>   only re-uploads the changed chunks.
> - Multi-remote is first-class — cloud + USB + a Git host in one repo.
> - Advisory file locking for binaries that can't be merged.
>
> It's early (v0.1.0), written in Haskell. Prebuilt Linux/Windows binaries are
> on the releases page; the macOS/Apple-Silicon binary is pending a dependency
> fix (building from source works). It's been tested most heavily against Google
> Drive — other rclone backends should work but are less battle-tested.
>
> I'd love feedback on the core idea (Git-metadata + rclone-sync) and especially
> on where it would break for your workflow.
>
> Repo: https://github.com/david-hoze/bit

**Tone notes:** lead with the problem, state the trick plainly, list concrete
consequences, be upfront about "early / less-tested / no Mac binary yet." Don't
claim benchmarks you can't back (a bit-vs-LFS benchmark is a planned follow-up).

---

## Reddit

Post the link, then a top comment with context (similar to the HN first comment
but tuned per sub). Read each sub's self-promo rules first.

### r/selfhosted

**Title:** `Bit: self-hosted version control for large files — your storage, no server, no vendor lock-in`

**Body:** Lead with the self-hoster value: bit versions big/binary files using
Git for metadata and rclone for the bytes, so your data lives on *your* backend
(NAS, S3-compatible box, a USB drive, Drive) with **zero server-side software**.
Normal working tree, git-compatible, multi-remote. v0.1.0, Linux/Windows
binaries, MIT.

### r/programming

**Title:** `Bit – large-file version control by orchestrating Git + rclone instead of reimplementing either`

**Body:** Lead with the architecture: the interesting bit is what it *doesn't*
build. Git owns the DAG/merge/diff; rclone owns transfer; bit is a thin layer
that wires them so binaries live in a content-addressed store on dumb storage.
Link the design notes (`docs/spec/`). Invite critique of the approach.

### r/gamedev (follow-up launch)

**Title:** `Bit – free, serverless version control for game assets (no Perforce server, no Git LFS)`

**Body:** Lead with the artist pain: free + offline + real file locking +
big-asset friendly, on any storage. Honest about early status.

---

## Lobsters

Submit the repo with tags `programming`, `vcs`. Add an authored comment mirroring
the HN first comment. Lobsters skews technical and values honesty about scope and
trade-offs — emphasize the "don't reinvent git/rclone" design decision.

---

## Tactics (research-backed)

- **When:** Show HN performs best **Tue–Thu, ~8–10 AM PT**. Pick a day you can
  sit with it for a few hours.
- **First comment within ~5 minutes** of submitting (the context above).
- **Cross-post to Reddit ~30 minutes later**, not simultaneously.
- **Reply to every comment quickly and non-defensively** for the first few
  hours — engagement keeps a post alive; argue-y replies kill it.
- **Don't ask for upvotes** anywhere (against HN/Reddit rules; backfires).
- **Have the obvious questions pre-answered** in the first comment: how is this
  different from LFS / git-annex / DVC / Perforce? why Haskell? is it safe for my
  data? what's the catch (early, Mac binary pending, Drive-tested)?
- **Make the landing instant:** the README opens with what it is, a 5-line
  quickstart, and one-command install — already in place.

## Pre-launch checklist

- [ ] README hero + install are tight (done — prebuilt binaries + badges).
- [ ] A 30–60s asciinema/GIF of `init → add → commit → push` in the README.
- [ ] macOS binary (or a clear "build from source" note — present).
- [ ] A reproducible bit-vs-Git-LFS benchmark to cite if asked (planned).
- [ ] Decide the lead audience and pick the matching title above.
