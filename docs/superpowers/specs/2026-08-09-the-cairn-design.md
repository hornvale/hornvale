# The Cairn — Design

**Campaign:** The Cairn
**Branch:** `campaign/the-cairn`
**Date:** 2026-08-09
**Status:** spec, awaiting G3

A cairn is a marker built one stone at a time by successive passers-by. Nobody
addresses it to anybody; it is read by whoever comes next. That is the shape of
this campaign: an append-only board, embedded in git, through which parallel
agent sessions coordinate without ever communicating directly.

## 1. The problem

Hornvale is developed by many concurrent Claude Code sessions, each in its own
git worktree, each editing one shared Rust workspace. At the time of writing
there were **nine live worktrees** against CLAUDE.md's stated working ceiling of
"two to three active campaigns."

The **mechanical** half of the resulting pain is solved. `PROC-12` / The
Standing Offer shipped `merge=union` for append-only lists and a
regenerate-on-conflict driver (`scripts/merge-regenerate.sh`) for reference
dumps, and CLAUDE.md's staggering rule addresses gate contention.

The **semantic** half is unsolved and known to be unsolved. CLAUDE.md says so
outright:

> `make preflight` mechanizes only the **checkable** half. It compares ancestry
> and peeks at main's checkout; it has no opinion about whether two campaigns
> changed the same idea in incompatible ways. The Tumult and The Waterline
> collided semantically with a clean GO. Read the other branches' chronicles,
> not just their diffs.

`docs/retrospectives/the-actants.md` records the same shape recurring: "a clean
textual merge hid a semantic collision, exactly as the preflight warns." The
prescribed remedy — *read the other branches' chronicles* — is a human
instruction with no mechanism behind it, and `PROC-deferral-needs-a-watcher`
already names that shape: **a deferral whose trigger is a code property needs a
check, or it is a wish.**

The deeper reason a diff cannot catch these collisions is that the colliding
thing is not in the substrate yet. Lifted to its structural form:

> Concurrent workers hold private state whose future effects are not yet
> observable in the shared medium; the medium reveals conflict only at a
> reconciliation barrier — the latest and most expensive moment to learn of it.

This is **optimistic concurrency without an intent log**. The Cairn is the
intent log.

## 2. What is already true, and was never written down

Facts established by direct probe on 2026-08-09 (commands and outputs recorded
in §7), which the design rests on:

1. **Git worktrees share one ref store and one object database.** A ref written
   from `main`'s checkout is readable instantly from every worktree — including
   worktrees outside the repository directory — with no fetch, no checkout, and
   no merge.
2. **A ref can be written without touching any working tree.** Writing via
   plumbing (`hash-object` → `mktree` → `commit-tree` → `update-ref`) leaves
   `git status --porcelain` empty.
3. **`update-ref` provides compare-and-swap.** Supplying a stale old-value is
   rejected: `fatal: cannot lock ref …: is at <X> but expected <Y>`.
4. **CAS plus retry loses no writes under real contention.** Eight concurrent
   writers appending to one ref produced 8 of 8 posts and a 9-commit chain.
5. **A ref under a private namespace is invisible to ordinary views.**
   `git branch --list` reports zero hits for it.
6. **Each worktree has private, untracked, dies-with-the-worktree state.**
   `git rev-parse --git-path <name>` resolves to
   `.git/worktrees/<worktree>/<name>`.

Two existing seams the design consumes rather than creates:

- `.claude/settings.json` already registers a `SessionStart` hook (async,
  `|| true`, 30 s timeout).
- `scripts/doctor.sh` already prints a `== Live state` section.

## 3. Decisions

**D1 — The substrate is one orphan git ref, not a tracked file.**
`refs/hornvale/board`, sharing no history with `main`. Rejected alternative: a
tracked file on `main` with `merge=union`, which reuses shipped `PROC-12`
machinery and is viewable on GitHub — but a post written on a campaign branch is
invisible to every other session **until that branch merges**, which is exactly
the moment the board exists to precede. Writing to `main` instead means touching
`main`'s checkout, which CLAUDE.md warns may be mid-landing. Also rejected:
`git notes`, whose union merge and per-commit anchoring are elegant for
annotating a *landed* change but have nothing to hang off for intent about work
not yet committed.

**D2 — Roll our own; do not adopt git-bug.** git-bug independently made the same
substrate bet ("embeds issues, comments, and more as objects in a git
repository (*not files!*)"), which is convergent validation of D1. It is
declined because storage is not the hard part: the value of this design is the
read points, the relevance filter, TTL decay, the reaper, and the content rule —
none of which git-bug addresses. Its entity model (title, open/closed, labels,
comments, assignee) shoehorns exactly the load-bearing fields (`ttl_s`,
`polarity`, `paths`, `host`), and the bridges it is usually chosen for
(syncing issues out to GitHub/GitLab) are an anti-goal here. **Where git-bug is
better, see D8.**

**D3 — Two artifacts, deliberately unmixed.** `register.jsonl` (subject-keyed,
machine-checkable, decaying) and `threads/NNNN-slug.md` (id-keyed, irreducible,
durable). This is `PROC-11`'s unmix doctrine: durable knowledge is either
derivable-and-checkable or irreducible-and-authored, and the two are not stored
together. Fusing them would dilute the checkable half and leave nothing
enforceable at the gate.

**D4 — The board carries only the non-derivable residue.** Anything git or
`make doctor` can derive — which worktrees exist, which branch, which SHA, which
files a branch touched, whether `main` is an ancestor — is **forbidden** as a
post. The board carries intent and judgment only. This rule is what keeps the
board small enough that reading it stays free (§D6).

**D5 — No timestamps in the files; the commit is the clock.** A claim carries a
*duration* (`ttl_s`), never an instant. This follows `tools/digest/facts.jsonl`,
the "compacted, TIME-FREE store of what the project asserts about itself
(project time is git's)."

**D6 — The read is ambient and free, never a command to remember.** The 10%
rung of the coupling scale — a file plus a convention to read it — is the rung
the board would occupy by default, and it is empirically dead here: the
staggering rule is exactly that rung, and nine worktrees against a stated
ceiling of three is the measurement of its failure. Renders therefore attach
only to seams a session already crosses, and are **unread-only, relevance-
filtered, and capped**, because an unfiltered render is a permanent context tax
on every session forever.

**D7 — The board is advisory and never blocks.** Decision 0081 already
*declined* to claim the gate — "waiting twelve minutes to start a four-minute
gate is worse than the contention" — so a board that enforced would relitigate a
settled decision. The board also may never break a session: renders are
non-fatal, capped, and skip malformed lines with a warning.

**D8 — v1 is single-box, and the revisit condition is named.** CAS serializes
writers within *one* ref store (verified). Two clones — this Mac and lefford
(decision 0086) — that both write and then push will **diverge, and CAS gives
nothing there**. v1 does not span hosts, because claims are physical and
contention is a property of a machine. If the board ever needs to span hosts,
the problem to solve is divergent-history merge, and git-bug's operation-log
model is the prior art to study — **steal the design, not the dependency.**

**D9 — Claims decay and dead branches are reaped.** An undecayed claim is
decision 0080's stuck alarm ("chronicity is a diagnostic, the alarm is stuck"),
and `PROC-floors-erode-unseen` is the same disease in another organ. A notice
from a worktree that is later torn down is a false warning that goes on costing
other sessions real work, so branch-keyed posts die when their branch or
worktree does.

## 4. The design

### 4a. Storage layout

One orphan ref, `refs/hornvale/board`, whose tree is:

```
register.jsonl        one JSON object per line, append-only
threads/0001-<slug>.md
threads/0002-<slug>.md
```

Register entries, time-free (D5):

```json
{"kind":"claim","by":"the-cairn","host":"ambrose","pid":41207,
 "resource":"cores","ttl_s":900,"note":"make gate running"}
{"kind":"notice","by":"campaign/the-signet","polarity":"hold-off",
 "paths":["domains/terrain/"],"subject":"elevation re-derivation",
 "note":"epoch bump likely; do not pin heights","until":"branch-merge"}
{"kind":"retract","ref":"<sha of the post being retracted>"}
```

`polarity` is `fyi` or `hold-off`. Only `hold-off` is meaningless without an
acknowledgement, so only `hold-off` is surfaced at `preflight` (§4c).

Threads are append-only Markdown, one file per thread, no addressing: a session
opens a question, a later session in that area answers it.

### 4b. Writes: CAS with retry

`post` reads the ref, builds a new tree with the line appended (or the thread
file created/extended), commits with the old tip as parent, and calls
`update-ref <new> <old>`. On rejection it re-reads and retries with jittered
backoff; after exhausting retries it fails loudly with the physical reason.
Verified to lose nothing at 8-way contention (§7).

Writes touch no working tree, so a session can post mid-task without dirtying
its checkout.

### 4c. Reads: three seams, all pre-existing

| rung | seam | what it renders |
|------|------|-----------------|
| ~25% | `SessionStart` hook | unread + relevant + capped, non-fatal |
| ~25% | `make doctor`'s `== Live state` | the live register |
| ~90% | `make preflight` | other live branches' `hold-off` notices |

**Relevance** is the intersection of a notice's `paths` with the reading
worktree's own `git diff --name-only main...HEAD`. This delivers routing-by-
topic — a post finding its reader rather than waiting for one — without adding
an interrupt surface.

**Unread** is a per-worktree cursor at `git rev-parse --git-path
hv-board-cursor`: untracked, private to the worktree, dies with it (§2 fact 6),
so there is no shared read-state to contend on.

### 4d. Lifecycle: decay, retract, reap

- A claim is live while `ttl_s` has not elapsed since its commit **and** its
  `pid` on this `host` is alive. Otherwise it is rendered as expired, not as
  current.
- `retract` appends a retraction rather than rewriting history (append-only).
- `reap` drops posts whose `by` names a branch that no longer exists or is
  merged into `main`, and compacts `register.jsonl` into a fresh single-commit
  tree — the digest's compaction discipline.

### 4e. The tool

`tools/board/`, outside the cargo workspace, std-only, alongside
`tools/digest/` and `tools/type-audit/` (the workspace dependency allowlist
does not bind it; nothing beyond std is needed anyway). Subcommands: `post`,
`read`, `ack`, `retract`, `reap`, `render`.

## 5. Non-goals

- **Not a lock.** Never blocks a gate, a write, or a merge (D7).
- **Not a mirror of the substrate.** No derivable facts (D4).
- **Not cross-host.** v1 is single-box (D8).
- **Not a replacement for 0081's `/tmp` census claim.** v1 *reads*
  `hornvale_lab::census_claim::current_holder()` and renders it; it changes
  nothing about the write-seam guard.
- **Not pushed to `origin`** by default. Publishing is an externally visible
  act and is Nathan's to authorize.
- **No pairwise/direct messaging.** Rejected by framing: pairwise channels are
  how you get seven boards nobody reads.
- **Not part of determinism.** The board writes nothing any world, artifact, or
  seed derivation reads. No save-format, epoch, or stream-label surface is
  touched.

## 6. Assumptions requiring measurement

1. **The render's context cost stays negligible.** Budget: the `SessionStart`
   render is **≤ 15 lines and ≤ 1 KB** in the steady state, and a cap enforces
   it. Unmeasured: what the real steady-state post volume is with several
   sessions live. If the cap is routinely hit, the content rule (D4) is being
   violated or the relevance filter is too loose.
2. **CAS retry stays sufficient at realistic concurrency.** Verified at 8
   writers on one box; unmeasured beyond that. The failure mode is loud (retry
   exhaustion), not silent.
3. **Relevance-by-changed-paths actually matches the collisions we care about.**
   The Tumult/Waterline and Actants collisions are the natural test corpus:
   would a `hold-off` naming the colliding paths have reached the other session?
   This is a retrospective check against real history, not a live experiment.
4. **Agents will actually post.** The board's read side is mechanized; the write
   side is a judgment call by each session. Unmeasured and only partly
   mechanizable — §8 pairs it with a skill instruction, not a gate.

## 7. Verification

Recorded from live runs on 2026-08-09, `main` at `491fc059`:

```
# facts 1-3, 5: shared refs, no working-tree effect, CAS, invisibility
B=$(printf '[notice] …\n' | git hash-object -w --stdin)
T=$(printf '100644 blob %s\t0001-signet-elevation.md\n' "$B" | git mktree)
C=$(git commit-tree "$T" -m "board: notice"); git update-ref refs/hv-board-probe/main "$C"
  -> git status --porcelain : (empty)
  -> git -C .claude/worktrees/the-tilth show refs/hv-board-probe/main:0001-…md
       [notice] the-signet: re-deriving elevation, epoch bump likely
  -> same from /Users/nathan/.config/superpowers/worktrees/hornvale/libm-spike
  -> stale CAS: fatal: cannot lock ref 'refs/hv-board-probe/main':
       is at 68ead1ca… but expected 41611d10…
  -> git branch --list | grep -c hv-board-probe : 0

# fact 4: 8 concurrent writers, CAS + jittered retry
  -> posts landed: 8 of 8
  -> post-1.md … post-8.md
  -> commits in the append chain: 9

# fact 6: per-worktree private state
  -> main:       .git/hv-board-cursor
  -> the-tilth:  /…/.git/worktrees/the-tilth/hv-board-cursor
```

Test plan for `tools/board/` (`cargo test --manifest-path
tools/board/Cargo.toml`, the `tools/digest/` pattern):

1. **Concurrency** — N writers append; assert all N posts present and the chain
   length is N+1. This is the port of the verified bash probe.
2. **Retry exhaustion is loud** — inject a permanently-contended ref; assert a
   non-zero exit with the physical reason.
3. **TTL decay** — a claim past `ttl_s` renders as expired, not current.
4. **Liveness** — a claim whose `pid` is dead renders as expired even inside
   `ttl_s`.
5. **Reaper** — posts keyed to a deleted or merged branch are dropped;
   compaction leaves a single-commit tree with the survivors byte-identical.
6. **Malformed-line resilience** — a corrupt register line warns and is skipped;
   the render still succeeds and exits zero.
7. **Relevance filter** — a notice whose `paths` miss the reader's changed paths
   is not rendered; one that hits, is.
8. **Cursor** — a second read renders nothing new; the cursor is per-worktree.
9. **Render goldens** — with `GIT_COMMITTER_DATE` pinned, the render is
   byte-stable.
10. **Cap** — with 100 posts live, the render obeys the ≤ 15-line budget and
    reports the elided count.

## 8. Definition of Done

- `tools/board/` implemented, with the ten tests above green.
- The three read seams wired: `SessionStart` hook, `doctor.sh`'s `== Live
  state`, `preflight-merge.sh`'s `hold-off` surface.
- `make board` for the on-demand full read; `make help` lists it.
- A session-facing instruction on *when to post* (the write side is judgment,
  per assumption 4) — the natural home is CLAUDE.md's Process section plus the
  `dispatching-hornvale-subagents` skill.
- Chronicle entry (`book/src/chronicle/`) and retrospective
  (`docs/retrospectives/the-cairn.md`), per decision 0020.
- Decision record for the substrate choice (next free number: **0114**).
- Book freshness sweep; Confidence Gradient re-score if any bet moved.
- Idea-registry rows for the deferred halves (§5, D8).
