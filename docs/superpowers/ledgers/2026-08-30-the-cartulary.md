# The Cartulary — decision ledger

Campaign: `campaign/the-cartulary` · Spec:
`docs/superpowers/specs/2026-08-30-the-cartulary-design.md` · Plan:
`docs/superpowers/plans/2026-08-30-the-cartulary.md` · Decision block:
0486–0495.

This is the durable ledger this campaign exists to create (spec §3, §4a).
From Task 2 onward, every ruling is written here as it happens, not to
`.superpowers/sdd/2026-08-30-the-cartulary/progress.md` (which remains the
plugin's own scratch file for task state and resume-after-compaction
material — see entry #4 below).

## Seeded from pre-existing rulings (spec §1, §4a; pre-flight scan)

These were decided during spec authoring and plan-writing, before this
ledger existed to record them contemporaneously. Recorded here now, at
Task 1, so the campaign's own ledger does not start empty of the decisions
that justify its own existence.

### #1 [G1] — why does this campaign exist at all?

**Question:** Is the five-times-failed "promote scratch material into the
retrospective at close" remedy worth patching again, or does the ledger
need to move to a durable location entirely?

**Decision:** Stop relying on a manual copy at close. Make the campaign
decision ledger a committed, per-campaign document from the start.

**Why (precedent cited):** Five recorded losses of exactly this material:

| campaign | what happened |
|---|---|
| The Ell | promoted nine items, lost six |
| The Quoin | "the material was written down, the instruction to promote it was written down beside it, and it was still lost" (`docs/retrospectives/the-quoin.md:219`) |
| The Gallery / Lodestar | wrote a 193-line retrospective, skipped the verification; belief and `grep` disagreed on eleven counts (`docs/retrospectives/the-lodestar.md:92`) |
| The Overture | worktree recycled before close; nine decision records reconstructed from module doc comments |
| The Attestation | worktree recycled between the merge landing and the close walk; nine deferred minors and two parked findings lost, reconstructed from a session transcript |

The Quoin's reading governs this campaign's design: the second loss matters
more than the first, because one loss reads as a careless close and two do
not — the check is not a backstop for sloppiness, it catches the ordinary
case. The two most recent failures (The Quoin, The Lodestar) both already
tried the obvious fix — verification that reads the ledger and confirms
each item landed — and it did not hold, because the ledger was gone before
anything could verify against it. A check that requires an artifact to
exist cannot detect that artifact ceasing to exist.

**Alternatives discarded:** Adding more verification around the existing
promote-at-close step — rejected, for the reason above (verification cannot
address an artifact vanishing before the check runs). Mirroring scratch
material to a durable location at intervals — rejected as reintroducing the
manual-copy step this campaign exists to remove.

**Capture actions:** `docs/superpowers/ledgers/` tree created (this task);
`docs/superpowers/ledgers/README.md` states what's durable and why.

### #2 [G2] — what becomes durable, and what deliberately does not?

**Decision:** Only the ledger (rulings, deferred minors, parked findings)
moves to a committed, per-campaign path. Implementer reports, reviews and
review packages stay in `.superpowers/` scratch and continue to die with
the worktree.

**Why:** Measured 2026-08-30 across three live campaign worktrees:

```
                regenerable (.diff)     durable prose
  the-chattel        3,412 K            208 K  (10 files)
  the-stile            964 K            204 K  (21 files)
  the-winze             16 K            196 K  (15 files)
```

Durable prose is remarkably stable at ~200 K per campaign; the bulk of
scratch is review packages, which vary by two orders of magnitude. A review
package is `git diff` output, regenerable by construction from two SHAs. An
implementer report is evidence *for* a ruling the ledger already records,
not the ruling itself. The ledger is the only artifact in that scratch
directory that is nobody else's derivative — lose a report and you lose the
working, lose the ledger and you lose the decision.

**Accepted cost:** A lost report (as happened in The Overture) costs a
campaign's effort to reconstruct; a lost ledger costs the project's memory.
The asymmetry between those two costs is the entire justification for the
split, and it is deliberate rather than a budget constraint.

**Capture actions:** `README.md`'s "what does NOT belong here" section
states this explicitly, with the measured numbers, so a later reader does
not "helpfully" widen the tree to also hold reports or reviews.

### #3 [G2] — where does the ledger live, and per-file or shared?

**Decision:** `docs/superpowers/ledgers/YYYY-MM-DD-<slug>.md` — one file
per campaign, matching `specs/` and `plans/`'s naming convention and
sibling tree.

**Why:** Per-campaign paths are what make committing safe, and this is the
load-bearing point. `CLAUDE.md` warns that a committed ledger "silently
clobbers every parallel session's on absorption, raising no conflict."
That hazard is real and is a property of the *shared filename*:
`.superpowers/sdd/progress.md` is written identically by every campaign, so
two campaigns editing "the" ledger merge to one side silently. A path keyed
by campaign slug is touched by exactly one campaign, ever — an absorption
sees a clean add, and there is no collision to have.

**Alternatives discarded:** A single shared ledger file for all campaigns —
rejected, as it reintroduces exactly the collision `CLAUDE.md` already
warns about. Committing only at campaign close — rejected, because both
worktree-recycling losses (The Overture, The Attestation) happened before
or during the close walk; a ledger that first reaches git at close is
exposed for the entire campaign up to that point, which is exactly the
window in which The Attestation lost its material.

**Capture actions:** this file's path and name; the controller commits at
each task boundary and each ruling from here on (small, docs-only commits
that skip `gate-commit`, roughly 8–12 per campaign).

### #4 [G2] — two ledgers, two owners (found during plan-writing)

**Decision:** The vendored plugin's
`.superpowers/sdd/2026-08-30-the-cartulary/progress.md` (task state, fix
rounds, deferred minors, parked findings, resume-after-compaction material)
is **not** touched and **not** mirrored. Instead, the durable kinds
(rulings, deferred minors, parked findings) are written directly to this
committed ledger, by the controller, from the start — via the in-repo
skills this repository *can* edit (`campaign-autopilot`,
`closing-a-campaign`, `dispatching-hornvale-subagents`).

**Why:** Plan-writing established something the spec did not yet know:
there are two ledgers with different owners.

```
  decision-ledger.md   rulings, Q entries, ideonomy passes
                       defined by .claude/skills/campaign-autopilot/ -- IN THIS REPO

  progress.md          task state, fix rounds, deferred minors, parked findings
                       defined by the VENDORED superpowers plugin at
                       ~/.claude/plugins/cache/.../superpowers/6.3.0/ -- NOT in this repo
```

A campaign cannot change where the plugin writes, and a local edit to a
versioned plugin path would be overwritten by the next plugin update. The
material The Attestation actually lost — deferred minors, parked findings —
lived in exactly that plugin-owned file. Mirroring from scratch to durable
at intervals (considered and rejected already under #1) would reintroduce
the manual-copy step this campaign exists to remove. Writing the durable
kinds directly to the committed ledger from the start avoids both problems
at once: nothing is mirrored, nothing is copied, and the plugin's
`progress.md` keeps its narrower job — which does not need to survive
worktree recycling, because a recycled worktree means the campaign is
over, and task-completion lines are recoverable from `git log` regardless.

**Capture actions:** spec §4a records the finding; this ledger inherits it;
a later task (not this one — see Ruling A below and spec §4a's own note
that "T4 re-points `campaign-autopilot`'s ledger location at this path")
updates the in-repo skills to point at `docs/superpowers/ledgers/` instead
of `.superpowers/sdd/decision-ledger.md`.

## Pre-flight scan rulings (carried from `.superpowers/sdd/2026-08-30-the-cartulary/progress.md`)

### Ruling A — resolved conflict: T1 → T3 over `docs/superpowers/ledgers/`

**Finding:** Task 1 (this task) creates a `README.md` in the same directory
as campaign ledgers. Task 3's non-empty-ledger freshness check (spec §5)
must not mistake the README for a campaign's ledger, and must correctly
treat `the-cartulary` itself as *not* exempt from that check, since this
task gives it a real ledger.

**Ruling:** Task 3 must resolve a campaign's ledger **by name from the
campaign slug** — `docs/superpowers/ledgers/<slug>.md` — and must never
list the directory and treat every file in it as a ledger. `README.md` is
then invisible to the check by construction, with no exclusion rule to
maintain, and a stray file dropped into the tree later cannot confuse it
either.

**Cost if wrong:** If a campaign's ledger is ever named something other
than its slug, the check reports it missing when it in fact exists — a
false positive. That is the loud failure direction and therefore the
right way for this check to be wrong, versus a false negative that stays
silent.

**Status:** binding on Task 3, not yet verified against Task 3's
implementation (that task has not run yet as of this entry).

## Task 1 execution note

`docs/superpowers/ledgers/` created with this file and `README.md` as its
first two members. Verified not `.gitignore`d
(`git check-ignore -v docs/superpowers/ledgers/2026-08-30-the-cartulary.md`
→ exit 1, no output — `.gitignore:24`'s `.superpowers/` rule does not reach
a path with no leading dot) and not refused by `scripts/hooks/pre-commit`'s
`.superpowers/` guard (regex `(^|/)\.superpowers/`, which this path does
not match).

---

## Task 1 — complete (`aaa6f6170..b11b2dfd9`, review clean, no fix rounds)

**This entry is the first one written to the committed ledger rather than to
scratch.** Everything above it was seeded from the spec and the pre-flight
scan; everything below it is the campaign using its own deliverable. The
bootstrap gap — rulings made before the durable home existed — closes here.

Review verdict: spec ✅, quality approved, two Minors, no Critical or
Important. The reviewer re-ran `git check-ignore` on both new files itself
(exit 1, not ignored) rather than accepting the implementer's report, and
cross-checked all four seeded entries against spec §1/§2/§3/§4a and the
pre-flight scan — verbatim-faithful, including the measurement table
digit-for-digit.

### Deferred minors

1. **The four seeded entries omit `campaign-autopilot`'s
   `ideonomy passes / overturns` field.** Defensible — they are backfilled
   decisions that predate the ledger's existence and had no ideonomy pass to
   record — but the README does not say that anywhere, so a reader comparing
   the seeded entries against the documented format finds an unexplained gap.
   **Carried into Task 4**, which re-points `campaign-autopilot`'s ledger
   specification and is the natural place to state how a backfilled entry
   differs from a live one.
2. `docs/timings.md` moved alongside the two created files — the expected
   `make gate-commit` byproduct, matching repo convention. Not a defect.

### What the review confirmed that I most wanted confirmed

The README's "what does NOT belong" section states **mechanism and reason**,
not just a rule, carries the measured numbers inline, and closes with an
explicit appeal not to widen the tree. The reviewer judged it likely to
survive a cold read a year out. That was the task's durable half and the
thing most able to fail quietly — a boundary that is merely asserted gets
widened by the first campaign that finds it inconvenient.
