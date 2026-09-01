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
`.superpowers/sdd/decision-ledger.md` is written identically by every
campaign, so two campaigns editing "the" ledger merge to one side silently.
A path keyed by campaign slug is touched by exactly one campaign, ever — an
absorption sees a clean add, and there is no collision to have.

**Correction, final review, 2026-08-30 (finding I2):** this entry originally
named the shared file as `.superpowers/sdd/progress.md`. That was wrong —
`progress.md` has always lived at the per-campaign-keyed
`.superpowers/sdd/<slug>/progress.md` and never carried the hazard this
ruling describes. `decision-ledger.md` (flat, unkeyed, at
`.superpowers/sdd/decision-ledger.md`) is the file the argument actually
needs, and the paragraph above is corrected in place rather than left wrong
beside a footnote, because the wrong name was never load-bearing to the
*decision* — only to which file the reader should picture — and leaving it
wrong in the primary sentence while correcting it only in a footnote is how
a stale fact survives a correction. See decision 0493, which supersedes
0487's own copy of this same mistake.

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

---

## Task 2 — complete (`0281b01fa..352b8a51c`, review clean, no fix rounds)

Superseded the two statements Task 1 made false. Review: spec ✅, quality
approved, one Minor.

**The distinction held in both directions**, which was the substance of the
task and the thing most able to go wrong quietly:

- the **hook's** claim was FALSIFIED — it rested on "promotion at close
  works", and five recorded failures say otherwise;
- **`CLAUDE.md`'s** claim was NARROWED — the absorption clobber is a real
  property of the shared filename, stays true of `.superpowers/sdd/`, and
  stops applying to a per-campaign path.

Presenting either as the other would have been a defect, in opposite
directions. Both edits quote the superseded sentence verbatim before replacing
it, so a cold reader learns that a claim was made and what measured it wrong.

**The guard still refuses, reproduced independently.** The reviewer checked
`core.hooksPath` = `scripts/hooks` first, staged a scratch file with
`git add -f`, and got the refusal at exit 1. That mattered: a superseded
rationale that had quietly become a weakened guard would read as a pure
documentation diff and pass any review that only read the prose.

### Deferred minor

**The hook's own citation undercounts its evidence.** It reads "failed FIVE
recorded times (spec §1 — `docs/retrospectives/{the-quoin,the-lodestar}.md`
plus two more)", naming or implying four sources for a five-row table. The
reviewer verified all five have retrospective files on disk: `the-ell`,
`the-quoin`, `the-lodestar`, `the-overture`, `the-attestation`. The primary
citation (spec §1) is correct and sufficient, so nothing substantive is
wrong — but the paragraph's entire purpose is exactness-by-measurement, which
makes an imprecise citation inside it worse than it would be anywhere else.
*Ruling:* deferred, not a fix round — the fix is naming the five, and Task 6
writes the chronicle and decisions where the same five are cited anyway.
*Cost if wrong:* a reader chases four sources for a five-source claim and
wonders which one is missing.

### Two implementer judgment calls, both upheld

It declined to cite decision numbers 0486/0487 — which do not exist until
Task 6 — and cited the campaign by name and spec path instead. Dangling
references to unwritten decisions are exactly the rot this project keeps
finding. And it was careful to write that the *committed* ledger no longer
needs `.superpowers/`, rather than that the scratch file is retired, because
Task 4 has not re-pointed the skills yet. A statement true only after a later
task is false now.

---

## Task 3 — the ratchet, and a demonstrated hole in it

Review: spec ✅, quality approved, one Important (framing) + one Minor. Both
Step 5 mutations reproduced verbatim by the reviewer; the exemption list
independently regenerated in Python and diffed against the committed fixture
(exact match both directions); `the-cartulary` confirmed absent from it, its
own 282-line ledger present — dogfooding intact.

**Ruling: build the unmatched-plan count ratchet.**

The reviewer demonstrated the hole rather than reasoning about it. A future
campaign whose spec and plan names defeat the exact-slug matcher — the
`the-deed-design`/`the-deed-state` shape the code's own comment cites — is
invisible to BOTH tests: never flagged missing, never exempted, simply unseen,
permanently. It created exactly that pair and the check passed clean.

The hole is real. What I got wrong is where the reassurance lived: the code's
doc comment already states this blindness in the harsh, accurate form. It was
the *report's* summary ("safe for the ratchet's correctness") that read as
"nothing to worry about" — a narrower claim, true only of the exemption list's
internal consistency.

So this is not a documentation failure. It is a stated blindness that can be
turned into a visible red cheaply, and this campaign's whole thesis is that an
absence needs a row. A campaign the check never looks at is an absence with no
row, inside the instrument built to remove them.

*The fix:* freeze the unmatched-plan count (54) as its own ratchet, the same
frozen-count idiom The Attestation used for decision-block declarations and
that `tropes check` and the timings baseline use. A new unmatched campaign
moves the count and reddens the day it happens.
*Cost if wrong:* one fix round, and a frozen number that must be re-derived
whenever the corpus legitimately grows — the same maintenance every ratchet in
this repo carries.

### Minor (deferred)

The report's narrative says all 54 unmatched plans are "date drift / stage
words / umbrella specs". The reviewer found ~13 with no companion spec under
any name — correctly outside the population rather than matcher misses. The
count is right; the characterisation is looser than the evidence.

### Task 3 — complete (`64b28a2a6..c8dae50cb`, review clean after 1 fix round)

The ratchet landed and the demonstrated hole now has a row. Two things the
re-review settled better than I had:

**`==` is necessary, not merely defensible.** I asked whether equality would
redden on legitimate corpus growth and get deleted the first time it fired
inconveniently. The reviewer checked by hand: `>=` would have let its OWN
probe pass (55 >= 54), making the ratchet a no-op against the exact defect it
exists to catch. Unlike the decision-block count it was modelled on — where
growth is inherently safe because reservations are append-never — this
population mixes legitimate spec-less growth with real matcher misses, so a
rise cannot be waved through by rule.

**It makes the hole visible; it does not close it, and it says so.** I asked
whether a moved count actually requires a ledger or merely reports a number
changed. It is the latter, and the fix's own doc comment states it: *"This
test does not close the hole ... it only makes the count that hole hides in
impossible to move quietly."* The finding was about silence — "never flagged,
never exempted, simply unseen, permanently" — and after the fix a human is
forced to look, so shipping without a ledger becomes a visible reviewable diff
rather than an absence with no row. Guarantee stated accurately; no overclaim.

Red reproduced by the reviewer with a *different* shape than the implementer
used — a one-day date drift rather than a stage word — so the evidence does
not rest on one self-selected case.

---

## Task 4 — the skills re-pointed, and one instruction left behind

Review: spec ✅, quality approved, one Important + one Minor. The reviewer
re-ran the stale-claim grep itself and reproduced every per-location count
exactly, spot-checked one hit from each correct-as-history bucket (a frozen
spec, two plans, a retrospective, `CLAUDE.md`'s own superseding paragraph) and
found no stale claim misfiled as history, and read all three skills as an
executor would rather than grepping for the word "mirror" — confirming no copy
step was smuggled in.

**Ruling (the Important, entering fix round 1): `closing-a-campaign`'s step 2
routes a bullet its own discovery mechanism can no longer find.** The step
still says to route "ledger entries made after the G3 stop", and its shell
commands still `ls`/`grep` over `.superpowers/sdd/` — where the ledger no
longer is. A closer following it literally sees no ledger and concludes there
is nothing to route.

This is the campaign's own thesis committed by the task that moves the
ledger: a record that outlives its subject produces wrong answers from
good-faith readers, and here the *instruction* outlived the *location* it
assumed. Fixing it, and asking the implementer to sweep all three edited
skills for siblings — any step naming a mechanism (a path, a grep, a command)
rather than a thing to find.
*Cost if wrong:* one fix round on skill prose.

**Ruling (the classification table): all 182 remaining hits stay untouched,
and that is correct rather than incomplete.** 81 in frozen specs, 59 in
plans, 38 in retrospectives, 2 in `CLAUDE.md`, 2 in this task's own new text.
The specs and plans describe the ledger as it stood when they were written —
including this campaign's own spec, which is the "before" its plan exists to
change, and its own plan, which quotes the pre-change instruction verbatim as
Task 4's instruction. Editing them would destroy the evidence the campaign
argues from, and the project's supersede-never-edit convention exists for
exactly this. Only the two `campaign-autopilot` lines were genuinely stale,
and both are fixed.
*Cost if wrong:* a future reader takes a frozen spec's description of the old
arrangement as current. Mitigated by those documents being dated and by the
supersession Task 2 landed in the two live files.

**Harness issue observed, not a campaign defect.** The implementer reported
the Edit tool returning success twice for edits that never reached disk, then
failing to match text it had itself reported as applied — its cached view
diverged from the filesystem. It caught this by verifying `git diff` after
each edit and redid the work through Bash. Nothing wrong shipped. Recorded
here because an agent that trusted the success return would have committed a
change missing edits it believed it had made, and that failure is silent.

### Task 4 — complete (`bdc531723..6443f2874`, review clean after 1 fix round)

The fix split step 2 into **2A** (the scratch sweep — what still dies) and
**2B** (`cat` the committed ledger — what survives but still needs reading),
with the failure mode named before either half. The sibling sweep I asked for
earned itself: it found a **second** instance in the Quick Reference table's
"Scratch promotion" row, which the targeted fix would have left behind.

The re-review **executed step 2 literally rather than reading it for intent** —
the original defect was an instruction that read correctly and found nothing
when followed, so reading the fix for intent would have reproduced the mistake
that made it. Both halves reach their material.

It also re-derived the sibling sweep independently, and in doing so caught its
own first pass being incomplete: an anchored `grep '^```'` missed indented
fences, so it widened the search and re-ran. Same discipline, applied one
level up.

Confirmed: only the Quick Reference row was a sibling.
`campaign-autopilot`'s `.superpowers/sdd/followups.md` mention is a
genuinely-still-scratch item, not an instance of this bug.

### Task 5 — H1/H2 measurement (`d6421dd60..`, this campaign's own falsifiable claim)

**H1 — the ledger survives recycling. CONFIRMED.**

Simulated the loss The Attestation suffered (worktree recycled between
landing and close: branch survived, scratch did not) without touching the
live worktree, per the task's safety rule. Method: a fresh scratch clone of
`campaign/the-cartulary` at HEAD (`d6421dd60`) into a throwaway directory
outside this repo, then a `rm -rf` of that clone afterward — a clone has no
`.superpowers/sdd/` history to begin with, which *is* the simulation, since
that directory is git-ignored and per-worktree by construction (confirmed via
`git check-ignore -v .superpowers/sdd` → `.gitignore:24:.superpowers/`).

```
git clone --branch campaign/the-cartulary --single-branch \
  /Users/nathan/Projects/hornvale/hornvale <scratch>/the-cartulary-h1-sim
# .superpowers/ absent in the clone (ls: No such file or directory)
# docs/superpowers/ledgers/2026-08-30-the-cartulary.md present, 424 lines
diff <scratch>/.../2026-08-30-the-cartulary.md \
     <(git show HEAD:docs/superpowers/ledgers/2026-08-30-the-cartulary.md)
# → IDENTICAL, no output
```

The ledger was present, complete (424 lines, byte-identical to `git show
HEAD:...` in the live worktree), and readable as an ordinary file with no
`.superpowers/` in sight — exactly the artifact The Attestation's nine
deferred minors and two parked findings needed and didn't have. **The
campaign's premise holds; Task 6 may proceed.**

**H2 — committing does not change what gets written. UNMEASURABLE FROM
INSIDE, as preregistered (spec §8).**

Preregistered prediction was that committing *does* slightly change candour —
the entry most worth keeping ("I do not know why this works") is the one most
likely to be softened once it's headed for a permanent, reviewed record — and
that this would not be measurable from inside the campaign that makes the
change. That prediction stands unfalsified and unconfirmed: I have no control
condition. Measuring it would require the same campaign run twice, once
writing to committed `docs/superpowers/ledgers/` and once to the old
git-ignored `.superpowers/sdd/decision-ledger.md` [corrected 2026-08-30,
final review finding I2 — this originally named `.superpowers/sdd/
progress.md`, the wrong file; `progress.md` holds task state and was never
where rulings lived, `decision-ledger.md` was; see decision 0493], with the
same decisions arising and independent judges (blinded to which arm
produced which text) comparing candour. That is not an instrument this task
can build or run — it would
need a second full campaign execution as a control arm, decided in advance
of Task 1, which this campaign did not do. No such control exists, so no
comparison is possible after the fact. I did not construct a proxy (e.g.
comparing this ledger's tone against an unrelated campaign's retrospective)
because a proxy answering a different question and reporting it as H2 is the
exact failure spec §8 warns against ("a campaign that claims to have
confirmed H2 should be disbelieved unless it names its instrument").

**Verdict: STOP-condition not triggered.** H1 confirmed, campaign has
purpose. Task 6 may run.

### Task 5 — review fix: the omitted line, a cheap H2 instrument, and a corrected H2 status

**Reviewer finding, accepted in full.** The Task 5 scratch report
(`.superpowers/sdd/2026-08-30-the-cartulary/task-5-report.md`, git-ignored)
carried a self-critical line that did not make it into the ledger entry
above:

> H2's honest "unmeasurable" answer is only as trustworthy as the
> preregistration itself — since the prediction and the "expect
> unmeasurable" clause were both written into the spec by the same author,
> one could argue the unfalsifiability was baked in rather than discovered.

The mechanical H1/H2 content carried through from scratch report to
committed ledger nearly verbatim; this line, specifically, did not. My
stated reason at the time — that spec §9 item 2 already covers it — does not
hold: §9 item 2 is the general prediction that committing changes candour
("the entry most worth keeping... is the one most likely to be softened");
this line is a narrower, sharper claim about H2's own *construction* (a
prediction paired with an admission that it wasn't expected to be tested is
unfalsifiable by design, independent of whether committing changes anything).
Writing the sharp form here, in the permanent record, is the fix for a
candid line vanishing from it — see below for what this is and is not
evidence of.

**A cheap H2 instrument existed and was applied, post hoc, to exactly this
occurrence.** The scratch report and this ledger's H1/H2 entry are a matched
pair: same task, same author, minutes apart, one written to the git-ignored
*old regime* (`.superpowers/sdd/`) and one to the committed *new regime*
(`docs/superpowers/ledgers/`). Comparing them is not the twin-campaign,
blinded-judge study §8 calls for — it costs nothing beyond a `diff`-by-eye
of two files already on disk, on a sample of one task from one campaign, with
no blinding and no control for authorial fatigue, ordering effects, or plain
inconsistency across two writing passes.

**The result:** the mechanical content (commands, output, H1 verdict, H2
verdict) carried through unchanged; the one line most resembling "I'm not
sure this holds up" did not. That is **one confounded data point consistent
with H2's prediction that committing softens candour** — not a confirmation.
The confound stated plainly: an author naturally tightens a report's tone on
a second pass regardless of where it's headed, and dropping a line during
condensation is ordinary editing, not necessarily self-censorship aimed at a
permanent record. Distinguishing "committed record → softened" from
"second pass → tightened, and this record happens to be the second pass"
needs exactly the blinded, controlled comparison this one post-hoc pair does
not have. **Read this as: an instrument existed, it was applied once, it
produced one confounded observation pointing the direction §8 predicted —
not as H2 confirmed.**

**H2's status, corrected.** H2 itself is falsifiable in principle — a
preregistered, blinded twin-campaign study (same decisions, one arm
committing to a permanent ledger, one arm writing to ephemeral scratch,
independent judges scoring candour without knowing which arm produced which
text) could settle it. What this campaign's spec actually rendered
unfalsifiable was its own *operationalization* of H2: §8 preregistered "I
expect not to be able to measure it," and no instrument was built to try —
which guarantees a report of "unmeasurable" regardless of whether H2 is true
or false. That is a methodological error in the spec's design of the
measurement, not a limitation of this task's execution of it, and it belongs
in the retrospective as a lesson (preregister the instrument, not just the
expectation of failing to find one) rather than filed under "structural, not
a defect" as the earlier entry did.

### Task 5 — complete (`d6421dd60..cc8e97f12`, review clean after 1 fix round)

**H1 CONFIRMED, on stronger ground than the task claimed.** The reviewer read
`scripts/worktree-take.sh`'s actual mechanism and found the clone is not a
weaker analogy for recycling but a harder test: real recycling only fires on
already-merged branches and switches branch within a shared object store,
while the simulation fully disconnected from the repo and tested an unmerged
branch.

**H2's operationalization was unfalsifiable, and that is my error in the
spec.** The hypothesis is falsifiable in principle — a preregistered blinded
twin-campaign study could settle it. What guaranteed an "unmeasurable" report
regardless of truth was preregistering "I expect not to measure this" and then
building no instrument at all.

**A cheap instrument existed and went unused**, sitting in the implementer's
own working directory: this campaign's scratch reports (ephemeral, the old
regime) against its matching committed ledger entries (permanent, the new
regime) — same task, same author, minutes apart.

**Applied post hoc to Task 5 itself, it found one confounded observation
consistent with H2.** The report's most self-critical line survived in scratch
and was absent from the committed ledger, while mechanical content carried
through verbatim. Not a confirmation: an unblinded single post-hoc pair cannot
distinguish "committed, therefore softened" from "second pass, therefore
tightened — and the second pass happens to be the committed one."

**The fix's own trap was checked and did not spring.** A remedy for "the
candid line was softened out" that landed a softened version of that line
would have been the same defect committed by its own cure, and would have read
as complete. The reviewer put the two wordings side by side: verbatim,
character-for-character, with only the incorrect framing dropped.

*The recursion is the campaign's finding.* It built a durable ledger so candid
material would stop being lost, and then lost a candid line from that ledger,
in the task that tested whether the ledger works — and caught it.

---

## Task 6 — the close, and the third instance

This entry is late, and its lateness was itself a finding (final review M1):
the ledger stopped at Task 5 and skipped its own final task boundary — the
exemplar breaking the every-boundary cadence it exists to demonstrate. Written
now by the controller, which is whose write it was.

Decisions **0486–0492** minted, plus **0493** superseding 0487. Chronicle,
retrospective, registry row, freshness sweep. Drift check exit 0, nothing under
`domesday/`, `gallery/` or an almanac moved.

### The third instance — an ABSENCE, which is why nothing else caught it

Final review I1. Task 4's rewrite split `closing-a-campaign`'s step 2 into two
halves and, in doing so, **deleted the "Deferred minors → a home" bullet with
no replacement.** Half 2A routes minors that never reached the ledger; half 2B
routes post-G3 entries; a *ledgered deferred minor* falls between them with
nowhere to go. The campaign then closed without routing its own three.

The two earlier instances were both **stale pointers** — a comment naming a
moved path, a line missing from a file. A diff shows those. This one is an
instruction that stopped existing, which no per-task review was positioned to
see, and which only a whole-branch read found.

The reviewer's sentence is the one worth keeping: *nothing material was lost
this time — which is exactly the state the five prior campaigns were in before
the loss they are now remembered for.*

### I2 — the load-bearing argument named the wrong file, and that is mine

The claim that per-campaign paths make committing safe rests on identifying
which file is shared. I named `progress.md` in spec §3, spec §4, the plan, and
two ledger entries. Measured across 13 worktrees:

```
  progress.md         .superpowers/sdd/<campaign-slug>/progress.md   campaign-KEYED, always
  decision-ledger.md  .superpowers/sdd/decision-ledger.md            flat, SHARED, every worktree
```

`progress.md` has always been per-campaign and never carried the hazard.
`decision-ledger.md` is the shared one. **The conclusion survives — the
collision is a property of a shared filename and per-campaign paths dissolve
it — but the exemplar was wrong**, and the error compounded into 0487
contradicting 0490. Corrected in all five places; 0493 supersedes 0487 rather
than editing it, because decisions are append-only.

*Cost of the error:* a committed decision record asserted a false fact about
which file is still written, and would have been cited by the next campaign
that touched this area.

### I3 — an asserted rule nothing enforced

"A new campaign cannot add itself to the exemption list" was stated and not
enforced; the reviewer mutation-proved a fresh spec + plan + self-exemption
passing all three checks green. The fix idiom was one function away in the same
commit. Now ratcheted, red-then-green proven.

---

## Final fix wave — adjudicated, not re-fixed

All eight in-scope findings ADDRESSED. The wave introduced a **fourth
instance** and three lesser defects. Per process there is no second fix wave;
these are adjudicated here and surfaced to Nathan rather than swung at again.

**The reason that is the right call, and it is a finding in itself: every fix
wave in this campaign has introduced a new instance of the campaign's own
defect.** Task 4's fix created I1's dropped bullet. The wave fixing I1 created
the stability overclaim. A fifth swing would most likely create a fifth. That
is a convergence failure, and it is exactly what the no-second-wave rule
anticipates.

### The fourth instance (load-bearing — recommended for a fix)

The wave corrected "remarkably stable ~200 K" (falsified at n=3) and wrote
**"a consistent 15–19 K regardless of which sample is read"** six lines away,
about the object that same commit was enlarging. The source
(`task-6-report.md:81`) says *mean ~15–19 K, range 12–32 K* — the restatement
dropped the range and promoted a mean to a value. There is no n=13 ledger
measurement; that pass measured scratch prose. And the only committed ledger
in existence was already outside the band:

```
  010242e67 exemplar ledger:  31,647 bytes
  b1400b4f5 exemplar ledger:  35,617 bytes   (this commit)
```

*Why load-bearing:* it is a false factual claim in the README that defines
this tree's contract, about this tree's only inhabitant, in the campaign that
exists to make such records trustworthy. A future campaign reads it.

> **Correction, pre-merge (see "The fifth instance" below): this entry names
> the wrong file for the quoted wording.** "a consistent 15–19 K regardless
> of which sample is read" is the CHRONICLE's sentence,
> `book/src/chronicle/the-cartulary.md`. The README carried a softer variant
> of the same error. Both came from the same fix-wave commit, `75fa38e1c`,
> and both are corrected.

### Parked, with rulings

**The assert message names a cause that cannot fire.** It says a rise means a
"fabricated/duplicated entry"; `ledger_exempt_campaigns()` returns a
`BTreeSet`, so a duplicate line cannot move the count — proven, the mutation
stayed PASS. *Ruling: real, parked.* It misdirects a reader debugging a red,
but the check itself is correct and the red still means what it should.

**The ceiling's `<=` lets a swap through, unstated.** One exempt campaign
gains a ledger and drops out, a new one takes its slot, length stays 239, all
five tests green. *Ruling: real, parked.* Under 0491 that direction should be
named in `# What it cannot see`; it is a stated-blindness gap rather than a
coverage gap, and the swap requires two simultaneous changes.

**M2's carve-out narrowed and dropped a category.** The README rewrite went
from "pre-flight cross-task rulings and deferred/parked findings" to "deferred
minors and parked findings", leaving the exemplar's own `### Ruling A`
conforming to neither documented shape. *Ruling: real, parked* — and noted as
I1's shape at smaller scale, which is the third time a narrowing edit in this
campaign has dropped a category on the way past.

**Chronicle:184 says both findings "are corrected below"; nothing below
describes the I3 correction.** *Ruling: cosmetic, parked.*

**M4 was never in scope and remains open:** nothing documents that the ratchet
binds before a campaign's first ruling.

## Pre-merge absorb — the ratchet reddened on its own author

Absorbing `origin/main` (333 commits) before submitting to the merge queue
turned `every_campaign_with_a_spec_and_a_plan_has_a_ledger` RED. Five
campaigns landed on main after this branch's base and carry no ledger:
`2026-08-19-the-winze`, `2026-08-28-the-chattel`, `2026-08-28-the-legend`,
`2026-08-30-the-company`, `2026-08-30-the-repertory`.

`gate-commit` was GREEN across the same merge product — this test is not in
the sub-floor roster, so nothing local would have caught it. It would have
reddened in the chamber.

**Q — is this the self-exemption the ceiling exists to catch, or is the list
simply stale?** *Stale, and checkably so.* The list was frozen against THIS
BRANCH'S BASE, not main's tip. Each of the five has its spec and its plan
absent from `git merge-base HEAD origin/main` and present on `origin/main`
— so each was already merged before the absorb, and no live session could
have added its own slug. Each predates the convention in the only sense that
binds: the convention does not exist on main until this branch lands.

*Decision:* add the five, raise `EXPECTED_LEDGER_EXEMPT_CEILING` 239 → 244,
and restate the invariant as a **crossover window** that closes when this
branch lands — after that a rise means what the original comment said it
meant. Precedent is the ceiling's own assert message, which already provides
for "a legitimate need to widen the ceiling after a human checks by hand that
the added slug truly predates the ledger convention"; that check is the
merge-base evidence above.

*Alternatives discarded.* (a) Backfill ledgers for the five — impossible and
dishonest: their scratch is gone, which is this campaign's own premise, and a
fabricated ledger is worse than an absent one. (b) Replace the list with a
slug-date floor, self-maintaining — rejected on a concrete counterexample:
slug dates are authored rather than merge dates, and `the-company` and
`the-repertory` carry the same date as The Cartulary itself, so no cutoff
separates them. (c) Redesign the exemption mechanism — a fifth fix wave, in
the campaign whose defining finding is that every fix wave produced a new
instance of its own defect.

*Ideonomy: 1 pass, 0 overturns, 1 enrichment* — inversion ("shrink the
population instead of growing the list") produced (b) and its refutation;
implication-mining produced the crossover-window framing, which is a strictly
better statement of the invariant than "append-never" and is what got
written.

*Positive control.* The ceiling was mutated 244 → 243 and the test went RED
naming both numbers; restored. The assert reads the live fixture length, so
the raise is a real re-pin rather than a constant nobody consults.

*Blast radius.* Two further sites restated the old claim and were corrected
in the same commit: `ledger_exempt_campaigns`'s doc ("never added") and the
check's own `# Direction this check enforces` section ("the 239").

**One parked residual is touched, and this says so.** The rewritten ceiling
doc does not restate "or a legitimately exempt slug was duplicated" — the
cause proven above to be unable to fire. The assert MESSAGE at the bottom of
`the_ledger_exemption_list_only_shrinks` still carries it and is left parked
as ruled. So the two now differ in what they name, deliberately: re-adding a
known-false cause to a doc comment in the campaign built to make records
trustworthy was not a defensible way to stay consistent.


## The fifth instance — the record named the wrong file, and the stronger copy was in the book

Fixing the fourth instance found a fifth, and it was found by grepping the
CLAIM rather than by reading the record that reported it.

The entry above, the chronicle's own account, and the handoff into this
session all located the overclaim in `docs/superpowers/ledgers/README.md`.
It is in two files. `git log -S'consistent 15–19 K regardless'` returns one
commit, `75fa38e1c` — the whole-branch fix wave — and its diff writes the
strong wording into the CHRONICLE and a softer variant into the README, six
lines apart in the same commit:

```
-puts the committed ledger's own share at roughly 15–19 K of that ~200 K —
+is a consistent 15–19 K regardless of which sample is read, meaning the   # chronicle
+are all on the regenerable side. A ledger itself is ~15–19 K on the same  # README
```

So a session acting faithfully on the record would have corrected the weaker
instance in a docs tree and shipped the stronger one in the published book.
The pattern is the campaign's own, at one further remove: not a dropped
category this time but a **dropped site** — a correction that names one
location for a claim written to two.

*What caught it:* `grep -rn '15–19'` across the chronicle, retrospective,
ledger, README and the campaign's decisions, run because a correction has a
blast radius and the record is not a reliable index of it. Re-reading the
entry could not have caught it: the entry is internally consistent and cites
a real file that really carried a real version of the error.

*Both are corrected*, with the range restored, the mean labelled as a mean,
and the tree's only inhabitant (41 K, above the 12–32 K range's ceiling)
named as the evidence that these are a floor rather than a size. Decided by
Nathan at the pre-merge stop, over parking it or deleting the claim.

*Instance count for the campaign's headline finding: five, not four.* The
fifth arrived after the fix wave that was supposed to be the last, which is
the finding rather than an exception to it — and it is the reason the wave
before it was adjudicated instead of re-swung at.
