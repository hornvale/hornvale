# The Terrier — retrospective

Process, not product. The chronicle carries what was built.

## A cost read off a two-point difference is the whole derivation, not one step

The Rack measured a chamber snapshot at 8.4 ms after `look` (the sighting
memo already held an answer) and 16.3–16.8 ms after `map` or a chamber `go`
(it did not, and derived one). The difference, ~8 ms, was written into a
chronicle, a retrospective, and a registry row as "one shadowcast" — the
step the two readings' shared derivation happens to be *named* for, not the
step that actually cost the money. A campaign was then opened on that row.

The lesson generalises past this one row: **subtracting two timings never
isolates a step, only a derivation.** A derivation of five, ten, or twenty
steps produces exactly one number from that subtraction, and whoever writes
it down will reach for whatever noun the derivation is colloquially called —
here, "the shadowcast", because a sighting *is* a shadowcast in every other
sentence this project writes about it. Nothing about the arithmetic
distinguishes "the shadowcast costs 8 ms" from "the thing this derivation
is for costs 8 ms and eleven other steps ride along for free." Both readings
fit the same two numbers equally well, and only one of them is checkable
without more measurement — which is exactly why it went unchecked for five
weeks and travelled through eight sites in six files.

**The check that would have caught it costs less than the mistake did.**
`SIGHT_RADIUS` is 4, so the shadowcast visits at most 81 cells; a plausible
per-cell cost of even a few microseconds bounds the whole operation to
fractions of a millisecond by arithmetic alone, before a single `Instant` is
placed. Ten scratch timing prints — inside `derive_sighting`, `chamber_plan`,
`describe_chamber_here`, `enter`, `brief_here`, and `brief_of` — took perhaps ten minutes
and found the real cost sitting in `brief::brief_of`, which was
reconstructing the world's *entire* occupation register from the ledger on
every call, two to five times a turn. That function's own doc had prescribed
the fix — hoist the map, do not memoize inside it — since the day it was
written, five weeks before this campaign opened.

The rule this earns a registry row for
([[PROC-a-two-point-difference-names-a-step-not-a-cost]] in the frontier's
idea registry): before a cost derived from a difference carries a step's name
into a committed artifact, decompose the derivation; when a campaign is
opened on a row that was itself a measurement, re-measure the split before
designing anything. This campaign's own opening ruling (ledger entry #1) did
exactly that, and the redirection it produced — from "make the shadowcast
cheaper" to "stop rebuilding a whole-world register per call" — is the entire
reason the fix that shipped moved milliseconds instead of microseconds.

## The plan's own text carried three defects

**The evidence clause asked for the wrong red.** Task 1's text said "paste
the red you saw against the pre-hoist tree", and the red an implementer
would see there is the scan's *positive control* — `WorldContext::build`
does not yet name the register, because the hoist has not landed — which
proves the scan can fail, not that it catches a per-call read anywhere else.
The reviewer caught it as plan-mandated rather than a defect in the
implementer's work; the ruling witnessed the actual offender scan by
mutating the finished tree's `brief_here` to reintroduce the call, which the
scan caught at `session.rs:7232`. The pre-hoist red is kept, separately
labelled, because it is a real observation — just not the one the clause
thought it was asking for.

**The `go n`/twelve-step script was a null at both seeds, and it is the
fourth null script at seed 42 in two campaigns.** Task 2's plan prescribed a
walk and a step count from outside the code; neither held. Seed 42's
flagship never leaves its own vertex walking north in twenty-four steps (it
does at step 18 walking east), and seed 7's flagship *starts* on ground with
no occupation record at all (a living one is seven steps west). The plan's
own decision rule — if the script is a null, re-measure the real bearing and
record which, rather than debug the assertion — is what kept this a
ten-minute detour instead of a round trip through review, and it is the
standing lesson that a probe prescribed from outside the code is a
hypothesis, not a fixture, earning a fourth occurrence rather than a new
one.

**A stub-then-full chronicle sequencing note collided with a rule that
protects verified prose.** The plan told Task 4 to write a stub chronicle
and leave the full one for Task 5, but the implementer wrote the complete
chronicle in Task 4 instead — every number and mechanism in it checked
against source and spec by that task's own review. The ruling at review kept
it rather than deleting verified prose to honour a sequencing instruction
that no longer served a purpose: this task (Task 5) *extends* it — the
numbers table, the ratchet's direction, the correction sweep and honest
limits — rather than writing it from nothing. Worth naming because the
alternative (delete-then-rewrite) would have cost real editorial work to
regenerate content already confirmed correct, for no gain in the plan's own
terms.

## The ratchet that under-scanned its own target

The structural scan built in Task 1 to forbid a whole-world occupation read
on any session path (spec §3.3) is the single check this campaign leans on
hardest, and its first draft did not look at roughly 4,100 lines of one file
and about 200 of another. It split "production" from "test" code at the
first `#[cfg(test)]` attribute anywhere in a file — which, in
`windows/vessel/src/liveness.rs`, gates a test-only helper (`fn alarm_at`)
at line 4353 while the file's real test module does not begin until line
8491; everything between, including a production function
(`species_activity`), was silently outside the scan's reach.
`windows/vessel/src/roster.rs` had the identical shape from line 237 to 446.
The controller caught this between fix rounds, not the implementer and not
the first review pass. A guard that reads green over code it never looked at
is exactly the failure class this project has a standing lesson about (*a
check that can never fire is worse than an absent one*), and the fact that
it recurred here, on a check written specifically to close a different
instance of the same class (The Rack's dead `TurnWork` counter), is the
finding worth carrying forward: **a scan's reach has to be witnessed by
running it against a shape designed to defeat it, not assumed from reading
its own logic.** The fix — split at the real test *module* boundary, blank
comment lines instead of deleting them so a cited line number stays the
file's own, and add both a per-file coverage control and a synthetic-shape
regression test — leaves two small, still-live loose ends, in the deferred
minors table below.

**This exact failure class already had a registry row before this campaign
opened one on it.** `TOOL-a-text-scan-guard-splits-on-prose-about-its-needle`
(The Plumb) records the same shape — a guard locating its production/test
boundary by `src.find("#[cfg(test)]")`, landing on a comment that merely
mentions the attribute — in a different `windows/vessel` guard. Two sites in
this crate still carry the broken form and are not fixed here:
`liveness.rs:16531` (`production_reaches_fatigue_through_exactly_one_door`)
and `underground.rs:1256`. The registry row's Where cell now also points at
this campaign's `production_code` (`the_terrier.rs`) as the shape that
closes it, and the two sites are carried forward as a follow-up in the
ledger rather than fixed here — both are pre-existing guards outside this
campaign's own scope.

## The stage gate was held twice by things outside this campaign

Task 2's submission to the stage gate found `outboard`'s `test-sluice-vet.sh`
picking `campaign/the-prospect` — the first campaign branch on `origin` that
mints a decision — as its collision probe, and that branch had minted
decision `0540` inside The Pawl's already-reserved block while `main`
already carried The Pawl's own `0540`. The vet's "negative control" fired a
true collision and read as a harness failure rather than as evidence about
this campaign. Nothing about The Terrier caused it, and nothing about The
Terrier could resolve it — the fix belonged to whichever campaign renumbered
first. Posted to the board and worked around by sequencing (Task 3 read as
prose and did not need the gate; the stage-gate resubmission and Task 3
shipped together once the collision cleared). The register gains a second
finding from it: a negative control whose fixture is a live branch cannot
tell a harness bug from a real collision, and needs a synthetic probe
instead (follow-up, below).

The absorb before close found the second: the stage gate at `8b4f7f490`
reddened on `registry_ids_are_unique` because `main` had moved 45 commits
(The Nettle) and rewritten two `PROC` rows this campaign's own new row sat
beside, and the auto-merge kept both versions of each neighbour. This is
the **fourth recorded instance** of a hand-edited table merging clean and
wrong (The Rack's own retrospective counted three before this one), and it
is worth stating plainly rather than filing quietly a fourth time: **a new
registry row belongs at the end of its table, not beside a row another
campaign may be rewriting concurrently** — appending is the one edit shape a
line-based merge cannot turn into silent duplication.

## Tasks 3 and 4 ran in the opposite order the plan gave them

The plan sequenced Task 3 (the readings) before Task 4 (the corrections and
decision record), and the box's load average was 122 when Task 3 came up —
high enough that a ≤ 3 ms line would have measured nothing but contention.
Task 4 needed no number at all — prose, a decision record, and a
rebaseline — so it ran first while the box was unusable, and Task 3 ran five
hours later once the box read 2.0 and the stage-gate collision above had
cleared, submitted together with the stage-gate resubmission. No cost was
observed from the reorder: both readings that did land were taken quiet (all
three `uptime` averages at or under 2.67), and nothing in Task 4's content
depended on Task 3's numbers existing yet.

## Deferred minors, and what became of each

| carried | outcome |
| --- | --- |
| Task 1 → close: the boundary detector takes the FIRST `#[cfg(test)]`+`mod` sequence, and `liveness.rs` has two adjacent ones (lines 8486 and 8490) | Checked at close: nothing but test-module declarations sits between the two — the earlier sequence gates `mod emitter_scan_tests`, a `#[path]`-included test file, and the five lines between it and the real `mod tests` are a doc comment and attributes. Picking the first costs nothing here; a file where real production code sat between two such sequences would still be a live risk, unaddressed by this campaign. |
| Task 1 → close: the scan's own doc comment cites line 8490 for `liveness.rs`'s `mod tests` | Confirmed still present and still off by one at close: 8490 is the `#[cfg(test)]` attribute; `mod tests` itself is 8491. Harmless to the check's behaviour (the split point is correct either way), cosmetic to a reader who goes looking. Not fixed here — flagged rather than touched, since editing a shipped test's doc comment outside its own task was judged not worth reopening the file for one digit. *(Fixed in the final-review pass, which also corrects a claim this row itself never made explicit: the boundary the scan actually cuts at is line 8486 — an earlier `#[cfg(test)] #[path = …] mod emitter_scan_tests;` declaration — not 8490 or 8491, which are five lines later and separated from it by only a doc comment and attributes.)* |
| Task 2 → close: the walk script's step bound (20) carries only two steps of margin over the seed-42 reading it was calibrated to (18) | Accepted as calibrated, not tightened: the bound exists to catch a script that never reaches both states at all, and 2 steps of margin on a script that is re-derived from the fixture (not authored blind) is adequate for that job. A future seed-42 fixture change that moves the flagship would need this bound re-measured, not merely re-passed. |
| Task 3 → close: the client bench's outdoor range is compared against The Rack's AFTER block without stating that one was quiet and the other contended | Left as found in the bench file's prose; recorded here rather than edited, because the two ranges (9.27–13.07 ms here, 9.97–14.10 ms there) already agree within noise regardless of load, so the omission does not change the reading — only its rigor. A reader comparing the two numbers closely should know which was which. |
| Task 4 → close: both idea-registry rows had to be trimmed to the 600-character cap (890→599, 763→592) | Reviewed at trim time: nothing load-bearing dropped except the phrase "two to five times per indoor turn", which survives in both the chronicle and decision 0636, so the registry row's compression cost no information the project actually needed from that row. |
| Final review: slug-style registry IDs (`[[PROC-a-two-point-difference-names-a-step-not-a-cost]]`, `TOOL-...`) appear throughout this campaign's own chronicle and retrospective, and `docs_consistency`'s uniqueness/reference checks (decision `0026-slugs-not-numbers`) only police *numbered* IDs, not slugs | Rides: this is an existing, project-wide gap in the check's coverage, not opened by this campaign and not closed by it — a stale or mistyped slug reference would not be caught mechanically today. Not scoped to The Terrier, so not fixed here. |
| Final review: the retrospective and `docs/retrospectives/README.md` both said "travelled through four committed documents" in their process headline while the chronicle and README's own enumeration said "seven committed places" for the same underlying fact, and a later correction (`book/src/open-questions.md`) had already added an eighth site neither count carried | Reconciled in the final-review pass: all three now read "eight sites in six files," matching the corrected enumeration (two idea-registry rows, two Rack-chronicle sentences, one Rack-retrospective bullet, two bench headers, one open-questions passage). |

## Follow-ups, with reasons

- **The 2–5 `brief_here` calls per turn stay un-deduplicated.** Threading one
  `Brief` through `enter`'s four call sites (and the two others that run per
  chamber turn) would remove the last redundancy this campaign found, but at
  a per-call cost now bounded by arithmetic rather than measured directly:
  `enter`'s handle fell from 55.4 ms (four `brief_here` calls, contended) to
  0.175–0.178 ms (the same four calls, quiet) in this campaign's own
  readings, against roughly 0.1 ms of non-brief work in the same handle by
  The Rack's decomposition — which bounds the *marginal* cost of a hoisted
  `brief_here` call at a few microseconds, not zero. No scratch
  instrumentation isolated it further. Recorded so a future campaign can
  re-check the bound rather than re-argue the decision: threading is
  complexity with no measurable return at this size, and stays that way
  until something moves the number.
- **`chamber_interior_here`'s seventeen callers are not audited.** It is
  `Session::brief_here`'s single largest fan-in and the site any future
  dedup or caching change would have to reason about; this campaign touched
  none of them because the cost each one now pays is microseconds, not
  because the fan-in is understood to be minimal.
- **`CLIENT-cache-demography-report`'s 480 ms of client startup is the next
  largest item in the same `build` block this campaign shortened.** Out of
  scope by spec §3.6 — it is a different cost, in a different subsystem, and
  bundling it here would have doubled this campaign's surface for no shared
  mechanism.
- **`test-sluice-vet.sh`'s negative control needs a synthetic probe.** Its
  current fixture — the first campaign branch on `origin` that mints a
  decision — is a live branch that can, and did, collide with another
  campaign's reserved block for reasons that have nothing to do with the
  branch under test. A fixture built from a fixed, synthetic pair of
  decision numbers would distinguish a harness failure from a real collision
  without depending on which campaigns happen to be live on `origin` at
  submission time.
