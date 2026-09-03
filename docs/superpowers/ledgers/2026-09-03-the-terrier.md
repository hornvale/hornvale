# The Terrier — decision ledger

Campaign: `campaign/the-terrier` · Spec:
`docs/superpowers/specs/2026-09-03-the-terrier-design.md` · Plan: (written
after G3) · Decision block: 0636–0645 (reserved 2026-09-03, main ceiling
0628).

Nathan's brief, verbatim (2026-09-03): *"The chamber shadowcast, ~8 ms per
derivation. After The Rack it is the largest single item left in a chamber
turn, and it is why enter and indoor look still miss the 15 ms client line.
The memo already keys correctly; the lever is the derivation itself.
Registry row with the number. A campaign."*

## Brainstorm (pre-G3)

### #1 [G1] — what is the ~8 ms, and where does the lever go?

**Question:** The registry row (`TOOL-chamber-snapshot-prices-a-shadowcast`)
and The Rack's chronicle both price "one shadowcast" at ~8 ms, read off the
difference between a chamber snapshot after `look` (8.4 ms, sighting memo
hit) and after `map`/`go` (16.3–16.8 ms, memo miss). The brief says the
lever is "the derivation itself". Which step of the derivation?

**Decision:** Measured before designing, per the autopilot's
"verify tool-behaviour claims by running the command" rule and memory's
*a mechanism read from the code is a hypothesis until counted*. The
shadowcast is **0.011–0.013 ms** (`SIGHT_RADIUS` is 4, so it visits at most
81 cells). The whole of the cost is `Session::brief_here` →
`brief::brief_of` → `hornvale_worldgen::occupations_by_vertex(world)`,
which reconstructs **every occupation in the world** (452 vertices' worth)
from the ledger on **every call**, at 8.7–28.8 ms per call under load
28–51, and a chamber turn makes two to five of them (`enter` 4 in `handle`
+ 1 in `snapshot`; chamber `look` 2 + 1; chamber `map`/`go` 0 + 2). *(28.8 ms
was a run-1 scratch reading that never entered the committed table; the
committed range is 8.7–26 ms and every other site now says so.)* The
function's own doc has carried a `NOTE ON COST` prescribing the fix since
2026-07-27 (`4569d883d`, the brief's founding commit): "hoist the map to
the caller (the session can hold it for the possession's life) — do NOT
memoize inside this function".

The lever adopted: **(A)** build the occupation map once in
`WorldContext::build`, beside terrain, climate, the locale context and the
demography report, and have `brief_of` take the map instead of the world.

**Why:** the map is a pure function of the immutable `World` — the ledger
it reads is `world.ledger`, never the session's — so it belongs with the
other world-scoped, derived-once, shared-across-sessions reads The Weir and
The Quire already hoisted there. The cost note prescribes exactly this
shape and forbids the memo shape. Simplest possible version of the fix:
one field, one signature change, one caller.

**Alternatives discarded:** (B) a session-lived `Derived<Facet, Brief>`
memo keyed on walk-locale — keeps the 9 ms scan on every cold key, adds a
key-completeness obligation the hoist never incurs, and the cost note
forbids memoising the derivation; (C) dedupe the 2–5 `brief_here` calls
per turn by threading one `Brief` through — the calls each cost
microseconds once the map is hoisted, so threading is complexity with no
measurable return (recorded as a follow-up with the number, not done);
(D) make per-vertex reconstruction cheap in `windows/worldgen` — a deeper
change to a shared decoder for a cost that the hoist removes entirely from
the turn path.

**Ideonomy passes / overturns:** 1 / 0 — tree-finding + abstraction-lift,
scale organon, prompts complexity/visibility/scope. No overturn; three
enrichments the brief did not name. *Tree, walking up:* the brief's
occupation read is the **one un-hoisted world-scoped read** among the four
`brief_of` makes (`is_built`, `is_cold`, `containing_vertex` are 0.000,
0.003, 0.003 ms — already served by hoisted state); the asymmetry is the
finding. *Scale (how often the map is derived):* per call → per turn (C)
→ per session (B) → per `WorldContext` (A) → at genesis as a fact (not
derived; rejected). Position (A) is where the siblings already sit.
*Scope prompt:* a lazy per-visited-vertex memo (`occupations_at`) costs one
full scan per vertex, i.e. worse — whole-world-once is right, and 9 ms
once against a ~3 s build is 0.3%. *Visibility prompt:* the cost note was
**latent** — correct, in source, ignored for five weeks — and The Rack
misattributed the cost because its memo made only derive-vs-reuse legible
and nothing decomposed the derivation. That produced #3 (the instrument)
and the correction sweep in spec §3.4. A sibling sweep for other per-turn
`hornvale_worldgen::` scans on the chamber path found none (`lexicon_from_in`
is the `ask` verb; `barrier_of` is a test). Called converged: the pass
produced enrichment, not a new branch.

**Capture:** spec §1–§3; `TOOL-chamber-snapshot-prices-a-shadowcast` →
body corrected, `shipped` at close, Where → this spec; new
`PROC-a-two-point-difference-names-a-step-not-a-cost` row; follow-up
register entry for (C) with the post-hoist per-call number.

### #2 [Q] — where does the hoisted map live: `WorldContext` or `Session`?

**Question:** the cost note says "the session can hold it for the
possession's life". `WorldContext` did not exist when it was written (The
Quire split it out of `Session::start` later).

**Decision:** `WorldContext`. Sub-decision of #1's tree walk (same pass).

**Why:** `WorldContext`'s own doc: "Nothing here is agent-scoped, and
nothing here is mutated after `build` returns: a `&WorldContext` is shared
by every session started from it." The map is exactly that. Holding it on
`Session` would rebuild it per possession (`repossess` pays again) and put
a world-scoped object beside session-scoped ones. The game client's
`Driver` starts sessions with `start_in` over one shared context, so the
map is built once per world there too.

**Alternatives discarded:** `Session` field (per-possession rebuild;
wrong owner); `LocaleContext` (a `windows/locale` type — a history read
does not belong in the locale window).

**Ideonomy passes / overturns:** shares #1's pass (tree "up" is what
decided it); 0 overturns.

**Capture:** spec §3.1.

### #3 [Q] — what pins the fix so a per-turn world scan cannot come back?

**Question:** The Rack's rule is a counted budget, not a clocked one. But a
`TurnWork` counter for "occupation scans" would have no writer on any turn
path once the fix lands — the dead-zero The Rack argued against.

**Decision:** a **structural source scan** in `windows/vessel`: production
code under `windows/vessel/src` names `occupations_by_vertex`,
`occupations_at` or `occupation_records` **only** inside
`WorldContext::build`. Witnessed red before green by restoring the
per-call read. Precedent: `underground.rs::the_reach_seam_is_the_only_source_of_the_radius`
and `affordance.rs::no_verb_by_object_table_exists` (a property about what
the code does not contain, which no runtime assertion can witness). The
wall clock stays demoted to the box instrument (`session_cost.rs`,
`move_cost.rs` Measured blocks). Plus a VIEW ≡ SCAN test for the map
itself (spec §4 P6).

**Why:** the regression shape is "someone reintroduces a whole-world read
on a per-call path". A source ratchet reddens on that commit; a counter
could not (no writer), and a wall-clock gate is host-gated and blunt
(The Rack's own finding). The direction the check enforces is stated in
its doc: it forbids *presence outside one block*; it does not prove the
block's map is complete — that is P6's job.

**Alternatives discarded:** `TurnWork::occupation_scans` (dead zero);
re-pinning `session_cost.rs` ceilings downward (the ceilings are upper
bounds and stay valid; a downward re-pin is a separate reviewed act The
Rack already declined to bundle — left as is, with the new reading
recorded in the constant's doc if the implementer takes one).

**Ideonomy passes / overturns:** shares #1's pass (the visibility prompt
produced it); 0 overturns.

**Capture:** spec §3.3, §4 P1.

### #4 [Q] — the campaign name

**Decision:** The Terrier — a terrier is an estate's register of who holds
which land, which is what the hoisted map is: the world's occupation
register, kept once and consulted instead of re-surveyed. Grepped free
(`docs`, `book`, branches) before adoption. The worktree was taken as
`the-squint` while the lever was still believed to be sight; renamed with
`git branch -m` and `git worktree move` once the measurement said
otherwise. Routine; no ideonomy pass run for a name.

### #5 [Q] — is `enter`'s 33.7 ms handle in scope?

**Question:** the brief names `enter` as missing the 15 ms line. `enter`'s
handle (33.7 ms in The Rack's reading, 55–60 ms here under load) is not a
snapshot cost.

**Decision:** in scope, and it is the same lever: `enter` makes **four**
brief derivations in `handle` (its own, `lattice_of`'s via `descend`,
`describe_chamber_here`'s, and `derive_sighting`'s via the presence line)
and one more in `snapshot` (`chamber_sources`). Measured: 9.4 + 26.3 +
10.2 + 9.0 = 55 of a 55.4 ms handle; `structure_at` 0.003 ms, the lattice
embedding ~0.1 ms, prose 0.003 ms. Nothing else in `enter` is a millisecond.

**Ideonomy passes / overturns:** shares #1's pass (the scope prompt); 0.

**Capture:** spec §1, §4 P2.

## Plan (post-G3)

### #6 [G4] — plan self-review against the approved spec

**Decision:** proceed. Spec §3.1/§3.2/§3.3 → Task 1; §4 P6 → Task 2;
§3.4 (bench halves) and §3.5, P2–P4 → Task 3; §3.4 (registry, chronicle,
retro), §5's rows and decision 0636, P5 → Task 4; DoD's chronicle,
retrospective, census, merge → Task 5. Two plan-text defects found by
verifying the brief against the code before commit: `session.rs` has no
top-level `use std::collections`, so the new field's type is written in
full; the in-module `world_at(seed)` returns `Option<World>` and seed 42's
fixture is `seam_world()`, so Task 2's test now branches on the seed. Main
absorbed at `2e85f2d72` (62 commits, The Brattice) before Task 1; every
anchor the spec cites was re-verified after the merge.

**Ideonomy passes / overturns:** none run for the plan itself; the design
pass is #1's.

**Capture:** `docs/superpowers/plans/2026-09-03-the-terrier.md`.

## Execution

### Task 1 — complete (2026-09-03, commits `319134e3f..57219df62`, two fix rounds)

The hoist itself landed as written: `WorldContext.occupations`, `brief_of`
takes the register, `World` gone from `brief.rs`, the type-audit report did
not drift, gate green. Two rulings, both against the plan's own text:

- **The plan's evidence clause asked for the wrong red.** Task 1 said
  "paste the red you saw against the pre-hoist tree", and the red seen was
  the positive control (`build` did not yet name the register) — a red from
  the wrong assertion, proving the test can fail and nothing about whether
  the offender scan catches a per-call read. The reviewer flagged it as
  plan-mandated; ruling: witness the offender scan by mutating
  `brief_here` on the finished tree. Observed: the scan names
  `src/session.rs:7232`. Cost if wrong: none now; the pre-hoist red is kept
  as a separately labelled second observation.
- **The ratchet under-scanned three files, found by the controller between
  rounds.** `production_code` split at the first `#[cfg(test)]` in a file,
  which gates test-only HELPERS mid-production: `liveness.rs`'s first is at
  4353 and its module at 8490, so ~4,100 production lines (including
  `species_activity`) were never scanned; `roster.rs` likewise from 237 to
  446. A guard that reads green over code it never looked at is the exact
  class it was built to close. Ruling: split at the test MODULE (an attribute
  followed, across attribute lines, by `mod `), blank comment lines instead
  of dropping them so cited lines are the file's own, add per-file coverage
  controls and a synthetic-shape test. Same lesson as memory's
  *a check that can never fire*: the scan's reach had to be witnessed, not
  assumed.

Deferred minors (for the final review): the boundary detector takes the
FIRST module-declaring sequence and `liveness.rs` has two adjacent ones; a
doc cite of 8490 where the `mod` line is 8491. No ideonomy pass was run for
either ruling; both are corrections of a claim against the code.

### Task 2 — complete (2026-09-03, commits `a3248e0a3..f1112d39e`)

P6 landed as an in-module test at seeds 42 and 7, red under the prescribed
mutation (an empty register reds the identity assertion before the walk
runs), green restored. **The plan's script was a null on both fixtures**,
and the implementer measured rather than obeyed: seed 42 never leaves its
flagship's vertex walking north in 24 steps (it does at step 18 walking
east), and seed 7's flagship *starts* on a vertex with no occupation record
at all (a living one is seven steps west). Ruling at review: the per-seed
bearings and a bound of 20 are a calibration to the real fixture, not a
weakening — both non-vacuity directions and the per-step equality are
intact, and the calibration is written in the test's doc so it can be
reproduced. That is the fourth plan-prescribed script in two campaigns to
be a null at seed 42; memory's *never prescribe a probe from outside the
code* holds, and the plan's own decision rule ("if the guard fires, change
the bearing and record which") is what made it cost minutes rather than a
round.

**The Stage 1 gate is held, and not by this campaign.** `outboard`'s
`test-sluice-vet.sh` picks the first campaign branch on `origin` that mints a
decision as its collision probe; that branch is `campaign/the-prospect`,
which mints `0540` inside The Pawl's reserved block 0536–0545 while `main`
already carries The Pawl's `0540`. The test's "negative control" therefore
fires a true collision and reads as a harness failure. Posted to the board
(technique `66f8a5348`). Ruling: Task 3 is readings and prose and proceeds;
`make sluice-stage` is resubmitted once The Prospect renumbers, and before
Task 4. Cost if wrong: Stage 1's full-workspace suite is unverified until
then, against local vessel-crate greens of 412 and 658 tests. Second
finding for the register: a negative control whose fixture is a live
branch cannot distinguish a harness bug from a real collision — the vet
test needs a synthetic probe.

### Tasks 3 and 4 — complete (2026-09-03)

**Task 4 first, Task 3 second — a reorder ruled at dispatch.** The box sat
at load average 122 when Task 3 came up, which would have made a ≤ 3 ms
line meaningless; Task 4 is prose, a decision record and a rebaseline and
needs no number, so it ran first. Five hours later the box read 2.0 and
`campaign/the-prospect` no longer minted 0540, so Task 3 and the stage-gate
resubmission (`req-8b4f7f49065e`, at `8b4f7f490`) went out together. Cost
if wrong: none observed; both readings were taken quiet (all three
averages ≤ 2.67).

**Task 3 (commits `d0487e1a1..35b3ba981`): every preregistered line met.**
Chamber `snapshot()+json` after `map`/`go` 16.3–16.8 → **0.457–0.525 ms**
(P2 ≤ 3); `enter` handle 33.7 → **0.175–0.178 ms** (P2 ≤ 3); chamber
`look` handle 16.5 → **0.095–0.114 ms** (P2 ≤ 1); `Session::start` 845 →
868 ms, **+23 ms** (P4 ≤ +30); outdoor rows 4.06–4.98 ms, the control,
unmoved. Client, default profile: every indoor movement turn 18.7–47.4 →
**0.61–0.82 ms** (P3 ≤ 15, the line the campaign was opened on); outdoor
9.27–13.07 ms, unmoved beyond noise. The reviewer re-derived every min,
max, rounding and delta from the pasted output. Deferred minor: the
client block compares a quiet outdoor range against The Rack's contended
one without saying so.

**Task 4 (commit `8b4f7f490`, fix `d73429896`): the corrections landed;
two rulings.** Both registry rows had to be trimmed to the 600-character
Idea cap (890→599, 763→592) and the reviewer confirmed nothing load-bearing
dropped except "two to five times per indoor turn", which the chronicle and
decision 0636 carry — deferred minor. The implementer wrote the chronicle
in full where the brief said "stub now, real one in Task 5", and put it
after The Rack in `SUMMARY.md` where three newer chronicles follow. Ruling
on the first: **keep it** — every number and mechanism in it was verified
against source and spec by the reviewer, and deleting verified prose to
honour a sequencing note buys nothing; Task 5 extends it (numbers table,
ratchet direction, honest limits) instead of creating it. The second was a
one-line move the controller made inline under the dispatch skill's
trivial-exact-content carve-out, re-reviewed. `make rebaseline` moved only
`docs/digest/decisions-in-force.md`, as P5's rule allows.

### The absorb before close (2026-09-03, `63f08555a` + `78013ce66`)

The stage gate at `8b4f7f490` went red in its `gate` phase on
`registry_ids_are_unique`: `main` had moved 45 commits (The Nettle), which
rewrote two PROC rows in the registry, and this branch had inserted its new
PROC row directly beside them, so the auto-merge kept both versions of each
neighbour. The fourth recorded instance of a hand-edited table merging
clean and wrong (The Rack's retrospective counted three). Resolved by
keeping `main`'s rows; the rebaseline on the merge product moved nothing.
The absorb also brought a stricter pre-commit — docs-only commits now run
the prose-subject tests — which caught a bare `anchor_cells` token in
Task 3's bench header that the lexicon guard counts; waived on the line as
the function's own name. Cost if wrong: none; both are review-visible
text. Lesson for the register: put a new registry row at the END of its
table, not beside a row another campaign may be rewriting.

## Follow-ups

This campaign's follow-up register; there is no scratch one (The Cartulary).
Each item names the reason it was not attempted here.

- **The 2–5 `brief_here` calls per turn stay un-deduplicated.** Threading one
  `Brief` through `enter`'s four call sites (and the two others a chamber
  turn runs) would remove the last redundancy this campaign found. Not
  attempted: after the hoist a call costs `is_cold` (3 µs, cached) plus a
  map lookup, and the arithmetic bound from Task 3's own readings —
  `enter`'s handle fell from 55.4 ms (four calls, contended) to 0.175–0.178
  ms (the same four calls, quiet) against roughly 0.1 ms of non-brief work
  in the same handle (The Rack's decomposition) — puts the marginal cost of
  one hoisted call at a few microseconds, not zero, but no scratch
  instrumentation isolated it further. Threading is complexity with no
  measurable return at this size; re-check the bound before re-litigating
  the decision.
- **`chamber_interior_here`'s seventeen callers are unaudited.** It is
  `Session::brief_here`'s single largest fan-in and the site any future
  dedup or caching change would have to reason about first. Not attempted:
  every one of the seventeen now pays microseconds, not milliseconds, so
  there is no cost pressure to audit the fan-in itself.
- **`CLIENT-cache-demography-report`'s 480 ms of client startup is the next
  largest item in the same `build` block this campaign shortened.** Out of
  scope by spec §3.6: a different cost, in a different subsystem, sharing no
  mechanism with the occupation-register hoist.
- **`test-sluice-vet.sh`'s negative control needs a synthetic probe.** Its
  fixture is the first campaign branch on `origin` that mints a decision —
  a live branch that collided with another campaign's reserved block during
  Task 2 for reasons unrelated to this campaign, and read as a harness
  failure rather than a real result. Not attempted here because the fix
  belongs to the vet harness, not to a campaign it happened to be evaluating
  when the collision occurred. A fixed, synthetic pair of decision numbers
  would distinguish a harness failure from a real collision without
  depending on which campaigns are live on `origin` at submission time.
- **Two more sites still split "production" from "test" code at the first
  `#[cfg(test)]` in the file** — the exact failure class this campaign's own
  ratchet was found to have (see the retrospective's ratchet section):
  `windows/vessel/src/liveness.rs:16531`
  (`production_reaches_fatigue_through_exactly_one_door`, `src.find("#[cfg(test)]")`)
  and `windows/vessel/src/underground.rs:1256`
  (`here.split("#[cfg(test)]")`). Not fixed here: both are pre-existing guards
  outside The Terrier's own scope, and the class already has a registry row
  (`TOOL-a-text-scan-guard-splits-on-prose-about-its-needle`, minted by The
  Plumb), whose Where cell now also points at `the_terrier.rs`'s
  `production_code` as the correct shape to copy.
