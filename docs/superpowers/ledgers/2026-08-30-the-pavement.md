# The Pavement — decision ledger

Campaign: `campaign/the-pavement` · Spec:
`docs/superpowers/specs/2026-08-30-the-pavement-design.md` · Plan:
`docs/superpowers/plans/2026-08-30-the-pavement.md` · Decision block:
0506–0513.

**This file was created mid-campaign, at the absorption of main, and that
fact is itself the first thing worth recording.** This campaign ran its
whole execution phase writing rulings to
`.superpowers/sdd/2026-08-30-the-pavement/progress.md` — git-ignored,
per-worktree scratch — because The Cartulary's convention did not exist on
main when the campaign started. It landed while this branch was working. The
gate (`docs_consistency::every_campaign_with_a_spec_and_a_plan_has_a_ledger`)
refused the absorption commit, which is the check working exactly as designed:
44 `Ruling:` lines were sitting in a file that dies with the worktree, and
nothing but that gate would have said so. The entries below are promoted from
that scratch, at the moment the convention reached this branch.

## Pre-flight cross-task scan (before Task 1)

The scan produced a table of every task pair sharing a file or interface.
Four findings needed rulings; the rest were clean.

### #1 [G4] — `water_reading.rs:715` hard-codes a walk depth

**Question:** Task 2 changes `walk_depth`; a locale test asserts a specific
depth-derived value. Which task owns the update?

**Decision:** It moves with `walk_depth`, in Task 2, and gains a bidirectional
agreement test rather than a re-pinned literal.

**Why (precedent cited):** Decision 0456 — a one-directional check reads as
total to the next reader. A re-pinned literal would go green while proving
only that someone typed the new number.

**Alternatives discarded:** Leaving it for Task 9's sweep — rejected: the group
that changes the depth cannot be green with a stale assertion inside it, and
Task 2/3/6 land as one commit (#5 below).

**Ideonomy:** 1 pass, 0 overturns.

### #2 [G4] — `rill_probe.rs`'s comment describes the pre-change geometry

**Decision:** The fix moves from Task 9 into Task 2, and the comment is
corrected rather than deleted.

**Why:** The comment states a *reason* (why the probe samples where it does)
that survives the geometry change with different numbers. Deleting it would
lose the reason to save the numbers.

**Ideonomy:** 1 pass, 0 overturns.

### #3 [G4, LOAD-BEARING] — Task 6 owns every `cell_delta` call site

**Question:** The plan gave Task 6 the `go` verb. Does it own the other
`cell_delta` consumers?

**Decision:** Yes — every call site, not just `go`.

**Why:** `cell_delta` returning a 4-way answer where 8 are now possible is a
silent wrong answer, not a compile error, at each site.

**This ruling was later found TOO NARROW and is corrected at #14.** It covered
`cell_delta`'s sites and missed that `neighbours()` had five more consumers
(`grow.rs`, `classify.rs`, `anchor_cells.rs`, `light.rs`, and a test) that
would have silently admitted corner-cutting diagonals. Recorded as made, and
as corrected, because the shape of the miss is the lesson: I audited the
function I was changing and not the function it was changing *for*.

**Ideonomy:** 2 passes, 1 overturn (the overturn is #14).

### #4 [G4] — undefined test helpers in plan text

**Decision:** The implementer writes them; the plan's omission is not a blocker.

**Why:** `campaign-autopilot`'s rule against prescribing a mutation from
outside the code — a plan author does not know which helpers the surrounding
suite already provides, and the implementer does after reading.

**Ideonomy:** 1 pass, 0 overturns.

## Controller rulings during execution

### #5 [G5] — Tasks 2, 3 and 6 become ONE atomic commit

**Question:** Can the geometry change land incrementally?

**Decision:** No. `{Task 2, Task 3, Task 6}` land as a single commit.

**Why:** No ordering of them is green at every step. `windows/vessel/src/
course.rs` (the rhumb) cannot survive the base-geometry change, and the `go`
verb cannot lose the rhumb before 8-connectivity exists to replace it. Since
bypassing the pre-commit hook is forbidden without exception, "commit red and
fix forward" is not available, so the only honest arrangement is one commit.

**Alternatives discarded:** A temporary shim keeping `course.rs` compiling
against the new geometry — rejected as writing code whose only consumer is the
gate, for one commit's benefit. Bypassing the hook — refused on standing
instruction, and the attempt was later blocked by the hook itself, correctly.

**Cost if wrong:** A 121-path commit is hard to bisect. Accepted knowingly, and
the cost was quantified before accepting: 159 commits behind main, ten
overlapping files.

**Ideonomy:** 3 passes, 0 overturns.

### #6 [G5] — the corner-arity assertion is 24, not 8

**Question:** The plan said "8 rooms have 7 neighbours". Is that right?

**Decision:** No — it is 24. My plan's arithmetic was wrong.

**Why:** 6 faces x 4 corner quads = 24; equivalently 8 cube corners x 3 quads
meeting at each = 24. **Corners: 8. Rooms at corners: 24.** The two are
different quantities and the plan collapsed them.

**Capture actions:** Propagated to 7 sites across 3 generations of the plan and
its briefs. Verified two independent ways before propagating.

**Ideonomy:** 1 pass, 1 overturn (the plan's own figure).

### #7 [G5, determinism] — warp at the face edge is special-cased

**Question:** `math::tan(1.0 * PI/4)` returns one ULP below 1.0. Does it matter?

**Decision:** Special-case `warp(±1.0)` to exactly ±1.0. Face seams must be
watertight exactly, not to 1.11e-16.

**Why:** A face boundary is where two faces must agree on a point's address.
One ULP of disagreement there is a point that belongs to neither face, or to
both — a determinism defect, not a precision preference.

**Ideonomy:** 2 passes, 0 overturns.

### #8 [G5] — absorb main AFTER the atomic group commits, not before

**Question:** The Legend landed. Absorb before or after the group?

**Decision:** After. Absorb once, and re-order Task 8 (client input bindings)
to follow the absorption.

**Why:** Absorbing into a tree that cannot compile — which is every
intermediate state of the atomic group — would make every conflict
unresolvable-by-testing. Task 8 owns `BAND_B_RUNG` and ~80 readers, and main
had moved the same client files, so doing it before the absorption would mean
doing it twice.

**Alternatives discarded:** Absorbing first and holding the group — rejected:
the group was already written and 159 commits of drift would only grow.

**Ideonomy:** 2 passes, 1 overturn (the original plan order had Task 8 before
the absorption).

### #9 [G5] — chamber tunnels stay orthogonal-only, ON MERITS

**Question:** Everything else went 8-connected. Do the underground tunnels?

**Decision:** No — they stay orthogonal-only, and this is a positive design
call rather than a conservative default.

**Why:** A tunnel is a *carved* passage; a diagonal tunnel would mean two
carved cells meeting at a corner with no shared face, which is not a passage a
digger could make. The surface band's diagonals are open ground, a different
thing. `grow.rs`'s doc comment was rewritten to say this, because it previously
read as an unexamined default.

**Alternatives discarded:** 8-connecting tunnels for consistency — rejected as
consistency for its own sake against a physical argument.

**Ideonomy:** 2 passes, 0 overturns.

### #10 [G5] — `cell_delta` becomes total, not `Option`

**Decision:** Accept the implementer's deviation: `cell_delta` returns
`(i32, i32)` rather than `Option<(i32, i32)>`.

**Why:** This is better than what I specified, and for the reason my own
ruling #3 was about. My complaint was that widening `cell_delta` was SILENT at
eight sites. Making the return total converts the widening into a compile error
at every one of them. The implementer solved the problem my ruling only
policed.

**Ideonomy:** 1 pass, 0 overturns.

### #11 [G5] — the octile weights are 12/17

**Decision:** Integer weights 12 and 17 — the third continued-fraction
convergent of √2.

**Why:** Preregistered tolerance was 0.5%; 12/17 lands at +0.173%, the smallest
convergent inside it. Measured on the real lattice the diagonal is 1.411786
edges, not 1.414214, because the mesh is not a perfect square lattice — so the
zigzag exploit an unweighted diagonal would have allowed was 41.18%, not 41.42%.
`gcd(12,17)=1`, which means the octile metric *removed* cost ties rather than
creating them.

**Ideonomy:** 2 passes, 0 overturns.

### #12 [G5] — the corner rule checks flanks, never the destination

**Decision:** A diagonal is refused only when BOTH flanking orthogonals are
impassable. One open flank permits it.

**Why:** The user's own framing — "it bugs me to have open spaces you can't
move diagonally across", against a dislike of sneaky diagonals through walls.
Both-flanks-blocked is exactly the geometric case where the diagonal passes
through solid matter. Implemented as a closure,
`diagonal_is_blocked(from, d, open: impl Fn(Cell) -> bool)`, after the original
signature turned out not to serve the underground band (`CellGrid`, not
`Lattice`).

**Ideonomy:** 3 passes, 1 overturn (the first signature).

## The two findings worth more than the feature

### #13 [G5] — `grow.rs`'s `rotated()` used `HEADINGS.len()` as a stream modulus

**The finding:** `rotated(draw)` computed `draw % HEADINGS.len()` over a seeded
stream draw. `HEADINGS` was about to widen from 4 to 8. Widening it would have
silently changed every generated world for the same seed, and no guard in the
project could have seen it — determinism tests compare a seed against itself,
not against history.

**Decision:** `const ORTHOGONAL: usize = 4` as the explicit modulus, with the
pinned orthogonal-first ordering of `HEADINGS` documented as a save-format
contract.

**Why this is the campaign's most valuable output:** it is a live instance of
the class decision 0102 exists for — keying a draw on something that is not a
fixed lattice position. The array length was a generation-time incidental
masquerading as a constant.

### #14 [G5] — the epoch is an instrument, and four tests were resting on
collapsed distinctions

**The finding:** Four tests passed for the wrong reason, each because a
distinction that does not exist in a triangular mesh cannot be tested for:

| test | the coincidence |
|---|---|
| placement probe | 31 placed happened to equal 31 cells |
| chart symmetry | three distinct symmetries coincide on a triangular diamond |
| corner arity | 8 corners vs 24 rooms at corners |
| an H2 sweep | `k` cancelled out of the expression entirely |

**Decision:** Re-found each on the distinction rather than re-pinning its
literal. Three value-comparing witnesses were RE-FOUNDED, not re-captured and
not retired.

**Why:** A test that silently means the conjunction of everything that
coincided at its site is not a test of the thing it names. The geometry change
pulled the coincidences apart, which is what makes an epoch an instrument
rather than only a cost.

**Capture actions:** `PSY-oscillation-corpus` and the chart-projection row filed
in the idea registry; the projection row deliberately notes that vertical
stretch is acceptable now and matters for a future tilemap.

## Errors I made and corrected, recorded because the shapes recur

- **The spec specified the naive cube projection** (5.2x max/min cell area —
  *worse* than the icosphere it replaced) while claiming an improvement. Caught
  before implementation; fixed by adding §2.0 requiring the tangent warp, which
  measures 1.41x. Decision 0512. The shape: I verified the addressing change and
  asserted the distortion consequence without evaluating the function. Compare
  the standing rule *evaluate the curve, not the constant*.
- **I grepped The Legend's worktree and wrote the result as a fact about mine.**
  The spec asserted `relief`/`relief_legend` were present on this branch. They
  were not; that branch carried them. Verified false three ways, corrected in
  place, board post `534015fbf`. This absorption is where they actually arrived.
- **Three citation misattributions**, all to `docs/CLAUDE.md` for rules whose
  real homes are root `CLAUDE.md` and decision 0456. Verified with
  `grep -ci "cost" docs/CLAUDE.md` → 0. Fixed three; a fourth citation checked
  and left because it was correct.
- **A false §2 argument about drive arbitration.** I told the user the limit
  cycle needed a design decision. It was `PlantedTerrain` fixture drift:
  unplanted rooms read INFINITY temperature (urgency 0 — a *perfect* thermal
  target) and `Hazards::ZERO` (a *perfect* refuge), and twenty fixtures listed 3
  of 8 neighbours. Corrected to the user.
- **Seven counting errors** across the campaign (8 call sites reported as seven,
  11 files as one, 7 sites as five, 9 as "twelve of eighteen", 10 as nine, face
  16 as 14, 49.6% as "three fifths"). No single one was load-bearing; the rate
  is the finding.
- **Every failure count I reported for most of the campaign was workspace-only.**
  `make game-check` was red the entire time because `clients/game/core` is
  outside the cargo workspace. A gate's scope is part of its result.

## Deferred minors and parked findings

- **Drive-arbitration hysteresis is absent, and the campaign made one case
  worse.** A wild rust-monster oscillates `fled the uncanny ground (fear)` ↔
  `drifted homeward, missing its people (belonging)`, `agent-at` churning
  12→40→74 while every other agent quietened, 145 steps to move 5 rooms. The
  belonging leg went 6→32 *because* the octile ball is smaller at equal reach
  (73 nodes vs 121), so `PLAN_BUDGET=1000` reach rose ~15→~19 rooms and homeward
  plans that used to return `None` now succeed. **Pre-existing** — a
  carrion-crawler runs comfort↔thirst at HEAD — and **not** a tie pathology,
  since `gcd(12,17)=1` removed ties. User's ruling: "We definitely have to add
  hysteresis into the system, but now is not the time."
- **`STEADY_STATE_CEILING` was raised 1.5 → 2.5 as a documented override**, not
  a re-tune. Rate went 0.958 → 1.333 (Tasks 2–6) → 1.650 (Task 7). The override
  is the honest form: the ceiling's premise (bounded per-tick commit churn) is
  intact, one creature violates it, and the cause is understood.
- **`PSY-oscillation-corpus` must be FROZEN BEFORE the hysteresis fix**, per
  decision 0016 — a corpus authored after the fix cannot measure it. The user
  asked for pathological cases at 3, 5, 7 and 13 places.
- **`agent-at` should not be committed to the ledger at all.** The user's
  architectural ruling: "agent-at is deterministic given the ledger, so there is
  no reason to persist it except for snapshotting to reduce startup time. All
  the other information — hysteresis, tick-by-tick agent position — should be
  managed in the ECS. The ECS is a data-oriented layer over derived data, an
  adaptive cache. The LedgerFold is just part of that." Not this campaign's work.
- **`docs/decisions/README.md` is missing 61 of 287 decision records (21%), and
  nothing checks it.** `origin/main` is missing exactly the same 61, so this
  campaign neither caused nor worsened it. The GENERATED index
  (`docs/digest/decisions-in-force.md`) is complete and correct — it carries 278
  and correctly excludes 9 Superseded records, including 0141 as superseded by
  this campaign's 0506. Only the hand-maintained README drifts. Worth a
  `docs_consistency` check of its own, since root `CLAUDE.md` directs sessions to
  grep that tree before relitigating and the README is its human entry point.
- **`make vessel-check` needs `~/.deno/bin` on PATH and does not say so.** Two
  independent instances this campaign (mine and a reviewer's). `make rebaseline`
  silently *degrades* without deno rather than failing. One of the two announces
  the problem; neither should be silent.
- **`pre-rill-wetness.jsonl` has zero readers.** Disposition not decided.
- **`mesh_addressing_agrees_with_the_spatial_search` should be reported
  DISSOLVED, not passed or failed.** Its own doc says so: the cube-sphere retires
  its premise (a facet's nearest mesh vertex is no longer guaranteed to be among
  its own corners). This needs a deliberate call in Task 8, not a constant bump.

## Remaining work at the time of this entry

Task 8 (client input bindings: `BAND_B_RUNG` 12→13 across ~80 readers, two
stale literals to re-derive, and the DISSOLVED call above); Task 10 (close: H1/H2
with positive controls, H3a dissolved, H3b re-measured on an idle box, chronicle,
retrospective); four workspace failures (a census refresh needing
`make sluice-census` on lefford, a 0016 ruling on a baseline whose subject no
longer exists, and two live behavioural findings in water/wetness reading).
