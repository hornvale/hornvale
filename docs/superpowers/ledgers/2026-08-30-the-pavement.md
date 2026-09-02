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

**Why (precedent cited):** Decision 0456 — a rule stated in two places needs a
bidirectional agreement test; the depth constant and the test's expectation are
two such places. A re-pinned literal would go green while proving only that
someone typed the new number.

**CORRECTION (2026-09-01, caught by Task 10's implementer):** this entry
originally glossed 0456 as "a one-directional check reads as total to the next
reader." That sentence is decision **0491**'s content (a stated blindness gets a
visible ratchet, not a silent fix), not 0456's. The citation was right and the
gloss was borrowed from a neighbouring decision — which is the more insidious
error of the two, because the number checks out.

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
  real homes are root `CLAUDE.md` and decision **0491** (not 0456 — see the
  correction under ruling #1; I made the same 0456/0491 substitution twice, in
  two different documents, which makes it a habit rather than a slip).
  Verified with
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

## Post-absorption rulings

### #15 [G5] — `furnishing_marks.rs`'s seed is re-founded, not re-pinned

**Question:** The absorption's only two new failures are
`furnishing_marks::{a_chamber_with_a_hearth_emits_a_furnishing_mark,
an_unlit_hearth_is_not_emitted}`, both panicking at the same shared helper with
"the hearth stopped existing in the hearthroom: You see no a hearth here."
Is this a defect this campaign introduced, or a moved fixture?

**Decision:** A moved fixture, and the fix re-founds the seed rather than
swapping the literal 35 for 13.

**Why (evidence, not inference):** Probed ten seeds through the live CLI
(`possess --seed N --script`, `enter` / `enter further in` / `examine a
hearth`). Nine show "a doorway and an alcove" and no hearth; **seed 13 shows
"Stones set in a ring, and the ash inside them still …"**, so hearth placement
works end to end and nothing in the mechanism broke. The gate is climate:
`interior/pattern.rs`'s `the-fire` carries `needs_cold: true` while
`the-alcove` carries `needs_cold: false`, which is exactly the observed
signature — the alcove places, the fire within it does not. Seed 35 sat near
the cold threshold and the walk band moving to `globe_level + 7` (decision
0511) moved the sampled position off it.

**This campaign already met this class once and wrote it down.** The doc on
`anchor_cells.rs`'s `GROWN_RELAXATIONS`: *"the fixture addresses are built from
`WALK` and the walk band went to `globe_level + 7`, so the same seeds draw
different blobs."* That case was resolved by widening the corpus from 256 to
1024 cases and CHECKING THE RATE HELD (2.0–2.3% → 1.95%) rather than relaxing
the guard. Same discipline applies here.

**Alternatives discarded:** `Seed(35)` → `Seed(13)` — rejected. It is a
one-line green and it re-pins the exact kind of literal The Legend's own module
doc predicted would break again: *"a future genesis change could move the
hearth and break these tests for an unrelated reason."* The Legend
de-hardcoded the COORDINATES and left the SEED; the seed is the half that
actually carries the climate gate. Fixing it by moving the literal one seed
over leaves the next geometry campaign the same failure.

**Cost if wrong:** A bounded seed search costs world builds, and these tests
already run 3.8–4.4 s. If the search is slow the honest fallback is the pinned
seed WITH a loud premise assertion naming why it was chosen; that is strictly
better than a bare literal either way.

**Ideonomy:** 2 passes, 1 overturn (my first instinct was the literal swap).

**Capture actions:** brief at
`.superpowers/sdd/2026-08-30-the-pavement/task-absorb-furnishing-brief.md`.

### Parked finding: `water_reading::the_width_clause_binds_when_the_step_shrinks`

**Verified (from the failure output, not conjecture):** at `Vertex(2656)`, water
full width `3.5692404593032806e-5` rad, longest 16-depth step
`1.9635450708660834e-5` rad. Water is 1.82x wider than every step the test
enumerates, so the width clause should refuse every step and the crossing should
read `Impassable`. It reads `Fordable`. The test's own comment says
`wide < shortest_shallow` "is sound only because `step_lengths()` covers EVERY
step `verdict()` prices — see its doc for what happened when it did not", so this
file has been bitten by a coverage mismatch before.

**Verified (read the code):** `step_lengths()` at
`windows/locale/tests/suite/water_reading.rs:960` is
`self.steps.iter().map(|step| home.min(room_edge(step)))`. It covers exactly the
steps the transect holds, so this is not a set-coverage gap.

**HYPOTHESIS, NOT ESTABLISHED — a subagent must verify or refute it before
acting:** it may be a VALUE mismatch rather than a coverage one. `room_edge`
returns one characteristic edge length per room. With 8-connectivity a
diagonal step's true crossing distance is `edge * sqrt(2)`, but
`home.min(room_edge(step))` returns the plain orthogonal edge for a diagonal
step, understating it by ~1.41x. That would make `longest_deep` an
underestimate.

**Why the hypothesis is INSUFFICIENT as stated, and this is the part worth
carrying:** 1.41 x 1.9635e-5 = 2.77e-5, still below the 3.569e-5 width. So the
sqrt(2) factor alone does NOT flip the verdict, and anyone who stops at the
tidy explanation will produce a fix that does not work. Either `verdict()`
prices something `step_lengths()` does not model at all, or the width clause is
short-circuited before it is reached. Read `verdict()` first.

**Do not re-pin the literal.** This is a behavioural disagreement between two
functions, not a moved constant.

### #19 [G5] — the water-width finding is a TEST-MODEL defect, and the hypothesis above is refuted

**Question:** the parked finding directly above. Is `crossing_between`'s width
clause admitting a crossing over water wider than every available step?

**Decision:** No. The gate is right; the test's model of it was wrong, and the
fix corrects the test's *operands* rather than its assertions or its filter's
purpose. `Transect::step_lengths()` becomes `Transect::width_pricing()`,
returning the gate's own per-step operands.

**Why (instrumented, not inferred):** the test paired its step lengths with
`2.0 * a.edges[0]` — the full width of the reach **being transected**,
`ChannelNetwork::band_edges[i][j]`. `crossing_between` prices no such quantity:
it prices `2.0 * r.band_edges[0]` for each `BankReading` the two rooms of a step
**independently win**. At `Vertex(2656)` (polyline 284, index 3) the transected
reach is 3.5692e-5 rad wide, but the home room wins **line 1106** at 1.6007e-5,
and its eight neighbours win lines 1097, 1084, 1106, 23, 284, 1106, 23 and 260 —
**six distinct channels in one eight-neighbourhood**, widths 1.5922e-5 to
4.5276e-5. The `Fordable` step at depth 16 is the line-1106 pair, 1.6007e-5
against a 1.9635e-5 step. The clause was deciding correctly about water the
filter never looked at.

**Two by-products of the same dump.** `Vertex(2656)` is not an identity for
"the water" — a grid vertex carries one polyline vertex per run terminating on
it, each with its own discharge and bands, which is how six readings name that
one vertex at six widths; so the old message's "the SAME water" was false for a
second, independent reason. And the discharge filter was on the wrong quantity
too: it read `drainage_at(a.vertex)` while `wadeable` reads it per priced
reading.

**The `sqrt(2)` hypothesis is REFUTED, on two independent grounds.**
`crossing_between` prices `room_edge(a).min(room_edge(b))` with no diagonal
factor, so the old reporter already matched the gate exactly; and 1.411786 x
1.9635e-5 = 2.772e-5 is still under 3.5692e-5, so the factor could not flip this
verdict even if applied. Recording the refutation matters because the tidy
explanation was load-bearing in the parked entry above.

**Alternatives discarded:** narrowing the filter to transects whose sampled
polyline IS every room's winning line — rejected: it would keep the wrong
operand and make `Vertex(2656)` drop out as a premise failure, which is the
shape the brief forbids. Applying a diagonal factor in the gate — rejected: it
does not fix this, and it moves live behaviour on an unruled design question.

**The distinction was already in the file.**
`the_fordable_fraction_of_the_network_is_within_its_interval` computes both §8
clauses "on the reach BEING transected ... rather than on whichever reading a
room happened to win" and says outright that they are "reported, never asserted
on". The width-clause control asserted on them. Same shape as ruling #14: a test
resting on a distinction that used to collapse.

**What the fix makes stricter:** widths and discharges are now per *priced*
reading (interpretable only, matching clause 3); both arms cover every step
`verdict()` prices; and the control's "same water" claim is now **asserted** —
the set of priced polylines must be identical at both depths — where it was
previously assumed from the grid vertex, which cannot carry it. Population
488 of 1600, all 488 flipping, against a floor of 70. The historical rates in
that test's comment (96/400, 37/400) are recorded as history, not baseline: both
were measured with the wrong operand.

**Deferred:** whether §8 should charge a diagonal ~1.41 room edges of water is a
live design question this campaign created. Conservative as it stands, unruled,
and deliberately not decided by a test-fix task.

**Capture actions:** brief and report at
`.superpowers/sdd/2026-08-30-the-pavement/task-absorb-water-{brief,report}.md`.

**Ideonomy:** 2 passes, 1 overturn (the parked entry's own hypothesis).

**Also observed:** `scripts/hooks/pre-commit`'s "two cargo test runs in one
command" guard fires on the TEXT of a command, and refused a `cat > report.md
<<EOF` heredoc whose only two occurrences were inside a markdown table.
Reworded rather than overridden; `HV_TEST_OK=1` was not used.

### #16 [G5, measurement integrity] — the H1 illumination baseline is VOID, and the test staying red is correct

**The failure:** `illumination_hypotheses::the_h1_band_is_still_the_population_the_baseline_was_taken_over`
asserts `left: 81, right: 31` — *"the H1 band changed shape; the bedrock
baseline of 1 distinct colour over 31 cells was measured at b0f20c71 and cannot
be re-taken. Do not compare colour counts until this is explained."*

**Decision:** The test is doing exactly its job and must NOT be re-pinned to 81.
The H1 baseline is void, and re-measuring it is Task 10's already-scheduled work
("H1/H2 with positive controls"). It stays red until that measurement runs.

**Why (precedent cited):** Decision 0016 — a study freezes its hypothesis and
success criteria before the code that would move them. The walk band moving to
`globe_level + 7` took the band's population from 31 cells to 81. A colour count
over 81 cells is not a larger sample of the same measurement; it is a different
measurement. Changing `31` to `81` would make the assertion pass while destroying
the only thing it protects, and the test's own message forbids exactly that
("Do not compare colour counts until this is explained").

**This is the same number as the client-side finding**, and they are one fact:
`clients/game/bin`'s `assert_eq!(checked, 31)` needs the same 81. Neither is a
literal to bump; both are the band population changing.

**Alternatives discarded:** Re-pinning to 81 — rejected above. Marking the test
`#[ignore]` — rejected: `preregistration_guard.rs` is a default-deny scan
requiring every `#[ignore]` in a lab calibration test to name a cost or cite a
decision, and "my campaign moved the number" is neither.

**Ideonomy:** 2 passes, 0 overturns.

### #17 [G5, measurement integrity] — the wetness trunk null's ATTRIBUTION is void, not its finding

**The failure:** `wetness_reading::a_walk_gets_damper_as_it_descends` fails a
PREMISE guard, not an outcome assertion: *"the land sample has no more trunk-band
rooms than the rill-head walks (74 vs 96), so the trunk null cannot be attributed
to the walk population."*

**Decision:** Also Task 10's, and for a sharper reason than #16. The guard exists
to make a *conditional* claim checkable: The Rill's trunk null was attributed to
the walk population not visiting trunk-band rooms. At the finer walk band the
rill-head walks now visit MORE trunk-band rooms (96) than the land sample does
(74), so that attribution is no longer available. The null itself may well still
hold — what died is the reason given for it.

**Why this one must not be "fixed" by flipping the inequality:** the assertion is
load-bearing in the direction it is written. Reversing it to `trunk_in_walks >
trunk_in_land` would assert the opposite conditionality and pass, and the
resulting green would mean a claim nobody made. This is the same shape as the
value-comparing witnesses this campaign already re-founded rather than
re-captured.

**What Task 10 owes it:** re-read whether the trunk null survives on the new
population, and either re-attribute it with the new numbers or record it as
falsified. A null is a result either way.

**Ideonomy:** 2 passes, 1 overturn (my first read was that this was a stale
count, which it is not — it is an inequality between two live populations).

### #18 [G5] — `census_sentinel` cannot be resolved on this machine, and that is a hard boundary

**Decision:** `census_sentinel::the_first_three_census_worlds_match_the_committed_rows`
stays red locally. It compares a live probe against census goldens authored on
lefford, and this is a Mac. `scripts/census-run.sh` fails closed on the hostname
by design (decisions 0063/0079). The resolution is
`make sluice-census BRANCH=<branch> REF=<full-sha>` against a pushed SHA at
pre-merge close — the once-per-campaign refresh the standing rule already
prescribes. Not a defect, not deferrable to a local fix, and not something to
work around.

### #19 [Task 8] — `BAND_B_RUNG` was one rung stale, and the SILENCE was the defect

**The failure:** `clients/game/bin/src/plate.rs`'s `BAND_B_RUNG` — the mesh
depth the game client draws band B (its finest zoom rung) at — read `12` while
`hornvale_locale::walk_depth` had moved to 13 (decision 0511). The client drew a
whole band coarser than the possession it was drawing, and **no client test
failed**: all ~80 readers read the constant, so a wrong constant is perfectly
self-consistent, and `make game-check` held nothing that compared the number to
the sim.

**Decision:** Move the constant, correct its doc, and close the silence on the
client's own side with `clients/game/bin/tests/walk_band_agreement.rs` —
`BAND_B_RUNG == hornvale_vessel::walk_depth(ctx)` against a real
`WorldContext`, plus a checked premise that the live globe level is still
`plate::GLOBE_RUNG`. It asks the sim's function rather than restating the
arithmetic, which is the whole point: re-deriving the offset would pass against
a stale sim as happily as a current one. Observed failing at 12 and passing at
13 before being accepted; cost 5.23 s.

**Why a second guard when the workspace already has one.**
`cli/tests/suite/walk_depth_agreement.rs`'s absolute roster carried this
constant as `Absolute::StaleAt { value: 12 }` — the declaration working exactly
as designed, and now deleted in favour of `Absolute::Tracks`. But that roster
runs under a *workspace* gate, which by construction cannot see
`clients/game/bin` at all: it reads the file as TEXT. A client-side test is the
only thing that can fail in the client's own gate, which is the gate a client
change actually reaches.

**No new dependency.** `WorldContext` and `walk_depth` both come from
`hornvale-vessel`, already a path dependency, and the `LocaleContext` type is
never named — so `hornvale-locale` did not have to be added to reach it.

### #20 [Task 8] — the client's `assert_eq!(checked, 31)` is NOT ledger #16's problem, and the difference decides the fix

Entry #16 ruled that the H1 illumination baseline's `31` must not become `81`,
because a colour count over 81 cells is a different measurement, and added
"this is the same number as the client-side finding … neither is a literal to
bump". The number is the same fact; **the two assertions are not the same kind
of thing**, and treating them alike would have been wrong in one direction or
the other.

`the_perception_overlay_lands_exactly_where_the_raster_puts_that_facet`'s `31`
is a **vacuity guard** on a projection test — its own comment says so ("an
empty band, or a projection that placed nothing, would sail through the loop
above"). It is not a preregistered baseline and nothing is being compared
across time. So the honest fix is neither to re-pin 81 nor to leave it red: it
is to stop stating the count at all and derive it from the wire's own
`scene.radius`, `(2r+1)^2`, which is what an 8-connected purview *is*. Same for
`every_rung_of_the_ladder_is_reachable_and_distinct`'s `6`/`7` (now
`BAND_B_RUNG - GLOBE_RUNG + 1`) and
`stripping_the_escapes_leaves_the_picture_standing`'s `"      _"` (now derived
from the sim's own SGR-stripped picture). #16's ruling stands untouched for the
lab baseline it was about.

### #21 [Task 8, measurement integrity] — `mesh_addressing_agrees_with_the_spatial_search`: the deliberate call

**The failure:** 2,912 of 5,000 tiles agreed where the test demanded all 5,000.
Its own doc already recorded that the premise was retired: on the icosphere a
grid-level triangle's corners WERE geosphere vertices, so a point inside a facet
had its nearest mesh vertex among that facet's corners by construction. A
cube-sphere quad's corners are not geosphere vertices, so the guarantee is gone.
The ledger's scratch predecessor flagged this as "DISSOLVED rather than passed
or failed — a design decision, not a constant bump", and left the call to
Task 8.

**Decision: bound it, do not delete it and do not re-pin it.** Deleting drops
the only coverage `terrain_at_tile`'s addressing has. Re-pinning exact equality
pins a claim the geometry cannot support. So the exact-equality assertion is
replaced by the two claims that are true, both **measured before being written
down**:

1. the addressed vertex sits at most `MAX_ADDRESSING_EXCESS_SPACINGS = 1.5`
   grid spacings farther from a tile's own centre than the true nearest vertex
   does — measured max **1.1051**, mean **0.1651**. Not a ratchet: a breach
   means the wrong facet was resolved.
2. exact agreement stays at or above `MIN_ADDRESSING_AGREEMENT = 0.55` —
   measured **0.5824**. A ratchet; raising it is always allowed.

**Why both clauses.** They catch different failures, and either alone is weak.
Clause 1 catches addressing that resolves the wrong facet altogether. Clause 2
catches addressing that resolves the right facet and then picks badly among its
corners — a mutation returning the first corner unconditionally stays inside
clause 1's bound.

**What is no longer proved, stated at the site:** the drawn terrain is not
guaranteed to be the terrain at the tile's nearest mesh vertex. Whether that
matters visually is spec §7's H3a; this test bounds it rather than deciding it.

**One more instance of the campaign's own headline finding.**
`a_tile_resolves_to_the_facet_that_contains_it`'s discrimination guard fired
correctly — 1 distinct facet over 16 tiles — because it sampled the chart's
top-left corner, i.e. the north pole, where the square lattice puts a whole 4×4
tile block inside one facet. Moving it to the equator fixed it AND exposed a
collapsed distinction underneath: with `origin_row: 0` the test's own
`unproject(row, col, …)` and `terrain_at_tile`'s `unproject(origin_row + row,
…)` were the same expression, so nothing could tell them apart. The equator
offset pulls them apart, and the test now goes through the window origin the
way the function does.

### #22 [G5, measurement] — H1's floor is RE-FOUNDED and H1 is CONFIRMED; ruling #16 was too pessimistic

**This supersedes the disposition in ruling #16.** #16 said the H1 baseline was
void and the test should stay red until re-measured. The first half was right
about the NUMBER and wrong about the ARM, and the distinction is the whole
finding.

**The decisive question, answered by measurement and against my expectation:**
the 81-cell band is a physically **DIFFERENT region**, not the same angular
region sampled more densely. Band extent is defined in BFS rings at the walk
depth, never in radians, so it moved with the depth: 31 cells over ~47 km²
reaching ~4.3 km became 81 cells over ~121 km² reaching a measured 6.4905 km,
with per-cell areas within 2% of each other — decision 0511 chose the depth to
preserve step length and preserved cell area with it. **Resolution unchanged,
region 2.55x larger.** My brief offered two branches and warned against picking
the tidier one; the implementer measured and picked the other.

**Then it found a third option neither branch contained.** The bedrock *number*
is unrecoverable, but the bedrock *arm* is not: `b0f20c71`'s `reflectance_at`
body was `lithology::reflectance` at the dominant corner, integrated, and every
call in that path is still `pub`. So the control re-runs over today's band.
Measured: **mixture 3 distinct colours, bedrock 1, over 81 cells** — floor and
ceiling both hold, H1 CONFIRMED on a re-derived control rather than a
remembered one.

**Why leaving `BEDROCK_BASELINE = 1` would have been the worst option, and it is
subtler than "stale literal":** a larger region is *likelier* to span a second
rock class, so band growth alone could satisfy `> 1`. The assertion would have
gone green on the confound instead of the effect. That is a stronger objection
than the one #16 made.

**Capture:** `windows/scene/tests/common/mod.rs::bedrock_colours` is the
re-derivable arm; `illumination_probe.rs`'s "only moment this number can be
taken" comment is corrected in place, because saying only its true half is what
would send the next reader back to "unrecoverable".

### #23 [G5, measurement] — the wetness trunk null is FALSIFIED downward, and its conclusion survives strengthened

**Decision:** The guard is removed rather than flipped, and what replaces it
asserts the trunk term's materiality directly.

**The attribution reversed, measured:** rill-head walks enter a trunk band on
**96/448 (21.4%)** against the land spread's **74/1048 (7.1%)** — 3.0x, where
The Rill's reading required them to be trunk-POOR. So the conditional the guard
protected no longer exists, and flipping the inequality (ruling #17 forbade it,
correctly) would have asserted a claim nobody made.

**And "adding the coarse trunk changes nothing" is itself false.** Implemented as
two arms through `grounded_wetness`: the trunk term is active on 96 rooms,
reverses **49 of 420** step verdicts, and moves the descending-step fraction
**0.7405 → 0.6452** (swapped in) or 0.6286 (nearer of the two) — *away* from
R-7's 0.80 floor.

**So the finding is stronger than the caveat it replaces.** "The coarse trunk
does not rescue R-7" was a conditional resting on an untested population; it is
now unconditional and measured, with the trunk actively costing the fraction.
R-7 remains falsified on the emitted axis (0.5262). A null that gets *more*
robust when its stated reason dies is worth recording as such.

### #24 [Q] — the lexicon ceiling is RAISED, with the human reason the guard requires

**Decision:** `docs/audits/lexicon-inventory.tsv` rises on exactly two rows —
`windows/scene/tests/common/mod.rs` 2 → 17 and
`illumination_hypotheses.rs` 37 → 57.

**Why:** the guard's own rebaseline header says *"A number may fall freely.
Raising one needs a human's reason."* The reason: the H1 band's population is
the subject under measurement and `SurroundsScene.cells` is the field's own
name, so these occurrences are the AREA/collection sense the inventory already
admits, not the mesh-VERTEX sense the guard prohibits.

**Alternatives discarded:** rewording to synonyms — which was the CORRECT call
earlier this campaign in `furnishing_marks.rs`, and is the wrong call here. The
difference is whether the word is load-bearing: there it was incidental prose,
here the measurement is literally a count of cells. A ratchet aimed at one
defect should not be allowed to degrade accurate prose about a different one.

**A THIRD ROW ROSE, mine, and it takes the same reason.**
`windows/scene/examples/illumination_probe.rs` 20 → 22, from the correction to
its "only moment this number can be taken" comment (ruling #22). The added
occurrences are "the 31-cell band became an 81-cell one" — the band's population
again, the collection sense. `clients/game/bin/src/plate.rs` also FELL 46 → 43
from the rung re-measurement, which needs no reason: a count may fall freely.

**Ideonomy:** 1 pass, 0 overturns.

### #25 [G5] — I substituted decision 0491 for 0456 twice, in two documents

Caught by Task 10's implementer. **0456** is *a rule stated in two places needs
a bidirectional agreement test*; **0491** is *a stated blindness gets a visible
ratchet, not a silent fix*. Ruling #1 cited 0456 correctly for the agreement
test and then glossed it with 0491's sentence; the deferred-minors list
attributed 0491's content to 0456 outright. Both corrected in place.

**Why it is worth a numbered entry rather than a quiet fix:** a wrong number is
caught by anyone who opens the record. A right number with a borrowed gloss
survives review, because the citation checks out — and this campaign's whole
first day was spent undoing a decision record that read as settled. Two
instances is a habit, not a slip.

## Fix rounds after the final review

### #26 [G5, HIGH] — the campaign reintroduced the defect class 0141 existed to prevent

**The finding, from the whole-branch review and reproduced by me through the
shipped CLI before acting on it:** at seed-42 room `FacetId(2169509120)`,
`go E` walked WEST. Longitude −34.9969 → −35.0079, while `go W` went to
−35.0131. Both decreased. `look` reported `E` as open, so the prose and the
movement agreed on the same falsehood — precisely the "one-turn observable
falsehood" decision 0141 was written to remove, reintroduced by 0506 which
supersedes it.

**Cause:** `heading_rose` matched compass words to neighbours GREEDILY — sort all
(word, neighbour) pairs by angular error, accept each pair whose word and
neighbour are both still free. That guarantees CARDINALITY and bounds nothing.
Each acceptance consumes a word *and* a neighbour, so the final pair is forced:
whatever word is left is stapled to whatever neighbour is left. 5.96% of rooms
carried a word >45° off, 0.82% >90°, worst 156.1°.

**Decision:** replaced with an exact lexicographic min-max assignment over the
8x8 cost matrix. Worst error **156.5155° → 34.577273°, zero rooms past 45°** in
either population (a uniform 12,696-room grid and an exact enumeration of ring 0,
196,584 rooms). 11.6 µs/call against greedy's 5.9.

**THE REAL FINDING IS THE MISSING ASSERTION, not the bug.** No test anywhere in
the workspace looked at angular error. Every assertion about the rose checked
that each neighbour received exactly one word — the one property greedy can never
fail. `ROSE_WORST_DEG = 34.578` is now pinned two-sided (floor 33.5, whose
failure message says to BANK an improvement rather than absorb it), with five
blindnesses disclosed per decision 0491.

**Why it survived review until the end:** a rule wrong everywhere gets noticed; a
rule perfect seven times in eight reads as correct. Seven of the eight words at
that room were 0–23° off.

**MY OWN CONTRIBUTION TO IT, recorded because the shape recurs.** I measured the
greedy mismatch mid-campaign — 1,216 of 4,800 samples, 25.3% — and used it
correctly, to reject keying movement cost on the compass word. Then I stopped. It
was evidence about the ASSIGNMENT RULE and I read it only as evidence about the
decision I happened to be making. A number interesting enough to change your mind
is interesting enough to explain.

### #27 [G5] — the invertibility trade, and the argument for it that was WRONG

Non-inverting pairs rose 82 → **190 of 12,282** (0.668% → 1.547%), pinned
two-sided.

**The trade is right; the first argument for it was refuted.** Fix round 1's doc
(and my own relay of it to Nathan) said 190 was "a property of the mesh plus any
per-room rule." The re-review refuted that **from the doc's own table**: greedy is
also a per-room rule and scores 82, and a non-bijective nearest-word rule
measures 14 of 12,282 (0.114%).

**What 190 actually is:** 14 is the mesh's floor; the other 176 are the price of
the **bijection** — every neighbour gets exactly one word, every word names at
most one neighbour. Fix round 2 then MEASURED what dropping the bijection costs
rather than asserting it: 152 of 1,536 rooms (9.9%) non-bijective, 2.0% of steps
reachable by no unambiguous word — `go E` either refuses or picks one of two
rooms. Worse to hand a player than a one-word round-trip failure.

**The real shape is a trilemma: bijection, bounded per-room accuracy,
invertibility — any two, not all three.** The 45°-bucket rule took accuracy and
invertibility and lost bijection (duplicate/missing letters). Greedy took
bijection and invertibility and lost accuracy. This rule takes bijection and
accuracy. Nobody had named the triangle in three campaigns of moving around it.

### #28 [G5, MEDIUM-HIGH] — a bound loosened 42% under a constant that never changed

`plate.rs` measured the addressing excess in "grid spacings" and fix round 1
switched the divisor to the **cube's** facet arc — while the vertices being
measured are the **icosphere's**, because 0506 deliberately kept the icosphere as
the field substrate. π/2 ÷ acos(1/√5) = 1.4188, so:

- the reported improvement 1.1051 → 0.7487 was **a change of unit, not of error**;
  the true figure is 1.0622 real spacings, ~3.9% better rather than 32%;
- `MAX_ADDRESSING_EXCESS_SPACINGS = 1.5` therefore came to admit **2.13 real
  spacings** while its doc still claimed it excluded a second one — a ~42%
  loosening of a live bound, invisible in a diff because the constant's value
  never moved.

**Decision:** the divisor is `min_edge_rad(&geo)` — **measured off the mesh**
rather than derived from a base angle — and the bound is re-set from the
re-measurement to **1.25**. Fix round 2 re-ran it rather than converting the old
figure arithmetically, which was the explicit instruction, because carrying a
number across a change of unit without re-measuring is the whole defect.

**I RELAYED THE FALSE IMPROVEMENT TO NATHAN AS FACT.** I accepted a ratio without
checking what its denominator meant — hours after writing that exact failure
shape into my own notes. `MIN_ADDRESSING_AGREEMENT`'s lowering 0.55 → 0.50 is
still correct (2,518/5,000 against 2,500, deterministic over a fixed world and
window, so it cannot flap) but the justification I gave for it was not.

## Parked findings from the final review, promoted at close

The whole-branch review raised 21 findings. Four were fixed in fix round 1
(F1, F2, F3, F7) and five more in fix round 2 (N1-N5). **The seventeen below
were NOT addressed**, and are recorded here because the review report itself is
git-ignored scratch that dies with the worktree. Severities are the reviewer's.

**F8 (MEDIUM) is the one a player would notice** and it should be read first.

### MEDIUM — worth a campaign or a deliberate decision

- **F8. The ASCII surrounds render now hides up to 41% of the band, and no
  assertion bounds it.** The band went from a 31-room triangular ring to an
  81-room square block, but the polar (bearing, distance) placement is
  unchanged, so it no longer has room for the cells. Measured: flagship
  53 of 81 drawn / **28 occluded** (was 31 of 31, 0 occluded); coastline 63 of
  81. This is the campaign's most user-visible unhandled consequence — the
  renderer is showing two thirds of a band it used to show whole — and it is
  the same root cause as `CLIENT-chart-needs-no-projection`: a polar projection
  built for triangles, kept over squares.
- **F9. Three preregistered properties from the deleted `course_properties.rs`
  are unreplaced, and the new rule is where they would bite.** Two of its five
  tests are genuinely meaningless (no carried course exists). Three are not —
  notably `no_lateral_refusal_survives_anywhere_on_the_walk_band`, which was
  `claim: invariant(forall-seed)`-tagged over >=200 rooms and >=8 seeds
  **through the real `Session::go`**, and whose nominated successor does not
  exercise that path. Dissolving a suite is legitimate; dissolving the two that
  died and silently dropping the three that did not is not.
- **F4. `map_out_names_the_drawn_rooms_own_exits_not_the_walk_depths` no longer
  discriminates.** The old form pinned two exit triads and asserted them
  DISJOINT, which is what made a footer leaking walk-depth exits onto a coarser
  chart fail. The new form asserts each footer equals its own room's corners,
  and `assert_ne!(fine_room, coarse_room)` checks the rooms differ, not their
  exit sets. Same shape as ruling #14: the discriminating half was dropped.
- **F5. A tolerance widened by eight orders of magnitude (1e-9 -> 10%) on the
  one function this campaign changed.** The new analytic ruler cannot be exact
  and 10% still catches the depth hazard — but the old 1e-9 also pinned
  `room_spacing`'s FORMULA, and that formula changed in this campaign. The
  function has no production caller, which is why nobody felt it.
- **F6. `every_cell_of_the_seam_band_is_drawn_including_the_seam_cells` no
  longer distinguishes dropped ground from occluded ground.** Old form:
  `31 of 31 drawn, 0 occluded`, which entailed all 12 seam cells drew. New form
  asserts only `drawn + occluded == 81`, so `drawn=45, occluded=36` passes with
  nothing checking a seam room is in the drawn bucket.

### LOW-MEDIUM and LOW — cheap, and mostly documentation

- **F10.** A tautological assertion (`f(x) == f(x)`) where `assert_eq!(walk_depth(&ctx), globe_level + 6)` previously pinned the offset. Avoiding a second copy of the arithmetic is right; the result has no content.
- **F11.** Stale arity in a live `.expect`.
- **F12.** Stale references to the deleted `course` module, one of them in brand-new code.
- **F13.** A settling gate raised 67%, disclosed rather than hidden.
- **F14.** Decision 0510's justification does not survive the compass layer, though its conclusion does. A decision record whose reasoning has lapsed is exactly what cost this campaign its first day (0141); worth an amendment.
- **F15.** Two small soft spots in the new octile suite.
- **F16.** The "pre-epoch session fails loudly" claim is probabilistic and untested.
- **F17.** `region.rs` silently moved from the naive to the warped projection.
- **F18.** (pre-existing) `clients/CLAUDE.md` still describes the Orrery as a live cross-repo consumer, which decision 0356 retired.
- **F19.** (bookkeeping) The decision count is nine, not eight.
- **F20.** (pre-existing) `docs/audits/tier-comparison-spike.txt` is a declaration defect with newly stale content; fix round 1 reproduced the drift and reverted rather than landing it.
- **F21.** A `plate.rs` equality assertion is now empirical rather than derived.

### Also parked, from elsewhere in the campaign

- **A flake nobody owns.** `repertory_corpus::no_scene_has_fallen_below_its_recorded_floor` fails under full-suite load and passes in isolation, reproduced on a stashed clean tree; green in four full runs of mine. A flaky red in the chamber's gate phase is indistinguishable from a real one.
- **Nothing gates `examples/`.** Two example binaries panicked outright on the new faces (rc=101 each) while all three gates stayed green. Fix round 1 repaired both and deliberately did not add a gate; the options and their costs are in its report.
- **`docs/decisions/README.md` indexes 226 of 287 records** and `main` is missing the same 61, so no branch caused it. The generated in-force index is complete; only the hand-maintained entry point drifts. Board post `4e0aaf79b`.

## Two findings Nathan raised at close, 2026-09-01

### The water label's resolution is 18x coarser than the band it paints

**Filed as `CLIM-water-label-resolution-vs-walk-band` (confidence: high).**
Nathan noticed the area east of seed 42's start renders like open water despite
being tropical seasonal forest. It does, and `+` is literally the RIVER glyph
(`windows/scene/src/surrounds_ascii.rs:73`), so the render is asserting river
rather than merely looking wet.

Measured:

```text
  start room 3733133217   tropical seasonal forest, 156 m above sea level, on a rise
  east neighbour          tropical seasonal forest, 153 m
  level-6 geosphere edge  0.01729920 - 0.02067341 rad = 110 - 132 km (mean 120)
  walk band, radius 4     6.4905 km across, 81 facets, ~121 km^2
```

`WaterKind` is classified per level-6 geosphere vertex and the `water` layer is
**nearest-vertex, not interpolated** (`windows/scene/src/region.rs` says so in
the doc main corrected during this campaign's absorption). So a band roughly a
EIGHTEENTH the size of the gap between data points inherits one point's verdict,
with no gradient available to soften it.

**Not this campaign's defect, and that is the uncomfortable part.**
`windows/CLAUDE.md` already records "the flagship walk band as 100% river on all
five seeds sampled (42, 13, 7, 1, 100)" — written down as *context for a
rendering question* and never interrogated. A measured absurdity sat in a
guidance file as a supporting detail. Same shape as ruling #26: a number
observed, recorded, and read only for the question in front of the observer.

**The general defect it exposes:** nothing anywhere asserts that a consumer's
spatial scale matches its field's. Decision 0038 makes fields resolution-free,
but a discrete per-vertex LABEL is not a field in that sense, and the two are
consumed through the same surface.

### Rendering a square grid as a square grid

**Extends `CLIENT-chart-needs-no-projection` with an implementation sketch.**
Finding F8 (up to 41% of the band occluded) and this are one problem: the
renderer computes each facet's bearing and distance and drops it into a
character box, "one row per ring." That was right for a triangular ring. For a
9x9 block it is a lossy round-trip through polar coordinates out of data that is
already a grid, and it collides two facets into one box 28 times at the
flagship.

The facets already carry `(x, y)` on a cube face, so the whole placement is:

```text
  row = y - y_centre
  col = x - x_centre
```

No trigonometry, no bearings, no collisions, and **strictly less code than the
polar path**. It also dissolves F8's missing-assertion problem rather than
satisfying it: a direct index cannot occlude anything, so there is no ratio left
to bound.

Two real wrinkles, neither hard: a band straddling a cube seam holds facets on
two faces whose axes differ (that is `NAV-two-frames`, and the honest answer is
to SHOW the seam rather than smooth it), and terminal cells are about twice as
tall as wide — which is what the existing "east doubled" hack already handles
and can keep handling.
