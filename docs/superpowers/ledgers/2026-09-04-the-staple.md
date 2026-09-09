# The Staple — decision ledger

Campaign: **The Staple** — a metaplan for how a dwelling becomes a city.
Opened as `The Precincts` (Rose Window rung 2) and renamed at entry #4, once the
measurements showed the district rung had no subject yet.
Branch: `campaign/the-staple`. Decision block: **0706–0715**
(reserved 2026-09-04; verified untaken on `main` — ceiling 0698 — and on every
`origin/campaign/*` branch, none of which carries any `07xx` record).

## Task 0 record

Probe: `windows/worldgen/tests/suite/brief_axis_probe.rs`, run at
`f20fdbecb`. Output captured to `.superpowers/sdd/baselines/precinct-axis-probe.txt`.
Population: **4,002 occupations** (1,006 alive, 2,996 ended) over seeds
42 / 7 / 13 / 100 / 1234.

## Entries

#1 [Q] — **Is the metaplan's §1b.5 worked example reachable?** ·
**Decision: no — measured, not inferred; the campaign's premise is refounded on
the axes that actually vary.** ·
Why: §1b.5 states the rung-2 target as "a `Trade` + `Seat` + `Classical` +
high-population coastal site draws docks, warehouse row, market, curia, temple
precinct, uptown villas, tenements, walls and gates, an extramural suburb"
against "a `Backwater` `Agrarian` `Neolithic` site draws three patterns and is a
hamlet". The owner's own brief asked this be checked early rather than assumed.
Measured over 4,002 occupations:

```
  axis              measured                                       verdict
  ---------------   --------------------------------------------   ------------
  notability        Common 4002 / 4002 (alive AND ended)            CONSTANT
  function          Agrarian 98.6%, Mine 1.4%; Trade/Cult/Fort 0    NEAR-CONSTANT
  tech              alive: Classical 1006/1006; ended: all four     TEMPORAL ONLY
  peak_population   max ever 86 against a 150 hamlet ceiling        NEVER CLEARS
  people            12-15 distinct alive per world, well spread     VARIES
  coastal           19-45% of alive occupations                     VARIES
  strata (ruins)    0-23 ended occupations beneath a living one     VARIES
```

Both halves of the worked example are unreachable, and in both directions:
`Seat`, `Backwater`, `Trade`, `Cult`, `Fort`, `Neolithic`, `Bronze` and `Iron`
never occur on a living occupation, and `Trade & Seat & Classical` is 0 on every
seed. Root cause is two hardcoded literals at the one founding site,
`windows/worldgen/src/history_bake.rs:2682` (`function: Function::Agrarian`) and
`:2686` (`notability: Notability::Common`) — the axes are not derived at all.
`tech` alone is derived, by `tech_for(year + tech_offset)`, which makes it a
**world clock rather than a place axis**: it separates eras, never two
contemporaneous places, and only one era is ever observable.
Alternatives discarded: (a) taking decision 0398's `needs_populous` measurement
as sufficient — it covered one axis of five, and the four-axis result is a
different finding; (b) inferring reachability from the enum definitions, which
is what the metaplan did. ·
ideonomy passes / overturns: **1 pass, 1 overturn** (see #2). ·
Capture: this entry; the probe; leads the G3 flagged section.

#2 [G1] — **What should a district vocabulary be indexed on instead?** ·
**Decision: recommend indexing on `people`, on the ground, and on the
stratigraphy — not on polity class.** ·
Why: an ideonomy pass (operators: dimension-identification, combination;
organon: lattice; prompts: intentionality, animacy, reversibility) **overturned**
the conclusion #1 was heading for. The pass's animacy prompt asked which index is
alive rather than abstract, and surfaced the one brief axis the probe had not
measured: `people`. Measuring it found **12–15 distinct peoples alive per world**
(seed 42: kobold 130, bugbear 63, hobgoblin 40, wood-elf 29, hill-dwarf 26,
drow 24, …) — the richest axis available, already on `Brief`, and read by
nothing (`windows/vessel/src/brief.rs` module doc: "`tech` and `people` are
carried and not read at all"). It is also the axis the autonomy ladder's own
culture argument names: §1a.6, "a people's houses look alike, and that is what
makes them a people's houses." The reversibility prompt independently re-derives
the campaign table's `no` in the epoch column: a district assignment that is a
pure reading is reversible and costs no epoch, which is exactly The Plat's
discipline (decision 0646) one band up. ·
Alternatives discarded: (a) **fix the history bake first** so notability and
function vary spatially — the honest root cause, but it moves world identity
(epoch + census re-baseline + study pins → invariants) and is a history-domain
campaign, not this one; registered rather than absorbed. (b) **ship the
vocabulary on the dead axes with a synthetic seam** (decision 0398's `draw_from`
shape) — honest about the grammar's capability but delivers nothing the campaign
table's "a city has parts" promises. (c) **rung 3 (a constraint solve)** — does
not help: a solver over a degenerate index still produces one kind of place. ·
ideonomy passes / overturns: **1 pass, 1 overturn** (the pass moved the answer
from "the axes are dead, build the seam" to "one axis is alive and unread"). ·
Capture: this entry; the G3 flagged section; an idea-registry row for the
history-bake half.

## Follow-ups

- `domains/history/src/flesh.rs:601` `structures_of` is a settlement-scale
  composition that already exists, and **six of its eight `Structure` variants
  are unreachable** for exactly the reason #1 measures: `Market` (Trade),
  `Shrine`/`Temple` (Cult), `Wall` (Fort) need functions that never occur, and
  `Longhouse` needs `peak_population >= 200` against a measured maximum of 86.
  Every settlement in every world is 1–3 `Hut`s plus a `Granary`. Consumed by
  `windows/almanac/src/history.rs:798`.
- `domains/history/src/flesh.rs:538` branches on `Notability::Seat` and
  `:466`/`:543` on `Function::Fort`/`Cult` — dead branches by the same cause.
- `windows/vessel/src/interior/pattern.rs` `role_for` index 2 selects `Hall` on
  `Seat` and `Shrine` on `Cult`; both appear unreachable in the shipped path.
  Verify before relying on it either way.
- `windows/vessel/src/liveness.rs:8666` `built_rooms` gives a settlement
  **exactly one walk-band facet** (1.126 km side) and names the gap in its own
  doc: "widening it to a settlement's outskirts or worked fields is a real
  question … a later campaign's to ask". The Precincts is that campaign.
- `windows/vessel/src/structure.rs:43` and `:104`, and
  `windows/vessel/src/session.rs:7073`, each name The Precincts as the campaign
  that must revisit `links` being a path graph in depth order
  (`Session::further_in` relies on it).
- The Circuit metaplan's non-goal "The Plat records what it learns for it" was
  unpaid — `precinct` appears nowhere in The Plat's chronicle, retrospective,
  decisions or registry rows. Paid here rather than left as archaeology.

#3 [Q] — **Owner reframing (2026-09-04): a city is the apex of a supply
network, and Hornvale has no network; also, chambers spring into being fully
formed and may not make sense as bugbear dwellings.** ·
**Decision: the work is a LADDER, not a campaign; the scope question becomes
which rung starts.** Both observations verified:

- **`Brief::people` is written at exactly one site and read at ZERO.**
  `windows/vessel/src/brief.rs:296` writes it; no read exists anywhere in
  `windows/vessel/`. A bugbear warren and a human cottage are byte-identical
  given the same seed. `brief.rs`'s own module doc already said so ("`tech` and
  `people` are carried and not read at all") and nothing had drawn the
  consequence.
- **A structure's shape is drawn from the locale seed alone.**
  `windows/vessel/src/structure.rs` — "The brief is a GATE and never a parameter
  of the draw" — so a cave and a village get identically distributed structures,
  and `role_for`'s only live input is chamber depth (its `notability`/`function`
  arms select `Hall` and `Shrine`, both unreachable per #1).

**The reframing's premise is PARTLY WRONG, and the correction is load-bearing:
a subsistence flow between communities already exists.**
`history_bake.rs:3135-3136` — `communities[sub].population -= remittance;
communities[rel.patron].stores += remittance` — moves surplus from a
subordinate community to its patron, bounded by `min(assessment, harvest ×
(1 - conceal))`, with a `FLIGHT_BURDEN` threshold at which the vassal flees, a
seasonal granary at 12 phases/year, and a calibrated `ASSESS_RATE` coupled to
`GROWTH_RATE`. So flow exists — by **coercion**, for **one resource**,
between **points**. What is missing is exchange (no market, no price, no
specialization), any resource but food (no fuel, water, stone, timber), and a
hinterland (`territories` is a set of settlement vertices, not worked land;
`is_built` is one facet). ·
Alternatives discarded: treating the reframing as a scope veto on cities
(it is a sequencing correction, not a veto); and my own #2 recommendation that
`function` derives from a catchment — **superseded**: function is a consequence
of exchange, so it is downstream of a flow rung, not derivable beside it. ·
ideonomy passes / overturns: 0 — this entry records an owner reframing and its
verification, not a decision I resolved; the rung choice it opens is #4. ·
Capture: this entry; the ladder goes to the owner as a decomposition.

#4 [G1] — **The metaplan's spine, its name, and what code a prose campaign may
land.** · **Decisions, all three owner-approved before adoption:**
(a) organize as a **wheel of two arcs under a partial order** — a READING arc
(no epoch, pure functions, independently orderable) and a DYNAMICS arc (every
rung an epoch, strictly sequenced), closed through decay and refounding;
(b) rename the campaign **The Precincts → The Staple**, keeping `The Precincts`
alive as rung R3 rather than retiring it;
(c) **commit the Task 0 probe** as an `#[ignore]`d readout rather than leaving
it in scratch. ·
Why: (a) an ideonomy pass (cross-domain re-instantiation, negation; organon
cycle; prompts rate, predictability, side-effect) found that sorting the rungs
by characteristic RATE splits them exactly along the epoch column, and that
the split is load-bearing — R3's machinery is independent of the dynamics arc
even though its subject is not, so the cheapest visible work need not be
scheduled behind four epochs. The same pass's negation step ("it ends at a
city" → "it ends at a ruin") found the missing decay arc, against a measured
2,996 ended to 1,006 alive; its cross-domain step (metabolism: capillaries →
veins → heart) found the missing RETURN flow, which is where `Fort`/`Trade`/
`Cult` actually come from and which superseded #2's catchment-derived
`function`. (b) grepped first, per standing practice: **The Watershed and The
Assize are both already merged campaigns** (322 in `docs/retrospectives/`);
`The Staple` is free as a branch, a directory and a proper noun. It carries
both senses the metaplan needs — the commodity a place lives on, and the
staple town that exists only because a hinterland brings goods to it.
(c) "build nothing" read as "ship no mechanism", not "leave the measurement
unreproducible": the metaplan's every number comes from this probe, and the
Rose Window metaplan's own evidence lived in a scratchpad and went stale,
which CLAUDE.md now warns about. One file plus one mod line; trivially
reversible. ·
Alternatives discarded: a strict seven-rung ladder (would schedule R1 — which
depends on nothing and costs no epoch — behind four epochs it does not need);
organizing by epoch cost alone (honest about the constraint, loses the causal
story); keeping the name The Precincts (the metaplan, the campaign table and
three source comments all name it as the district rung, so the chronicle would
disagree with the code citing it); leaving the probe in scratch. ·
ideonomy passes / overturns: **1 pass, 2 material additions** (the decay arc
and the return flow — neither was in the ladder the pass was run on). ·
Capture: the metaplan §2, §2.2, §3; registry rows below.

#5 [Q] — **Owner addition: cities are rivers of people through time; stocks
and flows should be measurable, with prices and climate-driven collapse.** ·
**Decision: adopted as the dynamics arc's organizing frame, and it named a
defect.** · Why: checked against the bake rather than accepted. `Community`
already carries TWO stocks (`population`, `stores`) and ONE flow
(`remittance`), and `stores` is documented *"NEVER eaten … Lost with the
community when it closes"* — a reservoir with one inflow and no outflow but
annihilation. `remittance` subtracts from the subordinate's **population** and
adds to the patron's **stores**, so people convert to wealth 1:1 and wealth
converts back to nothing. So the framing is right and the model is a stub, not
absent: the river runs into a sink. Separately, `caps_by_era` means climate
ALREADY moves capacity through time, so the climate → collapse chain is built
and missing exactly one link — a city's capacity is its own vertex's, so it
cannot be starved by its hinterland failing, which is the same link the return
flow needs (metaplan §3, consequence 3: one mechanism, two payoffs). The price
half is recorded at §3.1 as the furthest-out claim, flagged as measuring
nothing, and it is what would make R3's own vocabulary ("uptown villas,
tenements") a derivable rent gradient rather than an authored one. ·
Alternatives discarded: treating prices as a near-term rung (depends on D2 and
D4 both landing); adding fuel/stone before splitting people from subsistence
(the commodity must be un-conflated first, or a third stock inherits the
conflation). ·
ideonomy passes / overturns: 0 for this entry — it records an owner addition
and its verification against the code; the pass that shaped the arc it joins
is #4. ·
Capture: metaplan §3 and §3.1; `SOC-staple-ladder`.

## Capture manifest

- `book/src/frontier/idea-registry.md` — **new row `SOC-staple-ladder`**;
  `SOC-dense-settlement` corrected (its cited enforcer `Bake::vacant_habitable`
  no longer exists; `vacant_for` keys by `(vertex, rung)`, so only the
  same-rung half stands); `CLIENT-district-patterns` updated to record that the
  district rung is unbuilt for want of a SUBJECT, not a composer.
- `windows/worldgen/tests/suite/brief_axis_probe.rs` — the Task 0 readout,
  rostered in `cli/tests/fixtures/world-build-sites.tsv` as `identity:1`
  (it measures a five-seed distribution; four of those seeds have no fixture,
  so `seed_42_world()` cannot serve it — and a single world would be an
  anecdote, which is the whole point of the probe).
- Three commit-gate ratchets caught defects in this campaign's own additions
  and all three were fixed rather than worked around: `claim_shape` (a seed
  loop with no declared claim shape), `docs_consistency` (three registry Idea
  cells over the 600-char budget), `world_build_sites` (an unrostered world
  build).

#6 [Q] — **Self-correction to #1's registry finding, made before the G3 stop.** ·
**Decision: credit decision 0145; the ROW is stale, the decision log is not.** ·
Why: #1 and the first draft of metaplan §1.3 said the cited enforcer no longer
exists and left the impression the change went unrecorded. Checked: **decision
0145** (2026-08-18, relating 0102 and 0143) ratified the re-key to
`(CellId, DelveRung)` explicitly. The correct finding is narrower and more
useful — a ratified amendment landed and the live index never absorbed it, and
nothing mechanical propagates one to the other. Caught by grepping for
supersessions of 0102 rather than trusting my own correction, which is the
standing rule that a correction is itself unaudited text. ·
Alternatives discarded: editing decision 0102 (append-only — supersede, never
edit; and 0102 correctly records what was true when it was ratified); amending
the pushed commit (a force-push is an owner carve-out, and a follow-up commit
costs nothing). ·
ideonomy passes / overturns: 0 — a factual correction with one right answer,
not a choice between candidates. ·
Capture: metaplan §1.3; the `SOC-dense-settlement` row.

## Close (post-G3)

#7 [Q] — **The book freshness sweep found no book lag and one METAPLAN gap.** ·
**Decision: restate D1 — the catchment already exists and the bake discards
it.** · Why: sweeping `book/src/domains/settlement.md` for claims the
measurement might contradict found the opposite — the chapter is accurate, and
it says settlements *partition* a carrying-capacity budget by **catchment**,
which the metaplan's D1 had proposed adding. Checked:
`domains/demography/src/flow.rs` is the terrain drainage algorithm with the
gradient flipped ("people climb the K-gradient as water descends elevation")
and `condense.rs` reads settlements off it as attractors whose catchment
clears a threshold, conserving `Sigma population == Sigma K` exactly. So
**genesis is already a watershed model.** And `history_bake.rs` contains no
read of `flow`, `condense`, `Condensation` or any accumulation: the bake gets
a per-vertex `CapacityMap`, so the catchment deciding where a settlement IS is
discarded the moment history decides how big it GETS. That explains the
measurement — a ~22-person genesis catchment yielding an 86-person bake peak —
and makes D1 a reconciliation of two disagreeing halves rather than a new
mechanism, which is smaller, better founded, and inherits `flow`'s
draw-free integer determinism. ·
Alternatives discarded: leaving D1 as written (it would have had a campaign
build a second catchment beside an existing one — the two-sources-of-truth
shape this repo has found repeatedly); treating the chapter as stale (it is
correct, and it was the metaplan that lagged the code). ·
ideonomy passes / overturns: 0 — a factual finding from the mandated sweep,
with one right answer. ·
Capture: metaplan §1.1 and §4 (D1); chronicle; retrospective.

#8 [G4-equivalent] — **DoD artifacts for a prose-only campaign.** ·
**Decision: The Staple gets both a chronicle entry and a retrospective.** ·
Why: `docs/retrospectives/README.md`'s own conventions say "one page per merged
campaign, written at merge time alongside the chronicle entry (decision 0020)",
and The Staple is a merged campaign — branch, gate, queue. Checked the nearest
precedent and it cuts the other way: `the-rose-window`, a pure metaplan
campaign, has neither, and is not indexed. Treated as an omission rather than a
sanctioned exception, because the README lists no such exception and the
campaign has real process lessons. Also noted: the README index is lapsed —
The Plat and The Wash both merged without a row — so a row was added rather
than the lapse followed. ·
Alternatives discarded: following `the-rose-window` (would propagate an
omission); chronicle without retrospective (the process lessons are the more
valuable half here). ·
ideonomy passes / overturns: 0 — precedent-resolved with a stated conflict, per
the autopilot's clarifying-question rule. ·
Capture: `book/src/chronicle/the-staple.md` + SUMMARY; `docs/retrospectives/the-staple.md`
+ README row; `book/src/open-questions.md` gradient entry.

## R3 — district substrate

#9 [Q] — **R3 lands the smallest evidence-backed district substrate.** ·
**Decision:** districts are pure, basis-specific projections over typed relation
views; they do not create population facts, persons, households, or canon. ·
**Evidence:** commits `a5c7c6c7c`, `40f2dccf2`, `aa3c04a23`, `09e7c4a3f`,
`27ab0c6a6`, `53c5c0f39`, `344bdbca4`, and `3ba8c91d5`; 27 focused probes
and the full worldgen suite pass. · **Alternatives discarded:** a universal
household-shaped district class; fuzzy identity by overlapping members; and
importing Brattice pattern semantics into worldgen. · **Scope:** relation
envelopes, spatial/presence/access/exchange views, deterministic district graph
projection, temporal continuity/recurrence/dissolution/recomposition,
aggregate pattern readout, and synthetic probes. Household, kinship, lifecycle,
gender, and institutional interpretation remain outside R3.
