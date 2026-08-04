# The Fare — weather's charge on a journey

**Campaign:** The Fare (Weather Consequence C2)
**Date:** 2026-08-04
**Status:** spec, pre-plan
**Predecessor:** [The Mire](../../../book/src/chronicle/the-mire.md) (Weather Consequence C1)

## 1. The question

The Mire asked whether weather moves the world's passable geography and
answered no: a median swing of 0.95% of land against a preregistered 5%
floor, running backward from the predicted latitude gradient. That null is
published as the headline and it is not in dispute.

What is in dispute is the instrument. The Mire measured **passability** — a
binary, read as "fraction of land in the largest reachable region" under a
single conductance threshold. Its own chronicle records the limitation
twice: that the metric under-reports a graph that restructures without
changing the size of its biggest piece, and that only ~4% of real directed
land edges ever cross the threshold in a year at all. A large effect on
**cost** — how much slower or harder an open route becomes — could sit
entirely beneath that view and register as nothing.

The Fare re-asks the question through a cost instrument, and adds the
readout The Mire could not take: whether the answer is large enough to
matter to a consumer that already exists.

## 2. Two facts established before this spec was written

Both were verified in code, not inferred, and both are now registry rows.

**The weather gating has no production consumer.** Every `GraphConfig`
construction in the workspace is `::default()`, which is `day: None` — the
unweathered graph. The only `day: Some(..)` in the repository is inside
`windows/worldgen/src/graph_derive.rs`'s own unit test. The Mire built the
capability and measured it directly; nothing in world generation reads it.
(`MAP-weather-gating-is-unconsumed`.)

**A continuous cost-reading consumer already exists and is live.**
`defensibility()` in `windows/worldgen/src/history_bake.rs` reads
`-ln(conductance)`, tanh-shaped about the calibrated constant `DEF_CENTER
= 6.256709`, and multiplies the holder's side of the raid dominance test at
three live call sites. It reads cost continuously, with no threshold. It is
precisely the shape of consumer H1's instrument was blind to, and it runs
today on unweathered conductance.

**Weather never reaches the pathfinder, and the cost field has no
ground-softness term.** Land corridors are planned by `least_cost` over the
**dry** traversal-cost field and weather is applied afterwards, by
`scale_conductance`. That field reads elevation slope only — biome enters it
solely as the marine/impassable test — so a bog and a grassland at the same
slope cost the same. The Mire put mud on the edges of a graph whose roads were
surveyed in dry weather. Fully developed in §5; it is the reason this campaign
routes over a cost field rather than over the graph.

Together the first two say: the question "does weather's cost effect matter?"
has a concrete, already-calibrated place to be answered, and answering it does
not require wiring anything. The third says the instrument has to be built one
layer lower than The Mire's, or it inherits The Mire's ceiling.

## 3. The trap this design is built to avoid

The obvious cost measurement is the per-edge cost ratio between the weathered
and unweathered graphs. It must not be the headline, because it is
**definitional**: an edge's weathered conductance is its unweathered
conductance times the mean of its two endpoints' `weather_conductance_factor`,
so the per-edge cost ratio is exactly `-ln` of that mean. Measuring it
re-derives the input and asserts nothing about the world.

This is the failure shape The Mire's retrospective records as defect #4 — a
keystone test that passed under a mutation that should have broken it,
because it checked a quantity the mutated version also satisfied. Every
readout below is chosen to be non-definitional: each depends on **where** the
weather-exposed edges sit relative to the routes that are actually used, which
is a property of the world and not of the gating formula.

The per-edge ratio is still computed, as a **control**. If the headline
readouts move and the control does not, or vice versa, that is diagnostic.
It is a control, never a result.

## 4. Preregistered hypotheses

Per decision 0016 and the project's preregistration discipline, the freeze
lives here in the spec — a study JSON carries no hypothesis field. The
numeric floors in F1 and F2 are deliberately **not stated yet**; see §6.
F4 is dropped — see its heading below.

### 4a. The sampling frame — geographic pairs, not settlement pairs

**Revised 2026-08-04, project owner's ruling.** F1, F2 and F3 are measured
between **deterministically sampled land-cell pairs**, not between settlement
pairs. Settlement-pair routing is retained as a clearly labelled *secondary*
readout.

Two independent reasons, and the second is the stronger one.

**It removes a cross-campaign dependency.** Every settlement-pair readout is
conditional on placement, which a parallel campaign is about to change wholesale
(§6a). A geographic frame is placement-independent, so the primary measurement
neither perishes nor owes a re-run.

**It removes a real sampling bias, which is the better reason.** Settlements sit
in a handful of tight contiguous carpets on high-capacity river basins — six to
twelve clusters per world, at a one-per-cell floor of roughly 110 km. That is a
badly biased sample of *terrain*. Weather bites hardest on marginal ground —
boggy lowland, snow-loaded upland — which is exactly where settlements are not.
Measuring weather's effect on travel using only routes between clustered
river-basin sites systematically under-samples the terrain where the effect
lives, and would understate every readout in this spec.

The frame: land cells (`!Biome::is_marine()`) drawn by a fixed deterministic
stride, paired at a controlled great-circle separation so the population is not
dominated by trivially short or unroutable pairs. Never random. The stride, the
separation band, and the realised pair count are set from the pilot and frozen
with the floors in §6.

**The settlement-pair readout survives as the perishable one**, and that is
where §6a's forward prediction belongs: scattering settlements should raise
F2 on settlement pairs. Reported alongside, never as the headline.

### F1 — magnitude

*Does weather move the cost of a real journey?*

Least-cost path cost between §4a's sampled land-cell pairs over the
**weathered cost field** (§5) versus the dry one, sampled across days of the
converged year.
The seasonal swing is the max-minus-min across sampled days, expressed as a
fraction of the dry cost.

Non-definitional because it depends on whether a world's cheap routes are the
weather-exposed ones. A world whose corridors run through permanently frozen
or permanently dry ground will show a small swing at any gating strength.

**Why F1 may beat The Mire's 0.95% despite a modest per-cell surcharge.** Path
cost is a *sum along a route*, and the substrate is spatially correlated — a
route crossing a wet region pays the surcharge at every cell of the crossing.
The path-level effect therefore grows with the substrate's correlation length
relative to the route, rather than staying at the per-cell scale. This is the
mechanism that a threshold instrument reading one edge at a time is least able
to see, and it is the specific reason to expect the cost instrument to
disagree with the passability one. Stated as a prediction so that it can be
wrong.

### F2 — re-routing

*Does weather make travellers take a different road?*

The fraction of §4a's sampled pairs whose least-cost **path identity** changes
between that pair's **own** cheapest and costliest sampled day, routed over the
weathered cost field.

This is the qualitatively new phenomenon and it cannot be read off the
per-cell weather term under any transformation — a *uniform* penalty on every
cell changes no path's identity at all, so any nonzero F2 is evidence of
spatial structure in weather relative to terrain. A nonzero F2 with a small F1
is a coherent and interesting result: weather that produces detours rather
than delays. It is `DESIGN-weather-is-journey-scale` made testable.

**F2 is the readout that forced §5's design.** Measured over the weathered
`ConnectionGraph` instead, its ceiling would be set by corridors surveyed on
dry ground — the instrument, not the world. That is a subtler instance of §3's
trap and it is why this campaign routes one layer lower than The Mire did.

**F2 requires a redundancy control, or a zero is uninterpretable.** Re-instantiating
this problem in road-traffic routing and in airline networks gives the same
answer in both: congestion reroutes traffic only where a *comparable
alternative already exists*, and the re-routing rate is governed by the density
of near-optimal alternatives rather than by the size of the penalty. A pair
joined by one dominant corridor will show `F2 = 0` at any weather strength.

That is the same structural zero as The Mire's polar band — a place that
*cannot* vary, reported as a place that *does not* — and shipping it
uncritically would repeat the exact error this campaign exists to correct. So
F2 is reported **conditioned on route redundancy**: for each pair, the ratio of
the second-best substantially-disjoint dry path's cost to the best. A pair with
no alternative within a preregistered redundancy band is excluded from F2's
denominator and reported separately as a count.

A world where most pairs have no alternative is itself a finding — it says
weather cannot produce detours *in this geography*, which is a claim about
terrain and belongs in the chronicle as one.

**Sampling is per-pair, not global.** The swing is each pair's own max-minus-min
across sampled days. The substrate year is periodic but its phase is not
globally aligned — a monsoon peak differs by latitude and longitude — so a
single global "wettest day" would understate every pair whose own peak falls
elsewhere.

### F3 — does the latitude reversal survive a better instrument?

*Was the polar zero a property of the world or of the threshold?*

Re-run F1's swing partitioned by the same latitude bands The Mire used
(equatorial / temperate / polar), and compare the ordering against The Mire's
measured `equatorial 0.0224 > temperate 0.0021 > polar 0.0000`.

**Both outcomes are publishable and one of them amends a chronicle.** If the
reversal holds on cost, `CLIM-variation-needs-alternation` is strengthened
from a threshold artifact to a property of the world. If it collapses, The
Mire's H2 was partly an instrument effect and the chronicle says so. This
readout is the reason F3 is not optional.

The mechanism The Mire identified predicts the reversal **should** hold:
a permanently frozen cell has constant conductance, so its cost is constant
too, and constancy is instrument-independent. F3 is therefore a genuine test
of that mechanism rather than a re-description of it.

### F4 — DROPPED (project owner's ruling, 2026-08-04)

F4 was to recompute `defensibility()` under weathered conductance for the
ordered pairs that actually contested a raid, and report what fraction would
resolve differently against `RAID_MARGIN` — a pure read answering whether the
wiring campaign is worth running.

**It is dropped from this campaign, not abandoned.** It reads pairs that
contested a raid during the history bake, so it is placement-dependent all the
way down and cannot be re-based on the geographic sampling frame §4a adopts. It
would have been the one part of The Fare still hostage to a parallel campaign's
re-placement of every settlement (§6a), and its purpose was always to justify
the wiring campaign — so it belongs to that campaign, measured against whatever
placement is current when it runs.

Consequence: this campaign touches `defensibility`, `DEF_CENTER`,
`RAID_MARGIN`, and the history bake **not at all**. Recorded as a registry row
at close.

### H-control — the per-edge ratio

Reported for diagnosis only, per §3. Never a headline.

## 5. Where weather enters, and why that is the whole design

There are two places weather could act on travel, and The Mire chose the one
that cannot move a road.

**What ships today.** `windows/worldgen/src/traversal.rs` derives a per-cell
`CellMap<u64>` land-travel cost: `BASE_COST` (10) plus the largest elevation
gap from the cell to any neighbour, scaled by `SLOPE_SCALE` (1.0) and
truncated. `traversal_cost_at` is the era-aware twin that decides ocean by sea
level rather than by biome (The Sundering's moving sea). Over that field,
`hornvale_topology::route::least_cost` — a `SearchSpace` (`CellRoute`) solved
by the kernel's `astar` — finds least-cost paths. Its `heuristic()` returns
`0`, documented as deliberate: **it is already plain Dijkstra**, chosen for
correctness over pruning.

**Two facts about that field, both verified in code.**

*Biome is consulted only for the marine test.* `traversal_cost` returns
`u64::MAX` for a marine biome and thereafter reads elevation alone. A bog and
a grassland at identical slope cost identically. There is no ground-softness,
vegetation, or substrate term anywhere in the field routes are planned over.

*Weather never reaches the pathfinder.* In `connection_graph_of`, land
corridors are found by `least_cost` over the **dry** cost field, and weather is
applied afterwards by `graph.scale_conductance(..)`. The ordering is explicit
in the source. So **which corridors exist is decided weather-blind; weather
only reweights roads already surveyed on dry ground.**

**The consequence for F2.** Re-routing measured over the weathered
`ConnectionGraph` can only shuffle among weather-blind corridors. It
structurally cannot answer "would a traveller take a different road," because
no road was ever proposed on wet ground. Measuring F2 that way would be a
subtler instance of §3's trap: a readout whose ceiling is set by the
instrument rather than by the world.

**So The Fare routes over a weathered cost field, not over the weathered
graph.** The campaign builds `traversal_cost` plus a substrate term — a
weathered `CellMap<u64>` — inside the lab harness, and routes over it with the
existing `least_cost`. This:

- reuses shipped, determinism-hardened machinery instead of building a second
  `SearchSpace` over `ConnectionGraph`;
- asks the question F2 was meant to ask;
- **remains a pure read** — `connection_graph_of`, `traversal.rs`, and every
  production path are untouched. The weathered field lives and dies inside the
  study.

### 5a. The substrate→cost transform (authored, frozen here)

The project owner's ruling: the algorithm matters, the constants do not need to
be physically precise. These are therefore **AUTHORED**, and must carry the
`/// AUTHORED` doc-comment convention `history_bake.rs` already uses to
distinguish an authored prior from a calibrated one. No later reader should be
able to mistake these for measured numbers.

**Anchor: difficult terrain costs double movement** (tabletop convention). That
fixes the scale with a citable reference rather than an arbitrary one.

**Form: an additive surcharge, not a multiplier.** The field is
`BASE_COST (10) + slope_term`, where the slope term reaches the hundreds or
thousands on an escarpment. Multiplying the whole cost by a weather factor
would make weather's *absolute* contribution scale with slope — largest on
mountains, smallest on the flat routes travellers actually use. That is a
terrain effect wearing a weather costume, and it would corrupt F1 and F2 in the
same direction. An additive surcharge keeps weather's contribution independent
of relief, so it bites hardest where terrain is cheap, which is where roads go.

**Definition.** Reusing `weather_conductance_factor`'s own inputs, so both
instruments read identical substrate state and differ only in transform:

```
surcharge(f) = round(BASE_COST * (1 / max(f, WEATHER_FACTOR_FLOOR) - 1))
```

`1/f` is thus the movement-rate multiplier: `f = 0.5` yields `+10`, exactly
doubling flat ground — difficult terrain. The weathered field is
`traversal_cost(..) + surcharge(..)` per cell, saturating, and marine cells
stay `u64::MAX` untouched.

**`WEATHER_FACTOR_FLOOR = 0.25`, and weather never returns `u64::MAX`.** This
is the load-bearing clause, not a rounding detail. `weather_conductance_factor`
clamps to `[0,1]` and its penalties sum past 1.0 — a saturated *and* snowed
unfrozen cell yields `1 - 0.6 - 0.7 → 0.0` exactly. Without a floor the
surcharge diverges; mapping it to `u64::MAX` instead would be worse in a
specific way:

- it would conflate "muddy" with "ocean", which is the passability threshold
  The Mire **already measured** — reintroducing it defeats the campaign's thesis;
- `least_cost` returns `None` for an unreachable pair, so weather-impassable
  cells would silently **drop pairs from the sample**, biasing F1 toward the
  pairs that happened to stay connected. A measurement that discards its hardest
  cases and reports the mean of the rest is the failure The Mire's chronicle
  calls "the metric can register a landmass-identity switch as a seasonal
  swing", in a new costume.

At `0.25` the cap is `4x` flat ground and it binds only in the combined
mud-plus-snow extreme — the regime where the underlying physics is least
trustworthy anyway. The ordinary cases sit well inside it: saturated unfrozen
(`f = 0.4`) gives `2.5x`, fully snowed (`f = 0.3`) gives `3.3x`.

**This transform is frozen by this spec.** It is not a pilot-derived floor; it
is an authored model choice, and retuning it after seeing F1 or F2 is the
metric-chasing the preregistration discipline exists to prevent. If it turns
out wrong, that is a finding for the chronicle, not a constant to adjust.

**Costs from sources, not per pair.** `least_cost` is single-target: it routes
one `from` to one `to`. F1 and F2 need costs from a set of source settlements
to many destinations, so running it per pair is `S²` searches when `S` Dijkstra
sweeps would do. Adding the sweep variant to `domains/topology/src/route.rs` is
in scope; it is a generalisation of a shipped function, not a new solver.

**The sweep must return predecessors, and its tie-break must be a pure
function of the cost field.** F2 reads path *identity*, not cost, so the sweep
has to reconstruct paths — and `least_cost`, which does return a path, is
single-target with `heuristic() == 0`, making per-pair use ~`S²` full Dijkstras
per sampled day. That is the reason the sweep carries predecessors rather than
F2 falling back on the shipped function.

The tie-break is load-bearing rather than incidental. F2 compares paths across
two cost fields that differ only slightly, so a predecessor chosen by
expansion order could return different equally-cheap paths on the two runs and
report re-routing that never happened — the campaign would be measuring its
router, which is the precise failure §5 already rejected hierarchical routing
to avoid. The rule is therefore explicit and total: on a strict improvement set
the predecessor normally; on an exact tie, keep the candidate predecessor with
the lower `CellId`. The agreement test asserts optimality and well-formedness
(adjacency, endpoints, summed cost equals reported cost) rather than
cell-for-cell equality with `least_cost`'s path — both are optimal, and their
tie-breaks legitimately differ.

**Hierarchical routing is explicitly out of scope**, and not merely on cost
grounds. A contraction hierarchy or multi-level overlay changes which of
several equal-cost paths is returned. F2 measures path-identity change, so an
approximate or reordering router would contribute its own churn to the exact
quantity being read — the campaign would be measuring its router. If the pilot
shows both flat approaches are infeasible, that is a finding to bring back, not
a licence to approximate. Captured as `MAP-hierarchical-routing`.

**Reuse.** `windows/lab/tests/the_mire_calibration.rs` already builds both
`SubstrateField`s per world (via `compute_pair`) and holds the day-sampling
harness. The Fare extends it rather than standing up a parallel instrument.

**Nothing in this campaign reads the graph.** F4 was the only readout that
would have, and it is dropped (§4). Every surviving readout routes over the
cost field. The `ConnectionGraph`, `defensibility`, and `DEF_CENTER` are
untouched.

## 6. The pilot, and why the floors are blank

The Mire's retrospective records as defect #10 an unspecified threshold that
made the instrument blind: the preregistration said "at the default
`min_conductance`" without stating a value, and the constant first reached for
(0.05) exceeded the *maximum* real edge conductance observed in a pilot
(0.0417). Every edge was below threshold before any weather scaling ran. The
instrument could not have registered a swing of any size.

The mitigation is procedural and it is why §4 carries no numbers:

1. Run a pilot of 5–10 seeds.
2. From the pilot's own distributions, set: the F1 swing floor, the F2
   fraction floor, the F2 redundancy band, the §4a geographic sampling frame
   (stride, separation band, realised pair count), and the seed count.
3. Record the pilot's measured per-world cost for both routing approaches.
4. **Freeze all of the above in this spec, in a commit that predates the full
   run**, and say in the chronicle that the floors were pilot-derived.

No floor in this campaign is inherited from a constant defined elsewhere. A
floor that cannot be traced to this campaign's own pilot is a defect.

**Superseded (2026-08-04): F4 is dropped entirely (§4), so the separate
freeze point it needed is moot.** An earlier amendment moved F4's floor to the
top of its own task, on the grounds that F4 ran on a different and costlier
instrument. Dropping F4 removes the exception with it, and every surviving
floor is frozen once, here, in Task 4. The earlier amendment is recorded as
superseded rather than deleted, because the reasoning that produced it — a
floor may be frozen in its own commit provided it predates the number it
gates — remains sound and may be wanted again.

The pilot also settles the scope question the seed count depends on, which is
why the population is not fixed here. The Mire's 200 seeds is the target for
F3 comparability; whether F1/F2 can afford it is a measured question.

## 6a. What these measurements are conditional on (added 2026-08-04)

**Every readout here is measured between settlement pairs, so every readout is
conditional on the settlement geography.** That geography is scheduled to
change. The parallel campaign **The Keeping** targets the species-blind
habitability gate; its §5 states that every seed re-places, and its H1/H3
predict *more* settlements in *more* clusters than today's 6–12. The Fare's
pilot measured 189–375 settlements per seed at the one-per-cell ~110 km floor
(decision 0102 records that floor as an index artifact, separately removable).

This is a semantic collision `make preflight` cannot detect — it compares
ancestry, not meaning. It is recorded here rather than discovered later.

**The Fare is not blocked on it, for two reasons.** The Keeping is currently
stopped: its own Task 0 falsified its §3, and its successor lattice puts the
world-identity-moving step (B+C, per-species capacity desaturated) behind a
naming pass and ahead of three more. And under decision 0099 re-placement is a
cost rather than a corruption, so there is no epoch ritual coupling the two.

**The mechanism finding should outlive the geography, and that is stated here
as a prediction so it can be wrong.** F2 measures whether weather changes which
road is cheapest. Scattering settlements lengthens the median route, and a
longer route crosses more terrain and more distinct weather. So if The Keeping
lands and re-places settlements more widely, **F2's re-routing fraction should
rise, not fall** — and F1's cost swing should rise with it, for the
correlation-length reason §4 already gives. A post-Keeping re-run that finds F2
*falling* would falsify the mechanism this campaign proposes, not merely
restate it on new ground.

The honest statement for the chronicle: The Fare's numbers are a measurement of
*this* world's settlement geography, and its *mechanism* is a claim about
terrain and weather that should survive re-placement. The first is perishable;
the second is the finding.

## 7. Operational constraints

- **The Mac is the gate box; lefford is the heavy/census box** (decision
  0086). The pilot and the full run are lab work, not censuses. If the full
  run's measured cost pushes it into heavy-tier territory, it is dispatched to
  lefford via `make heavy-remote REF=<sha>`, not run locally.
- **Run the population in `--release`.** The Mire's first full-run attempt
  died at the 60-minute tool ceiling in debug with zero visible progress and
  nothing recoverable. Same seed and pins guarantee byte-identical output
  across build profiles.
- **Per-seed progress logging is mandatory** for any run expected to exceed a
  few minutes, for the same reason.
- **`std::time::Instant` is banned in test code too**, enforced workspace-wide
  by `clippy.toml`'s `disallowed-types` under `-D warnings`. Diagnostic timing
  for the pilot needs a different instrument.
- **`the-commonplace` is live and edits `book/src/frontier/idea-registry.md`.**
  This campaign appends to the same file; expect a conflict at whichever
  merges second and resolve by keeping both row sets.

## 8. Bundled

`domains/terrain/src/lithology.rs:528` re-derives an `Fbm` per cell via
`fbm_2d`, which constructs `Fbm::new(seed, octaves)` on every call. The
derive-once fix is free, bit-identical, and worth roughly 1 ms per world. It
is bundled here rather than branched for, per the standing guidance, because
this campaign touches climate and terrain anyway. It must land in its own
commit with a byte-identity assertion, not folded into a measurement commit.

## 9. Non-goals

- **Wiring the weathered graph into production.** A separate campaign with
  its own spec, because it recalibrates `DEF_CENTER`, a save-format constant,
  and that decision deserves its own G3. F4 — the read that would have sized
  its warrant — now belongs to that campaign too (§4).
- **F4, and with it the whole `defensibility` / history-bake surface.** Dropped
  to the wiring campaign (§4). This campaign does not read `DEF_CENTER`,
  `RAID_MARGIN`, or the bake at all.
- **Sea ice.** `EdgeKind::WaterRoute` stays hard-coded at conductance 1.0.
  `MAP-polar-zero-is-a-land-result` remains open and is the strongest
  candidate to follow this campaign. Note the ordering constraint: sea ice
  changes water conductance, which `defensibility` reads, so sea ice should
  precede any `DEF_CENTER` recalibration rather than follow it.
- **Interannual forcing.** Out of scope, and now known to be more expensive
  than it looked — `spin_up` is *defined by* periodicity, so a drought year is
  not a parameter it accepts (`CLIM-spin-up-assumes-periodicity`).
- **A lazy per-cell substrate path.** Its consumer is `CLIM-body-integral`,
  which has no spec. Optimising for an absent consumer is premature.
- **Any change to the substrate physics.** The Mire's recurrence, its
  constants, and its glacier fast path are untouched. This campaign changes
  the instrument, not the world.
- **Any change to production `traversal_cost`.** The weathered cost field is
  built and consumed inside the study. Adding a ground-softness term to the
  shipped field would change which `LandRoute` corridors exist in every world's
  connection graph — a derived-structure change with its own byte-identity
  blast radius. If F1/F2 say it is warranted, that is the follow-up campaign,
  and it wants its own G3. Captured as a registry row at close.
- **Re-running The Mire's H1/H2/H3.** Their results stand as published. F3
  compares against them; it does not replace them.

## 10. Definition of done

- F1–F3 measured on the frozen §4a geographic population, with pilot-derived
  floors, plus the secondary settlement-pair readout.
- A chronicle entry (`book/src/chronicle/the-fare.md`) and a retrospective
  (`docs/retrospectives/the-fare.md`).
- Registry rows updated: `CLIM-cost-not-passability` flipped from `raw` to
  `shipped` and repointed; `MAP-weather-gating-is-unconsumed` repointed;
  `MAP-hierarchical-routing` added.
- A freshness sweep of `book/src/open-questions.md` — the Confidence Gradient
  is re-scored if F3 moves a bet.
- **If F3 collapses the latitude reversal, The Mire's chronicle is amended**,
  not silently superseded.
- The bundled `lithology.rs` fix, in its own commit, with byte-identity
  asserted.
