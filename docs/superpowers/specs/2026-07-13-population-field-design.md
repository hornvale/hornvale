# Population as a Field, Settlements as Condensations — Design

**Status:** approved (design), pending spec review
**Date:** 2026-07-13
**Campaign:** the-gathering (MAP-7, field-first half; the era-ticked history
half is a deliberately separate later campaign)

## Problem

Every settlement in a Hornvale world today is placed by a **static
equilibrium**: `domains/settlement/placement.rs` scores each habitable cell
with an ad-hoc suitability formula (a hand-weighted sum of freshwater, coast,
temperance, minus hostility), greedily scatters spaced sites from most to
least suitable, and reads or draws a population number per site. The model
answers "where would settlements be" and never "how many people, and why
there." Its consequences:

- **Population is a free variable.** It is drawn or handed in at the
  composition root, disconnected from any physical account of what the land
  can support. There is no conserved quantity, so nothing catches a world
  whose settlements sum to an absurd population.
- **The suitability formula is ungrounded.** Its weights are tuned to look
  reasonable, not to reproduce a measurable gradient. The Lab cannot grade
  it against anything real.
- **There is no field to build history on.** MAP-7's whole point is that
  history arrives as a *field evolving over deep time*. A static scatter
  leaves nothing for a later relaxation to move — no density field, no
  carrying capacity, no catchments.

MAP-7 in the frontier is a five-part pipeline (carrying-capacity field →
population-density field relaxing toward it → settlements as condensations →
era-ticked history → species dispersal). This campaign builds the **field
half only, at equilibrium**: the physically-grounded field and the
condensation that reads settlements off it. All dynamics — temporal
relaxation, era-ticked founding/growth/abandonment, dispersal, the N=1
endling edge — are a separate later campaign. See *Scope boundary* below;
the seam is deliberate and load-bearing.

## Goal

A new `demography` domain that derives a **carrying-capacity field** from
climate and terrain, condenses discrete settlements out of it as
**conserved attractors of a population flow**, and leaves the residual
density field in memory for the later history campaign to re-flow — with the
size distribution calibrated to the real rank-size law and the
sum-of-populations conserved against total carrying capacity by
construction.

- A `demography` crate (kernel-only) owning the field, the flow, the
  condensation, and the migrated MAP-22 founder floor.
- Settlements whose population is a **readout of the field**, not a draw:
  `Σ settlement populations == Σ carrying capacity`, per species, exactly.
- A settlement size distribution tuned once to **Zipf rank-size** and frozen.
- Carrying capacity that tracks the **real biomass-by-latitude gradient**,
  a calibration-checked claim.
- The suitability-scatter mechanism **retired**; flow-condensation the sole
  path; drift artifacts rebaselined in the landing commit.

## Scope boundary — what this campaign does NOT build

The field/history seam is the central scoping decision. Deferred, by design,
to the later history campaign (MAP-7's second half):

- **Temporal relaxation and the era-tick.** Campaign 1 is pure equilibrium:
  `density = f(K)` in closed form, no iteration, no time axis.
- **All history predicates** — founding, growth, fission, abandonment, ghost
  towns, mother-city lineages. None are coined now; populating them with
  nothing to say would violate the smallest-version discipline.
- **The N=1 endling edge** (MAP-7 registry row) — it only bites once
  population moves through time.
- **Species dispersal appetite / tolerance bands** — wants PSY-1, unshipped.
- **Full cross-species competitive exclusion** (Gause, the might-ordered
  draft, niche differentiation) — this is MAP-22's own campaign. Campaign 1
  keeps only MAP-22's already-shipped **founder floor**, and therefore
  slightly *loosens* today's cross-tag spacing: two species may co-occupy
  geography. This is an accepted, documented regression — the field is the
  better foundation, and exclusion deserves to be done properly in MAP-22.
- **A shared kernel field→fact operator.** The ideation surfaced that
  condensation is the same operation the codebase already performs for
  biomes (Whittaker classification) and rivers (`drainage.rs` extraction);
  lifting all three onto one kernel primitive is real future work but is
  **not** built here. Campaign 1 builds the local instance, and the spec
  names the kinship so the eventual lift is a refactor, not a rediscovery.

## 1. Architecture & layering

A new `domains/demography/` crate, kernel-only (depends on
`hornvale-kernel` and nothing else), sibling to `settlement`. It owns the
field→fact projection; `settlement` remains the naming / ledger-commit /
render presentation and *consumes* demography's output at the composition
root.

```
domains/demography/   NEW  (kernel-only)
  carrying_capacity.rs  K(cell, species) = f(climate, terrain) -> CellMap<f64>
  flow.rs               up-gradient accumulation on K
                        (domains/terrain/src/drainage.rs's shape, comparator
                         flipped: people climb the K-gradient as water
                         descends elevation)
  condense.rs           attractor extraction + catchment partition +
                        conserved population readout
  founder.rs            the MAP-22 founder floor, migrated from
                        settlement/placement.rs (with its tests)
  render.rs             a density/accumulation debug map (PPM), like other
                        domains' render modules
  lib.rs                register_concepts (if any), stream_labels (none
                        expected: the pipeline draws nothing), the
                        DemographyReport in-memory type
domains/settlement/    genesis.rs / render.rs / predicates UNCHANGED;
                       placement.rs's suitability scatter RETIRED; the crate
                       now receives condensed sites instead of scoring cells
windows/worldgen/      wires demography -> settlement; the root assembles the
                       bare per-cell climate/terrain inputs (as it already
                       does for SiteInput) and hands them to demography
```

**Founder-floor migration.** MAP-22's pigeonhole guarantee — every people
keeps its single strongest basin even where outcompeted — is the allocation
layer and belongs with the field. `founder_pass` and its test battery move
from `settlement/placement.rs` to `demography/founder.rs`. The behaviour is
preserved; only its inputs change (it now floors over attractors of the
flow, not over suitability scores).

**Settlement's surviving surface.** `settlement::genesis` and its predicate
constants (`IS_SETTLEMENT`, `NAME`, `POPULATION`, `CELL_ID`, `LATITUDE`,
`LONGITUDE`, `BIOME`, `IS_PLACE`) are unchanged, so the ledger schema and
every downstream consumer (almanac, locale, historiography) keep working
untouched — only the committed *values* shift. `settlement::place` /
`place_tagged` / `suitability*` are removed.

**The settlement scenario pin.** The current `--min-suitability` pin
(`SETTLEMENT_PIN`, `windows/worldgen/src/settlement_pins.rs`) is defined
against the retired suitability floor and retires with it — its round-trip
string and the founder-floor semantics it documented no longer have a
referent. The concentration threshold `T` is calibrated once and frozen as a
constant, **not** exposed as a pin (YAGNI: no world needs to choose its
rank-size slope in this campaign). Exposing a demography pin later is a
clean additive follow-up. Retiring `SETTLEMENT_PIN` is a save-format change:
1a/1b-era saves carrying a `settlement-pin` fact must still load (the fact is
ignored, as unknown pins already are), and this is called out in the
artifact-rebaseline diff.

**Layering check** (`cli/tests/architecture.rs`): `demography` is a domain,
so it may not import `settlement`, `terrain`, or `climate`; it sees only
bare kernel types the root assembles. `settlement` likewise does not import
`demography`. The two meet only at `windows/worldgen` (the composition
root). Adding `demography` edits no existing domain.

## 2. The carrying-capacity field K

`K(cell)` is a closed-form, **seed-free** `CellMap<f64>` per species — an
absolute count-density of people the cell can support — grounded so the Lab
can grade it:

```
  NPP_proxy = Miami model: min( f_temp(T), f_moist(M) )      [Lieth; a
              standard 2-line empirical NPP fit from temperature + moisture]
  K(cell)   = base
            * NPP_proxy
            * (1 + freshwater_bonus)      freshwater / drainage, from root
            * (1 + coast_or_upwelling)    climate already flags upwelling
            * (1 - aridity_penalty)        below the aridity floor -> ~0
            ; exactly 0 on ocean and uninhabitable cells
```

The exact constants (`base`, the Miami coefficients, the bonus/penalty
scales) are set during the calibration step (§5) and then frozen as
save-format constants. Per-species variation enters through the same
psychology-derived weighting the current model already uses at the root
(the `SuitabilityWeights` analog): each species weights temperance,
freshwater, and hostility differently, so each gets its own K.

**Field/grid contract (decision 0038).** K is *pointwise* and
resolution-free — a `Field`-like prior any grid may resample, joining the
crust contract's pointwise half (like crust thickness/age). The flow
accumulation and condensation *over* K are *mesh-bound* — they need the
neighbour graph — and join the compute-once half (like sea level,
drainage, placement). Consequence for MAP-30 room-scale refinement: a room
may resample K below cell scale without re-condensing settlements; the
settlements it inherits are mesh-bound truth, unchanged.

## 3. Flow-accumulation & condensation

`domains/terrain/src/drainage.rs`'s proven shape, comparator flipped, run
per species on that species' K:

```
  1. gradient    per land cell, up-gradient target = highest-K neighbour;
                 K strictly ordered by a per-cell epsilon so there are no
                 ties (exactly as drainage strictly orders elevation);
                 flat-K plateaus resolved by that epsilon + cell-id tie-break
  2. accumulate  each cell routes its own K "upstream"; a cell's accumulation
                 = sum of K over all cells whose up-gradient path passes
                 through it (memoised path-trace, bounded by n, straight from
                 drainage's implementation)
  3. attractors  a cell with no higher-K neighbour is an attractor (the K
                 analog of drainage's endorheic minimum): a candidate
                 settlement
  4. extract     nodes = attractors whose accumulation >= threshold T
                 (T is the single concentration knob; §5 tunes it to Zipf)
  5. catchment   every cell joins the attractor its up-gradient path ends at
  6. population  pop(node) = accumulation at that node = sum of K over its
                 catchment  ->  CONSERVED: Σ node pops == Σ K, exactly
  7. floor       the founder pass guarantees each species its strongest
                 attractor even if that attractor's accumulation is below T
```

**Conservation is structural.** Settlements *partition* the K budget rather
than each sampling a local value, so `Σ pop == Σ K` holds by construction,
not by tuning. This is the campaign's strongest invariant (§5).

**Determinism.** K is seed-free closed-form; the accumulation is the same
memoised integer-and-comparison walk drainage already ships. The pipeline
draws **nothing** from the seed and is trivially cross-platform: the only
transcendentals live inside K (routed through `kernel::math`/libm), and
quantization applies at emit boundaries only, never in the compute path
(Lorenz guard-rail — moot here since nothing is chaotic, but observed).

**The flagship** is redefined as the highest-population attractor
(deterministic; ties by cell id), committed first, preserving the "flagship
is the first `is-settlement` fact" contract.

## 4. Ledger facts & the residual density field

Genesis-time output is two things: settlement Facts (unchanged schema) and a
residual Field held in memory.

- **Facts.** `settlement::genesis` commits exactly today's predicates, still
  at `day = 0.0`. The only change is the *provenance* of the values:
  `population` is now the catchment readout. No new predicates in this
  campaign (history predicates belong to the later campaign).
- **Residual density field.** The per-species K and accumulation `CellMap`s
  are **not serialized** — re-derived from the seed on demand, exactly like
  paleoclimate's `PaleoRecord` and the existing `terrain_of`/`climate_of`
  pattern. They are exposed on an in-memory `DemographyReport` (the
  `demography_of(world)` return) so an in-process consumer — almanac, Lab
  metric, the later relaxation — computes them once and holds them rather
  than re-deriving in a loop.
  - *Cost, for the record.* A full demography pass is single-digit
    milliseconds at the level-6 globe (40,962 cells × a handful of species):
    a closed-form K plus one memoised flow pass, marginal on top of the
    terrain+climate re-derivation that inspecting a world already performs.
    Serializing the fields was rejected: ~2.6 MB of raw per-cell floats
    (far more JSON-encoded) and a new save-format contract to version,
    quantize, and drift-check, buying nothing — K/accumulation are not
    chaotic, so there is no Lorenz-checkpoint reason to persist them.
- **Null-control honesty** (mirroring paleoclimate §9): a species whose K is
  zero everywhere habitable condenses to no settlements except its
  founder-floor cell, and that is the honest measurement, not an error.

## 5. Verification & calibration

MAP-7 is calibration-checked; checks are preregistered before the sweep
(decision 0016).

- **Conservation invariant (unit test; the strongest guard).**
  `Σ settlement populations == Σ K over catchment cells`, per species, to
  within quantization. Exact by construction; catches any accumulation or
  catchment-partition bug immediately.
- **Zipf rank-size (preregistered Lab study; headline calibration).** Across
  a seed census, settlement sizes should follow a rank-size law (the n-th
  settlement ≈ 1/n the flagship). The study registers its hypothesis (a
  target log-log slope near −1) in its JSON before the sweep; the
  concentration knob `T` is tuned **once** to land it, then frozen. Negative
  results are published like any other (the book's laboratory pages).
- **Population-vs-latitude / biome gradient (Lab metric).** Total supported
  population should track the real biomass gradient — high in wet tropics and
  temperate bands, low in deserts and ice. The "K is grounded, not noise"
  check.
- **Pin-isolation & determinism.** A demography pin (if any) must consume the
  same stream draws as the unpinned path (the genesis-properties pattern) —
  a small surface, since the pipeline draws nothing; determinism is mostly
  the founder floor's tie-breaks. Same seed + pins → byte-identical
  settlements, cross-platform.
- **Artifact rebaseline.** The three seed-42 almanacs, the elevation/settle
  map, and settlement-touching lab studies are regenerated in the landing
  commit, the diff reviewed as evidence (the *replace* decision made this
  explicit). Golden pins that move are re-pinned in the drifting commit, not
  deferred to the close.

## 6. Book & Definition of Done

- **Chronicle entry** for the-gathering campaign.
- **Freshness sweep** of the settlement/placement book chapters — they
  currently describe the suitability scatter being retired.
- **Registry:** `MAP-7` flips `elaborated → spec'd` (then `shipped` at
  merge) and repoints at this spec; `MAP-22`'s row gets a cross-link noting
  the founder floor migrated to `demography`.
- **Confidence Gradient rescore (partial).** This campaign lands the
  calibration-checked *field*; the "vary time" horizon bet rides on the
  later history campaign. The rescore records that split explicitly.
- **Retrospective** (`docs/retrospectives/`, decision 0020) — process, not
  product.

## Decisions to ratify

Recorded here for the decision log at merge (numbered in creation order per
decision 0043):

1. **A `demography` domain is extracted from `settlement`** — the frontier's
   stated domain map; population-as-field is a distinct object from
   named-places, and the later relaxation/history/contact work has a clean
   home. Supersedes nothing; additive.
2. **Flow-condensation replaces the suitability scatter** rather than
   coexisting as an opt-in tier — one mechanism to maintain; the scatter was
   never a fidelity a world would meaningfully choose. Blast radius
   (artifact rebaseline) accepted.
3. **The MAP-22 founder floor migrates to `demography`** — it is the
   allocation layer, and it belongs with the field it floors over.

## Risks & open questions

- **Rough K → adjacent attractors.** A noisy K could place two attractors in
  neighbouring cells. Mitigation: the same epsilon/tie-break drainage uses,
  and if needed a minimum-accumulation `T` that naturally spaces nodes; the
  founder floor never produces two nodes for one species in one cell.
- **Miami-proxy fidelity.** The NPP proxy is a coarse fit; the calibration
  step (§5) is where it either reproduces the latitude gradient or gets
  adjusted. If it cannot, that is a publishable negative result, not a
  silent fudge.
- **Cross-tag spacing regression.** Documented and accepted (Scope
  boundary); MAP-22 is the place to restore exclusion properly.
- **Naming.** "the-gathering" is provisional; rename at will.
