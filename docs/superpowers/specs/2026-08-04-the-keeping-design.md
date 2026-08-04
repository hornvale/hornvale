# The Keeping — species-aware habitability at the placement gate

**Status:** Draft for review (2026-08-04) · **Campaign:** the-keeping ·
**Decisions ratified in this campaign:**
[0098](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0098-hornvale-is-single-player.md),
[0099](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0099-worlds-are-version-locked.md),
[0100](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0100-fact-phenomenon-myth.md),
[0101](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0101-geometry-and-society-are-separate-vocabularies.md),
[0102](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0102-one-per-cell-was-an-index-artifact.md)

## 1. The problem, measured

Every generated world puts its settlements in a handful of tight contiguous
carpets. Measured across four seeds:

```
  seed                    n    clusters  nn_km (min/med)   |lat| max
  16244526067196353746  356       7      110.2 / 111.2      32.2
  42                    158       9      110.2 / 111.4      23.4
  7                     212      12      110.2 / 114.1      35.0
  999999                291       6      110.2 / 114.1      75.4
  1234                    0       -           -               -
```

(Clusters: single-link at 500 km. `nn` distances are great-circle at an assumed
Earth radius — the sim defines none; the radius-free figure is 0.99°, one cell.)

Two causes, and this spec addresses the second because the first is now known to
be free:

1. **The ~110 km floor** is one-community-per-cell, an artifact of
   `node_index`'s `BTreeMap<CellId, _>` type — see 0102. Relaxing it needs no
   design argument, but on its own it only lets carpets *overlap*; it does not
   scatter them.
2. **The gate that decides where anyone may live is species-blind.** This is the
   binding constraint on Nathan's stated goals — creatures everywhere, and more
   settlements as the roster grows — and it is what this campaign changes.

## 2. The finding: niche differentiation was dropped from placement

`Bake::factor` is the habitability gate, and it takes no species:

```rust
fn factor(era: &EraClimate, cell: CellId) -> f64 {
    if *era.ice.get(cell) || !*era.habitable.get(cell) { 0.0 } else { 1.0 }
}
```

`era.habitable` is `hornvale_climate::habitability_map`, which is three hard
global thresholds — land, `-5 °C ≤ mean ≤ 35 °C`, `moisture ≥ 0.2`. And the
bake's capacity is the species-blind base field:

```rust
let suitability = hornvale_demography::carrying_capacity(geo, &carrying_inputs_of(..));
let capacity = CellMap::from_fn(geo, |c| *suitability.get(c) * SETTLERS_PER_CAPACITY);
```

Meanwhile the niche-aware machinery exists, is authored for 30 species, and is
measured — `ConditionNiche` carries per-species response curves over
temperature, moisture, insolation and elevation, and `niche_per_species_k`
scores them into per-species carrying-capacity fields. The kobold niche's doc
comment quotes its own validation: *"kobold is the best-fit people on every
settleable cell above 3000 m (mean fit 0.130 against hobgoblin 0.041, goblin
0.049, bugbear 0.004)."*

**It is not wired to placement.** `windows/worldgen/src/lib.rs:5569` says so:

> *The retired `coexist::pack`/`condense_stack` placer is gone from genesis; the
> same niche-differentiated stack still lives in `demography_report_from` for
> the Lab's coexistence-stack readout, which this rewire leaves untouched.*

So when The Living Community's deep-history bake took over as settlement
provider, placement regressed from niche-differentiated to species-blind, and
the niche stack survives only as a **report**. Consequences today:

- An authored extremophile **cannot** settle ice, deep desert or deep ocean no
  matter what its niche says, because `factor` zeroes the cell for everyone.
- Every people competes for the *same* cells, ranked by the *same* field, so
  they pile into the same few high-capacity river basins — which is what the
  carpets are.

### 2.1 The gnoll case — the bug is already live and has a name

The six settling species' authored optima are not near-clones. They are well
separated on every axis:

```
  species     temp °C      moisture      insolation    elevation m
  kobold       6.0 ±14     0.45 ±0.60    0.04 ±0.12    3000 ±1100
  hobgoblin   13.0 ±10     0.35 ±0.30    0.19 ±0.13     600 ±1400
  human       14.0 ±29     0.50 ±0.70    0.25 ±0.45    1500 ±4000
  goblin      18.0 ±28     0.50 ±0.60    0.13 ±0.30    1500 ±3000
  bugbear     21.0 ±11     0.82 ±0.20    0.15 ±0.40     150 ±1200
  gnoll       29.0 ±9      0.12 ±0.12    0.08 ±0.10     500 ±1300
```

Four specialists in distinct corners — cold dark highland (kobold), cool dry
lowland (hobgoblin), hot wet lowland (bugbear), hot **arid** lowland (gnoll) —
plus two deliberate generalists carrying the wide response curves (goblin,
human). A 23 °C temperature ladder and a moisture range spanning arid to
saturated.

Now put that beside the global gate's aridity floor:

```
  HABITABLE_MIN_MOISTURE = 0.2      (domains/climate/src/habitability.rs:13)
  gnoll moisture optimum  = 0.12, width 0.12
```

**The gnoll's optimum is below the floor.** Its ideal ground is classified
uninhabitable — for everyone — by a threshold that has never heard of gnolls.
Meaningful fit runs roughly 0.0–0.24 and the gate admits only ≥ 0.2, so the
roster's arid specialist is confined permanently to the wettest sliver of its own
niche.

This is not a coarse approximation to be refined later. It is an authored species
excluded from the conditions it was authored for, and it is the clearest single
statement of why this campaign exists.

## 3. Design

### 3.1 The gate becomes per-species

```rust
// before
fn factor(era: &EraClimate, cell: CellId) -> f64
fn eff_capacity(&self, era: &EraClimate, cell: CellId) -> f64
fn vacant_habitable(&self, era: &EraClimate, cell: CellId) -> bool

// after
fn factor(&self, era: &EraClimate, cell: CellId, people: usize) -> f64
fn eff_capacity(&self, era: &EraClimate, cell: CellId, people: usize) -> f64
fn vacant_for(&self, era: &EraClimate, cell: CellId, people: usize) -> bool
```

`people` is the **build-local dense index** into the bake's existing
`peoples: Vec<KindId>` — the same convention `HistoryPlacement.tag` already
uses, never serialized. Every call site has a people available:
`self.records[c.record].core.people` is a `KindId` throughout.

New `factor` semantics — **graded, resolved at review**:

- **Ice still zeroes the cell for everyone.** A glacier is not a niche
  disagreement.
- Otherwise the value is **this people's own graded niche fit**, taken from
  per-species capacity rather than a global boolean.

`factor` stops being `{0,1}`, which is what makes `eff_capacity` meaningful per
people rather than a shared ranking. **No new fit function is written**: the fit
is exactly what `niche_per_species_k` already composes —
`ConditionResponse::eval` per axis against the `substrate_field` reading, with
the axis semantics already authored (`lib.rs:1028`):

```
  temperature / moisture / insolation   buffer-able, floored by the species'
                                        sovereignty_floor(mass, potency)
  elevation                             HARD (floor 0.0)
                                        "sovereignty buffers physiology but
                                         not geometry"
```

### 3.1a Clinging to hostile ground is already modelled

The requirement that a people be able to **hang on where it has no better
choice** does not need new machinery. `ConditionResponse::eval` is
`floor + (1 - floor) · devotion · exp(-z²/2)` — a Gaussian that decays but never
reaches zero, lifted by a floor that
`sovereignty_floor(mass, potency) = 0.95 · (1 - e^{-(0.15·ln mass + potency)})`
derives from body mass and magical potency. Bigger and more magical bodies
buffer environmental extremes; small mundane ones do not. Computed for the
settling six:

```
  kobold      13.6 kg   floor 0.308
  goblin      18.1 kg   floor 0.335
  human       70.0 kg   floor 0.448
  hobgoblin   74.8 kg   floor 0.453
  bugbear    132.0 kg   floor 0.493
  gnoll      136.1 kg   floor 0.495
```

So each retains **31–50% of peak suitability on its worst temperature, moisture
and insolation** — a physiological account of why humanity hangs on in places it
plainly should not. Two properties fall out that the design should not disturb:

- **Differentiation survives the floors**, because elevation is unbuffered.
  Kobold's 3000 m stronghold stays exclusive on the one axis no body mass can
  argue with.
- **Gnoll is the best-buffered of the six** (136 kg), so the roster's arid
  specialist becomes widely persistent the moment the global moisture floor stops
  excluding arid land — §2.1's bug and this campaign's fix meet at the same
  species.

**The one genuinely new constraint** is the famine threshold. `step_community`
closes a community with `CauseOfEnd::Famine` when
`pressure = population · NEED / eff_capacity >= COLLAPSE_PRESSURE` (2.0), and it
does so *before* `grow` runs — so there is no negative-population hazard, but
there is a floor below which nothing can cling: a community of population *p*
survives only where `eff_capacity > p · NEED / 2`. Clinging is therefore possible
exactly where graded fit leaves capacity above that bar, and impossible below it.
That is the right shape — precarious, not immortal — and Task 0 must report where
the bar actually falls, because it, not the niche, is what decides whether a
remnant persists.

### 3.2 Per-species capacity replaces the shared field

`bake_history_from` swaps the single `capacity` for the per-species fields
`niche_per_species_k` already produces, indexed by the peoples roster position.
The `Bake` struct's `capacity: &'a CellMap<f64>` becomes
`capacity: &'a [(u32, CellMap<f64>)]` (or a `Vec<CellMap<f64>>` aligned to
`peoples` — task-time choice, whichever keeps the hot path a direct index).

`SETTLERS_PER_CAPACITY` scaling is preserved so the headcount frame the bake
reasons in (`pressure = population / eff_capacity`) is unchanged in units.

### 3.3 What deliberately does *not* change

- **One community per cell stays.** 0102 makes relaxing it free of design
  argument, but bundling it here would confound the measurement in §4 and
  double the blast radius. Separate campaign.
- **`is_habitable` / `habitability_map` keep their definitions and their names —
  for now — but stop gating placement.** Nathan's ruling at review: a global
  tolerability rule "doesn't make any sense at all… it should have been based on
  per-species tolerability from the very beginning," and the *name* is wrong.
  Both accepted. The rename is nonetheless **deferred to its own campaign**, and
  §7 q3 records why and corrects my earlier reasoning for deferring it.
  Meanwhile the chronicle must say plainly that "habitable" now names *"land a
  generic vale dweller would tolerate"* — a geographic statistic — and no longer
  *"somewhere anyone lives."*
- **The ocean stays closed to everyone.** `niche_per_species_k` yields
  `K = 0` on every submerged cell for the whole roster (`lib.rs:1034`), so
  aquatic and floating settlement remains impossible after this campaign. That is
  a separate hard gate from the one being fixed here, and it is the remaining
  blocker on "underwater, floating on the ocean" — flagged, not addressed.
- **`demography_report_from`** and the Lab coexistence readout are untouched.
- **No new stream labels, no new draws.** The gate does not roll dice; it reads
  fields. Stream consumption order is unchanged, so the pin-isolation tests
  should stay green — which is itself a check that this was done right.
- **The roster is not extended.** Authoring extremophiles is the *next*
  campaign; this one removes the reason authoring them would be pointless.

## 4. Preregistration (decision 0016)

Frozen before the rewire. Probe seeds **42, 7, 999999, 16244526067196353746**
plus **1234** (today's zero-settlement world), measured with the cluster/latitude
script in §1.

**H1 — scatter.** Distinct clusters (single-link, 500 km) rises above today's
6–12 on at least three of the four populated probe seeds.

**H2 — reach.** The `|latitude|` span of settlements widens on at least three
probe seeds; specifically kobold settlements appear above 3000 m where its
authored stronghold predicts them.

**H3 — count.** Total settlements rises. No target: the direction is the claim.

**H4 — the null, stated in advance.** Spread *optima* do not guarantee the
*world* offers cells in those corners. **If a typical world has little or no
land at (29 °C, moisture ≈ 0.12), gnolls stay rare after the rewire for a
geographic reason rather than a gate reason** — and likewise for kobold above
3000 m, whose stronghold the niche doc comment reports at p79 of settleable
land. If H1–H3 barely move, the finding is that *the world's supply of extreme
ground*, not the gate, bounds diversity — pointing at terrain and climate rather
than at placement. That is a real possible outcome, it would be the campaign's
headline, and it must not be rescued by retuning a niche or a threshold after
unblinding.

*An earlier draft of H4 predicted the opposite risk — that the six settling
species' niches would substantially overlap, making a species-aware gate a
no-op. §2.1 falsified that by inspection before the rewire was written: the
optima are well separated on every axis. The hypothesis is replaced rather than
deleted, because the residual risk is real and is a different claim.*

### Task 0 — the field half of the pre-check

The **static half is done** and is recorded in §2.1: the authored optima are well
separated, so the original overlap worry is closed and needs no measurement.

What remains is the field half — whether the *world* supplies the ground those
niches want. **Before** the rewire, over the probe seeds, measure:

1. Per settling species, the count of land cells whose fit is non-negligible, and
   the count where it is the **best-fit** species (the frame the kobold niche's
   own validation already uses).
2. The land-cell count at gnoll's corner (moisture < 0.2, temp > 25 °C) — ground
   that is currently uninhabitable *by definition* and would become gnoll
   country. **This number is the campaign's headroom**, and it is the single most
   informative figure available before any code changes.
3. The same for cells above 3000 m (kobold) and for ice-free cells outside the
   `-5..35 °C` band.
4. **The clinging bar.** For each settling species, the count of land cells where
   graded `eff_capacity` exceeds `NEED` (a lone survivor's famine threshold at
   `pressure < 2`) but falls below what a daughter needs
   (`DAUGHTER_MAX_PRESSURE = 0.7`). That band **is** the hang-on zone §3.1a
   describes; if it is empty, marginal persistence is theoretical.
5. **The expansion magnitude.** Total land cells with non-negligible fit for *at
   least one* species, against today's habitable count. With sovereignty floors
   of 0.31–0.50 this could approach *all unglaciated land*, so this number sizes
   the risk in §6 and the cost in §5.

Interpretation, fixed in advance:

- **Headroom large** → H1–H3 are live; proceed to the rewire.
- **Headroom small** (the world has little extreme ground) → **stop and report.**
  The rewire is still correct, but the unlock is upstream in terrain/climate, and
  Nathan chooses whether to continue. This remains a genuine gate, not a
  formality.

## 5. Blast radius

- **World identity moves.** Every seed re-places, so: golden fixtures, the three
  seed-42 almanacs, the settlement map, the dictionary's biome-gap rows, and a
  **census re-baseline**. Under 0099 this is a cost, not a corruption, and the
  epoch-suffix ritual is no longer required.
- **Study pins keyed on settlement counts or the flagship will move.** Per 0097,
  convert threshold-adjacent pins to census-measured rates rather than re-pinning
  them; robust invariants stay in the gate.
- **`SOC-flagship-selection` interacts**: `flagship_of` resolves the oldest
  surviving occupation, so re-placement re-selects flagships and any metric keyed
  on one is measuring the re-selection. Expect movement there and do not read it
  as a regression.
- **Seed 1234 has zero settlements today.** It is in the probe set precisely
  because a species-aware gate is the mechanism most likely to give it any, and
  if it stays empty that is diagnostic.

## 6. Risks

- **The settleable set may balloon, and that grows the one accumulating layer.**
  This is now the primary risk. Sovereignty floors of 0.31–0.50 mean every
  settling species retains a third to a half of peak suitability on its worst
  buffer-able axis, so *nearly all unglaciated land above sea level* may become
  settleable at reduced capacity. That is the stated goal, but committed
  settlements are the only layer that accumulates (decision 0100, corollary 4),
  and they drive bake CPU and census hours. Task 0 item 5 sizes it **before** the
  rewire. If the expansion is very large, the correct response is *not* to
  re-tighten the gate but to raise the bar for *committing* a settlement — the
  condensation threshold and `VIABLE_MIN` are the knobs, and
  `SOC-settlement-tiers`' "commit the contingent, derive the regular" is the
  principle.
- **Graded `factor` changes pressure arithmetic**, though less dangerously than I
  first assumed. `step_community` closes on `pressure >= COLLAPSE_PRESSURE`
  *before* `grow` runs, so the `1 + GROWTH_RATE·(1 - pressure)` term can never
  drive population negative. The live risk is a *rate* shift: many more
  communities living near the famine bar means many more `CauseOfEnd::Famine`
  closures and a ruin-heavy world. Watch the bake's `collapsed`/`grew`/`founded`
  tallies on the probe seeds before trusting any H1 result.
- **Founder-floor interaction.** `demography::founder::condense_tagged` already
  guarantees each species its strongest attractor. A per-species gate may make
  that floor load-bearing far more often; its "one deliberate exception to
  conservation" comment should be re-read, not assumed.
- **Performance.** Per-species capacity is N fields instead of one, and
  `best_home`'s ring scan now evaluates a per-species factor. The census is
  ~700 s wall / ~17,500 s CPU at ~25× parallelism; a large regression here is
  felt at every campaign close. Measure the world-build time on a probe seed
  (baseline: **1.96 s** release) before and after.

## 7. Questions — resolved at review (2026-08-04)

1. **Graded or boolean `factor`? → GRADED** (Nathan). §3.1 and §3.1a are written
   to it. Consequence: the fit comes from `niche_per_species_k`'s existing
   `eval`/`sovereignty_floor` composition rather than any new function, and
   marginal persistence becomes expressible rather than needing separate
   machinery.
2. **What is "non-negligible fit"** for Task 0's cell counts and for the gate's
   zero? A hard zero makes exclusion crisp; an epsilon avoids a cliff at the
   niche margin. Recommendation: pick it in Task 0 from the measured fit
   distribution rather than authoring a constant blind.
3. **Does the global habitability rule survive? → NOT AS A GATE** (Nathan): it
   "doesn't make any sense at all… it should have been based on per-species
   tolerability from the very beginning," and its *name* makes no sense either.
   Both accepted; §3.3 records it.

   **The rename is deferred to its own campaign, and my earlier reason for
   deferring it was wrong.** I argued that renaming would "make the §4
   measurement unreadable." That is false — a rename is behaviour-neutral and
   cannot move a measurement. The real reasons are size and reviewability:
   **578 occurrences of "habitable"** across the repo, and `"habitable-fraction"`
   is a **Lab metric name**, hence a census column, hence live in
   `golden-pins.sql`, the analysis harness, and six published study pages. That
   is a bounded mechanical campaign of its own, and bundling it here would bury a
   behavioural change under several hundred lines of prose churn.

   For the successor campaign: the honest name is in the module's own doc
   comment, which already calls it *"where a vale-like place could be"* — so
   `is_vale_like` / `vale_fraction`, keeping the measurement (a world's supply of
   temperate wet lowland is genuinely worth knowing) and dropping the claim that
   it bounds where anyone can live.
4. **Aquatic and floating settlement** is still blocked after this campaign by a
   *different* hard gate — `K = 0` on every submerged cell for the whole roster.
   Not in scope; recorded so "creatures everywhere" is not read as delivered.
