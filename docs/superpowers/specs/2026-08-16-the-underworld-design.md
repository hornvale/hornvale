# The Underworld — design

*Chorography campaign 2. Consumer of campaign 0 (The Fathom) and campaign 1
(The Axes). Restores Mountain-dwarf and Duergar.*

**Program:** [The Chorography](2026-08-12-the-chorography-metaplan.md) ·
**Predecessors:** [The Fathom](2026-08-12-the-fathom-design.md),
[The Axes](2026-08-13-the-axes-design.md)

---

## 1. What occasioned it

The Delvers (C2c) authored five dwarf kinds and withdrew two of them before
merge. The source still carries the reason and the return condition, in as
many words (`domains/species/src/lib.rs:1721`):

> **Roster cut to three (spec §11).** Mountain and Duergar were authored and
> then withdrawn: both are defined by DEPTH, and the model's elevation axis is
> metres above sea level, so authoring "deep" as "low ASL" was the same class
> of fake The Warren spent itself removing. They return in a successor
> campaign that gives the underworld biomes.

Two campaigns have since been run to make that successor possible. The Fathom
made a column askable; The Axes decomposed community into a five-axis basis
and deliberately left `EnvironmentNiche` unbuilt for want of a consumer. This
campaign is the consumer.

## 2. Keystone

> **The rock was given a ladder that answers "what is this rock and how old is
> it", and was then asked "how deep am I and what is it like here". Those are
> different questions and they need different ladders.**

The corollary that does the work, and the reason this is not a re-run of The
Delvers: *a depth coordinate must vary with the thing that makes depth matter.*
Metres above sea level does not. Metres below the surface barely does either,
for reasons §3.1 measures. Temperature does, and the model already computes it.

## 3. The findings this rests on

Every claim in this section was verified in the tree at `1e92c152` by running
the command named, not inferred from a prior document.

### 3.1 The depth bimodality is an artifact of the ladder, not a fact about caves

`delver_depth_probe` (`windows/worldgen/tests/`) measured, over seeds 42 / 7 /
1234, that two thirds of caves bottom out in `Cover`/`Basement` at a **median
habitable depth of 0.0 m**, and the remaining 28–33% reach `Roots` at
**14–21 km**. Nothing occurs between. The Delvers read that as a fact about
caves and withdrew two kinds on it.

It is a fact about `strata.rs::column`, which spaces the ladder by *rock-unit
boundaries*:

```
Regolith.top   = 0.0
Cover.top      = soil_depth_m                       (probe: max 9–10 m)
Basement.top   = dtb = soil_depth_m + sediment_m    (probe: p50 0.0 m, max 1807 m)
Roots.top      = dtb + (moho_m − dtb) × 0.5         (~14–21 km)
Underneath.top = moho_m                             (~28–42 km)
```

Soil and sediment are thin, so three of the five rungs stack within metres of
the surface; the next natural stratigraphic boundary is halfway to the Moho.
**There is a ~13 km hole in the ladder covering the entire depth range an
underworld occupies.** That is the whole bimodality.

### 3.2 The blast radius of changing this is two call sites

`grep -rn "deepest_band" --include=*.rs domains/ windows/ cli/`, excluding
test directories, returns exactly two production consumers:
`domains/terrain/src/provider.rs` (which produces it via
`features::cave_depth`) and `windows/worldgen/src/chamber.rs::chamber_exists`
(which gates on it). Nothing else in the workspace reads a cave's depth band.

### 3.3 The affordance for inserting rungs was built deliberately

`chamber_key` (`windows/worldgen/src/chamber.rs`) spells a band **by name,
never by index**, and its doc names this exact work as the reason:

> An index is a declaration position: if `Stratum`/`BandKind` ever gains a
> variant in the middle of the ladder — the open `MAP-cave-depth-weld` work is
> the named candidate — every index below it shifts, and a numeral-keyed
> chamber would silently move to a different derived stream.

### 3.4 The heat model is shipped and has never been called

`domains/terrain/src/strata.rs` ships `geothermal_gradient(crust_thickness_km,
crust_age, continental)`, clamped to **15–30 K/km**
(`CRATONIC_GRADIENT_K_PER_KM` / `OCEANIC_GRADIENT_K_PER_KM`), and
`temperature_at_depth(surface, gradient, depth_km)`.

**`temperature_at_depth` has zero consumers** anywhere in `domains/`,
`windows/` or `cli/`. `geothermal_gradient_at` has exactly two, and both are
narration: `windows/worldgen/src/lib.rs:3830` reports min/max gradient for the
almanac's column paragraph, and `windows/lab/src/metrics.rs:1612` averages it
for a metric. The gradient reaches prose and never reaches physics.

That module's own doc already names this campaign as its consumer: the
gradient is *"the deep's energy base (the 'inner sun' a later chemo/lithotroph
ecology reads)"*.

### 3.5 Nothing underground can be told apart, and the cause is one function

`subterranean_substrate` (`windows/worldgen/src/lib.rs:2650`) is four lines:
temperature passes through **unchanged at every depth**, `moisture` is the
world constant `SUBTERRANEAN_MOISTURE`, `insolation` is `0.0`, and
`height_asl_m` is inherited from the surface cell. Its own doc concedes that
only two axes distinguish a chamber. Every chamber in every world reads
identically except for its surface cell's temperature.

### 3.6 A cell holds exactly one community

`windows/worldgen/src/history_bake.rs:1139` declares `node_index:
BTreeMap<CellId, usize>`; `vacant_habitable` (:1377) gates on
`!self.node_index.contains_key(&cell)`. A `BTreeMap<CellId, _>` structurally
cannot hold two communities per cell.

At `GLOBE_LEVEL = 6` there are 40,962 cells (0102's own figure). Dividing
Earth's surface by that count gives **~12,400 km² per cell**; the registry's
`SOC-settlement-tiers` row states **~10,000 km²**. Both are order-of-magnitude
statements of the same thing and this spec does not need them reconciled — the
point survives either figure: one settled community per area the size of
Northern Ireland. Decision 0102 ruled that this constraint
"carries no design rationale": it was an index chosen for lookup speed on
2026-07-20, whose emergent ~110 km spacing was written up a week later as
though it had been designed. `SOC-dense-settlement` carries `ratified (0102)`
and the sentence *"Nathan wants this soon-ish."* Three campaigns have since
routed around it.

### 3.7 There is no water table

`grep -rn "water_table\|phreatic\|vadose" --include=*.rs domains/ windows/`
returns nothing. Every column is implicitly dry at every depth.

### 3.8 The inputs a chamber could vary on are already owned

All verified present: `CaveKind {Karst, LavaTube, Fracture}` and `deposit_at`
→ `Commodity` (`domains/terrain/src/features.rs`); `MaterialBuffer` with
`carbonate`, `porosity`, `induration`, `metamorphic_grade`, `silica`, `grain`
(`lithology.rs`); `BandSample.rock` / `.era` and `unconformity` (`strata.rs`);
`drainage_field` (`drainage.rs`) and `ChannelNetwork` (`channel.rs`);
`WaterKind {Ocean, SaltBasin, River, DryLand}` (`water.rs`).

### 3.9 This is the third shipped seam with no producer in a row

`EnvironmentNiche` (The Axes deferred it for want of a consumer),
`ChamberOrigin` (shipped with a seam and no writer), `temperature_at_depth`
(shipped with no caller). The Axes' retrospective catalogues this shape as
F-5 and counts it three times already. This campaign closes three at once and
should say so as one finding, not three fixes.

## 4. The design

### 4.0 A cave's depth is a budget in metres — AMENDED 2026-08-16, POST-TASK-1

**This section did not exist when the spec was written. Task 1's measurement
required it, and Nathan authorized the scope change.**

§4.1 below was written on an assumption Task 1 falsified: that spacing the
ladder by temperature would redistribute chambers across it. It does not,
because ΔT = gradient × depth and the measured gradient spread is **1.27×**
(p10 21.795 → p90 27.780 K/km) against a depth spread of **~10⁴×**. ΔT is
therefore depth rescaled by a near-constant, and inherits depth's bimodality
exactly: two occupied classes, `[0, 2) K` holding 62–75% of caves and
`[50, ∞) K` holding 24–38%, with the middle rungs holding 1–25 caves out of
874–1681. The `[50, ∞)` count matches the `Roots` band count essentially 1:1
on every seed, so the heat ladder was reproducing the band ladder rather than
refining it.

§3.1's diagnosis was right as a description and wrong as a cause. There is a
13 km hole in the ladder; re-spacing rungs cannot populate a hole that no
chamber's depth ever lands in. `top_depth_m(deepest_band)` is effectively
two-valued because `dtb = soil + sediment` is ~0 almost everywhere and
`roots_top ≈ moho/2 ≈ 14 km`, and any monotone function of a two-valued input
is two-valued.

**So a cave gets a depth reach in METRES, derived independently of the
stratigraphic band boundaries.** This is `MAP-cave-depth-weld`, which §7
listed as a non-goal; that row's own **Where** cell states it was waiting for
a consumer to "supply the evidence for or against a split", and Task 1 is that
evidence. Taking it now is the smallest change that makes §4.1 work.

Three constraints on the derivation:

- **It must not read the presence proneness.** That shared scalar *is* the
  weld: the presence gate and the depth budget want opposite calibrations, and
  reading one field for both is why `Fracture` could not be tuned without
  flooding its share.
- **Prefer a pure function of fields terrain already owns** — cave kind,
  lithology, relief — in the manner of `geothermal_gradient`. A pure
  derivation adds no draw, so it cannot perturb stream consumption order. Only
  if a pure derivation proves degenerate should a draw be added, and then it
  takes its own stream label.
- **The physical anchors, which are Earth trivia used as a sanity ceiling and
  not as a derivation:** lava tubes form inside a flow and are shallow (tens
  to a few hundred metres); karst follows dissolution and reaches ~1–2.2 km
  (Veryovkina, 2212 m); fracture voids are fault-controlled and close under
  lithostatic load within a few km. A budget landing in 0–3 km is the target,
  because that is the window §4.1's ladder covers.

`deepest_band` stays, and becomes *derived from* the depth budget by comparing
it against the column — so the archive keeps answering "which bands does this
void penetrate" correctly while no longer being the depth coordinate itself.

### 4.1 The delve ladder — spaced by heat

`BandKind` keeps its five stratigraphic rungs and its entire archival job
(`Era`, `RockClass`, `unconformity`). It is not modified.

A new **delve ladder** carries habitation depth. Its rungs are placed at
**temperature offsets above the cell's surface datum**, so a rung's depth in
metres is derived from `geothermal_gradient` and therefore *varies by cell*:
the same rung sits roughly twice as deep under an ancient craton (15 K/km) as
under young thin crust (30 K/km).

`ChamberAddr.band` is re-pointed at the delve ladder. A `Chamber` reports both
its delve rung and the `BandKind` it sits in; neither derives the other.

**The rung table is the OUTPUT of Task 0, not its input.** What is frozen
here, before any measurement, is the *principle* and its bounds:

- rungs are contiguous, ordered, and defined by ΔT thresholds above the
  surface datum;
- the ladder's top rung begins at ΔT = 0 and its bottom rung is open-ended;
- the ladder has **at least 4 and at most 6 rungs** — fewer than 4 cannot
  carry two dwarf kinds plus a surface-adjacent zone, more than 6 exceeds the
  pelagic ladder's 5 without a measured reason to;
- the habitable ceiling sits at the ΔT beyond which the campaign declares a
  chamber uninhabitable, and that threshold is **authored once, in the spec,
  before the fit**: **ΔT = 50 K**, chosen because it puts a temperate cell's
  chamber near 60 °C, past sustained human tolerance, and because it lands the
  ladder's floor at 1.7–3.3 km — the same order as the deepest worked mines on
  Earth. It is a fidelity choice and is recorded as one.

The illustrative table below is what those bounds imply at the gradient
extremes. **It is not frozen** and Task 0 may move every boundary inside the
constraints above:

```
rung          ΔT above surface     depth @15 K/km    depth @30 K/km
Undercroft      < 2 K                  < 130 m           < 67 m
Shallows        2 – 10 K            130 – 670 m        67 – 330 m
Deeps          10 – 25 K           670 m – 1.7 km     330 – 830 m
Underdeep      25 – 50 K            1.7 – 3.3 km      830 m – 1.7 km
Sunless          > 50 K                > 3.3 km          > 1.7 km
```

### 4.2 The water table

A derived depth per cell, from overhead `drainage`, `porosity` and elevation,
splitting each column into **vadose** (air-filled, walkable) above and
**phreatic** (flooded) below. It is a pure function of fields terrain already
owns; it draws nothing and commits nothing.

Two consequences it exists to produce: a habitable window per column that is
not the same everywhere (a wet karst cell drowns shallow, a dry craton cell is
open to the Deeps), and **sumps** — a chamber below the table, which the
passage graph represents as a missing edge rather than a new type.

#### 4.2.1 AMENDED 2026-08-17, POST-TASK-3 — the underworld is allowed to be dry

Task 3 shipped H3 passing (29.9 / 45.5 / 42.5% of cave-bearing columns wholly
phreatic) and surfaced a consequence H3 does not test: with the table topping
out near 500 m against rung floors at 1042 m and 2083 m, **no `Underdeep` or
`Sunless` chamber is dry in any of the three worlds**, and `Deeps` is dry on
0–13.5%. A people whose identity is depth would then be a people living
underwater, and §5's H2 would fail by construction rather than by measurement.

Nathan's ruling, and it has three parts.

**1. The relief calibration is corrected on physical grounds.** Vadose-zone
thickness in karst is set by elevation above the local base level, and on Earth
that reaches ~2 km in high-relief massifs — Krubera-Voronja (2197 m) and
Veryovkina (2212 m) are air-filled almost to the bottom, with sumps only at
depth, because the Arabika Massif stands ~2300 m above its base level.
`RELIEF_HALF_M = 800.0` saturates the relief term far below that, so the model
under-deepens the vadose zone exactly where deep caves occur. **This is a
correction, not a rescue, and it is held to that standard: the calibration must
be argued from karst physics alone, H3 and the distribution's shape must be
re-measured after, and only then may the effect on rung dryness be looked at.
If the correction breaks H3, the correction is wrong.**

**2. A made chamber may be drained.** `ChamberOrigin::Made` — shipped by The
Deep Realm with a seam and no writer — now carries a meaning: a chamber cut for
a purpose can be dry regardless of the water table, because keeping a working
depth dry is what mining *is*. This makes a dwarven hall something a people
**does**, not merely a place it finds, and it closes the third of this
campaign's three no-producer seams.
**The producer is not optional and is not deferred silently:** §4.6's capacity
task must write `Made` for a settled subterranean community's own chambers. A
drainage rule with no producer would be a fourth dangling seam in a campaign
whose stated finding is that three already existed.

**3. This is a fantasy RPG, and its underworld may be larger and drier than
Earth's.** Earth calibration is a *floor on plausibility*, not a ceiling on
scale: the model may carry large subterranean spaces at varied moisture, and a
derivation that produces only Earth-typical vadose depths is under-serving the
genre rather than being admirably rigorous. Where the two pull apart, say which
one a number serves. A constant chosen for playability is authored and its doc
says so — the same discipline every other authored constant here carries.

### 4.3 Chamber conditions stop being constants

`subterranean_substrate` gains the chamber's depth and routes temperature
through `temperature_at_depth`. `moisture` derives from the water table's
distance and `porosity` rather than a world constant. `insolation` stays
`0.0`, which is correct and is §4.4's finding, not an oversight.

**CORRECTED 2026-08-17, POST-TASK-5 — the depth coordinate is metres, not a
rung.** This section originally said "gains the chamber's delve *rung*", and
that is not jointly satisfiable with routing temperature through
`temperature_at_depth`. Rungs are **ΔT bands**, so a temperature sampled at a
rung is gradient-free by construction: every chamber in a rung would share one
ΔT and crust age would drop out — the exact opposite of the variation §4.3
exists to create. A chamber's conditions therefore take its depth in **metres**,
and the rung remains what it always was, a *place-type* for addressing and
description rather than a coordinate to sample conditions at. Found by the
implementer, not by review.

**AND A CONSUMER-SIDE FINDING THAT BINDS §4.7.** Task 5 made chambers differ
at the substrate — the ΔT between the shallowest and deepest cave-reach
deciles spreads **0.7 → 56.0 K** on seed 42 (5.5 → 57.1 and 0.8 → 58.4 on
seeds 7 and 1234), out of a pre-change ΔT that was **identically 0.0 K at
every cave column in every world**, and 17.5–19.9% of surface-temperature
buckets now carry more than one chamber reading where pre-change that was
impossible by construction — and **no live consumer can see it.**
`tolerance_liebig` floors temperature/moisture/insolation at
`sovereignty_floor(mass, potency)` but passes elevation a literal `0.0`, so
for any kind whose elevation devotion
sits below its own floor, elevation is the Liebig minimum on every cell and no
improvement to the other three axes can reach the score. Drow's devotion is
0.30 against a floor of 0.424802; `warren_readout`'s P1 tripwire reads
ratio = 1.000 before and after, unchanged to six figures.

*Corrected 2026-08-17, same day.* The paragraph above first read "807/874,
1483/1681 and 1172/1266 cave columns now carry a distinct (temperature,
moisture) reading **where before there was one value**". The distinct-pair
figures are right and the comparison was never taken: pre-change a chamber
read `(that cell's own surface temperature, one constant)`, and the surface
temperature already varied per cell, so the "before" count was 691 / 1323 /
1030 rather than 1. The rise is real but modest (+16.8 / +12.1 / +13.8%), and
it was the wrong statistic to lead with. The controlled quantities are the two
above, and `underworld_conditions_probe` now prints the control beside every
figure it reports. Right measurement, wrong attribution — this project's most
common failure, committed here at the spec level.

**Consequence for §4.7, and it is a precondition rather than a preference:
Mountain and Duergar must be authored with `devotion_elev >
sovereignty_floor(mass, potency)`.** Otherwise the entire depth apparatus this
campaign built is invisible to exactly the two kinds it was built for, and
§5's H2 fails regardless of how well the ladder works. The pattern is
precedented — The Delvers' desert-dwarf sits at devotion 0.70 against a floor
of 0.443 and carries its identity on climate for this reason, while gully and
hill sit below their floors and are elevation-bound on 100% of land. Compute
the floor live from `hornvale_kernel::sovereignty_floor`; The Delvers' own
plan table was wrong in the fourth decimal for two of its three kinds.

### 4.4 Underworld communities as points in the axis space

The Axes' five occupied axes take underworld readings from §3.8's inputs:

- **energy inverts with depth.** Shallow chambers are powered by *detrital
  import* — organic matter arriving from above, a function of overhead
  drainage and cave kind, which is the fungal food web The Delvers corrected
  dwarves onto (`fba4c880`). Deep chambers are powered by *chemolithotrophy*
  off the gradient. The deep is not poorer; it is differently powered, and
  that is what makes a deep people viable rather than merely stubborn.
- **water** is the richest discriminator: `carbonate` says whether dissolution
  was possible at all, `porosity` what the rock holds, overhead drainage what
  arrives, and a `SaltBasin` overhead means brine.
- **substrate** takes the band's `RockClass` with `induration` and
  `metamorphic_grade`; an ore-bearing chamber reads differently from barren
  gneiss.
- **physiognomy** follows process: karst dissolves into rounded galleries, a
  lava tube is a smooth tube with collapse sections, a fracture cave is
  angular and fault-aligned.
- **light is expected to read nearly empty**, and the campaign reports that
  rather than papering it. It is the same shape as The Axes' `DISTURBANCE` — a
  declared axis the vocabulary cannot occupy — and it is informative: the
  underworld is a place where one of five axes has collapsed.

### 4.5 `EnvironmentNiche`

The species-side counterpart The Axes specified and deferred: a niche
expressed in the same five-axis basis, so a kind's preference and a place's
character are stated in one vocabulary. The Axes pinned and proved the
invariant that makes the addition safe (retrospective A-1), so this campaign
inherits a guarded seam.

### 4.6 Realm-aware capacity, and the node-index re-key

`node_index` is re-keyed from `CellId` to `(CellId, Rung)`, where `Rung` is
the delve ladder **plus an explicit `Surface` variant** — the overworld is a
rung of the same ladder, not an absence of one, so that a reader cannot mistake
"no rung" for "surface" and so the type is total. **Surface density is
unchanged**: `Surface` is a single rung, so a cell still holds one surface
community. What changes: an underworld community no longer competes
with the surface one for the cell's single slot, and two underworld
communities at different delve rungs can share a column.

Capacity for an underworld community is computed against the chamber's
conditions and its energy base, not the surface cell's.

**Explicitly not in this campaign** (decision, §7): the four carpet causes
decision 0102 names — `GENESIS_TOP_CELLS = 64` of 40,962,
`GENESIS_SITES_MIN/MAX = 2..4`, `DAUGHTER_PROB = 0.06`, and a species-blind
`factor()` — and `SOC-settlement-tiers`' derived Christaller lattice. 0102's
own words: relaxing the index alone *"would only let carpets overlap."*
**The surface will still look sparse after this campaign, and that is
expected, not a regression.**

### 4.7 Mountain-dwarf and Duergar

Authored **after** Task 0 and Task 6 have measured the ladder and the
communities, against §5's frozen criterion, and **only if that criterion is
met**. Each carries its identity on the delve rung and its community, in the
way the surviving three carry theirs on climate and elevation. `LifeSchedule`
stays the family trait it already is — long life is a dwarf trait, not a cave
trait, which is why the withdrawn kinds' departure moved nothing.

### 4.8 Language

Two names re-enter the accession cohort. The Delvers' withdrawal commit
established that cohort 9 is strictly last, so shrinking it displaced nothing
earlier — but it also established that *the surviving dwarves' own words did
move*, because removing two names re-sorts the cohort they sit in. Re-adding
two will move them again. That is expected and is not a defect.

## 5. Preregistration

Frozen before any code. A falsified prediction is a finding; several of these
are expected to come back partly negative, and the campaign ships the null as
the headline if they do.

**H1 — the ladder varies.** Over the campaign's seeds, the delve rung of an
existing chamber is not concentrated in one rung.
*Floor:* at least `n − 1` of the ladder's `n` rungs occur, `n` being whatever
Task 0's table settles on within §4.1's 4-to-6 bound. Stated relative to the
ladder's size on purpose: a fixed "at least 3" is a weak claim against a
6-rung ladder and a near-total one against a 4-rung ladder, so a fixed number
would mean different things depending on an outcome not yet known.
*Anti-degenerate bound:* no single rung holds more than 70% of chambers. This
is the
half The Deep Realm's Task 0 named explicitly — *"a world where every cave is
`Roots` scores 100% on reach-the-deepest and is exactly the falsification"* —
and it is why H1 is not merely "the ladder is populated".

**H2 — the two kinds separate, but remain one family.** Stated as a count, not
a ratio, and with both bounds:
*Floor:* the modal delve rung of Mountain differs from Duergar's, and each
clears `hornvale_demography::FLOOR` on at least one cell of every seed
(`non_void_roster` admits no allowlist).
*Anti-degenerate bound (a MINIMUM overlap, not a maximum — the degenerate pass
here is total separation, not collision):* the two kinds' top-quartile cells
overlap by **at least 20%**. Two kinds of dwarf should share most of their
habitat and differ in where they are *best*; zero overlap would mean the axis
separated them into different peoples rather than different dwarves, which is
a failure that would otherwise read as a spectacular success.
**If H2's floor fails, the two kinds are not authored and the campaign reports
why.** That is the outcome The Delvers reached the expensive way; reaching it
cheaply is a success, not a failure.

**H3 — the water table is not degenerate.** Neither fewer than 5% nor more
than 95% of cave-bearing columns are wholly phreatic.

**H4 — The Axes' A-3, evaluated.** That retrospective recorded a forward
prediction — *"campaign 2's underworld communities should land in the cave
region of the space"* — as unevaluable and ungated. This campaign evaluates
it and states the result either way.

**H5 — light collapses.** The `LIGHT` axis takes at most 2 distinct values
across the underworld corpus. Predicted before assignment, in the same form
The Axes used for its ten resisters.

## 6. Save-format and epoch consequences

Under decision 0099 worlds are version-locked, so a label change is a **cost,
not a corruption**. The bill:

- `chamber/v1` → `chamber/v2`. An epoch suffix is *available, not required*
  under 0099; it is taken here for legibility because the address space's
  meaning changes, not merely its values.
- Every seeded golden and byte-identity fixture rebaselines.
- **One census run** (~880–980 s on lefford across the last four runs,
  `docs/timings.md`). This is an autopilot carve-out and was authorized
  explicitly.
- `RoomId` is untouched. Decision 0055's cross-repo scene schemas are
  untouched.

## 7. Non-goals

- **Settlement density calibration.** §4.6. Recorded, not attempted.
- **Cave dressing and prose.** Speleothems, flowstone, ore glitter and named
  formations are `windows/locale` grammar. The cave `variant_pool` stays
  `&[]`. Captured as `MAP-underworld-dressing`.
- **Breathing caves.** Blocked on terrain: `ChamberAddr.entrance` is a `u8`
  whose every caller passes `0`. Captured as `MAP-cave-breathing`.
- ~~**`MAP-cave-depth-weld`.**~~ **PROMOTED INTO SCOPE 2026-08-16 by §4.0**,
  after Task 1 measured that the ladder cannot work without it. It was listed
  here on the reasoning that splitting the ladders does not require splitting
  the weld — true, and beside the point, because splitting the ladders does
  not *achieve* anything without it either.
- **The underworld's own chart.** `MAP-underworld-chart` is unresolved and
  this campaign does not resolve it; the pane still shows the country
  overhead.
- **A third realm.** `MAP-realm-valence`'s observer axis (one people's tomb is
  another's cathedral) is adjacent and out of scope.

## 8. Task shape

```
0  measure the ladder            probe; the rung table is its output
0b a metre depth budget          §4.0; added after Task 1 falsified §4.1
1  the delve ladder              BandKind untouched; ChamberAddr re-pointed
2  the water table               vadose/phreatic split, sumps as missing edges
3  chamber conditions            subterranean_substrate stops being constant
4  underworld communities        the five axes take underworld readings
5  EnvironmentNiche              the consumer The Axes deferred
6  capacity + the node re-key    (CellId, Rung); surface density unchanged
7  Mountain and Duergar          gated on §5 H2, authored only if it holds
8  language + accession cohort   two names re-enter cohort 9
9  the separation readout        H1–H5 evaluated and stated
```

Task 0 precedes every authored number. Task 7 is gated on a measurement taken
in Task 0 and Task 6, which is the discipline The Delvers lacked.

## 9. Provenance

Brainstorm of 2026-08-16 under `campaign-autopilot`. The decision ledger is
`.superpowers/sdd/decision-ledger.md` in the campaign worktree and is
presented at G3.

The campaign's shape turned on Nathan rejecting the framing of the first
question. Offered a choice between separating the two kinds on band, on
origin, on community, or by fixing the depth model, he answered *"if we need
sublevels of depth, we could add sublevels?"* — which is the diagnosis none of
the four options contained, and which §3.1 then confirmed by reading
`strata.rs::column`.
