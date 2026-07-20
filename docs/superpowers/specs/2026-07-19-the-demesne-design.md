# The Demesne — per-axis spatial resource supply (BIO-35, Stage 1: abiotic)

**Campaign:** The Demesne — Stage 1 of the Living-Biomes arc
**Registry:** BIO-35 (per-axis spatial supply — this ships the abiotic half) ·
unblocks the Menagerie's strongholds test + BIO-27 (lairs) · consumes MAP-39 (The
Ground's mineral-prospectivity) and the climate NPP field
**Status:** G3 draft (awaiting Nathan)

---

## 1. Summary

The world is 100% goblins-and-hobgoblins. Measured on seed-42: **108 settlements —
goblin 45, hobgoblin 63, kobold 0, bugbear 0** — and all ~14 fauna (dragons, treant,
xorn, owlbear…) place on **zero** cells. The cause is a **rank collapse** in the
habitat carrying capacity: the resource space has 5 axes and every species carries a
`ResourceVector` uptake over them, but `supply(cell)` is a single scalar (NPP), so a
5-D niche is projected to 1-D *before* the spatial field applies. A uptake vector can
only *rescale* one common number — it never *selects a different place*. So a niche
shifts magnitude everywhere, never location, and the two strongest goblinoids win the
whole planet.

The Demesne **restores the rank**: `supply(cell)` becomes a **vector field** — one
spatial field per resource axis — and the per-species K's existing saturating
weighted sum `Σ_axis uptake[a]·supply[a](cell)` now peaks in *different places* for
*different niches*. This is Stage 1: the **abiotic** axes (photosynthate, plant
forage, mineral, detritus), whose fields already exist or are ambient. It's the
sibling of The Confluence (which spatialized water's contribution to K); together
they finish making K spatial. **Stage 2 (the `ANIMAL_PREY` field — the food web)** is
the explicit next campaign.

## 2. Background

The substrate is nearly all present. `kernel/src/ecology.rs`: the open resource-axis
basis `[PHOTOSYNTHATE, PLANT_FORAGE, ANIMAL_PREY, DETRITUS, MINERAL]` + `ResourceVector`
(per-species uptake weights) + ambient-vs-depletable axis kinds. `domains/demography/`:
the coexistence stack (`coexist.rs` SINR share), the trophic model (`niche.rs` guild
overlap / trophic levels / predation), the **stronghold & refugia machinery**
(`byproducts.rs` — `dominant_species`, per-species stronghold cells), and stack
condensation into settlements (`stack_condense.rs → CoexistStack → condense_stack`).
The K formula (BIO-25, The Niche) is `K = supply(saturating weighted sum over niche
uptake) × Π condition-response` — but it is *fed a rank-1 scalar supply*.

The Living-Biomes arc (captured): **Stage 1 The Demesne** (abiotic rank-restoration)
→ **Stage 2** the prey field / trophic food web → **Stage 3+** richer per-biome
rosters and dynamics (seasonal prey, predator-prey cycles, migration).

## 3. The design

### 3.1 The supply vector field

Replace the scalar supply with a per-axis `supply[axis](cell)`:

- **`PHOTOSYNTHATE`** — the spatial **NPP** field (insolation × moisture, already
  computed by climate/The Gathering). This is the primary-production supply.
- **`PLANT_FORAGE`** — an NPP-derived field (a documented transform/fraction of
  photosynthate — grazable plant matter tracks primary production spatially).
- **`MINERAL`** — **MAP-39's mineral-prospectivity field** (The Ground; a spatial
  per-cell mineral-availability read), normalized into the supply's `[0, …]` range.
- **`DETRITUS`** — **ambient** (the basis already marks axes ambient; dead-matter
  supply is treated as broadly available this stage — a small constant or a light
  NPP-derived floor). Not a new spatial field.
- **`ANIMAL_PREY`** — **Stage 2** (a derived herbivore-biomass field). This stage
  leaves it at its current (ambient/zero-level) treatment so predators simply do not
  yet gain spatial prey — honest: their strongholds wait.

Each field lands as a `CellMap<f64>` built at genesis where the inputs are in scope
(NPP from climate, mineral from terrain), assembled into a per-cell axis→supply map.
Fields are **normalized** so the axes are comparable in the weighted sum (the one
calibration surface — §8).

### 3.2 Wiring the K weighted sum

At the per-species K site (the `CoexistStack` builder that today multiplies uptake by
the single NPP), replace `uptake · NPP(cell)` with the axis dot product `Σ_axis
uptake[a]·supply[a](cell)`, fed to the same saturating curve. The uptake vectors, the
SINR competition, the trophic model, `dominant_species`, the stronghold machinery, and
`condense_stack` are all **unchanged** — they consume the richer per-species K field
automatically. This is the rank-restoration; the code delta at the K site is small.

### 3.3 What emerges (Stage 1)

A mineral-uptake kind (kobold, xorn) now peaks where minerals are; a photosynthate
kind (treant) peaks in high-NPP terrain; the goblinoids differentiate by the
terrain each favors instead of all winning the same NPP. Distinct dominants rise past
2; treant and xorn gain strongholds. Predators/dragons still lack a place (prey is
Stage 2) — stated honestly, not faked.

## 4. Architecture & files

- **`domains/demography/`** (or a small worldgen assembly) — the per-axis supply
  vector builder (`CellMap` per axis, from NPP + mineral-prospectivity + ambient);
  the K site consuming the dot product. Fields are pure over existing globe/climate
  fields — no seed draws.
- **`kernel/src/ecology.rs`** — if the supply needs a typed carrier (e.g. a
  per-cell `SupplyVector` parallel to `ResourceVector`), it lives beside the basis;
  otherwise a `BTreeMap<axis, CellMap<f64>>` in demography suffices. Prefer the
  minimal shape.
- **`windows/worldgen/src/lib.rs`** — the composition root passes the NPP and
  mineral-prospectivity fields into the supply builder (both already read there).
- **Re-calibration** — the biomass-gradient + settlement-count grounding
  (the-gathering discipline) re-checked; normalization constants re-pinned.
- **Genesis artifacts** — settlement gallery, census (settlement composition metrics),
  and downstream re-baseline under the epoch.

## 5. Testing & acceptance

The deliverable is **emergent placement diversity**, measured against the baseline:

1. **The supply vector is correct & deterministic** — per-axis fields on a synthetic
   globe: mineral peaks where prospectivity peaks, photosynthate where NPP peaks;
   pure, `total_cmp`, no HashMap, reload-stable.
2. **The K dot product restores rank** — a unit test: two species with different
   uptake vectors, over a planted supply where axis-A peaks in cell 1 and axis-B in
   cell 2, get their K peaks in *different* cells. (Mutation: the old scalar supply
   makes both peak in the same cell — fails.)
3. **The emergent keystone (preregistered, measured).** On seed-42 (+ a small sweep),
   the count of **distinct dominant kinds** rises materially past the baseline (2:
   goblin, hobgoblin), and treant/xorn appear as dominants or stronghold holders. The
   baseline is measured BEFORE the wiring and frozen as a const; the threshold is set
   from baseline + theory (the "distinct dominant" ruler must not trivially count
   every kind — the Confluence denominator-artifact lesson). Never authored per-cell.
4. **The Menagerie strongholds test, re-enabled to Stage-1 extent** —
   `menagerie_fauna_hold_resource_partitioned_strongholds` un-`#[ignore]`d for the
   abiotic-reachable part (≥ the preregistered distinct-dominant count + treant/xorn
   strongholds); the dragon/prey assertions stay `#[ignore]`d/split as Stage 2.
5. **K stays calibrated** — biomass-by-latitude gradient in tolerance; seed-42
   settlement count in a sane band (re-fit normalization if needed, the-gathering
   discipline).
6. **Same-seed byte-identity** — same seed + pins → byte-identical world under the
   new behavior.

Synthetic-globe tests for 1–2; real seed-42 (+ sweep) for 3–5, all `SKIP_CENSUS=1`.

## 6. Determinism & save-format (leads G3)

**Genesis-changing → an EPOCH + a CENSUS REGEN.** The spatial supply changes
per-species K → the coexistence stack → which kind dominates each cell → settlement
composition and placement MOVE. Likely **not a stream-label epoch** (placement is a
deterministic function of K, no new seed draws — same finding as The Confluence; the
plan confirms via the stream manifest). The **census regen is Nathan's explicit AWS
step, run BACKGROUNDED** (`make regen-remote` exceeds the 60-min foreground ceiling —
The Confluence tooling lesson); census fixtures lag until then (the accepted trade).
Local runs `SKIP_CENSUS=1`; artifacts (settlement gallery, downstream) re-baseline
in-commit; determinism rules hold (no HashMap, `total_cmp`, no RNG in the field
builders). Calibration constants are frozen save-format values.

## 7. Non-goals (the arc's later stages)

- **`ANIMAL_PREY` / the trophic food web (Stage 2)** — the derived herbivore-biomass
  field; predator/dragon strongholds; the full menagerie test.
- **Richer per-biome rosters, dynamics** (Stage 3+): many plants/animals per
  landscape, seasonal prey, predator-prey cycles, migration.
- **New peopled races** — a companion roster expansion; rides *behind* the spatial
  supply (new peoples need distinct terrain to hold, which this provides).
- **A fresh-water resource axis** — water already enters K via The Confluence's
  freshwater term (a condition, not a resource axis); a water *axis* is a basis
  extension, deferred.

## 8. Judgment calls

- **Field normalization** — each axis's supply field is normalized so the weighted
  sum is balanced (a mineral peak and an NPP peak comparably valued). The one
  calibration surface; tuned so K stays biomass-grounded and the settlement count
  stays sane, and documented with provenance.
- **The distinct-dominant threshold** — the preregistered acceptance number, set from
  the baseline (2) + theory, frozen before the readout; the ruler must be
  non-trivial (not "every kind that appears once").
- **DETRITUS treatment** — ambient this stage (a constant/NPP-floor); a real
  detritus field is a later refinement.

## 9. Task shape (for the plan)

Roughly four tasks: (T1) the per-axis supply vector builder (photosynthate/forage from
NPP, mineral from MAP-39, detritus ambient) + its determinism tests; (T2) wire the
per-species K to the axis dot product + the rank-restoration unit test + the emergent
distinct-dominants keystone (preregistered vs the measured baseline); (T3) re-calibrate
(biomass-gradient + settlement-count grounding, re-fit normalization) + the epoch
surface + same-seed identity + re-enable the menagerie strongholds test to Stage-1
extent; (T4) artifact re-baselines + chronicle/retro/registry (advance BIO-35, note the
Living-Biomes arc) + close. Census regen = Nathan's backgrounded AWS step at merge.
