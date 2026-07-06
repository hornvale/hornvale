# Campaign 3: The Land — Design

**Date:** 2026-07-06
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-05-hornvale-longterm-plan-design.md` (Constitution §2 governs)
**Provenance:** The third Year-1 campaign (climate + terrain). Reframed during
brainstorming from "a bounded region at a latitude" to "the coarse tectonic
globe" after Nathan's tectonics vision; see the frontier doc
(`docs/vision/frontier.md`) for the wider context this connects to.

---

## 1. Goal

Give a world a **planet**: a coarse, whole-globe map with tectonically-grounded
mountains and ocean basins, a climate derived from the sky, and biomes — land
and sea — that follow from their intersection. The deliverable is a **queryable,
tectonically-grounded global biome map**, and the campaign's thesis is the
Year-1 arc extended from religion to the physical world: *the same land under a
different sky produces a legibly different world.*

Local, walk-around detail (fine terrain, place names, embark) is explicitly a
**later** campaign that refines this skeleton. C3 builds the coarse tier.

## 2. Design principles

1. **Coarse-first, per the Constitution's Principle 2.** Tectonics is the coarse
   causal structure that pure noise lacks; noise refines *within* it in a later
   tier. We build the coarse global cause now; fine local detail constrains
   against it later. "Coarse constrains fine" is the spine of the whole campaign.
2. **Cheap-real, not physical simulation.** Tectonics is plate *geometry* plus a
   rule table (boundary type → uplift), not mantle convection. Climate is
   analytic circulation *bands*, not fluid dynamics. Each buys the recognizable
   real-world pattern at O(cells) cost and high verification surface (Earth is
   the answer key), the same discipline that made astronomy trustworthy.
3. **Derive from astronomy wherever possible.** Climate draws almost nothing of
   its own: luminosity, obliquity, and rotation (already generated) set the
   temperature baseline, seasonality, and the *number* of circulation bands.
   Upstream richness produces legible downstream difference.
4. **Providers coexist, tiered.** The static tectonic globe is a new provider
   behind the terrain interface; the tier-0 hand-placed Vale remains valid;
   evolved-history tectonics is a future provider. A world chooses — the
   `ConstantSun`/`GeneratedSky` pattern.
5. **Lab-cheap by construction.** Per-planet generation must stay in the
   low-milliseconds so the Lab keeps sweeping ~10k worlds in seconds and the CI
   drift study stays sub-second. Expensive fidelity (evolved tectonics) is an
   opt-in provider, never the census default.

## 3. Architecture

**The kernel gains one primitive: the `Geosphere`.** An icosphere region graph
(subdivided icosahedron): cells, adjacency, and per-cell latitude/longitude,
fully determined by its subdivision level and therefore **seed-independent**
(computed once, shared across all worlds, zero per-planet mesh cost). This is
the significant architectural decision of the campaign and it is principled: the
kernel already owns `Field` (typed functions over space × time); the Geosphere
is the discretized *space* those functions live on, and the only layer below the
domains — where both terrain and climate can share it without depending on each
other — is the kernel. Coarse resolution: a subdivision level yielding order
2,500–10,000 cells.

**Two domains compute over it; neither depends on the other.**
- `domains/terrain` is the **tectonic provider**: it produces an **elevation**
  and an **unrest** value per cell, publishing them as kernel `Field`s (plus a
  few summary plate/boundary/sea-level facts). Elevation is climate-independent,
  so terrain runs first.
- `domains/climate` **consumes an elevation `Field`** as an opaque kernel type —
  never by importing terrain — computes temperature and moisture fields, and
  derives the **biome** field. It stays kernel-only-dependent; the trace protocol
  does here exactly what it did for religion and astronomy.

**Dataflow is acyclic:** terrain (elevation, unrest) → climate (temperature,
moisture, biome), wired at the composition root (`windows/worldgen`), still the
only place the two domains meet. Astronomy and religion are untouched: adding the
land edits no existing domain.

**Biomes are a derived field, not facts.** Biome-per-cell is computed on demand
from (temperature, moisture, elevation), never committed as per-cell ledger
facts. The ledger keeps only singular authored truths (sea level, plate count, a
boundary summary). Saves stay small.

## 4. The tectonic model (`domains/terrain`, static provider)

- **Plates.** Draw a plate count (range, ~8–40) from a labeled stream; scatter
  that many plate seeds on the sphere; assign each cell to its nearest seed by
  spherical distance (Voronoi plates). Each plate is flagged continental or
  oceanic against a drawn target continental fraction.
- **Motion.** Each plate draws an Euler pole (rotation axis + angular rate); a
  cell's velocity is ω × r, tangent to the sphere.
- **Boundaries.** For each graph edge between differing plates, the relative
  velocity's normal component classifies the boundary: **convergent** (closing) →
  uplift, sub-typed continent–continent (collision range), ocean–continent
  (coastal range + trench + arc), ocean–ocean (island arc + trench);
  **divergent** (opening) → continental rift or oceanic ridge; **transform**
  (tangential) → fault (little elevation, some unrest).
- **Elevation, per cell.** A continental/oceanic base + a boundary contribution
  scaling with convergence rate and decaying inland (flood-fill distance from
  boundary cells) + a few drawn hotspots. Each orogenic belt draws a **maturity**
  reshaping its profile: young = high/sharp/narrow/active, old =
  low/rounded/wide/quiet (cheap geological-history flavor without time-stepping).
- **Sea level** is derived to hit the drawn target ocean fraction (bounded to a
  habitable range gated by the habitable-zone placement), yielding the
  land/ocean mask.
- **The unrest field**, per cell, combines proximity to active boundaries,
  boundary type, and inverse maturity. Nothing consumes it in C3; it is banked
  for future architecture/theology/legend (see §15).

**Model card.**
- *Drawn:* plate count, plate seed positions, continental/oceanic flags,
  Euler-pole motions, hotspot positions, per-belt maturity, target ocean fraction.
- *Derived (geometry/rules):* cell→plate assignment, velocities, boundary types,
  elevation, sea level and land mask, unrest.
- *Approximated (declared):* static present-day snapshot (no drift — deep-time
  provider); no erosion, sediment, or isostasy beyond base; instantaneous-motion
  boundary classification; analytic uplift falloff, not a stress solution.

## 5. The climate model (`domains/climate`)

**Inputs consumed** (astronomy values + terrain fields, wired at the root):
stellar luminosity (temperature baseline via habitable-zone placement); obliquity
(seasonal amplitude and shift — consumes `Sky::calendar()`); rotation period
(band count and Coriolis); rotation direction (wind deflection sign); the
elevation field (lapse rate, rain-shadow mountains); the land/ocean mask (thermal
moderation, moisture source).

**Circulation — banded three-cell family, band count derived from rotation.** The
number of circulation cells per hemisphere is a heuristic function of rotation
period (thermal-Rossby-like): slow → 1 giant cell, Earth-rate → 3
(Hadley/Ferrel/Polar), fast → 5, 7, more. Each band carries a prevailing wind
deflected by the planet's spin; rising-air bands (equator, ~60°) are wet, sinking
bands (~30° horse latitudes, poles) dry. **Tidal lock is a distinct regime:** the
organizing axis becomes substellar–antistellar (a single day–night overturning
cell), not equator–pole.

**Temperature field** (over space × time via the calendar): insolation from
luminosity and latitude + an obliquity-driven seasonal term − lapse-rate cooling
from elevation, with ocean-adjacent cells damped.

**Moisture field:** a base set by the circulation band, raised near oceans and
lowered inland, then a **rain-shadow pass** — trace prevailing wind across the
elevation field, wetting windward slopes and drying leeward ones.

**Model card.**
- *Drawn:* essentially nothing (optional small local-variability term, minimal).
- *Derived:* band count and wind directions (from rotation), temperature, moisture.
- *Approximated (declared):* analytic bands (not fluid dynamics); no ocean
  currents (ocean is thermal buffer + moisture source only); no cloud/albedo
  feedback; smooth-sinusoid seasons; single-pass rain shadow.

## 6. Biomes (land and sea)

Biome is a queryable field over the Geosphere, derived per cell.

- **Terrestrial: Whittaker** — a lookup on (temperature, moisture) yielding
  tundra, taiga, temperate grassland, shrubland, temperate/tropical forests,
  savanna, desert, rainforest — with specials: **ocean** (below sea level),
  **ice** (below a cold threshold), **alpine** (above the latitude-adjusted tree
  line).
- **Marine** — derived from inputs already present: **depth** (elevation below
  sea level → epipelagic / mesopelagic / bathypelagic / abyssal / hadal),
  **sea-surface temperature** (coral in warm shallows, kelp in cold shallows,
  sea-ice at cold surface), **boundary feature** (hadal trench at ocean–ocean
  convergent; ridge + **hydrothermal-vent** field at oceanic divergent), and
  **upwelling** (prevailing wind × coastline → high-productivity zones). ~8–10
  marine classes; roughly doubles the biome enum at near-zero cost.
- **Habitability classification.** Cells that could host a vale-like settlement =
  land + liquid water available + a tolerable climate band. This is the
  non-opinionated version of "where a place could be," it pre-wires the embark
  seam (§8), and it yields the Lab's new unknown number (habitable fraction, §10).

The tier-0 `biome` *fact* predicate stays with the Vale; the globe's biome is a
*field*, so there is no registry conflict.

## 7. The astronomy cascade (the exit-demo thesis)

Every arrow is derived or physical: **luminosity** → temperature baseline;
**habitable-zone placement** → liquid water → oceans → moisture; **obliquity** →
seasonal amplitude (violent vs placid seasons, migrating biome bands);
**rotation period** → number of climate/biome bands (fast = many narrow stripes,
slow = few broad zones); **rotation direction** → which mountain flank is wet;
**tidal lock** → the whole map reorganizes around the substellar point. The same
tectonic globe under different skies yields legibly different biome maps — with
Earth as the answer key for the whole chain.

## 8. Integration with the existing cascade

Tiered providers coexist. The coarse globe is generated for every new world and
is fully queryable (the biome-map deliverable). **Settlement, culture, and
religion keep running on the tier-0 Vale** — a deliberate, documented
transitional seam, since a subcontinental cell is far too coarse to be a village
site. Local sites come from the **later local-refinement campaign** (embark a
habitable cell → generate its interior → *that* is the place to stand). The
Year-1 exit demo (astronomy → religion) is untouched; C3 adds a parallel demo
(astronomy → biome map).

**No Vale-pinning.** Per Nathan's guidance that the Vale is a loose sketch, we do
*not* force a "locate the Vale on the map" thread. The habitability
classification (§6) is the non-opinionated substitute and the embark seam.

## 9. Pins and the exit demo

- **Tectonic pins** (join the astronomy pins, which already steer climate):
  `--plates N`, `--ocean-fraction F`, `--supercontinent` (one landmass vs
  scattered). Persisted as scenario facts, round-tripping like every pin; `scout`
  and the Lab get them for free. Pins fail loudly with a physical reason and obey
  stream-consumption isolation (§12).
- **Exit demo:** the same land seed under different skies. Showpiece pair — seed
  42 spinning (latitude-banded biomes) vs tidally-locked (substellar desert,
  frozen far side, habitable terminator ring). Second pair — fast vs slow
  rotation (biome bands multiply vs collapse).
- **Artifact:** a hand-rolled deterministic **global biome map** (BMP/PPM image +
  an ANSI/ASCII map for the REPL), committed and drift-checked in the First Light
  tradition. Gallery: "The Map of Seed 42" and its tidally-locked twin — the
  visual sibling of the Returning One / Unblinking Eye almanac pair.

## 10. The Lab: a Census of Lands

New metrics registered (the runner never changes): plate count, ocean fraction,
mountain coverage, band count, biome distribution (land + marine), unrest
coverage, **habitable fraction**. **Calibration metric (built in, as in the
Census of Skies):** band count must equal the known function of rotation regime
(slow → 1, Earth → 3, fast → more) — validated the day it ships. Genuinely
unknown numbers: habitable fraction, ocean fraction, mountain/unrest coverage,
biome mix. A 10k `census-of-lands` study runs at author time; a ~500-seed
`census-lands-drift` study runs in CI.

## 11. CI

The artifact step gains the biome-map render(s) for the exit-demo seed(s) and the
`census-lands-drift` study rerun, both folded into the existing
`git diff --exit-code` net (extended to the new gallery/laboratory paths). The
10k census is author-time only.

## 12. Testing

- **Determinism:** seed-independent mesh; all draws from labeled streams
  (manifest-published, epoch-suffixed); `BTreeMap`/`Vec`, `total_cmp`; no
  wall-clock; same seed + pins → byte-identical globe, map, artifacts.
- **Pin isolation** (the 2a lesson): `--plates` / `--ocean-fraction` consume the
  same draws as the unpinned path — tested explicitly.
- **Geosphere:** cell count per subdivision level; mutual adjacency and edge
  counts; lat/long correctness; byte-identical mesh; Euler velocities tangent to
  the sphere.
- **Tectonic property battery** (N-seed sweep): every cell in exactly one plate;
  boundary type agrees from both sides; elevation within a declared envelope;
  sea level hits target ocean fraction within tolerance; convergent boundaries
  exceed plate interiors in elevation.
- **Climate:** band count equals the known function of rotation *as a test* (the
  calibration); temperature falls with latitude and altitude; leeward drier than
  windward; tidally-locked hottest at substellar, coldest at antistellar; coastal
  seasonal swing < continental.
- **Biomes:** Whittaker correct at known points; ocean/ice/alpine specials
  correct; trench at ocean–ocean convergent, vent at oceanic divergent, reef at
  warm shallow.
- **End-to-end:** a world exposes a queryable biome map; the almanac renders it;
  the REPL answers a per-cell query; the exit-demo test asserts seed 42's biome
  map reorganizes between spinning and tidally-locked from the same land seed.

## 13. Openers (ride-along cleanups from the Campaign 2 close)

Folded into C3 where they fit, each small: the **`sky-epoch` fact** (saves detect
being reopened under a changed generator); **genesis-notes read from the ledger's
facts** rather than re-derived; the **`Sky::calendar()` accessor** at the
composition root (climate consumes it); the **`Mm` → `Megameters` rename** (the
only abbreviated unit type; mm-collision nit); the **`HabitableZone` newtype**
(inner < outer invariant; not `Option` — anchor-first makes a zone-less world
unrepresentable), now also consumed by climate's water/temperature baseline.

## 14. Book / Definition of Done

Chronicle entry; climate and terrain chapters promoted to tier 1 with their model
cards; the biome-map gallery artifact and its tidally-locked twin; a Census-of-
Lands study chapter with comprehension-gated analysis prose (answering the
habitable-fraction question and showing the band-count calibration); regenerated
stream manifest and concept registry; freshness sweep of stale chapters.

## 15. Explicitly deferred

- **Evolved (time-stepped) tectonics** → the deep-time campaign, as an opt-in
  provider behind the same interface (cost-isolated from the census).
- **Erosion, sediment transport, isostasy** beyond the base; **ocean currents**
  and **cloud/albedo feedback**.
- **Hydrology / inland water** (rivers, lakes) → a hydrology tier / local
  refinement; lakes are often sub-cell. Banked outputs: Baikal-style rift lakes
  (we classify continental rifts) and endorheic salt basins (arid interior lows,
  à la the Dead Sea / Great Salt Lake) — flag endorheic basins coarsely only if
  cheap.
- **Marine ecology and creature/culture placement** (whale migration between
  productivity peaks, kraken/leviathan in the aphotic deep, sea elves on the
  productive shelf, R'lyeh in a hadal trench or drowned fragment near a vent) →
  the ecology and settlement campaigns. C3 banks the substrate fields (depth,
  SST, productivity, marine biome, vent/trench flags, unrest) they will consume.
- **Local refinement / embark** (fine terrain, place names, the walk-around
  scale, the place-to-stand for settlement) → the immediately-following campaign.

## 16. Risks

- **Scope breadth.** Two domains to tier 1 plus a kernel primitive is a large
  campaign. Mitigation: coarse resolution, static tectonics, analytic climate,
  biomes-as-derived-field, and aggressive deferral (§15). If the tectonic model
  alone proves large, splitting the plan into terrain-then-climate stages within
  the campaign is acceptable (they are already acyclic).
- **The kernel addition.** The Geosphere is a rare kernel change; it must be as
  small and finished as `Field`. Keep it a pure spatial substrate — no domain
  logic leaks in.
- **"Convincing enough" tectonics** is partly a taste gate, like chart legibility
  — the biome-map artifact and Nathan's read of it are the gate. Mitigation: high
  verification surface (boundary signatures, Earth's biome belts) covers
  correctness; aesthetics iterate.
- **Lab throughput.** Static per-planet cost is estimated ~1–3 ms (≈100× the
  astronomy-only census); acceptable (10k in ~tens of seconds). If a full build
  overruns comfortable author-time, the census shrinks before the model grows
  complexity — measurement first.
- **Map artifact dataviz.** Rendering a legible biome map (color choice,
  projection to 2-D, dark/light) needs the same deliberate design pass the Lab's
  charts got.

## 17. Exit criteria

1. `hornvale new` produces a world with a queryable, tectonically-grounded global
   biome map (land and sea); the REPL answers a per-cell query; the almanac
   renders the map.
2. The biome-map artifact for the exit seed reproduces byte-identically on rerun,
   and the tidally-locked twin visibly reorganizes from the same land seed.
3. The Census of Lands answers the habitable-fraction question with a number, and
   the band-count-versus-rotation calibration matches its known function exactly.
4. The Laboratory and gallery additions are live in the published book,
   drift-checked; the full gate (test + fmt + clippy) and the artifact drift
   check are green.
