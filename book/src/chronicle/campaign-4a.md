# Campaign 4a: Placement & Drainage

**July 2026 · 7 commits · outcome: complete, merged — Campaign 4 (The
People) opens with the world's first generated settlements**

## What was attempted

Every campaign through 3c built a world with no one standing in it. Terrain
had elevation, plates, and boundaries; climate had temperature, moisture,
and biome — but the single settlement every world carried, the Vale, was
still hand-fed at tier 0: one name, one population, one biome, pinned to no
coordinate on the map. Campaign 4a asks where people would actually stand,
and how many, using only fields the world already computes. It closes with
the Vale retired and a scatter of tens of generated settlements — named,
placed, populated, and rendered — taking its place.

## What landed

**A coarse drainage skeleton.** A new terrain field, computed once from the
existing elevation grid with no seed draws, sends each land cell downhill to
its lowest neighbor and accumulates upstream area along the way — a bare
flow-accumulation count, not a river network. Cells whose downhill path
never reaches the sea are flagged endorheic: interior basins, the terrain
substrate for salt lakes and closed watersheds banked back in Campaign 3c's
deferred list. The declared approximations are named up front in the model
card — single lowest-neighbor flow direction, unit-area accumulation with
no precipitation weighting, no sub-cell river geometry or lake filling —
promotable later as an epoch bump, never a silent change. `drainage_at` and
`is_endorheic` join `TerrainProvider`, exposed on the globe next to the
boundary classification climate already reads.

**Suitability, scored and placed.** A new tier-1 `hornvale-settlement`
module scores every habitable cell in `[0, 1]`: freshwater (drainage,
coastal adjacency, or moisture, whichever is highest), a coastal bonus,
and a temperance term that favors mild annual means over extremes, combined
as a weighted sum. Placement is a greedy spaced scatter — rank every site by
suitability descending (ties broken by ascending cell id for determinism),
then accept a site only if it clears a minimum angular separation from
every settlement already placed and its suitability clears a floor. The
single most-suitable cell in the whole ranking, unconditionally, is the
flagship — placement never lets the spacing rule cost the world its capital.
None of this module ever imports terrain or climate directly; the
composition root (`windows/worldgen`) assembles each cell's bare
`SiteInput` — habitability, freshwater, coastal flag, temperature,
hostility (unrest or aridity, whichever is worse) — and hands it across the
seam.

**Tier-1 genesis, and the Vale's retirement.** Each placed site is
committed as its own place entity — cell, latitude, longitude, biome, a
generated name, and a population drawn against a carrying capacity implied
by its suitability — replacing the single hand-fed village fact the tier-0
path wrote. The flagship becomes the first `is-settlement` fact in the
ledger, the same seam culture and religion genesis already close over: the
world still forms one belief about one settlement's sky, it is simply no
longer told which settlement that is. `settlement/name` and
`settlement/placement` join the stream manifest as new save-format
contracts — permanent labels, epoch-suffixed if they ever need to change.

**The settlement map, the campaign's deliverable.** A settlement-mark
renderer — kernel-only, seeing nothing but bare `(latitude, longitude)`
pairs — overlays the scatter onto the same equirectangular projection the
biome map already uses, marking the flagship distinctly from the rest. Seed
42 under a spinning sky places **58 settlements**; the flagship, **Torgna**,
holds **506** souls in temperate rainforest. The same globe under a
tidally-locked sky places only **29** — habitability collapses toward the
terminator ring, exactly as Campaign 3c's biome map predicted — with
**Bolugrak** (**559** souls, also temperate rainforest) as its flagship.
The exit-demo pair, [spinning](../gallery/settlement-seed-42.md) against
[locked](../gallery/settlement-seed-42-locked.md), is a drift-checked
gallery artifact in the First Light tradition; the almanac's People section
now reports a settlement count and names the chief settlement by population
and biome, and the REPL answers settlement queries the same way it already
answers biome and elevation ones.

## What was learned

- **A field-driven decision can still be a single weighted sum.** Suitability
  needed no simulation and no iteration — freshwater, coast, and temperance,
  combined once per cell, reproduce the intuitive pattern (river deltas and
  coastlines crowd with villages; deserts and ice sheets stay empty) without
  a model any heavier than climate's biome lookup.
- **Determinism survives a scatter, not just a single draw.** Ranking by
  suitability with an explicit tie-break on cell id, and drawing each
  settlement's name and population from a salt keyed to its own cell id
  rather than its rank, means the flagship and every other settlement are
  reproducible independent of how many others happen to be placed around
  them — a property the pin-isolation discipline from Campaign 2 demanded
  before this campaign ever needed it.
- **Retiring a tier-0 seam is a ripple, not a rewrite.** The Vale's name was
  never load-bearing outside the one path that hand-fed it; once
  `hornvale-settlement` could produce a flagship, worldgen, the REPL, and
  the almanac each needed a small, honest fix — never a trivialized
  assertion — to read the generated flagship instead, and the tier-0
  culture and religion genesis seam closed over the new entity without
  changing shape.

## Deferred, deliberately (spec §13)

Culture stays tier-0 for this campaign: the flagship gets the same single
belief-from-phenomenon religion genesis every settlement has had since
Campaign 1b, and no subsistence or emergent-culture model runs yet. The
Census of Peoples — habitability-to-settlement-count calibration, a
subsistence-mix census over many seeds, the tier-1 culture and settlement
book chapters — waits for Campaign 4b, alongside settlement pins. This
campaign banks the substrate 4b will need: drainage and endorheic basins
for freshwater and salt-lake economies, and a scatter of named, populated,
located settlements for a culture model to differentiate.

## Artifacts

[The Peoples of Seed 42](../gallery/settlement-seed-42.md) and [its
tidally-locked twin](../gallery/settlement-seed-42-locked.md) — the
exit-demo pair, hand-rolled PPM and ASCII overlay on the biome map,
drift-checked in CI.
