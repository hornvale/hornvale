<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Settlement — The Domesday

Where and how peoples settle: placement, condensation, and the built shape of a community.

## Metrics

### `capacity-by-abs-latitude`

The carrying-capacity field's headline calibration (design spec §5): the ratio of mean per-land-vertex K (summed over the roster's PEOPLED kinds' individual fields, each species' own psychology folded in — fauna kinds have no psychology and are excluded, preserving this metric's pre-menagerie population) in the low-latitude band (|latitude| < 30) to the polar band (|latitude| > 60), the polar mean floored at POLE_FLOOR (1% of the K formula's baseline unit) so an exactly-zero polar band — the Miami NPP proxy's honest reading of hard cold, not a bug — reports a large-but-bounded ratio rather than a division blowup. A field grounded in the real biomass gradient reads well above 1 here; Absent if either band has no land vertices (a wholly ocean or wholly polar world)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.1895623 | 9.8151855 | 18.176133 | 25.467942 | 45.431826 | 18.126524 |

### `cold-built-room-share`

The share of this world's built settlement rooms that read `is_cold` (below `FURNISHING_COLD_C` at the frozen furnishing-reference day) — the fraction of the settled world where `interior_of` would compose a hearth (The Range). Replaces the cold-DOMINATION clause of `windows/lab/tests/hearth_population_calibration.rs`, which asked over 15 seeds whether ANY world exceeded 0.5 here; decision 0097 converts an existence claim sitting on a threshold into a census rate, because at n=15 the answer is decided by one world and at n=1000 it is a fraction with a sampling bound. A world's whole settled area can be temperate (0.0 is a real reading, not a broken fold); Absent only when the world has no built rooms at all.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.075539568 | 0.18742946 | 0.38928571 | 0.99428571 | 0.26315123 |

### `composition-variance`

Spatial heterogeneity of settlement composition (The Niche): the sum over roster species of the variance, across the demography report's `stack_settlements`, of each species' composition fraction. 0.0 iff every settlement has the identical species mix (the pre-Niche 'oatmeal' — one flat blend worldwide); > 0 when composition varies across space (species dominant in different strongholds). Read via `SettlementView::demography_report`, the view's own memoised build of `hornvale_worldgen::demography_report_from` (the niche-differentiated coexistence shadow) — shared with the settlement diversity metric registered above rather than each rebuilding it (task 3 of The Governor). Absent if the report fails to build or the world has fewer than 2 settlements

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.35497502 | 0.41468311 | 0.42850948 | 0.4386439 | 0.47735811 | 0.42658001 |

### `flagship-biome`

The goblin flagship settlement's committed biome; Absent if there is no goblin flagship

n = 999 present, 1 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `temperate-forest` | 521 | 52.2% |
| `taiga` | 224 | 22.4% |
| `tropical-seasonal-forest` | 217 | 21.7% |
| `alpine` | 11 | 1.1% |
| `temperate-rainforest` | 7 | 0.7% |
| `tropical-rainforest` | 7 | 0.7% |
| `epipelagic` | 4 | 0.4% |
| `savanna` | 4 | 0.4% |
| `upwelling` | 2 | 0.2% |
| `kelp-forest` | 1 | 0.1% |
| `tundra` | 1 | 0.1% |

### `flagship-coastal`

Whether the goblin flagship settlement's vertex borders an ocean vertex, recomputed from the terrain provider; Absent if there is no goblin flagship

n = 999 present, 1 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 173 | 17.3% |
| `false` | 826 | 82.7% |

### `flagship-structure-size`

Number of castes present in the goblin flagship settlement's emergent structure (a stratification proxy, matched against the same community religion's pantheon-verticality reasons about); Absent if there is no goblin flagship

n = 999 present, 1 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 3 | 3 | 3 | 3 | 2.956957 |

### `flagship-subsistence`

The goblin flagship settlement's committed subsistence mode (the pantheon's community, spec §6); Absent if there is no goblin flagship or no committed subsistence

n = 999 present, 1 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farming` | 980 | 98.1% |
| `foraging` | 11 | 1.1% |
| `fishing` | 8 | 0.8% |

### `goblin-flagship-coastal`

Whether the goblin flagship settlement's vertex borders an ocean vertex, recomputed from the terrain provider; Absent if goblins placed no settlement

n = 999 present, 1 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 173 | 17.3% |
| `false` | 826 | 82.7% |

### `goblin-flagship-roles`

The goblin flagship's committed role ladder, comma-joined, lowest to highest; Absent if goblins placed no settlement

n = 999 present, 1 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farmer,shaman,chief` | 956 | 95.7% |
| `farmer,chief` | 24 | 2.4% |
| `forager,chief` | 11 | 1.1% |
| `fisher,chief` | 8 | 0.8% |

### `goblin-flagship-surplus`

The goblin flagship vertex's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if goblins placed no settlement

n = 999 present, 1 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0061360004 | 0.48128597 | 0.51153935 | 0.53408095 | 0.81 | 0.50267552 |

### `goblin-settlement-count`

Number of settlements peopled by goblins

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 4 | 10 | 25 | 136 | 17.631 |

### `kobold-flagship-coastal`

Whether the kobold flagship settlement's vertex borders an ocean vertex, recomputed from the terrain provider; Absent if kobolds placed no settlement

n = 983 present, 17 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 9 | 0.9% |
| `false` | 974 | 99.1% |

### `kobold-flagship-roles`

The kobold flagship's committed role ladder, comma-joined, lowest to highest; Absent if kobolds placed no settlement

n = 983 present, 17 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `digger,keeper,elders` | 687 | 69.9% |
| `digger,elders` | 293 | 29.8% |
| `digger,warden,keeper,elders` | 3 | 0.3% |

### `kobold-flagship-surplus`

The kobold flagship vertex's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if kobolds placed no settlement

n = 983 present, 17 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0071005807 | 0.38193861 | 0.44417224 | 0.48714295 | 0.63 | 0.4006984 |

### `kobold-settlement-count`

Number of settlements peopled by kobolds

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 7 | 30.5 | 61 | 240 | 40.824 |

### `largest-holding-share`

M2: the largest live community's PEAK population as a share of the summed peak population of every community alive at bake end (peak_population is each occupation's historical high-water mark, not a true bake-end census — no end-state population accessor exists today; see task-4-report.md) — the entity-size reading the criticality campaigns never took

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0046330301 | 0.0078167116 | 0.0095295505 | 0.012132241 | 0.03968254 | 0.010665391 |

### `per-cell-diversity`

Mean per-vertex species diversity of the coexistence density stack (task A16a; feeds the A16b β calibration): the mean, over habitable land vertices, of the demography report's `byproducts.strife` field — already the per-vertex inverse-Herfindahl diversity 1/Σ frac_s² (1.0 when one species dominates a vertex, →N when N species share it evenly). Read via `SettlementView::demography_report`, the view's own memoised build of `hornvale_worldgen::demography_report_from` (task 3 of The Governor: shared with `composition-variance` below rather than each rebuilding it), which reconstructs the IDENTICAL report the settlement-genesis path builds internally (the shared-assembly refactor of task A16a), so this measures the stack the world actually ships, not a parallel one. Absent if the report fails to build or the world has no habitable vertices

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.3611725 | 1.647525 | 1.8448769 | 2.0110785 | 2.6063699 | 1.8364458 |

### `pop-weighted-abs-latitude`

The population-weighted mean absolute latitude across every settlement: Σ(pop·|lat|) / Σ(pop), reading each settlement's committed POPULATION and LATITUDE facts. The area-weighted mean |latitude| on a uniform sphere is ≈32.7°; people concentrating off the poles (design spec §5) should read below that baseline. Absent if there are no settlements with both facts

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 7.0988252 | 13.247148 | 16.130066 | 19.558199 | 53.175473 | 17.566463 |

### `rank-size-slope`

The OLS slope of log(population) on log(rank) across every settlement in the world (the classic Zipf rank-size diagnostic). Recorded as an OBSERVED metric only — this campaign's interim per-species condensation is deliberately NOT tuned to a rank-size target. Absent if fewer than 2 settlements exist

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.92805749 | -0.60305843 | -0.55303245 | -0.50507902 | -0.3204694 | -0.55392577 |

**Frozen claim** — *Holdings are distributed far more unequally than the endowments that produce them* (`sugarscape-1996` `sug-wealth-skew`; Ch. II, 'Emergence'; Animation II-3). Predicted median in [-1.2, -0.8]; measured -0.553032. FLAT. **NOT A BLIND TEST, disclosed under decision 0016: this statistic's distribution was measured during the brainstorm that motivated The Seedbed, before any corpus existed. Every other item in this corpus was authored before its statistic was looked at.** ([what this is](#frozen-claims))

### `settlement-count`

Number of settlements placed in the world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 61 | 231 | 303 | 373 | 629 | 304.922 |

## Weaknesses found here

### `flagship-coastal`

- **D1**: "false" holds 826/999 worlds (82.7%), at or above the 80% threshold

### `flagship-structure-size`

- **D3**: p25..p75 spans 0 (0.00% of the 1 min..max range), under the 5% bar
- **D4**: median 3 equals the max (2 .. 3)

### `flagship-subsistence`

- **D1**: "farming" holds 980/999 worlds (98.1%), at or above the 80% threshold

### `goblin-flagship-coastal`

- **D1**: "false" holds 826/999 worlds (82.7%), at or above the 80% threshold

### `goblin-flagship-roles`

- **D1**: "farmer,shaman,chief" holds 956/999 worlds (95.7%), at or above the 80% threshold

### `kobold-flagship-coastal`

- **D1**: "false" holds 974/983 worlds (99.1%), at or above the 80% threshold

### `pop-weighted-abs-latitude`

- **D5 strength**: declared moderate tracking mean-land-temperature-c, but observed r = +0.130 (1000 pairs) is weak (positive)

### `settlement-count`

- **D5 strength**: declared strong tracking habitable-fraction, but observed r = +0.222 (1000 pairs) is weak (positive)
- **D5 strength**: declared moderate tracking mountain-coverage, but observed |r| = 0.021 (1000 pairs) is none
- **D5 strength**: declared moderate tracking ocean-fraction, but observed |r| = 0.058 (1000 pairs) is none

## Frozen claims

Some metrics above carry a **frozen claim**: a prediction an imported corpus made about this population, printed beside what the committed census says today. The corpus is data this survey only reads — the corpus supplies the regularity, its source and the criterion, and the survey supplies the measurement and re-states the recorded verdict. Every part of a claim line is derived from one of those two, so a corpus that changes moves the line.

Criteria here were authored before their statistics were looked at, with 1 declared exception(s) — `sug-wealth-skew` — each of which states its own disclosure on its claim line above. Do not read this page as a page of blind predictions without checking which.

### `sugarscape-1996`

Frozen corpus: `regularities/sugarscape-1996.regularity.json`

Joshua M. Epstein and Robert Axtell, *Growing Artificial Societies: Social Science From the Bottom Up* (Brookings Institution Press / MIT Press, 1996). Items are drawn from Appendix B's complete rule roster (growback, movement, replacement, seasonal growback, pollution formation and diffusion, mating, inheritance, cultural transmission, group membership, combat, trade, credit, immune response, disease transmission) and from the emergence claims the chapters attach to those rules. The taxonomy in `emergence_type` is the book's own, from Chapter II footnote 24: type 1 is a property meaningful for an individual but exhibited only by the collective (the diagonal migration wave — "the group adopts a heading unavailable to any individual"); type 2 is a property meaningful only for a collective (a wealth distribution). AN INSTRUMENT WITH KNOWN BIAS, NEVER A STANDARD (decision 0095). Sugarscape is one 1996 lattice model of agents harvesting a renewable resource, and roughly half of its roster is economic — trade, prices, credit, inheritance of holdings — because its authors were economists building toward generative social science. Hornvale has no economy, no per-individual wealth, and no disease model, so a large block of this corpus can only ever score `absent`; that is a property of the source's coverage, not a defect Hornvale is being charged with. Conversely Sugarscape has no terrain, no astronomy, no language and no deep time, so nothing here scores Hornvale's strongest ground. Coverage measures reach against this catalogue only. THE MAPPING FROM A SUGARSCAPE CLAIM TO A CENSUS COLUMN IS AN ANALOGY, AND EACH ITEM'S `note` STATES WHERE THE ANALOGY IS LOAD-BEARING. A Sugarscape agent is an individual; a Hornvale settlement is a community. Where an item reads a per-community statistic against a per-agent claim, the item says so, and a `flat` verdict on such an item may be about the analogy rather than about the world.

