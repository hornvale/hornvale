<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Settlement — The Domesday

Where and how peoples settle: placement, condensation, and the built shape of a community.

## Metrics

### `capacity-by-abs-latitude`

The carrying-capacity field's headline calibration (design spec §5): the ratio of mean per-land-vertex K (summed over the roster's PEOPLED kinds' individual fields, each species' own psychology folded in — fauna kinds have no psychology and are excluded, preserving this metric's pre-menagerie population) in the low-latitude band (|latitude| < 30) to the polar band (|latitude| > 60), the polar mean floored at POLE_FLOOR (1% of the K formula's baseline unit) so an exactly-zero polar band — the Miami NPP proxy's honest reading of hard cold, not a bug — reports a large-but-bounded ratio rather than a division blowup. A field grounded in the real biomass gradient reads well above 1 here; Absent if either band has no land vertices (a wholly ocean or wholly polar world)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.18976045 | 9.7876365 | 18.16338 | 25.370198 | 45.238175 | 18.046899 |

### `cold-built-room-share`

The share of this world's built settlement rooms that read `is_cold` (below `FURNISHING_COLD_C` at the frozen furnishing-reference day) — the fraction of the settled world where `interior_of` would compose a hearth (The Range). Replaces the cold-DOMINATION clause of `windows/lab/tests/hearth_population_calibration.rs`, which asked over 15 seeds whether ANY world exceeded 0.5 here; decision 0097 converts an existence claim sitting on a threshold into a census rate, because at n=15 the answer is decided by one world and at n=1000 it is a fraction with a sampling bound. A world's whole settled area can be temperate (0.0 is a real reading, not a broken fold); Absent only when the world has no built rooms at all.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.0651341 | 0.18529412 | 0.375 | 1 | 0.25863577 |

### `composition-variance`

Spatial heterogeneity of settlement composition (The Niche): the sum over roster species of the variance, across the demography report's `stack_settlements`, of each species' composition fraction. 0.0 iff every settlement has the identical species mix (the pre-Niche 'oatmeal' — one flat blend worldwide); > 0 when composition varies across space (species dominant in different strongholds). Read via `SettlementView::demography_report`, the view's own memoised build of `hornvale_worldgen::demography_report_from` (the niche-differentiated coexistence shadow) — shared with the settlement diversity metric registered above rather than each rebuilding it (task 3 of The Governor). Absent if the report fails to build or the world has fewer than 2 settlements

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.35610433 | 0.41498866 | 0.42792797 | 0.43874902 | 0.47702178 | 0.42660262 |

### `flagship-biome`

The goblin flagship settlement's committed biome; Absent if there is no goblin flagship

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `temperate-forest` | 519 | 51.9% |
| `taiga` | 219 | 21.9% |
| `tropical-seasonal-forest` | 215 | 21.5% |
| `alpine` | 11 | 1.1% |
| `tropical-rainforest` | 10 | 1.0% |
| `temperate-rainforest` | 9 | 0.9% |
| `savanna` | 5 | 0.5% |
| `epipelagic` | 3 | 0.3% |
| `kelp-forest` | 3 | 0.3% |
| `coral-reef` | 2 | 0.2% |
| `upwelling` | 2 | 0.2% |
| `hydrothermal-vent` | 1 | 0.1% |
| `shrubland` | 1 | 0.1% |

### `flagship-coastal`

Whether the goblin flagship settlement's vertex borders an ocean vertex, recomputed from the terrain provider; Absent if there is no goblin flagship

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 194 | 19.4% |
| `false` | 806 | 80.6% |

### `flagship-structure-size`

Number of castes present in the goblin flagship settlement's emergent structure (a stratification proxy, matched against the same community religion's pantheon-verticality reasons about); Absent if there is no goblin flagship

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 3 | 3 | 3 | 3 | 2.947 |

### `flagship-subsistence`

The goblin flagship settlement's committed subsistence mode (the pantheon's community, spec §6); Absent if there is no goblin flagship or no committed subsistence

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farming` | 977 | 97.7% |
| `fishing` | 13 | 1.3% |
| `foraging` | 10 | 1.0% |

### `goblin-flagship-coastal`

Whether the goblin flagship settlement's vertex borders an ocean vertex, recomputed from the terrain provider; Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 194 | 19.4% |
| `false` | 806 | 80.6% |

### `goblin-flagship-roles`

The goblin flagship's committed role ladder, comma-joined, lowest to highest; Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farmer,shaman,chief` | 947 | 94.7% |
| `farmer,chief` | 30 | 3.0% |
| `fisher,chief` | 13 | 1.3% |
| `forager,chief` | 10 | 1.0% |

### `goblin-flagship-surplus`

The goblin flagship vertex's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.04359502 | 0.48178659 | 0.51127257 | 0.53485672 | 0.81 | 0.50174169 |

### `goblin-settlement-count`

Number of settlements peopled by goblins

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 4 | 10 | 25 | 137 | 17.045 |

### `kobold-flagship-coastal`

Whether the kobold flagship settlement's vertex borders an ocean vertex, recomputed from the terrain provider; Absent if kobolds placed no settlement

n = 981 present, 19 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 5 | 0.5% |
| `false` | 976 | 99.5% |

### `kobold-flagship-roles`

The kobold flagship's committed role ladder, comma-joined, lowest to highest; Absent if kobolds placed no settlement

n = 981 present, 19 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `digger,keeper,elders` | 668 | 68.1% |
| `digger,elders` | 308 | 31.4% |
| `digger,warden,keeper,elders` | 3 | 0.3% |
| `digger,warden,elders` | 2 | 0.2% |

### `kobold-flagship-surplus`

The kobold flagship vertex's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if kobolds placed no settlement

n = 981 present, 19 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.011362697 | 0.37736925 | 0.44538897 | 0.48639147 | 0.675 | 0.39784515 |

### `kobold-settlement-count`

Number of settlements peopled by kobolds

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 7 | 27 | 59 | 228 | 37.985 |

### `largest-holding-share`

M2: the largest live community's PEAK population as a share of the summed peak population of every community alive at bake end (peak_population is each occupation's historical high-water mark, not a true bake-end census — no end-state population accessor exists today; see task-4-report.md) — the entity-size reading the criticality campaigns never took

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0046137466 | 0.0087194566 | 0.011000344 | 0.014722537 | 0.074398249 | 0.013437741 |

### `per-cell-diversity`

Mean per-vertex species diversity of the coexistence density stack (task A16a; feeds the A16b β calibration): the mean, over habitable land vertices, of the demography report's `byproducts.strife` field — already the per-vertex inverse-Herfindahl diversity 1/Σ frac_s² (1.0 when one species dominates a vertex, →N when N species share it evenly). Read via `SettlementView::demography_report`, the view's own memoised build of `hornvale_worldgen::demography_report_from` (task 3 of The Governor: shared with `composition-variance` below rather than each rebuilding it), which reconstructs the IDENTICAL report the settlement-genesis path builds internally (the shared-assembly refactor of task A16a), so this measures the stack the world actually ships, not a parallel one. Absent if the report fails to build or the world has no habitable vertices

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.36282 | 1.6598524 | 1.8527628 | 2.028553 | 2.6238258 | 1.8474065 |

### `pop-weighted-abs-latitude`

The population-weighted mean absolute latitude across every settlement: Σ(pop·|lat|) / Σ(pop), reading each settlement's committed POPULATION and LATITUDE facts. The area-weighted mean |latitude| on a uniform sphere is ≈32.7°; people concentrating off the poles (design spec §5) should read below that baseline. Absent if there are no settlements with both facts

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 6.4895428 | 12.546923 | 15.61277 | 20.015202 | 57.541575 | 17.477479 |

### `rank-size-slope`

The OLS slope of log(population) on log(rank) across every settlement in the world (the classic Zipf rank-size diagnostic). Recorded as an OBSERVED metric only — this campaign's interim per-species condensation is deliberately NOT tuned to a rank-size target. Absent if fewer than 2 settlements exist

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -1.0179198 | -0.63078877 | -0.58274663 | -0.53414205 | -0.33220686 | -0.58442826 |

**Frozen claim** — *Holdings are distributed far more unequally than the endowments that produce them* (`sugarscape-1996` `sug-wealth-skew`; Ch. II, 'Emergence'; Animation II-3). Predicted median in [-1.2, -0.8]; measured -0.582747. FLAT. **NOT A BLIND TEST, disclosed under decision 0016: this statistic's distribution was measured during the brainstorm that motivated The Seedbed, before any corpus existed. Every other item in this corpus was authored before its statistic was looked at.** ([what this is](#frozen-claims))

### `settlement-count`

Number of settlements placed in the world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 30 | 192 | 262.5 | 328 | 606 | 259.854 |

## Weaknesses found here

### `flagship-coastal`

- **D1**: "false" holds 806/1000 worlds (80.6%), at or above the 80% threshold

### `flagship-structure-size`

- **D3**: p25..p75 spans 0 (0.00% of the 1 min..max range), under the 5% bar
- **D4**: median 3 equals the max (2 .. 3)

### `flagship-subsistence`

- **D1**: "farming" holds 977/1000 worlds (97.7%), at or above the 80% threshold

### `goblin-flagship-coastal`

- **D1**: "false" holds 806/1000 worlds (80.6%), at or above the 80% threshold

### `goblin-flagship-roles`

- **D1**: "farmer,shaman,chief" holds 947/1000 worlds (94.7%), at or above the 80% threshold

### `kobold-flagship-coastal`

- **D1**: "false" holds 976/981 worlds (99.5%), at or above the 80% threshold

### `pop-weighted-abs-latitude`

- **D5 strength**: declared moderate tracking mean-land-temperature-c, but observed r = +0.154 (1000 pairs) is weak (positive)

### `settlement-count`

- **D5 strength**: declared strong tracking habitable-fraction, but observed r = +0.160 (1000 pairs) is weak (positive)
- **D5 strength**: declared moderate tracking mountain-coverage, but observed |r| = 0.071 (1000 pairs) is none
- **D5 strength**: declared moderate tracking ocean-fraction, but observed |r| = 0.023 (1000 pairs) is none

## Frozen claims

Some metrics above carry a **frozen claim**: a prediction an imported corpus made about this population, printed beside what the committed census says today. The corpus is data this survey only reads — the corpus supplies the regularity, its source and the criterion, and the survey supplies the measurement and re-states the recorded verdict. Every part of a claim line is derived from one of those two, so a corpus that changes moves the line.

Criteria here were authored before their statistics were looked at, with 1 declared exception(s) — `sug-wealth-skew` — each of which states its own disclosure on its claim line above. Do not read this page as a page of blind predictions without checking which.

### `sugarscape-1996`

Frozen corpus: `regularities/sugarscape-1996.regularity.json`

Joshua M. Epstein and Robert Axtell, *Growing Artificial Societies: Social Science From the Bottom Up* (Brookings Institution Press / MIT Press, 1996). Items are drawn from Appendix B's complete rule roster (growback, movement, replacement, seasonal growback, pollution formation and diffusion, mating, inheritance, cultural transmission, group membership, combat, trade, credit, immune response, disease transmission) and from the emergence claims the chapters attach to those rules. The taxonomy in `emergence_type` is the book's own, from Chapter II footnote 24: type 1 is a property meaningful for an individual but exhibited only by the collective (the diagonal migration wave — "the group adopts a heading unavailable to any individual"); type 2 is a property meaningful only for a collective (a wealth distribution). AN INSTRUMENT WITH KNOWN BIAS, NEVER A STANDARD (decision 0095). Sugarscape is one 1996 lattice model of agents harvesting a renewable resource, and roughly half of its roster is economic — trade, prices, credit, inheritance of holdings — because its authors were economists building toward generative social science. Hornvale has no economy, no per-individual wealth, and no disease model, so a large block of this corpus can only ever score `absent`; that is a property of the source's coverage, not a defect Hornvale is being charged with. Conversely Sugarscape has no terrain, no astronomy, no language and no deep time, so nothing here scores Hornvale's strongest ground. Coverage measures reach against this catalogue only. THE MAPPING FROM A SUGARSCAPE CLAIM TO A CENSUS COLUMN IS AN ANALOGY, AND EACH ITEM'S `note` STATES WHERE THE ANALOGY IS LOAD-BEARING. A Sugarscape agent is an individual; a Hornvale settlement is a community. Where an item reads a per-community statistic against a per-agent claim, the item says so, and a `flat` verdict on such an item may be about the analogy rather than about the world.

