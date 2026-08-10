<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Settlement — The Domesday

Where and how peoples settle: placement, condensation, and the built shape of a community.

## Metrics

### `capacity-by-abs-latitude`

The carrying-capacity field's headline calibration (design spec §5): the ratio of mean per-land-cell K (summed over the roster's PEOPLED kinds' individual fields, each species' own psychology folded in — fauna kinds have no psychology and are excluded, preserving this metric's pre-menagerie population) in the low-latitude band (|latitude| < 30) to the polar band (|latitude| > 60), the polar mean floored at POLE_FLOOR (1% of the K formula's baseline unit) so an exactly-zero polar band — the Miami NPP proxy's honest reading of hard cold, not a bug — reports a large-but-bounded ratio rather than a division blowup. A field grounded in the real biomass gradient reads well above 1 here; Absent if either band has no land cells (a wholly ocean or wholly polar world)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.10593833 | 10.112223 | 17.579329 | 27.300371 | 45.984621 | 18.942285 |

### `cold-built-room-share`

The share of this world's built settlement rooms that read `is_cold` (below `FURNISHING_COLD_C` at the frozen furnishing-reference day) — the fraction of the settled world where `interior_of` would compose a hearth (The Range). Replaces the cold-DOMINATION clause of `windows/lab/tests/hearth_population_calibration.rs`, which asked over 15 seeds whether ANY world exceeded 0.5 here; decision 0097 converts an existence claim sitting on a threshold into a census rate, because at n=15 the answer is decided by one world and at n=1000 it is a fraction with a sampling bound. A world's whole settled area can be temperate (0.0 is a real reading, not a broken fold); Absent only when the world has no built rooms at all.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.054644809 | 0.19088642 | 0.46046512 | 0.99259259 | 0.28284 |

### `composition-variance`

Spatial heterogeneity of settlement composition (The Niche): the sum over roster species of the variance, across the demography report's `stack_settlements`, of each species' composition fraction. 0.0 iff every settlement has the identical species mix (the pre-Niche 'oatmeal' — one flat blend worldwide); > 0 when composition varies across space (species dominant in different strongholds). Recomputed via `hornvale_worldgen::demography_report_from` (the niche-differentiated coexistence shadow). Absent if the report fails to build or the world has fewer than 2 settlements

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.29842643 | 0.40821089 | 0.42822476 | 0.4480998 | 0.48982938 | 0.4275802 |

### `flagship-biome`

The goblin flagship settlement's committed biome; Absent if there is no goblin flagship

n = 998 present, 2 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `temperate-forest` | 389 | 39.0% |
| `tropical-seasonal-forest` | 337 | 33.8% |
| `taiga` | 238 | 23.8% |
| `tropical-rainforest` | 10 | 1.0% |
| `alpine` | 8 | 0.8% |
| `kelp-forest` | 5 | 0.5% |
| `temperate-rainforest` | 5 | 0.5% |
| `savanna` | 4 | 0.4% |
| `tundra` | 2 | 0.2% |

### `flagship-coastal`

Whether the goblin flagship settlement's cell borders an ocean cell, recomputed from the terrain provider; Absent if there is no goblin flagship

n = 998 present, 2 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 217 | 21.7% |
| `false` | 781 | 78.3% |

### `flagship-structure-size`

Number of castes present in the goblin flagship settlement's emergent structure (a stratification proxy, matched against the same community religion's pantheon-verticality reasons about); Absent if there is no goblin flagship

n = 998 present, 2 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 3 | 3 | 3 | 3 | 2.9388778 |

### `flagship-subsistence`

The goblin flagship settlement's committed subsistence mode (the pantheon's community, spec §6); Absent if there is no goblin flagship or no committed subsistence

n = 998 present, 2 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farming` | 983 | 98.5% |
| `foraging` | 10 | 1.0% |
| `fishing` | 5 | 0.5% |

### `goblin-flagship-coastal`

Whether the goblin flagship settlement's cell borders an ocean cell, recomputed from the terrain provider; Absent if goblins placed no settlement

n = 998 present, 2 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 217 | 21.7% |
| `false` | 781 | 78.3% |

### `goblin-flagship-roles`

The goblin flagship's committed role ladder, comma-joined, lowest to highest; Absent if goblins placed no settlement

n = 998 present, 2 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farmer,shaman,chief` | 937 | 93.9% |
| `farmer,chief` | 46 | 4.6% |
| `forager,chief` | 10 | 1.0% |
| `fisher,chief` | 5 | 0.5% |

### `goblin-flagship-surplus`

The goblin flagship cell's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if goblins placed no settlement

n = 998 present, 2 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.048310916 | 0.48218897 | 0.51695861 | 0.53644979 | 0.675 | 0.50523845 |

### `goblin-settlement-count`

Number of settlements peopled by goblins

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 4 | 11 | 26 | 121 | 17.571 |

### `kobold-flagship-coastal`

Whether the kobold flagship settlement's cell borders an ocean cell, recomputed from the terrain provider; Absent if kobolds placed no settlement

n = 969 present, 31 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 12 | 1.2% |
| `false` | 957 | 98.8% |

### `kobold-flagship-roles`

The kobold flagship's committed role ladder, comma-joined, lowest to highest; Absent if kobolds placed no settlement

n = 969 present, 31 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `digger,keeper,elders` | 676 | 69.8% |
| `digger,elders` | 286 | 29.5% |
| `digger,warden,elders` | 4 | 0.4% |
| `digger,warden,keeper,elders` | 3 | 0.3% |

### `kobold-flagship-surplus`

The kobold flagship cell's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if kobolds placed no settlement

n = 969 present, 31 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0028792147 | 0.38537872 | 0.45917179 | 0.49979051 | 0.675 | 0.4055979 |

### `kobold-settlement-count`

Number of settlements peopled by kobolds

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 6 | 24 | 49 | 205 | 33.076 |

### `largest-holding-share`

M2: the largest live community's PEAK population as a share of the summed peak population of every community alive at bake end (peak_population is each occupation's historical high-water mark, not a true bake-end census — no end-state population accessor exists today; see task-4-report.md) — the entity-size reading the criticality campaigns never took

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0053854343 | 0.01077296 | 0.01437219 | 0.02016546 | 0.23880597 | 0.019785831 |

### `per-cell-diversity`

Mean per-cell species diversity of the coexistence density stack (task A16a; feeds the A16b β calibration): the mean, over habitable land cells, of the demography report's `byproducts.strife` field — already the per-cell inverse-Herfindahl diversity 1/Σ frac_s² (1.0 when one species dominates a cell, →N when N species share it evenly). Recomputed via `hornvale_worldgen::demography_report_from`, which reconstructs the IDENTICAL report the settlement-genesis path builds internally (the shared-assembly refactor of task A16a), so this measures the stack the world actually ships, not a parallel one. Absent if the report fails to build or the world has no habitable cells

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.3232607 | 1.5635338 | 2.0571674 | 2.3119391 | 2.7382842 | 1.9654343 |

### `pop-weighted-abs-latitude`

The population-weighted mean absolute latitude across every settlement: Σ(pop·|lat|) / Σ(pop), reading each settlement's committed POPULATION and LATITUDE facts. The area-weighted mean |latitude| on a uniform sphere is ≈32.7°; people concentrating off the poles (design spec §5) should read below that baseline. Absent if there are no settlements with both facts

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2.8611354 | 9.0939869 | 12.73172 | 19.659811 | 58.575656 | 15.527057 |

### `rank-size-slope`

The OLS slope of log(population) on log(rank) across every settlement in the world (the classic Zipf rank-size diagnostic). Recorded as an OBSERVED metric only — this campaign's interim per-species condensation is deliberately NOT tuned to a rank-size target. Absent if fewer than 2 settlements exist

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -1.2545318 | -0.63444004 | -0.58769245 | -0.53879972 | -0.32427745 | -0.59343007 |

### `settlement-count`

Number of settlements placed in the world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 15 | 131 | 182 | 245 | 487 | 186.718 |

## Weaknesses found here

### `flagship-structure-size`

- **D3**: p25..p75 spans 0 (0.00% of the 1 min..max range), under the 5% bar
- **D4**: median 3 equals the max (2 .. 3)

### `flagship-subsistence`

- **D1**: "farming" holds 983/998 worlds (98.5%), at or above the 80% threshold

### `goblin-flagship-roles`

- **D1**: "farmer,shaman,chief" holds 937/998 worlds (93.9%), at or above the 80% threshold

### `kobold-flagship-coastal`

- **D1**: "false" holds 957/969 worlds (98.8%), at or above the 80% threshold

### `largest-holding-share`

- **D3**: p25..p75 spans 0.0093925 (4.02% of the 0.23342053570000001 min..max range), under the 5% bar

### `settlement-count`

- **D5 strength**: declared strong tracking habitable-fraction, but observed r = +0.225 (1000 pairs) is weak (positive)
- **D5 strength**: declared moderate tracking mountain-coverage, but observed |r| = 0.015 (1000 pairs) is none
- **D5 strength**: declared moderate tracking ocean-fraction, but observed |r| = 0.062 (1000 pairs) is none

