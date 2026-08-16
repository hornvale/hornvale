<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Settlement — The Domesday

Where and how peoples settle: placement, condensation, and the built shape of a community.

## Metrics

### `capacity-by-abs-latitude`

The carrying-capacity field's headline calibration (design spec §5): the ratio of mean per-land-cell K (summed over the roster's PEOPLED kinds' individual fields, each species' own psychology folded in — fauna kinds have no psychology and are excluded, preserving this metric's pre-menagerie population) in the low-latitude band (|latitude| < 30) to the polar band (|latitude| > 60), the polar mean floored at POLE_FLOOR (1% of the K formula's baseline unit) so an exactly-zero polar band — the Miami NPP proxy's honest reading of hard cold, not a bug — reports a large-but-bounded ratio rather than a division blowup. A field grounded in the real biomass gradient reads well above 1 here; Absent if either band has no land cells (a wholly ocean or wholly polar world)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.18976045 | 9.8061221 | 18.180146 | 25.453434 | 45.238175 | 18.081944 |

### `cold-built-room-share`

The share of this world's built settlement rooms that read `is_cold` (below `FURNISHING_COLD_C` at the frozen furnishing-reference day) — the fraction of the settled world where `interior_of` would compose a hearth (The Range). Replaces the cold-DOMINATION clause of `windows/lab/tests/hearth_population_calibration.rs`, which asked over 15 seeds whether ANY world exceeded 0.5 here; decision 0097 converts an existence claim sitting on a threshold into a census rate, because at n=15 the answer is decided by one world and at n=1000 it is a fraction with a sampling bound. A world's whole settled area can be temperate (0.0 is a real reading, not a broken fold); Absent only when the world has no built rooms at all.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.071782178 | 0.18369718 | 0.37442922 | 0.99514563 | 0.25152237 |

### `composition-variance`

Spatial heterogeneity of settlement composition (The Niche): the sum over roster species of the variance, across the demography report's `stack_settlements`, of each species' composition fraction. 0.0 iff every settlement has the identical species mix (the pre-Niche 'oatmeal' — one flat blend worldwide); > 0 when composition varies across space (species dominant in different strongholds). Recomputed via `hornvale_worldgen::demography_report_from` (the niche-differentiated coexistence shadow). Absent if the report fails to build or the world has fewer than 2 settlements

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.35687662 | 0.41618936 | 0.42957855 | 0.43992486 | 0.47748056 | 0.42786437 |

### `flagship-biome`

The goblin flagship settlement's committed biome; Absent if there is no goblin flagship

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `temperate-forest` | 536 | 53.6% |
| `tropical-seasonal-forest` | 210 | 21.0% |
| `taiga` | 209 | 20.9% |
| `tropical-rainforest` | 13 | 1.3% |
| `alpine` | 10 | 1.0% |
| `temperate-rainforest` | 6 | 0.6% |
| `savanna` | 5 | 0.5% |
| `epipelagic` | 4 | 0.4% |
| `kelp-forest` | 4 | 0.4% |
| `shrubland` | 1 | 0.1% |
| `tundra` | 1 | 0.1% |
| `upwelling` | 1 | 0.1% |

### `flagship-coastal`

Whether the goblin flagship settlement's cell borders an ocean cell, recomputed from the terrain provider; Absent if there is no goblin flagship

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 189 | 18.9% |
| `false` | 811 | 81.1% |

### `flagship-structure-size`

Number of castes present in the goblin flagship settlement's emergent structure (a stratification proxy, matched against the same community religion's pantheon-verticality reasons about); Absent if there is no goblin flagship

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 3 | 3 | 3 | 3 | 2.943 |

### `flagship-subsistence`

The goblin flagship settlement's committed subsistence mode (the pantheon's community, spec §6); Absent if there is no goblin flagship or no committed subsistence

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farming` | 979 | 97.9% |
| `foraging` | 11 | 1.1% |
| `fishing` | 9 | 0.9% |
| `herding` | 1 | 0.1% |

### `goblin-flagship-coastal`

Whether the goblin flagship settlement's cell borders an ocean cell, recomputed from the terrain provider; Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 189 | 18.9% |
| `false` | 811 | 81.1% |

### `goblin-flagship-roles`

The goblin flagship's committed role ladder, comma-joined, lowest to highest; Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farmer,shaman,chief` | 943 | 94.3% |
| `farmer,chief` | 36 | 3.6% |
| `forager,chief` | 11 | 1.1% |
| `fisher,chief` | 9 | 0.9% |
| `herder,chief` | 1 | 0.1% |

### `goblin-flagship-surplus`

The goblin flagship cell's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.046627014 | 0.47957214 | 0.51156628 | 0.5352195 | 0.72 | 0.50127058 |

### `goblin-settlement-count`

Number of settlements peopled by goblins

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 4 | 12 | 27 | 110 | 18.517 |

### `kobold-flagship-coastal`

Whether the kobold flagship settlement's cell borders an ocean cell, recomputed from the terrain provider; Absent if kobolds placed no settlement

n = 982 present, 18 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 9 | 0.9% |
| `false` | 973 | 99.1% |

### `kobold-flagship-roles`

The kobold flagship's committed role ladder, comma-joined, lowest to highest; Absent if kobolds placed no settlement

n = 982 present, 18 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `digger,keeper,elders` | 694 | 70.7% |
| `digger,elders` | 285 | 29.0% |
| `digger,warden,keeper,elders` | 3 | 0.3% |

### `kobold-flagship-surplus`

The kobold flagship cell's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if kobolds placed no settlement

n = 982 present, 18 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0054824858 | 0.38576172 | 0.44863246 | 0.48845809 | 0.72 | 0.40405241 |

### `kobold-settlement-count`

Number of settlements peopled by kobolds

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 7 | 26 | 50 | 197 | 33.218 |

### `largest-holding-share`

M2: the largest live community's PEAK population as a share of the summed peak population of every community alive at bake end (peak_population is each occupation's historical high-water mark, not a true bake-end census — no end-state population accessor exists today; see task-4-report.md) — the entity-size reading the criticality campaigns never took

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0048845471 | 0.0087385167 | 0.01064025 | 0.014072709 | 0.064 | 0.012948304 |

### `per-cell-diversity`

Mean per-cell species diversity of the coexistence density stack (task A16a; feeds the A16b β calibration): the mean, over habitable land cells, of the demography report's `byproducts.strife` field — already the per-cell inverse-Herfindahl diversity 1/Σ frac_s² (1.0 when one species dominates a cell, →N when N species share it evenly). Recomputed via `hornvale_worldgen::demography_report_from`, which reconstructs the IDENTICAL report the settlement-genesis path builds internally (the shared-assembly refactor of task A16a), so this measures the stack the world actually ships, not a parallel one. Absent if the report fails to build or the world has no habitable cells

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.3591591 | 1.6501751 | 1.8462683 | 2.0148184 | 2.614093 | 1.8383944 |

### `pop-weighted-abs-latitude`

The population-weighted mean absolute latitude across every settlement: Σ(pop·|lat|) / Σ(pop), reading each settlement's committed POPULATION and LATITUDE facts. The area-weighted mean |latitude| on a uniform sphere is ≈32.7°; people concentrating off the poles (design spec §5) should read below that baseline. Absent if there are no settlements with both facts

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 4.4025005 | 12.676559 | 15.600462 | 19.732805 | 57.534294 | 17.376625 |

### `rank-size-slope`

The OLS slope of log(population) on log(rank) across every settlement in the world (the classic Zipf rank-size diagnostic). Recorded as an OBSERVED metric only — this campaign's interim per-species condensation is deliberately NOT tuned to a rank-size target. Absent if fewer than 2 settlements exist

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -1.0689988 | -0.60424983 | -0.56136521 | -0.51367556 | -0.32811709 | -0.56511822 |

### `settlement-count`

Number of settlements placed in the world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 40 | 196 | 260 | 320 | 548 | 257.515 |

## Weaknesses found here

### `flagship-coastal`

- **D1**: "false" holds 811/1000 worlds (81.1%), at or above the 80% threshold

### `flagship-structure-size`

- **D3**: p25..p75 spans 0 (0.00% of the 1 min..max range), under the 5% bar
- **D4**: median 3 equals the max (2 .. 3)

### `flagship-subsistence`

- **D1**: "farming" holds 979/1000 worlds (97.9%), at or above the 80% threshold

### `goblin-flagship-coastal`

- **D1**: "false" holds 811/1000 worlds (81.1%), at or above the 80% threshold

### `goblin-flagship-roles`

- **D1**: "farmer,shaman,chief" holds 943/1000 worlds (94.3%), at or above the 80% threshold

### `kobold-flagship-coastal`

- **D1**: "false" holds 973/982 worlds (99.1%), at or above the 80% threshold

### `pop-weighted-abs-latitude`

- **D5 strength**: declared moderate tracking mean-land-temperature-c, but observed r = +0.144 (1000 pairs) is weak (positive)

### `settlement-count`

- **D5 strength**: declared strong tracking habitable-fraction, but observed r = +0.218 (1000 pairs) is weak (positive)
- **D5 strength**: declared moderate tracking mountain-coverage, but observed |r| = 0.002 (1000 pairs) is none
- **D5 strength**: declared moderate tracking ocean-fraction, but observed |r| = 0.044 (1000 pairs) is none

