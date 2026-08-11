<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Settlement — The Domesday

Where and how peoples settle: placement, condensation, and the built shape of a community.

## Metrics

### `capacity-by-abs-latitude`

The carrying-capacity field's headline calibration (design spec §5): the ratio of mean per-land-cell K (summed over the roster's PEOPLED kinds' individual fields, each species' own psychology folded in — fauna kinds have no psychology and are excluded, preserving this metric's pre-menagerie population) in the low-latitude band (|latitude| < 30) to the polar band (|latitude| > 60), the polar mean floored at POLE_FLOOR (1% of the K formula's baseline unit) so an exactly-zero polar band — the Miami NPP proxy's honest reading of hard cold, not a bug — reports a large-but-bounded ratio rather than a division blowup. A field grounded in the real biomass gradient reads well above 1 here; Absent if either band has no land cells (a wholly ocean or wholly polar world)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.10523782 | 10.206438 | 17.748536 | 27.606996 | 46.012525 | 19.093892 |

### `cold-built-room-share`

The share of this world's built settlement rooms that read `is_cold` (below `FURNISHING_COLD_C` at the frozen furnishing-reference day) — the fraction of the settled world where `interior_of` would compose a hearth (The Range). Replaces the cold-DOMINATION clause of `windows/lab/tests/hearth_population_calibration.rs`, which asked over 15 seeds whether ANY world exceeded 0.5 here; decision 0097 converts an existence claim sitting on a threshold into a census rate, because at n=15 the answer is decided by one world and at n=1000 it is a fraction with a sampling bound. A world's whole settled area can be temperate (0.0 is a real reading, not a broken fold); Absent only when the world has no built rooms at all.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.090566038 | 0.20645257 | 0.45645646 | 0.99415205 | 0.29562584 |

### `composition-variance`

Spatial heterogeneity of settlement composition (The Niche): the sum over roster species of the variance, across the demography report's `stack_settlements`, of each species' composition fraction. 0.0 iff every settlement has the identical species mix (the pre-Niche 'oatmeal' — one flat blend worldwide); > 0 when composition varies across space (species dominant in different strongholds). Recomputed via `hornvale_worldgen::demography_report_from` (the niche-differentiated coexistence shadow). Absent if the report fails to build or the world has fewer than 2 settlements

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.26881532 | 0.40461843 | 0.42281361 | 0.44218324 | 0.47615939 | 0.42111674 |

### `flagship-biome`

The goblin flagship settlement's committed biome; Absent if there is no goblin flagship

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `temperate-forest` | 378 | 37.8% |
| `tropical-seasonal-forest` | 334 | 33.4% |
| `taiga` | 251 | 25.1% |
| `alpine` | 10 | 1.0% |
| `tropical-rainforest` | 9 | 0.9% |
| `savanna` | 7 | 0.7% |
| `temperate-rainforest` | 5 | 0.5% |
| `tundra` | 4 | 0.4% |
| `kelp-forest` | 2 | 0.2% |

### `flagship-coastal`

Whether the goblin flagship settlement's cell borders an ocean cell, recomputed from the terrain provider; Absent if there is no goblin flagship

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 196 | 19.6% |
| `false` | 804 | 80.4% |

### `flagship-structure-size`

Number of castes present in the goblin flagship settlement's emergent structure (a stratification proxy, matched against the same community religion's pantheon-verticality reasons about); Absent if there is no goblin flagship

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 3 | 3 | 3 | 3 | 2.941 |

### `flagship-subsistence`

The goblin flagship settlement's committed subsistence mode (the pantheon's community, spec §6); Absent if there is no goblin flagship or no committed subsistence

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farming` | 984 | 98.4% |
| `foraging` | 13 | 1.3% |
| `fishing` | 3 | 0.3% |

### `goblin-flagship-coastal`

Whether the goblin flagship settlement's cell borders an ocean cell, recomputed from the terrain provider; Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 196 | 19.6% |
| `false` | 804 | 80.4% |

### `goblin-flagship-roles`

The goblin flagship's committed role ladder, comma-joined, lowest to highest; Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farmer,shaman,chief` | 941 | 94.1% |
| `farmer,chief` | 43 | 4.3% |
| `forager,chief` | 13 | 1.3% |
| `fisher,chief` | 3 | 0.3% |

### `goblin-flagship-surplus`

The goblin flagship cell's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.051242011 | 0.48205121 | 0.51484233 | 0.53650558 | 0.675 | 0.50380561 |

### `goblin-settlement-count`

Number of settlements peopled by goblins

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 4 | 10 | 26 | 116 | 17.386 |

### `kobold-flagship-coastal`

Whether the kobold flagship settlement's cell borders an ocean cell, recomputed from the terrain provider; Absent if kobolds placed no settlement

n = 969 present, 31 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 8 | 0.8% |
| `false` | 961 | 99.2% |

### `kobold-flagship-roles`

The kobold flagship's committed role ladder, comma-joined, lowest to highest; Absent if kobolds placed no settlement

n = 969 present, 31 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `digger,keeper,elders` | 676 | 69.8% |
| `digger,elders` | 289 | 29.8% |
| `digger,warden,elders` | 3 | 0.3% |
| `digger,warden,keeper,elders` | 1 | 0.1% |

### `kobold-flagship-surplus`

The kobold flagship cell's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if kobolds placed no settlement

n = 969 present, 31 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.015684932 | 0.37897566 | 0.45917179 | 0.49651817 | 0.675 | 0.40163627 |

### `kobold-settlement-count`

Number of settlements peopled by kobolds

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 6 | 23 | 47 | 155 | 31.277 |

### `largest-holding-share`

M2: the largest live community's PEAK population as a share of the summed peak population of every community alive at bake end (peak_population is each occupation's historical high-water mark, not a true bake-end census — no end-state population accessor exists today; see task-4-report.md) — the entity-size reading the criticality campaigns never took

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.004535502 | 0.0090260285 | 0.011217747 | 0.014912532 | 0.0975 | 0.0137936 |

### `per-cell-diversity`

Mean per-cell species diversity of the coexistence density stack (task A16a; feeds the A16b β calibration): the mean, over habitable land cells, of the demography report's `byproducts.strife` field — already the per-cell inverse-Herfindahl diversity 1/Σ frac_s² (1.0 when one species dominates a cell, →N when N species share it evenly). Recomputed via `hornvale_worldgen::demography_report_from`, which reconstructs the IDENTICAL report the settlement-genesis path builds internally (the shared-assembly refactor of task A16a), so this measures the stack the world actually ships, not a parallel one. Absent if the report fails to build or the world has no habitable cells

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.3312554 | 1.5581067 | 2.0441962 | 2.3107915 | 2.7634865 | 1.9671027 |

### `pop-weighted-abs-latitude`

The population-weighted mean absolute latitude across every settlement: Σ(pop·|lat|) / Σ(pop), reading each settlement's committed POPULATION and LATITUDE facts. The area-weighted mean |latitude| on a uniform sphere is ≈32.7°; people concentrating off the poles (design spec §5) should read below that baseline. Absent if there are no settlements with both facts

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 4.5110446 | 9.7887782 | 13.63389 | 20.444082 | 60.614987 | 16.26462 |

### `rank-size-slope`

The OLS slope of log(population) on log(rank) across every settlement in the world (the classic Zipf rank-size diagnostic). Recorded as an OBSERVED metric only — this campaign's interim per-species condensation is deliberately NOT tuned to a rank-size target. Absent if fewer than 2 settlements exist

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -1.1826887 | -0.58498019 | -0.54219807 | -0.50306063 | -0.29811976 | -0.55340628 |

### `settlement-count`

Number of settlements placed in the world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 28 | 181 | 245 | 306 | 570 | 243.607 |

## Weaknesses found here

### `flagship-coastal`

- **D1**: "false" holds 804/1000 worlds (80.4%), at or above the 80% threshold

### `flagship-structure-size`

- **D3**: p25..p75 spans 0 (0.00% of the 1 min..max range), under the 5% bar
- **D4**: median 3 equals the max (2 .. 3)

### `flagship-subsistence`

- **D1**: "farming" holds 984/1000 worlds (98.4%), at or above the 80% threshold

### `goblin-flagship-coastal`

- **D1**: "false" holds 804/1000 worlds (80.4%), at or above the 80% threshold

### `goblin-flagship-roles`

- **D1**: "farmer,shaman,chief" holds 941/1000 worlds (94.1%), at or above the 80% threshold

### `kobold-flagship-coastal`

- **D1**: "false" holds 961/969 worlds (99.2%), at or above the 80% threshold

### `settlement-count`

- **D5 strength**: declared strong tracking habitable-fraction, but observed r = +0.261 (1000 pairs) is weak (positive)
- **D5 strength**: declared moderate tracking mountain-coverage, but observed |r| = 0.003 (1000 pairs) is none
- **D5 strength**: declared moderate tracking ocean-fraction, but observed |r| = 0.078 (1000 pairs) is none

