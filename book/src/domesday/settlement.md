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

### `composition-variance`

Spatial heterogeneity of settlement composition (The Niche): the sum over roster species of the variance, across the demography report's `stack_settlements`, of each species' composition fraction. 0.0 iff every settlement has the identical species mix (the pre-Niche 'oatmeal' — one flat blend worldwide); > 0 when composition varies across space (species dominant in different strongholds). Recomputed via `hornvale_worldgen::demography_report_from` (the niche-differentiated coexistence shadow). Absent if the report fails to build or the world has fewer than 2 settlements

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.30448407 | 0.41265045 | 0.4325848 | 0.45142757 | 0.49225642 | 0.43143243 |

### `flagship-biome`

The goblin flagship settlement's committed biome; Absent if there is no goblin flagship

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `temperate-forest` | 385 | 38.5% |
| `tropical-seasonal-forest` | 327 | 32.7% |
| `taiga` | 243 | 24.3% |
| `alpine` | 12 | 1.2% |
| `tropical-rainforest` | 11 | 1.1% |
| `savanna` | 7 | 0.7% |
| `kelp-forest` | 6 | 0.6% |
| `tundra` | 5 | 0.5% |
| `temperate-rainforest` | 3 | 0.3% |
| `shrubland` | 1 | 0.1% |

### `flagship-coastal`

Whether the goblin flagship settlement's cell borders an ocean cell, recomputed from the terrain provider; Absent if there is no goblin flagship

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 208 | 20.8% |
| `false` | 792 | 79.2% |

### `flagship-structure-size`

Number of castes present in the goblin flagship settlement's emergent structure (a stratification proxy, matched against the same community religion's pantheon-verticality reasons about); Absent if there is no goblin flagship

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 3 | 3 | 3 | 3 | 2.925 |

### `flagship-subsistence`

The goblin flagship settlement's committed subsistence mode (the pantheon's community, spec §6); Absent if there is no goblin flagship or no committed subsistence

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farming` | 976 | 97.6% |
| `foraging` | 17 | 1.7% |
| `fishing` | 6 | 0.6% |
| `herding` | 1 | 0.1% |

### `goblin-flagship-coastal`

Whether the goblin flagship settlement's cell borders an ocean cell, recomputed from the terrain provider; Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 208 | 20.8% |
| `false` | 792 | 79.2% |

### `goblin-flagship-roles`

The goblin flagship's committed role ladder, comma-joined, lowest to highest; Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farmer,shaman,chief` | 925 | 92.5% |
| `farmer,chief` | 51 | 5.1% |
| `forager,chief` | 17 | 1.7% |
| `fisher,chief` | 6 | 0.6% |
| `herder,chief` | 1 | 0.1% |

### `goblin-flagship-surplus`

The goblin flagship cell's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.044553549 | 0.47624221 | 0.51334468 | 0.53601493 | 0.69232571 | 0.49936407 |

### `goblin-settlement-count`

Number of settlements peopled by goblins

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 5 | 11 | 26 | 97 | 18.057 |

### `kobold-flagship-coastal`

Whether the kobold flagship settlement's cell borders an ocean cell, recomputed from the terrain provider; Absent if kobolds placed no settlement

n = 968 present, 32 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 15 | 1.5% |
| `false` | 953 | 98.5% |

### `kobold-flagship-roles`

The kobold flagship's committed role ladder, comma-joined, lowest to highest; Absent if kobolds placed no settlement

n = 968 present, 32 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `digger,keeper,elders` | 666 | 68.8% |
| `digger,elders` | 294 | 30.4% |
| `digger,warden,elders` | 4 | 0.4% |
| `digger,warden,keeper,elders` | 4 | 0.4% |

### `kobold-flagship-surplus`

The kobold flagship cell's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if kobolds placed no settlement

n = 968 present, 32 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.015684932 | 0.3766427 | 0.45813467 | 0.49754399 | 0.63 | 0.40033971 |

### `kobold-settlement-count`

Number of settlements peopled by kobolds

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 6 | 25 | 50 | 164 | 33.199 |

### `largest-holding-share`

M2: the largest live community's PEAK population as a share of the summed peak population of every community alive at bake end (peak_population is each occupation's historical high-water mark, not a true bake-end census — no end-state population accessor exists today; see task-4-report.md) — the entity-size reading the criticality campaigns never took

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0047540984 | 0.010068259 | 0.012838676 | 0.018139714 | 0.19277108 | 0.018041568 |

### `per-cell-diversity`

Mean per-cell species diversity of the coexistence density stack (task A16a; feeds the A16b β calibration): the mean, over habitable land cells, of the demography report's `byproducts.strife` field — already the per-cell inverse-Herfindahl diversity 1/Σ frac_s² (1.0 when one species dominates a cell, →N when N species share it evenly). Recomputed via `hornvale_worldgen::demography_report_from`, which reconstructs the IDENTICAL report the settlement-genesis path builds internally (the shared-assembly refactor of task A16a), so this measures the stack the world actually ships, not a parallel one. Absent if the report fails to build or the world has no habitable cells

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.3053109 | 1.5411724 | 2.0348938 | 2.2844276 | 2.7280422 | 1.9436718 |

### `pop-weighted-abs-latitude`

The population-weighted mean absolute latitude across every settlement: Σ(pop·|lat|) / Σ(pop), reading each settlement's committed POPULATION and LATITUDE facts. The area-weighted mean |latitude| on a uniform sphere is ≈32.7°; people concentrating off the poles (design spec §5) should read below that baseline. Absent if there are no settlements with both facts

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2.9350351 | 9.0366636 | 12.553247 | 18.452117 | 60.490007 | 15.033989 |

### `rank-size-slope`

The OLS slope of log(population) on log(rank) across every settlement in the world (the classic Zipf rank-size diagnostic). Recorded as an OBSERVED metric only — this campaign's interim per-species condensation is deliberately NOT tuned to a rank-size target. Absent if fewer than 2 settlements exist

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -1.2999613 | -0.63794079 | -0.58870719 | -0.53979814 | -0.30017383 | -0.59410211 |

### `settlement-count`

Number of settlements placed in the world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 16 | 145 | 205 | 265 | 521 | 206.035 |

## Weaknesses found here

### `flagship-structure-size`

- **D3**: p25..p75 spans 0 (0.00% of the 1 min..max range), under the 5% bar
- **D4**: median 3 equals the max (2 .. 3)

### `flagship-subsistence`

- **D1**: "farming" holds 976/1000 worlds (97.6%), at or above the 80% threshold

### `goblin-flagship-roles`

- **D1**: "farmer,shaman,chief" holds 925/1000 worlds (92.5%), at or above the 80% threshold

### `kobold-flagship-coastal`

- **D1**: "false" holds 953/968 worlds (98.5%), at or above the 80% threshold

### `largest-holding-share`

- **D3**: p25..p75 spans 0.008071455000000002 (4.29% of the 0.18801698160000002 min..max range), under the 5% bar

### `settlement-count`

- **D5 strength**: declared strong tracking habitable-fraction, but observed r = +0.230 (1000 pairs) is weak (positive)
- **D5 strength**: declared moderate tracking mountain-coverage, but observed |r| = 0.015 (1000 pairs) is none
- **D5 strength**: declared moderate tracking ocean-fraction, but observed |r| = 0.085 (1000 pairs) is none

