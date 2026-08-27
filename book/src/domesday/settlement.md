<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Settlement — The Domesday

Where and how peoples settle: placement, condensation, and the built shape of a community.

## Metrics

### `capacity-by-abs-latitude`

The carrying-capacity field's headline calibration (design spec §5): the ratio of mean per-land-vertex K (summed over the roster's PEOPLED kinds' individual fields, each species' own psychology folded in — fauna kinds have no psychology and are excluded, preserving this metric's pre-menagerie population) in the low-latitude band (|latitude| < 30) to the polar band (|latitude| > 60), the polar mean floored at POLE_FLOOR (1% of the K formula's baseline unit) so an exactly-zero polar band — the Miami NPP proxy's honest reading of hard cold, not a bug — reports a large-but-bounded ratio rather than a division blowup. A field grounded in the real biomass gradient reads well above 1 here; Absent if either band has no land vertices (a wholly ocean or wholly polar world)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.18976045 | 9.8061221 | 18.180146 | 25.453434 | 45.238175 | 18.081944 |

### `cold-built-room-share`

The share of this world's built settlement rooms that read `is_cold` (below `FURNISHING_COLD_C` at the frozen furnishing-reference day) — the fraction of the settled world where `interior_of` would compose a hearth (The Range). Replaces the cold-DOMINATION clause of `windows/lab/tests/hearth_population_calibration.rs`, which asked over 15 seeds whether ANY world exceeded 0.5 here; decision 0097 converts an existence claim sitting on a threshold into a census rate, because at n=15 the answer is decided by one world and at n=1000 it is a fraction with a sampling bound. A world's whole settled area can be temperate (0.0 is a real reading, not a broken fold); Absent only when the world has no built rooms at all.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.063106796 | 0.18979837 | 0.375 | 1 | 0.25409144 |

### `composition-variance`

Spatial heterogeneity of settlement composition (The Niche): the sum over roster species of the variance, across the demography report's `stack_settlements`, of each species' composition fraction. 0.0 iff every settlement has the identical species mix (the pre-Niche 'oatmeal' — one flat blend worldwide); > 0 when composition varies across space (species dominant in different strongholds). Recomputed via `hornvale_worldgen::demography_report_from` (the niche-differentiated coexistence shadow). Absent if the report fails to build or the world has fewer than 2 settlements

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.35157579 | 0.41483435 | 0.42870189 | 0.43917485 | 0.47774139 | 0.42667923 |

### `flagship-biome`

The goblin flagship settlement's committed biome; Absent if there is no goblin flagship

n = 999 present, 1 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `temperate-forest` | 535 | 53.6% |
| `taiga` | 214 | 21.4% |
| `tropical-seasonal-forest` | 212 | 21.2% |
| `tropical-rainforest` | 10 | 1.0% |
| `alpine` | 8 | 0.8% |
| `temperate-rainforest` | 7 | 0.7% |
| `savanna` | 4 | 0.4% |
| `epipelagic` | 3 | 0.3% |
| `kelp-forest` | 3 | 0.3% |
| `coral-reef` | 1 | 0.1% |
| `shrubland` | 1 | 0.1% |
| `upwelling` | 1 | 0.1% |

### `flagship-coastal`

Whether the goblin flagship settlement's vertex borders an ocean vertex, recomputed from the terrain provider; Absent if there is no goblin flagship

n = 999 present, 1 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 189 | 18.9% |
| `false` | 810 | 81.1% |

### `flagship-structure-size`

Number of castes present in the goblin flagship settlement's emergent structure (a stratification proxy, matched against the same community religion's pantheon-verticality reasons about); Absent if there is no goblin flagship

n = 999 present, 1 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 3 | 3 | 3 | 3 | 2.958959 |

### `flagship-subsistence`

The goblin flagship settlement's committed subsistence mode (the pantheon's community, spec §6); Absent if there is no goblin flagship or no committed subsistence

n = 999 present, 1 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farming` | 982 | 98.3% |
| `fishing` | 9 | 0.9% |
| `foraging` | 7 | 0.7% |
| `herding` | 1 | 0.1% |

### `goblin-flagship-coastal`

Whether the goblin flagship settlement's vertex borders an ocean vertex, recomputed from the terrain provider; Absent if goblins placed no settlement

n = 999 present, 1 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 189 | 18.9% |
| `false` | 810 | 81.1% |

### `goblin-flagship-roles`

The goblin flagship's committed role ladder, comma-joined, lowest to highest; Absent if goblins placed no settlement

n = 999 present, 1 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `farmer,shaman,chief` | 958 | 95.9% |
| `farmer,chief` | 24 | 2.4% |
| `fisher,chief` | 9 | 0.9% |
| `forager,chief` | 7 | 0.7% |
| `herder,chief` | 1 | 0.1% |

### `goblin-flagship-surplus`

The goblin flagship vertex's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if goblins placed no settlement

n = 999 present, 1 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.046627014 | 0.4826611 | 0.51231849 | 0.53543472 | 0.72 | 0.50501959 |

### `goblin-settlement-count`

Number of settlements peopled by goblins

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 5 | 12 | 27 | 117 | 18.008 |

### `kobold-flagship-coastal`

Whether the kobold flagship settlement's vertex borders an ocean vertex, recomputed from the terrain provider; Absent if kobolds placed no settlement

n = 980 present, 20 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `true` | 4 | 0.4% |
| `false` | 976 | 99.6% |

### `kobold-flagship-roles`

The kobold flagship's committed role ladder, comma-joined, lowest to highest; Absent if kobolds placed no settlement

n = 980 present, 20 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `digger,keeper,elders` | 669 | 68.3% |
| `digger,elders` | 307 | 31.3% |
| `digger,warden,keeper,elders` | 3 | 0.3% |
| `digger,warden,elders` | 1 | 0.1% |

### `kobold-flagship-surplus`

The kobold flagship vertex's subsistence surplus, recomputed from providers as fertility(biome_class) × moisture (the independent column the slave calibration needs); Absent if kobolds placed no settlement

n = 980 present, 20 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0074137685 | 0.38035093 | 0.43847835 | 0.48623645 | 0.60800137 | 0.4010092 |

### `kobold-settlement-count`

Number of settlements peopled by kobolds

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 7 | 30 | 60 | 224 | 40.226 |

### `largest-holding-share`

M2: the largest live community's PEAK population as a share of the summed peak population of every community alive at bake end (peak_population is each occupation's historical high-water mark, not a true bake-end census — no end-state population accessor exists today; see task-4-report.md) — the entity-size reading the criticality campaigns never took

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.004633486 | 0.0084139265 | 0.010311671 | 0.013727055 | 0.067085954 | 0.012937373 |

### `per-cell-diversity`

Mean per-vertex species diversity of the coexistence density stack (task A16a; feeds the A16b β calibration): the mean, over habitable land vertices, of the demography report's `byproducts.strife` field — already the per-vertex inverse-Herfindahl diversity 1/Σ frac_s² (1.0 when one species dominates a vertex, →N when N species share it evenly). Recomputed via `hornvale_worldgen::demography_report_from`, which reconstructs the IDENTICAL report the settlement-genesis path builds internally (the shared-assembly refactor of task A16a), so this measures the stack the world actually ships, not a parallel one. Absent if the report fails to build or the world has no habitable vertices

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1.36282 | 1.6545201 | 1.8507802 | 2.0216296 | 2.6162405 | 1.8437287 |

### `pop-weighted-abs-latitude`

The population-weighted mean absolute latitude across every settlement: Σ(pop·|lat|) / Σ(pop), reading each settlement's committed POPULATION and LATITUDE facts. The area-weighted mean |latitude| on a uniform sphere is ≈32.7°; people concentrating off the poles (design spec §5) should read below that baseline. Absent if there are no settlements with both facts

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 4.789578 | 12.329511 | 15.667516 | 19.714466 | 54.837652 | 17.252575 |

### `rank-size-slope`

The OLS slope of log(population) on log(rank) across every settlement in the world (the classic Zipf rank-size diagnostic). Recorded as an OBSERVED metric only — this campaign's interim per-species condensation is deliberately NOT tuned to a rank-size target. Absent if fewer than 2 settlements exist

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -1.0487627 | -0.62087732 | -0.57299544 | -0.52360711 | -0.30459 | -0.57619165 |

### `settlement-count`

Number of settlements placed in the world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 33 | 205 | 275 | 337 | 602 | 269.53 |

## Weaknesses found here

### `flagship-coastal`

- **D1**: "false" holds 810/999 worlds (81.1%), at or above the 80% threshold

### `flagship-structure-size`

- **D3**: p25..p75 spans 0 (0.00% of the 1 min..max range), under the 5% bar
- **D4**: median 3 equals the max (2 .. 3)

### `flagship-subsistence`

- **D1**: "farming" holds 982/999 worlds (98.3%), at or above the 80% threshold

### `goblin-flagship-coastal`

- **D1**: "false" holds 810/999 worlds (81.1%), at or above the 80% threshold

### `goblin-flagship-roles`

- **D1**: "farmer,shaman,chief" holds 958/999 worlds (95.9%), at or above the 80% threshold

### `kobold-flagship-coastal`

- **D1**: "false" holds 976/980 worlds (99.6%), at or above the 80% threshold

### `pop-weighted-abs-latitude`

- **D5 strength**: declared moderate tracking mean-land-temperature-c, but observed r = +0.148 (1000 pairs) is weak (positive)

### `settlement-count`

- **D5 strength**: declared strong tracking habitable-fraction, but observed r = +0.211 (1000 pairs) is weak (positive)
- **D5 strength**: declared moderate tracking mountain-coverage, but observed |r| = 0.073 (1000 pairs) is none
- **D5 strength**: declared moderate tracking ocean-fraction, but observed |r| = 0.003 (1000 pairs) is none

