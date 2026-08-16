<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Terrain — The Domesday

The solid shape of a world: its plates, its elevation, and the landforms the sculpting pipeline leaves behind.

## Metrics

### `cave-fraction`

Fraction of land cells with a cave

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.034492274 | 0.081764541 | 0.096868579 | 0.115397 | 0.20860729 | 0.099646079 |

### `continent-count`

Connected land components at least 0.5% of the world's total land cells (Task 9 iteration 3's size floor, Earth-calibrated: Greenland is ~1.4% of Earth's land and qualifies, Iceland ~0.07% does not) — the unfloored fringe of sub-floor fragments is preserved separately by landmass-count

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 5 | 6 | 7 | 12 | 6.092 |

### `deposit-density`

Fraction of land cells with an ore deposit (The Lode, spec §5)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.24181566 | 0.352657 | 0.38918014 | 0.43282997 | 0.58044349 | 0.39270569 |

### `dominant-commodity`

The most common land ore commodity by cell count (The Lode, spec §5); Absent where no land cell has a deposit

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `salt` | 999 | 99.9% |
| `iron` | 1 | 0.1% |

### `dominant-rock`

The most common land rock class by cell count, spec §4's fine taxonomy (The Ground); Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `evaporite` | 476 | 47.6% |
| `gneiss` | 294 | 29.4% |
| `andesite` | 158 | 15.8% |
| `sandstone` | 49 | 4.9% |
| `conglomerate` | 13 | 1.3% |
| `granite` | 10 | 1.0% |

### `hypsometric-bimodality`

Ashman's D between land and ocean elevation populations (Earth is strongly bimodal); Absent when a world lacks land or ocean

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2.4084567 | 3.6802133 | 3.8196246 | 3.9550742 | 4.464955 | 3.8156322 |

### `landmass-count`

Every connected land component regardless of size — the unfloored companion continent-count superseded away from; reported alongside forever

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 21 | 159 | 202 | 244 | 388 | 199.747 |

### `largest-continent-share`

Largest land component's share of all land cells; Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.1587584 | 0.30979371 | 0.38887345 | 0.52104859 | 0.99200276 | 0.43124834 |

### `mean-depth-to-basement`

Mean depth to crystalline basement over land (m) — the sedimentary archive's thickness.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 17.66636 | 36.032146 | 43.133104 | 50.334668 | 83.804049 | 43.505972 |

### `mean-geothermal-gradient`

Mean geothermal gradient over land (K/km) — the deep's energy base.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 22.686095 | 24.500174 | 24.934778 | 25.311537 | 26.652152 | 24.918351 |

### `mean-land-elevation-m`

Mean elevation above sea level over land cells, m — the term the lapse rate turns into a temperature penalty. Land is `e >= sea`, matching `mountain-coverage`'s land definition; `mean-land-temperature-c` uses `!is_ocean(cell)`, which is the same condition (`is_ocean` is `e < sea`), so the two metrics ARE mutually comparable — this is the coupling the campaign's lapse-rate regression rests on. Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1063.6052 | 1525.0853 | 1700.2625 | 1951.7233 | 2942.0455 | 1747.9432 |

### `mean-ore-grade`

Mean ore grade [0,1] over land cells with a deposit (The Lode, spec §5); 0.0 where no land cell has a deposit

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.034039592 | 0.056138445 | 0.062634712 | 0.069759128 | 0.095971967 | 0.062830804 |

### `mountain-coverage`

Fraction of land cells standing above 2000 m over the sea

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.13071099 | 0.3202934 | 0.39027332 | 0.48373203 | 0.72677542 | 0.40076979 |

### `ocean-fraction`

Fraction of globe cells below sea level

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.48867243 | 0.55644256 | 0.61714516 | 0.68331624 | 0.75999707 | 0.61835909 |

### `plate-count`

Number of tectonic plates the globe drew or was pinned to

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `27` | 46 | 4.6% |
| `30` | 44 | 4.4% |
| `35` | 41 | 4.1% |
| `15` | 37 | 3.7% |
| `23` | 37 | 3.7% |
| `24` | 35 | 3.5% |
| `34` | 35 | 3.5% |
| `11` | 34 | 3.4% |
| `32` | 34 | 3.4% |
| `33` | 33 | 3.3% |
| `19` | 32 | 3.2% |
| `21` | 32 | 3.2% |
| `16` | 31 | 3.1% |
| `38` | 31 | 3.1% |
| `17` | 30 | 3.0% |
| `28` | 30 | 3.0% |
| `10` | 29 | 2.9% |
| `13` | 29 | 2.9% |
| `12` | 28 | 2.8% |
| `26` | 28 | 2.8% |
| `37` | 28 | 2.8% |
| `39` | 28 | 2.8% |
| `40` | 28 | 2.8% |
| `25` | 27 | 2.7% |
| `8` | 27 | 2.7% |
| `20` | 26 | 2.6% |
| `36` | 26 | 2.6% |
| `29` | 24 | 2.4% |
| `22` | 23 | 2.3% |
| `9` | 23 | 2.3% |
| `31` | 22 | 2.2% |
| `14` | 21 | 2.1% |
| `18` | 21 | 2.1% |

### `plate-size-gini`

Gini coefficient over plate cell counts (Earth's plate sizes are heavy-tailed; uniform Voronoi scores low)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.25701382 | 0.65178653 | 0.70607485 | 0.74873663 | 0.85340293 | 0.69035659 |

### `sediment-volume`

Total deposited sediment volume proxy: Σ sediment thickness (meters) over every cell, one cell-area unit per cell — the carve's own volume-proxy convention (spec §5): repose's receiver-side gains, routing's floodplain/ playa deposit, the marine wedge/delta fill, and atoll cap material, all summed

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 620405.23 | 1077733.1 | 1243488.9 | 1418774.6 | 2065159.6 | 1252118 |

### `unconformity-fraction`

Fraction of land cells recording a nonconformity (missing time) — the archive's floating gaps.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.00734167 | 0.23918433 | 0.33543373 | 0.43654459 | 0.85466887 | 0.34133031 |

### `unrest-coverage`

Fraction of cells with tectonic unrest above 0.3

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.00043943167 | 0.013768859 | 0.020335921 | 0.028367755 | 0.061544846 | 0.02148567 |

## Weaknesses found here

### `dominant-commodity`

- **D1**: "salt" holds 999/1000 worlds (99.9%), at or above the 80% threshold

