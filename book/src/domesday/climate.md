<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Climate — The Domesday

The temperature and moisture a world's astronomy and terrain resolve into, world over world.

## Metrics

### `band-count`

Circulation bands per hemisphere; 'locked' if tidally locked

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `3` | 791 | 79.1% |
| `5` | 161 | 16.1% |
| `locked` | 48 | 4.8% |

### `dominant-land-biome`

The most common land biome by cell count, kebab-case

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `ice` | 651 | 65.1% |
| `alpine` | 295 | 29.5% |
| `taiga` | 20 | 2.0% |
| `tropical-seasonal-forest` | 14 | 1.4% |
| `temperate-forest` | 9 | 0.9% |
| `tundra` | 8 | 0.8% |
| `savanna` | 3 | 0.3% |

### `dominant-soil-order`

The most common land soil order by cell count, spec §4's soil taxonomy (The Ground); Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `leptosol` | 1000 | 100.0% |

### `fertile-land-fraction`

Fraction of land cells whose soil fertility's grain-suitability exceeds 0.6 (The Ground, spec §3/§4)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.020240892 | 0.034668967 | 0.051964734 | 0.15063325 | 0.037814834 |

### `habitable-fraction`

Fraction of cells that are habitable (land, water, tolerable season)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.02023827 | 0.078853572 | 0.13860407 | 0.18204678 | 0.29014697 | 0.13296858 |

### `mean-land-temperature-c`

Annual-mean temperature averaged over land cells, °C; Absent if the world has no land

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -47.151131 | -22.551606 | -11.988568 | 2.0373088 | 23.141691 | -9.9958769 |

## Weaknesses found here

### `dominant-soil-order`

- **D1**: "leptosol" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

### `fertile-land-fraction`

- **D5 strength**: declared moderate tracking mean-land-temperature-c, but observed r = +0.730 (1000 pairs) is dominant (positive)

### `habitable-fraction`

- **D5 strength**: declared moderate tracking obliquity-degrees, but observed |r| = 0.058 (1000 pairs) is none
- **D5 strength**: declared moderate tracking mountain-coverage, but observed r = +0.151 (1000 pairs) is weak -- and the sign is backwards: the coupling is positive, not the declared negative

### `mean-land-temperature-c`

- **D5 strength**: declared dominant tracking year-std-days, but observed r = -0.245 (1000 pairs) is weak (negative)
- **D5 strength**: declared weak tracking brightening-per-gyr, but observed |r| = 0.049 (1000 pairs) is none
- **D5 strength**: declared moderate tracking ocean-fraction, but observed |r| = 0.000 (1000 pairs) is none
- **D5 strength**: declared moderate tracking mountain-coverage, but observed |r| = 0.041 (1000 pairs) is none
- **D6**: median -11.988568 vs Earth's 14 differs by 25.988568, exceeding the band 10

