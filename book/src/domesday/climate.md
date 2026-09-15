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

The most common land biome by vertex count, kebab-case

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `taiga` | 245 | 24.5% |
| `ice` | 191 | 19.1% |
| `alpine` | 177 | 17.7% |
| `temperate-forest` | 162 | 16.2% |
| `tundra` | 139 | 13.9% |
| `tropical-seasonal-forest` | 64 | 6.4% |
| `temperate-grassland` | 20 | 2.0% |
| `savanna` | 2 | 0.2% |

### `dominant-soil-order`

The most common land soil order by vertex count, spec §4's soil taxonomy (The Ground); Absent on a landless world

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `leptosol` | 1000 | 100.0% |

### `fertile-land-fraction`

Fraction of land vertices whose soil fertility's grain-suitability exceeds 0.6 (The Ground, spec §3/§4)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.040109062 | 0.069163062 | 0.10575988 | 0.2169967 | 0.074346743 |

### `habitable-fraction`

Fraction of vertices that are habitable (land, water, tolerable season)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.029734876 | 0.13431961 | 0.17721303 | 0.21808017 | 0.30320785 | 0.17394041 |

### `mean-land-temperature-c`

Annual-mean temperature averaged over land vertices, °C; Absent if the world has no land

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -35.335421 | -10.707389 | -3.4531262 | 4.7295432 | 22.527559 | -3.2199515 |

## Weaknesses found here

### `dominant-soil-order`

- **D1**: "leptosol" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

### `fertile-land-fraction`

- **D5 strength**: declared moderate tracking mean-land-temperature-c, but observed r = +0.725 (1000 pairs) is dominant (positive)

### `habitable-fraction`

- **D5 strength**: declared moderate tracking obliquity-degrees, but observed |r| = 0.023 (1000 pairs) is none
- **D5 strength**: declared moderate tracking mountain-coverage, but observed r = +0.111 (1000 pairs) is weak — and the sign is backwards: the coupling is positive, not the declared negative

### `mean-land-temperature-c`

- **D5 strength**: declared dominant tracking year-std-days, but observed |r| = 0.086 (1000 pairs) is none
- **D5 strength**: declared weak tracking brightening-per-gyr, but observed |r| = 0.024 (1000 pairs) is none
- **D5 strength**: declared moderate tracking ocean-fraction, but observed r = +0.102 (1000 pairs) is weak (positive)
- **D5 strength**: declared moderate tracking mountain-coverage, but observed r = -0.127 (1000 pairs) is weak (negative)
- **D6**: median -3.4531262 vs Earth's 14 differs by 17.453126, exceeding the band 10

