<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# History — The Domesday

How a world's occupation record accumulates: strata, tenure, and how much of it survives to be read back.

## Metrics

### `dominant-hazard`

The most common hazard kind among land-cell vestiges by layer count (The Vestige, spec §9.2); Absent where no land cell bears a vestige

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `structural` | 1000 | 100.0% |

### `forgotten-fraction`

Over land cells with a non-empty vestige stack, the fraction whose most-dread layer is Forgotten rather than Venerated (The Vestige, spec §9.2); 0.0 where no land cell bears a vestige

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.2721519 | 0.52941176 | 0.59908753 | 0.6875 | 0.97297297 | 0.61928771 |

### `mean-warning-legibility`

Mean warning_legibility over every land-cell vestige layer (The Vestige, spec §9.2); 0.0 where no land cell bears a vestige

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.028691853 | 0.4199983 | 0.50277545 | 0.5662571 | 0.76155857 | 0.47804448 |

### `vestige-density`

Fraction of land cells with a non-empty vestige stack (The Vestige, spec §9.2)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0018192546 | 0.010502842 | 0.014231892 | 0.018775413 | 0.041485725 | 0.015125034 |

## Weaknesses found here

### `dominant-hazard`

- **D1**: "structural" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

