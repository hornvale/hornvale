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
| 0.21698113 | 0.50511945 | 0.56912695 | 0.65795724 | 0.97368421 | 0.59652934 |

### `mean-warning-legibility`

Mean warning_legibility over every land-cell vestige layer (The Vestige, spec §9.2); 0.0 where no land cell bears a vestige

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.032389129 | 0.41871327 | 0.51611038 | 0.57387919 | 0.7916476 | 0.48405628 |

### `vestige-density`

Fraction of land cells with a non-empty vestige stack (The Vestige, spec §9.2)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0041301996 | 0.014168573 | 0.018510906 | 0.023604176 | 0.049348934 | 0.019340543 |

## Weaknesses found here

### `dominant-hazard`

- **D1**: "structural" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

