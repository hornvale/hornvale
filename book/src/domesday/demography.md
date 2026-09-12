<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Demography — The Domesday

How many, and of what structure: the population counts a world's peoples carry.

## Metrics

### `climate-displacement-events`

How many occupations on this world ended in climate-driven migration (`occ-cause` = `migrated`), excluding conquest-relocations — the displacement mechanic's volume (The Assize). Replaces the seed-42 gate in `cli/tests/history_battery.rs` and the twelve-seed panel in `windows/worldgen/tests/history_sundering.rs`. Bimodal: most worlds sit in single digits and a minority run to the hundreds, and a real minority measure zero — a mild deep past, not an inert bake. Absent on a world with no occupation records.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 2 | 18 | 181 | 2529 | 183.111 |

### `epidemic-largest-metapopulation-now`

Largest connected host population in the era containing the present, reconstructed from occ-founded, occ-ended, occ-peak and occ-person-years over the bake's era graph; Absent if the world has no occupations or predates occ-person-years

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 413.95612 | 3250.2551 | 5422.6682 | 7820.072 | 17034.348 | 5800.8312 |

### `goblin-flagship-population`

The goblin flagship settlement's committed population; Absent if goblins placed no settlement

n = 999 present, 1 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 34 | 40 | 41 | 43 | 34.942943 |

### `kobold-flagship-population`

The kobold flagship settlement's committed population; Absent if kobolds placed no settlement

n = 982 present, 18 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 29 | 45 | 56 | 70 | 41.143585 |

### `mean-population`

Mean population across every settlement's committed population fact; Absent if there are none

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 9.3864542 | 23.1875 | 27.972747 | 30.352227 | 38.06962 | 26.430991 |

### `peoples-alive-at-bake-end`

M3: how many distinct peoples still hold a live community when the bake ends — the decision-0089 compliance reading

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 11 | 19 | 19 | 19 | 19 | 18.907 |

### `peoples-placed`

How many peoples hold a flagship settlement in this world — the n in the 1/n chance baseline The Namesake §5.1(2) is judged against, published so that verdict is re-derivable from rows.csv without inferring n; Absent if no people is placed

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 11 | 19 | 19 | 19 | 19 | 18.907 |

### `total-population`

Sum of every settlement's committed population fact; Absent if there are none

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 993 | 5927 | 8480.5 | 10771 | 18078 | 8292.477 |

## Weaknesses found here

### `climate-displacement-events`

- **D5 strength**: declared moderate tracking habitable-fraction, but observed r = -0.198 (1000 pairs) is weak (negative)

### `peoples-alive-at-bake-end`

- **D3**: p25..p75 spans 0 (0.00% of the 8 min..max range), under the 5% bar
- **D4**: median 19 equals the max (11 .. 19)

### `peoples-placed`

- **D3**: p25..p75 spans 0 (0.00% of the 8 min..max range), under the 5% bar
- **D4**: median 19 equals the max (11 .. 19)

### `total-population`

- **D5 strength**: declared strong tracking fertile-land-fraction, but observed r = +0.259 (1000 pairs) is weak (positive)
- **D5 strength**: declared strong tracking habitable-fraction, but observed r = +0.279 (1000 pairs) is weak (positive)

