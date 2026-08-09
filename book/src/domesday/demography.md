<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Demography — The Domesday

How many, and of what structure: the population counts a world's peoples carry.

## Metrics

### `climate-displacement-events`

How many occupations on this world ended in climate-driven migration (`occ-cause` = `migrated`), excluding conquest-relocations — the displacement mechanic's volume (The Assize). Replaces the seed-42 gate in `cli/tests/history_battery.rs` and the twelve-seed panel in `windows/worldgen/tests/history_sundering.rs`. Bimodal: most worlds sit in single digits and a minority run to the hundreds, and a real minority measure zero — a mild deep past, not an inert bake. Absent on a world with no occupation records.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 2 | 10 | 92 | 1924 | 113.526 |

### `goblin-flagship-population`

The goblin flagship settlement's committed population; Absent if goblins placed no settlement

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 34 | 40 | 41 | 43 | 35.295 |

### `kobold-flagship-population`

The kobold flagship settlement's committed population; Absent if kobolds placed no settlement

n = 968 present, 32 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 28 | 46 | 54 | 70 | 40.235537 |

### `mean-population`

Mean population across every settlement's committed population fact; Absent if there are none

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 5.3214286 | 24.850174 | 30.405836 | 33.448276 | 47.392857 | 28.419819 |

### `peoples-alive-at-bake-end`

M3: how many distinct peoples still hold a live community when the bake ends — the decision-0089 compliance reading

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 5 | 9 | 9 | 9 | 9 | 8.956 |

### `peoples-placed`

How many peoples hold a flagship settlement in this world — the n in the 1/n chance baseline The Namesake §5.1(2) is judged against, published so that verdict is re-derivable from rows.csv without inferring n; Absent if no people is placed

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 5 | 9 | 9 | 9 | 9 | 8.956 |

### `total-population`

Sum of every settlement's committed population fact; Absent if there are none

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 166 | 3890 | 6218 | 8221 | 18300 | 6128.73 |

## Weaknesses found here

### `climate-displacement-events`

- **D3**: p25..p75 spans 90 (4.68% of the 1924 min..max range), under the 5% bar

### `peoples-alive-at-bake-end`

- **D3**: p25..p75 spans 0 (0.00% of the 4 min..max range), under the 5% bar
- **D4**: median 9 equals the max (5 .. 9)

### `peoples-placed`

- **D3**: p25..p75 spans 0 (0.00% of the 4 min..max range), under the 5% bar
- **D4**: median 9 equals the max (5 .. 9)

### `total-population`

- **D5 strength**: declared strong tracking fertile-land-fraction, but observed |r| = 0.226 (1000 pairs) is weak
- **D5 strength**: declared strong tracking habitable-fraction, but observed |r| = 0.307 (1000 pairs) is moderate

