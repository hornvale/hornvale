<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Society — The Domesday

How settled peoples organize themselves — the social structures layered atop demography.

## Metrics

### `defensibility-capacity-rank-corr`

M4: Spearman rank correlation between a habitable vertex's weakest-point defensibility and its carrying capacity, BOTH READ FROM PRESENT-DAY terrain, climate, and connection graph — NOT the bake's own final era, which can differ on a world with real orbital forcing (spec §2.4 amendment 4). Checks §2.2's structural claim that defensible ground is also poor ground, on the geography as it stands today. Ties get average ranks; Absent if fewer than 2 habitable vertices, or if either series is constant (no variance, so no correlation is defined)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.43378899 | -0.17579011 | -0.11892863 | -0.057290909 | 0.44602481 | -0.11292187 |

### `granary-raid-phase-concentration`

Circular (Rayleigh-style) concentration R of this world's raid-caused occupation endings' day-of-year stamps (The Granary T8): the mean resultant length of the phases, 0 when uniformly spread around the year and approaching 1 as all raids land at one moment. Day-of-year is the fractional part of the record's bake-year `ended` stamp. Absent on a world with fewer than 5 raid-caused endings — below that floor the concentration statistic is noise, not signal.

n = 984 present, 16 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.56387674 | 0.88755946 | 0.92262457 | 0.94886494 | 1 | 0.91395791 |

### `granary-raids-in-depleted-half`

Fraction of this world's raid-caused occupation endings whose day-of-year stamp falls in the DEPLETED half of the victim site's authored harvest curve (The Granary T8): the half-year starting half a year past the curve peak, where `Curve::at` returns exactly zero and a settlement lives off stores. The curve is keyed exactly as the bake keys it at open — `geo.coord(site).latitude` and `biome_class(climate.biome_map())` through `worldgen::harvest::Curve` — so the column measures the same seasonality the granary integrates. Uniform raids give 0.5; hunger-side clustering gives > 0.5. Absent under the same 5-ending floor as `granary-raid-phase-concentration`.

n = 984 present, 16 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.31464174 | 0.50937441 | 0.70886076 | 1 | 0.50631743 |

### `raid-attribution-unresolved`

How many `Ended::By(raider)` references on this world fail to name EXACTLY ONE occupation record — the self-consistency guard the retired 30-world battery held, as a census-wide invariant (The Confusion). Expected 0 on every world; a nonzero count means the victim- and initiator-side rates beside it are attributing raids to the wrong settlements. Per-REFERENCE, so compensating errors cannot cancel the way they can in a pooled count comparison. Absent on a world with no occupation records.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0 | 0 | 0 | 0 | 0 |

### `raid-initiator-rate`

Share of this world's occupation records that ended at least one OTHER record — the OFFENCE side, the side the raid gate actually decides (The Confusion). Its own column rather than a derivative of `raid-victim-rate`, because the ratio of the two is the mean raids per raider: the two rates separate 'many settlements each raiding once' from 'a few serial raiders', which one rate alone cannot. Same unfiltered population as `raid-victim-rate`; Absent on a world with no occupation records. Measured on a 12-world scratch probe (seeds 1-12, NOT the census) the ratio of the two rates sits at 1.00-1.03, i.e. a raider almost never raids twice — that near-identity is the column's current reading, not a reason to drop it: nothing else would detect a shift toward serial raiders.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.19361702 | 0.27573958 | 0.32520944 | 0.4235589 | 0.24792544 |

### `raid-victim-rate`

Share of this world's occupation records that ended at another community's hand (`Ended::By`) — the DEFENCE side of the raid mechanic (The Confusion). Over EVERY people with a record, not the six The Tolerance froze: this is the raid rate, not that campaign's readout. Absent on a world with no occupation records. Replaces `windows/worldgen/tests/tolerance_baseline.rs`'s 30-world victim-side proxy.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.19598965 | 0.28201781 | 0.33716751 | 0.45229425 | 0.25547819 |

### `tribute-relations-standing`

How many standing tribute relations (`pays-tribute-to`) this world holds at `now` — the subordination stock (The Assize). Replaces `windows/worldgen/tests/history_tithe.rs`'s twelve-world tribute-volume panel, whose quantity lives on the bake's discarded tally and is unreachable from any census metric. Agrees with that flow at spearman 0.934 over 36 worlds — a measured witness, NOT an equivalence: this is a stock and that was a flow. Absent on a world with no occupation records.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 59 | 91 | 122 | 270 | 91.68 |

## Weaknesses found here

### `raid-attribution-unresolved`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

### `tribute-relations-standing`

- **D5 strength**: declared moderate tracking settlement-count, but observed r = +0.942 (1000 pairs) is dominant (positive)

