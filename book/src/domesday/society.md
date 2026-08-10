<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Society — The Domesday

How settled peoples organize themselves — the social structures layered atop demography.

## Metrics

### `defensibility-capacity-rank-corr`

M4: Spearman rank correlation between a habitable cell's weakest-point defensibility and its carrying capacity, BOTH READ FROM PRESENT-DAY terrain, climate, and connection graph — NOT the bake's own final era, which can differ on a world with real orbital forcing (spec §2.4 amendment 4). Checks §2.2's structural claim that defensible ground is also poor ground, on the geography as it stands today. Ties get average ranks; Absent if fewer than 2 habitable cells, or if either series is constant (no variance, so no correlation is defined)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.51502923 | -0.19730814 | -0.079080239 | -0.0084019608 | 0.42868056 | -0.098888751 |

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
| 0 | 0.19607843 | 0.27146013 | 0.31436314 | 0.41422122 | 0.24435622 |

### `raid-victim-rate`

Share of this world's occupation records that ended at another community's hand (`Ended::By`) — the DEFENCE side of the raid mechanic (The Confusion). Over EVERY people with a record, not the six The Tolerance froze: this is the raid rate, not that campaign's readout. Absent on a world with no occupation records. Replaces `windows/worldgen/tests/tolerance_baseline.rs`'s 30-world victim-side proxy.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.19910714 | 0.27586207 | 0.32631579 | 0.43459552 | 0.25130589 |

### `tribute-relations-standing`

How many standing tribute relations (`pays-tribute-to`) this world holds at `now` — the subordination stock (The Assize). Replaces `windows/worldgen/tests/history_tithe.rs`'s twelve-world tribute-volume panel, whose quantity lives on the bake's discarded tally and is unreachable from any census metric. Agrees with that flow at spearman 0.934 over 36 worlds — a measured witness, NOT an equivalence: this is a stock and that was a flow. Absent on a world with no occupation records.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 37 | 62 | 88 | 216 | 64.474 |

## Weaknesses found here

### `raid-attribution-unresolved`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

