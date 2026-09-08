<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Society — The Domesday

How settled peoples organize themselves — the social structures layered atop demography.

## Metrics

### `defensibility-capacity-rank-corr`

M4: Spearman rank correlation between a habitable vertex's weakest-point defensibility and its carrying capacity, BOTH READ FROM PRESENT-DAY terrain, climate, and connection graph — NOT the bake's own final era, which can differ on a world with real orbital forcing (spec §2.4 amendment 4). Checks §2.2's structural claim that defensible ground is also poor ground, on the geography as it stands today. Ties get average ranks; Absent if fewer than 2 habitable vertices, or if either series is constant (no variance, so no correlation is defined)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| -0.43406804 | -0.17606525 | -0.11946303 | -0.057509849 | 0.44266918 | -0.11278122 |

### `granary-raid-phase-concentration`

Circular (Rayleigh-style) concentration R of this world's raid-caused occupation endings' day-of-year stamps (The Granary T8): the mean resultant length of the phases, 0 when uniformly spread around the year and approaching 1 as all raids land at one moment. Day-of-year is the fractional part of the record's bake-year `ended` stamp. Absent on a world with fewer than 5 raid-caused endings — below that floor the concentration statistic is noise, not signal.

n = 981 present, 19 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.53740482 | 0.89042277 | 0.92440987 | 0.95122584 | 1 | 0.91557821 |

### `granary-raids-in-depleted-half`

Fraction of this world's raid-caused occupation endings whose day-of-year stamp falls in the DEPLETED half of the victim site's authored harvest curve (The Granary T8): the half-year starting half a year past the curve peak, where `Curve::at` returns exactly zero and a settlement lives off stores. The curve is keyed exactly as the bake keys it at open — `geo.coord(site).latitude` and `biome_class(climate.biome_map())` through `worldgen::harvest::Curve` — so the column measures the same seasonality the granary integrates. Uniform raids give 0.5; hunger-side clustering gives > 0.5. Absent under the same 5-ending floor as `granary-raid-phase-concentration`.

n = 981 present, 19 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.30136986 | 0.51515152 | 0.72727273 | 1 | 0.50588072 |

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
| 0 | 0.19325153 | 0.27430863 | 0.32581736 | 0.4228546 | 0.24639027 |

**Frozen claim** — *The prospect of retaliation deters attack, so most agents in a mixed population never initiate combat* (`sugarscape-1996` `sug-retaliation-deters`; Ch. III, 'Effect of Rule Changes on Emergent Structures' ('agents are deterred from racing forward to attack smaller agents ... by the presence of larger opposing agents within their vision')). Predicted median at most 0.5; measured 0.274309. GROWN. ([what this is](#frozen-claims))

### `raid-victim-rate`

Share of this world's occupation records that ended at another community's hand (`Ended::By`) — the DEFENCE side of the raid mechanic (The Confusion). Over EVERY people with a record, not the six The Tolerance froze: this is the raid rate, not that campaign's readout. Absent on a world with no occupation records. Replaces `windows/worldgen/tests/tolerance_baseline.rs`'s 30-world victim-side proxy.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0.19631902 | 0.27931485 | 0.33676232 | 0.45331616 | 0.2539182 |

**Frozen claim** — *Combat between groups claims a real but minority share of the population; predation does not consume the society that practises it* (`sugarscape-1996` `sug-predation-is-bounded`; Appendix B, agent combat rule C(alpha); Ch. III, 'Combat'). Predicted at least 0.5 of worlds in [0.02, 0.5]; measured 0.968000. GROWN. ([what this is](#frozen-claims))

### `tribute-relations-standing`

How many standing tribute relations (`pays-tribute-to`) this world holds at `now` — the subordination stock (The Assize). Replaces `windows/worldgen/tests/history_tithe.rs`'s twelve-world tribute-volume panel, whose quantity lives on the bake's discarded tally and is unreachable from any census metric. Agrees with that flow at spearman 0.934 over 36 worlds — a measured witness, NOT an equivalence: this is a stock and that was a flow. Absent on a world with no occupation records.

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 56 | 88 | 120 | 272 | 88.268 |

**Frozen claim** — *Persistent asymmetric obligation is what makes a flat society hierarchical; without it no agent is subordinate to another* (`sugarscape-1996` `sug-credit-makes-hierarchy`; Appendix B, agent credit rule L(d,r); Ch. IV, 'Credit Networks and the Emergence of Hierarchy'). Predicted median at least 1; measured 88.000000. GROWN. ([what this is](#frozen-claims))

## Weaknesses found here

### `raid-attribution-unresolved`

- **D2**: min == median == max == 0 across 1000 worlds
- **D4**: median 0 equals the min (0 .. 0)

### `tribute-relations-standing`

- **D5 strength**: declared moderate tracking settlement-count, but observed r = +0.943 (1000 pairs) is dominant (positive)

## Frozen claims

Some metrics above carry a **frozen claim**: a prediction an imported corpus made about this population *before* any of it was measured, printed beside what the committed census says today. The corpus is data this survey only reads — the corpus supplies the regularity, its source and the criterion, and the survey supplies the measurement and re-states the recorded verdict. Every part of a claim line is derived from one of those two, so a corpus that changes moves the line.

### `sugarscape-1996`

Frozen corpus: `regularities/sugarscape-1996.regularity.json`

Joshua M. Epstein and Robert Axtell, *Growing Artificial Societies: Social Science From the Bottom Up* (Brookings Institution Press / MIT Press, 1996). Items are drawn from Appendix B's complete rule roster (growback, movement, replacement, seasonal growback, pollution formation and diffusion, mating, inheritance, cultural transmission, group membership, combat, trade, credit, immune response, disease transmission) and from the emergence claims the chapters attach to those rules. The taxonomy in `emergence_type` is the book's own, from Chapter II footnote 24: type 1 is a property meaningful for an individual but exhibited only by the collective (the diagonal migration wave — "the group adopts a heading unavailable to any individual"); type 2 is a property meaningful only for a collective (a wealth distribution). AN INSTRUMENT WITH KNOWN BIAS, NEVER A STANDARD (decision 0095). Sugarscape is one 1996 lattice model of agents harvesting a renewable resource, and roughly half of its roster is economic — trade, prices, credit, inheritance of holdings — because its authors were economists building toward generative social science. Hornvale has no economy, no per-individual wealth, and no disease model, so a large block of this corpus can only ever score `absent`; that is a property of the source's coverage, not a defect Hornvale is being charged with. Conversely Sugarscape has no terrain, no astronomy, no language and no deep time, so nothing here scores Hornvale's strongest ground. Coverage measures reach against this catalogue only. THE MAPPING FROM A SUGARSCAPE CLAIM TO A CENSUS COLUMN IS AN ANALOGY, AND EACH ITEM'S `note` STATES WHERE THE ANALOGY IS LOAD-BEARING. A Sugarscape agent is an individual; a Hornvale settlement is a community. Where an item reads a per-community statistic against a per-agent claim, the item says so, and a `flat` verdict on such an item may be about the analogy rather than about the world.

