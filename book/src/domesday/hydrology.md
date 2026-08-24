<!-- GENERATED FILE — do not edit. Regenerate with `hornvale lab domesday`. -->

# Hydrology — The Domesday

Where water moves and gathers: rivers, lakes, aquifers, and the coasts between land and sea.

## Metrics

### `aquifer-fraction`

Fraction of land cells whose hydrogeology classifies as an aquifer (The Ground, spec §3)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.006483896 | 0.14363568 | 0.20083387 | 0.25994415 | 0.56800975 | 0.20553696 |

### `channel-band-monotonicity`

H4's axis (The Ford, spec §10), the position-continuous-noise guard stated as a measurement: the fraction of transects walked outward from a channel centreline whose transverse-band sequence is monotone with no re-entry. Preregistered floor 0.99; falsification means address-hashed noise leaked into a band edge. Scored over the prefix for which the originating polyline is still the nearest one — past that the ray has entered a DIFFERENT river's valley and is no longer a transect of this channel. Absent on a world with no channels

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 1 | 1 | 1 | 1 | 1 |

### `channel-band-monotonicity-untruncated`

`channel-band-monotonicity` over the SAME transects with the nearest-line truncation switched off — the whole sweep scored, including the stretch where a different river has become the nearest and its band legitimately falls back to `channel`. Published because a rule that changes a verdict must have the reading it changes on the record beside it rather than in a campaign report — and the record now runs both ways. **Both of the readings below are ON THE FORD'S NETWORK** (183 polylines, 883 vertices at seed 42), and the second is that network's reading rather than a standing property of the rule: before the confluence repair the truncation was load-bearing (47 of 64 probe worlds read below H4's 0.99 floor un-truncated, and none reached 1.0); with tributary mouths placed on their trunks all 64 read 1.0 on BOTH columns. That PAIR is the measured evidence that the gap was the confluence separation and never band-edge speckle: two lines meeting at a point share their distance minimum AT that point, so a transect leaving a join recedes from both at once and cannot descend into the partner, while two lines held 4.5 half-widths apart have no shared minimum and did. **On The Rill's network the same 64 worlds read 12 of 64 above the floor and NONE at 1.0** (mean 0.9843, min 0.9724, max 0.9941). The repair is untouched and `channel-band-monotonicity` is still 1.0 on all 64 — what changed is density: with 3,606 polylines where there were 183, a transect walking outward meets an UNRELATED line far sooner, ending its own-channel prefix where this column scores the interruption as a failure. Read it beside `channel-transect-dry-reach` (mean 0.7577): about a quarter of transects are now truncated before reaching `dry`, against a Ford network on which truncation was doing nothing. Absent on a world with no channels

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.96484375 | 0.98235294 | 0.98627451 | 0.99015748 | 1 | 0.98578551 |

### `channel-connectivity`

H2's axis (The Ford, spec §10): the fraction of downstream walks — one per channel run, each starting at a headwater — that reach the sea or a terminal sink without ever leaving the `channel` band. Preregistered floor 0.95: a river you fall out of is not a river. Travelling along a run is in-channel by construction, so this measures the JOINS. It first read 0.862-0.953 (falsified; 4 of 64 probe worlds cleared the floor), which diagnosed an anchoring asymmetry: a tributary's mouth sat at its cell's undisplaced position while the trunk's vertex for that same cell was meander-displaced. **Since the confluence repair this column is a CONSTANT: 1.0 on every world with a channel network, Absent on every world without one.** Both of its failure branches are unreachable — a join is now a zero-length crossing, and the walk can no longer leave the network because `build` pushes a cell onto its claiming run BEFORE testing whether it was already claimed, so any cell the flow continues past is necessarily a non-final vertex of a kept run and always has an owner. Read a 1.0 here as a tripwire that the repair is still in place, never as a measurement of the world. **THE RILL'S TASK 3 THEN LEFT THAT TRIPWIRE LARGELY UNARMED, AND THE MILLRACE MEASURED AND REPAIRED IT.** The walk used to continue only while the next cell classified as `River`, while `ChannelNetwork::build`'s reach predicate is `!Ocean && downhill.is_some()` — strictly wider once Task 3 rendered the whole land flow tree (3,606 runs at seed 42, from 183). A run ending on a SUB-THRESHOLD trunk — rendered as a narrow channel, but not classified a river — stopped the walk BEFORE its join was examined, and that walk scored intact with nothing tested. **Measured at last (The Millrace, prediction P3, seed 42 at the canonical grid): 2,987 of 3,606 walks — 82.83% — had a false FIRST continuation test**, 0.749-0.850 across the 64 probe worlds; The Rill's review had guessed ~3,500 and rightly declined to assert it. The walk now asks `build`'s own reach predicate, so it stops only where the flow stops: on seed 42 alone the joins actually crossed go 853 -> 2,333 (2.74x), and summed over all 64 probe worlds they go 90,537 -> 219,763 (2.43x). **The value did not move.** The two rules are bit-identical on all 64 probe worlds, both 1.0, so this is a vacuous 1.0 converted into a tested 1.0 — which is why the repair landed in this column rather than beside it in a second one, and why no census value changed. The 0.95 floor is The Ford's preregistration, scored against The Ford's network; it is neither restated, retuned, nor re-scored here. Above all, do not read this column's stillness across a change as evidence that joins are sound: it is a constant by construction, and a column that did not move because it became vacuous is not the same reassurance as one that did not move because nothing broke

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 1 | 1 | 1 | 1 | 1 |

### `channel-land-fraction`

H1's axis (The Ford, spec §10): the fraction of LAND AREA the channel network occupies — the area of the river tube (every polyline segment's arc length times its channel width) over the land area (land cells over all cells, times 4π). Preregistered interval [0.005%, 0.5%]; below it rivers are invisible at room scale, above it a river is still effectively as wide as the cell carrying it. NOT a per-cell reading: the polylines run THROUGH cell centres, so sampling at them overstates this by ~127x. ABSENT ONLY ON A WORLD WITH NO LAND: unlike its four Ford siblings, which go Absent whenever the network is empty, this column reads a true Number(0.0) on a land-bearing world that happens to have no channels. The asymmetry is deliberate — zero channel area over positive land area is a measurement, while a connectivity or monotonicity fraction over zero transects has no denominator to divide by — but a consumer reading the five columns together must not treat a 0.0 here as the same state as an Absent there

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.0019848061 | 0.0021244083 | 0.0021703794 | 0.0022181322 | 0.0023791491 | 0.0021706782 |

### `channel-transect-dry-reach`

The anti-vacuity companion to `channel-band-monotonicity` (The Ford, spec §10): the fraction of the SAME transects whose own-channel prefix reached the `dry` band before a different river became the nearest and truncated it. Monotonicity scores a truncated transect as a success however short its prefix, so without this column a world where truncation fired immediately everywhere would report a perfect 1.0 with nothing to show it. Read the two together: monotonicity near 1 is only a claim about full transverse profiles while this stays high. Absent on a world with no channels

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.66992188 | 0.743083 | 0.76086957 | 0.77929688 | 0.8359375 | 0.76055563 |

### `coast-roughness-slope`

Multi-scale coastline-roughness slope, unbanded: the least-squares slope of ln(shoreline development) against mesh level, measured at L4/L5/L6 by projecting each level's cells onto the canonical L6 land/ocean truth (NearestVertexIndex). A companion to shoreline-development, not a replacement — that estimator is unchanged. Positive means roughness concentrates at fine scales, which makes this slope immune to the single-hex land/ocean alternation exploit that inflates shoreline-development without changing the coast's coarse shape; Absent if any of the three levels has no shoreline

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.10170349 | 0.21659881 | 0.25541154 | 0.27934681 | 0.35013046 | 0.24641712 |

### `delta-count`

Count of cells a river-mouth delta lobe raised above sea level (spec §5's top-K discrete deltas) — a cell count, not a mouth count: each of the top-K mouths can raise the mouth cell itself plus up to two adjacent hop-1 ocean cells

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 2 | 4 | 5 | 5 | 9 | 4.791 |

### `endorheic-coverage`

Fraction of land cells that are endorheic (interior-draining)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.12058127 | 0.24541669 | 0.28209795 | 0.32391779 | 0.45921657 | 0.28494938 |

### `hydro-variant-coverage`

Which `Hydro` variants `hydro_at` reads anywhere on this world, as `+`-joined names in `Hydro::ALL` order (The Assay). Replaces `domains/terrain/tests/hydro_witness.rs`'s 8-seed reachability sweep: a variant no world in the census shows is structurally dead, and 1,000 worlds say so with a rate where 8 said so with a flag.

n = 1000 present, 0 absent (of 1000 worlds)

| value | count | share |
|---|---|---|
| `aquifer+aquitard+spring+runoff+karst` | 1000 | 100.0% |

### `karst-fraction`

Fraction of land cells whose hydrogeology classifies as karst (The Ground, spec §3)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.01216584 | 0.089948836 | 0.11243012 | 0.13081133 | 0.23541969 | 0.10916129 |

### `rerouted-flow-fraction`

The A→B→C escalation diagnostic (spec §8, preregistered, a permanent census column): the flux-weighted fraction of the world's 20 largest pre-carve rivers' mainstem cells whose downhill target changed across the carve. Thresholds: < 0.10 engine A self-consistent; 0.10-0.30 flag, Nathan decides; > 0.30 A rejected as sole engine, engine B enters evaluation

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.036942488 | 0.082850322 | 0.096764304 | 0.11391599 | 0.18882055 | 0.099774166 |

### `shelf-fraction`

Fraction of cells within the shelf band (±200 m) of sea level — the populated shelf Earth's hypsometry keeps

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0.057565549 | 0.074068649 | 0.080769982 | 0.087300425 | 0.17015771 | 0.081060666 |

### `shelf-width-active-median`

Median shelf width over ACTIVE-margin coast land cells: hops seaward from the coast cell, each hop to the deepest ocean neighbor, until depth first exceeds twice the sediment wedge's freeboard cap or 8 hops are spent — spec §8's passive/active shelf asymmetry battery (active median should be narrower than passive); Absent if the world has no active-margin coast

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 2 | 2 | 2 | 2.5 | 1.9975 |

### `shelf-width-passive-median`

Median shelf width over PASSIVE-margin coast land cells (Passive/Interior/Oceanic, mirroring the carve's own wedge-reach margin split): hops seaward from the coast cell, each hop to the deepest ocean neighbor, until depth first exceeds twice the sediment wedge's freeboard cap or 8 hops are spent — spec §8's passive/active shelf asymmetry battery (passive median should exceed active); Absent if the world has no passive-margin coast

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 1 | 1 | 1 | 1 | 2 | 1.2355 |

### `shoreline-development`

Shoreline development index: coastline length over the circumference of the circle with the land's area (1 = maximally compact); Absent without a shoreline

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 5.0999345 | 7.4104239 | 8.2780951 | 9.0688031 | 11.438392 | 8.2338318 |

### `waterfall-count`

Count of waterfall (knickpoint) sites the carve found: land cells where a high-drainage watercourse crosses a sharp PRE-carve induration step (spec §5's derived point observations)

n = 1000 present, 0 absent (of 1000 worlds)

| min | p25 | median | p75 | max | mean |
|---|---|---|---|---|---|
| 0 | 0 | 0 | 1 | 5 | 0.437 |

## Weaknesses found here

### `channel-band-monotonicity`

- **D2**: min == median == max == 1 across 1000 worlds
- **D4**: median 1 equals the min (1 .. 1)

### `channel-connectivity`

- **D2**: min == median == max == 1 across 1000 worlds
- **D4**: median 1 equals the min (1 .. 1)

### `hydro-variant-coverage`

- **D1**: "aquifer+aquitard+spring+runoff+karst" holds 1000/1000 worlds (100.0%), at or above the 80% threshold

### `karst-fraction`

- **D5 strength**: declared weak tracking mean-land-temperature-c, but observed |r| = 0.028 (1000 pairs) is none

### `shelf-fraction`

- **D5 direction**: declared positive moderate tracking ocean-fraction, but the observed coupling is negative (r = -0.398, 1000 pairs) — the link runs backwards

### `shelf-width-active-median`

- **D3**: p25..p75 spans 0 (0.00% of the 1.5 min..max range), under the 5% bar

### `shelf-width-passive-median`

- **D3**: p25..p75 spans 0 (0.00% of the 1 min..max range), under the 5% bar
- **D4**: median 1 equals the min (1 .. 2)

### `shoreline-development`

- **D5 strength**: declared weak tracking total-tide, but observed |r| = 0.009 (1000 pairs) is none
- **D5 strength**: declared moderate tracking largest-continent-share, but observed r = -0.258 (1000 pairs) is weak (negative)

### `waterfall-count`

- **D4**: median 0 equals the min (0 .. 5)

