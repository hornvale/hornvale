# The Census of Coasts IV

**Preregistered before any generator code landed** (rift-and-fit spec §6,
ledger #5, house style established by the Sculpting instrument this one is
kin to): this page records the Stage-0 synthetic-D probe's readout,
committed *before* Stage 1's fit core (assembly frame, fracture network,
conjugate clip) writes a single line. The instrument
(`domains/terrain/tests/rift_probe.rs`, `#[ignore]`d, run by hand) is
read-only against v3 — it injects each of §5's three banked rift-texture
mechanisms *synthetically and in isolation* onto real default-pin land
masks and measures the shoreline-development delta each produces, so the
activation constants §5 defers can be chosen from measured leverage rather
than guessed.

**Instrument, exactly as run:**

```
cargo test -p hornvale-terrain --release --test rift_probe -- --ignored --nocapture
```

20 world seeds (1..=20), default pins, the canonical globe level
(`hornvale_terrain::GLOBE_LEVEL` = 6), release build. Not a census — the
1000-seed-or-larger population never runs locally (decision 0046); this
probe ranks mechanisms by median leverage over a small, fixed, preregistered
seed set, the same discipline Sculpting's tuning-probe used.

## Context: the band this probe informs

Shoreline-development is the one band Sculpting's tuning season left open
(Census of Coasts III): the v3 tuning-probe median closed at **7.3202**
(100 seeds, iteration 4, every §-banked-then coastal mechanism firing), the
governing floor sits at **9.51** (contested — Census III's supersession
note flags the anchor as partly contaminated by a removed fragment-swarm
generator, re-derivation deferred to this campaign's §7). This probe's own
20-seed baseline median came in at **7.4332** — consistent with Census
III's 100-seed figure, confirming no drift since Sculpting's close. The gap
to the (contested) floor is therefore roughly **+2.08 to +2.18** in D,
depending on which baseline is used.

## Probe tables: median ΔD per mechanism per sweep point (20 seeds)

### Rift-shoulder sliver strings

Swept fraction `f` of coastal ocean cells flipped to just-above-sea land,
alternating and never adjacent (candidates are coastal ocean cells in
ascending `CellId`, every `k = ceil(1/f)`-th flipped, skipping any whose
neighbor was already flipped this pass):

| f (nominal coastal-ocean fraction) | median ΔD |
|---:|---:|
| 0.05 | 0.3234 |
| 0.15 | 0.8155 |
| 0.30 | 1.3470 |

### Failed rift arms (aulacogens)

`n_arms` evenly-spaced coastal land starting cells, each walked inland via
the highest-(pre-injection-)elevation unvisited neighbor for `depth` cells
total, flipping every visited cell to just-below-sea ocean (a 1-cell-wide
bay):

| n_arms | depth | median ΔD |
|---:|---:|---:|
| 4 | 3 | 0.0497 |
| 4 | 6 | 0.1125 |
| 8 | 3 | 0.0945 |
| 8 | 6 | 0.2038 |
| 16 | 3 | 0.2094 |
| 16 | 6 | 0.4485 |

### Fracture-line crenulation

Sculpting's parity experiment re-run against the same fraction sweep as
slivers, for direct comparability: swept fraction `f` of coastal land cells
flipped to just-below-sea ocean, single-hex alternation (every
`k = ceil(1/f)`-th coastal land cell, ascending `CellId`, no neighbor-skip
guard — the mechanism itself *is* the single-cell-scale alternation):

| f (nominal coastal-land fraction) | median ΔD |
|---:|---:|
| 0.05 | 0.2774 |
| 0.15 | 0.7759 |
| 0.30 | 1.2227 |

## Post-carve fit-degradation proxy (unbanded, spec §6/ledger #10)

Over cells that are coastal under the final (post-carve) elevation reading,
the fraction whose land/ocean classification differs between the pre-carve
reading (`elevation − carve_delta_m`) and the final reading, both judged
against the same final sea level — how much of the carve's own
wave-cut/wedge/barrier texture blurs a margin that was exactly fitted
before the carve ran:

**Median over 20 seeds: 0.0187** (seed range 0.0072–0.0584). Roughly 2% of
coastal cells flip classification across the carve — a small but nonzero
blur, recorded here unbanded per ledger #10 so §9's narration ("margins fit
*up to subsequent erosion*") has a measured number behind the honesty
caveat rather than an assertion.

## Activation reading

**Expected (spec §5):** slivers high, arms moderate, crenulation
small–moderate.

**Measured:** slivers and crenulation land in the *same* leverage class —
both climb to ΔD ≈ 1.2–1.35 at f=0.30, with crenulation trailing slivers by
5–14% across the sweep (closest at f=0.15, furthest apart at f=0.05) —
while arms top out at ΔD ≈ 0.45 (their
widest swept configuration, 16 arms × depth 6), roughly a third of the
fraction-based mechanisms' ceiling. **This falsifies the spec's predicted
ordering for crenulation**: rather than "small–moderate", crenulation
measures as a **high**-leverage mechanism, essentially co-equal with
slivers rather than trailing them. The mechanistic reason is visible in the
injection code itself — both slivers and crenulation are governed by the
same "flip every k-th coastal cell" fraction-sweep shape (one on the ocean
side of the coast, one on the land side), so it is unsurprising in
retrospect that they produce near-identical shoreline-perimeter leverage;
what differs is only which side of the existing coastline gains the new
edge. Arms are the outlier in the other direction: because each arm is a
single 1-cell-wide bay (not a set of independent alternating flips along
the whole coast), their perimeter contribution per flipped cell is far
lower — an arm mostly relocates coastline rather than multiplying it,
whereas slivers/crenulation add new coastline at every alternation step.

At the largest swept configuration for each mechanism, slivers (f=0.30,
ΔD=1.3470) close roughly **62%** and crenulation (f=0.30, ΔD=1.2227) close
roughly **56%** of the gap to the contested 9.51 floor from the 7.3202 v3
baseline (gap = 2.1898); arms' largest swept configuration (n=16, depth=6,
ΔD=0.4485) closes only about **20%**. None of the three mechanisms alone, even at
their swept ceilings, reaches the floor — consistent with §8's escalation
criterion anticipating that the fit core plus every banked mechanism,
swept, might still fall short, at which point the stop condition is the
band-supersession conversation (§7), not a fifth mechanism. Because
slivers and crenulation both show band-relevant, materially-moving
leverage, both are activation candidates for Stage 3's tuning season; arms
show real but comparatively modest leverage and remain a build candidate
for realism (the Benue Trough motivation stands on its own) more than for
closing this specific band. The final activation call, and the constants
each mechanism ships with, are Stage 3's — chosen by sweep against the
worst seeds, per spec §5, once the fit core (Stage 1) is in place to carry
them.
