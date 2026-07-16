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

## The Earth anchor

Shoreline-development is the one band Sculpting's tuning season inherited
without an Earth anchor (spec §7): its governing floor was 1.3×–3.0× a
*dead generator's own interim median* (7.315, v1@L6), and the supersession
note flags that anchor as partly fragment-swarm noise. §7 re-derives it
the way the other five bands were derived: rasterize Earth's real
coastline onto the canonical L6 mesh and run the **same, unchanged**
estimator over it.

**Instrument:** `tools/earth-mask` (outside the workspace, mirroring
`tools/type-audit`) reads Natural Earth's 110m land polygons (public
domain; source and checksum recorded in the tool's doc comment) and, for
every L6 cell center, runs an even-odd ray cast in longitude/latitude
degrees against every ring (outer boundaries and holes) of every feature.
The output — `book/src/laboratory/generated/earth-mask-l6/rows.csv`, a
committed `cell,land` fixture, one row per L6 cell, ascending — carries a
land fraction of **29.10%** (11,919 of 40,962 cells), matching Earth's
real land fraction (~29.2%) closely enough to serve as a sanity check on
the rasterization itself.

`domains/terrain/src/shape.rs`'s `shoreline_development` was factored into
a mask-based core, `shoreline_development_of_mask`, with the elevation
version now deriving a mask and delegating — same iteration order, same
float accumulation order, proved byte-identical by
`mask_estimator_matches_elevation_estimator`. `windows/lab/tests/earth_anchor.rs`
loads the fixture, builds the L6 `Geosphere`, and runs that same core over
Earth's own mask.

**Measured: `D_earth@L6` = 8.2106747** (quantized to 8 significant
digits, `hornvale_kernel::quantize`'s convention), comfortably clearing
the pre-registered sanity floor of `d > 1.5` (Earth is not a circle).

**Preregistered proposed band** (spec §7's multiplier, `[D_earth/1.6, 1.6
× D_earth]`, committed before the mask existed):

- lower bound: 8.2106747 / 1.6 = **5.1316717**
- upper bound: 8.2106747 × 1.6 = **13.1370795**

so the candidate replacement band is **[5.1316717, 13.1370795]** — versus
the contested current floor of 9.51 (itself only a lower bound, no stated
upper bound) and the v3 tuning-probe's measured baseline median of
7.3202–7.4332 (Census III / this probe's own 20-seed readout, above).
Notably, the v3 baseline already sits *inside* this candidate band, while
it sits below the current 9.51 floor — the two anchors disagree about
whether v3 already passes.

Per spec §7 and ledger #11: **the multipliers were committed in the spec
before the mask existed; adoption of this band is Nathan's ruling, reserved
to the season close.**

## The tuning season (Stage 3)

Kin to Census of Coasts III's tuning table, on the same 100-seed instrument
(`studies/sculpting-probe.study.json`, seeds 0..=99, default pins, L6,
release). Iteration 0 is the v4 fit core *untuned* (before any Stage-3 knob
fires); each subsequent row activates one banked mechanism and re-measures.

Two bands are open this season, not one. Shoreline-development was already
open below its floor at v3 (Census III closed at 7.3202 vs the contested 9.51
floor). Shelf-fraction was *closed* at v3 (0.0916, inside `[0.08, 0.22]`) but
the v4 fit clip regressed it back out (iteration-0 baseline 0.0794) — the
Sculpting shape in reverse: there Crust handed shelf out-of-band and Sculpting
closed it via wave-cut; here the v4 fit opened it and this season must close
it. Both are **movers** driven by the tuning; the hard-stop **guards** are the
four stayers (hypsometric-bimodality 2–8, continent-count 3–12,
largest-continent-share 0.25–0.65, plate-size-gini 0.45–0.75),
rerouted-flow-fraction < 0.10, and the single-craton `shelf_land_ratio > 0.05`
floor.

| # | knob / mechanism | shoreline | shelf | reroute (median) | four stayers | single-craton floor | verdict |
|---|---|---:|---:|---:|---|---|---|
| 0 (baseline) | v4 fit core, untuned (no crenulation) | 6.8647 | 0.0794 | 0.0840 | all inside (3.17 / 8 / 0.278 / 0.706) | battery green | shoreline open below floor; **shelf regressed out of band** (v3 closed 0.0916 → 0.0794) |
| 1 (shipped) | cell-scale fracture crenulation activated: `CRENULATION_AMP` = 0.175 (`FRACTURE_AMP` × 0.5, first probe), `CRENULATION_FREQ` = 48.0, 1 octave, distinct hash-derived seed | **7.2334** (+0.369) | 0.0799 (+0.0004, flat) | 0.0806 | all inside (3.16 / 8 / 0.277 / 0.706) | battery green | guards held; shoreline moved toward floor; **crenulation does not recover shelf-fraction** |
| 2a (probe only, NOT shipped) | `CRENULATION_AMP` bumped 0.175 → 0.30 (taper unchanged), to bank shoreline headroom | 7.6403 (+0.407) | 0.0803 | 0.0824 | all inside (3.14 / 8 / 0.274 / 0.706) | battery green | shoreline headroom banked; **fails the conjugate-fit battery** (see below) → not shipped |
| 2 (shipped) | `CLIP_TAPER` widened 0.08 → 0.16 (Lever A, root-cause shelf recovery); crenulation held at its fit ceiling 0.175 | 6.8686 | **0.0841** (inside) | 0.0796 | all inside (3.06 / 8 / 0.281 / 0.706) | battery green | **shelf-fraction recovered into band**; conjugate-fit battery green; wider taper smooths coast, so shoreline returns to ~baseline |

Iteration-2 reading. **Shelf-fraction is recovered into `[0.08, 0.22]`** — the
v4 fit clip regressed it by cutting margins too steeply, and widening the
clip's near-sea taper (`CLIP_TAPER` 0.08 → 0.16) restores the shallow band at
its root: less continental crust removed at the coast, so crust tapers through
sea level over a wider swath. Median shelf-fraction 0.0794 → **0.0841**
(43/100 seeds below 0.08, down from 52; distribution mean 0.0916, back to the
v3 figure). The conjugate fit is untouched: the clip reads exactly 0.5 at the
seam curve for *any* taper (`smoothstep(0.5) == 0.5`), so
`conjugate_margins_fit_by_construction` stays green — shelf was recovered
without trading the fit, the season's signature. Every stayer held (bimodality
softened 3.16 → 3.06 as the wider shelf taper predicts, still far inside its
2–8 band), and reroute held at 0.0796.

Two mechanism findings fell out of iteration 2.

*Crenulation has a fit-battery ceiling at ~0.175.* Bumping `CRENULATION_AMP`
to 0.30 (row 2a) does bank more shoreline (7.2334 → 7.6403), but crenulation
is high-frequency (`CRENULATION_FREQ` = 48), so past ~0.175 the seam curve
oscillates across the conjugate-fit battery's cross-arc strongly enough that
its single-root bisection can no longer sample ≥ 30 shared-curve points
(`conjugate_margins_fit_by_construction` finds 28 points at 0.20, 22 at 0.25,
19 at 0.30 — below the battery's 30-point floor). The fit *holds* wherever a
zero is found (0.5 for any noise); the curve is simply too crenulated to
sample. So 0.30 is a measurement, not a shippable amplitude — crenulation
ships at 0.175, its fit ceiling.

*Shelf-recovery and shoreline-roughening are antagonistic through the coast.*
The wider taper that recovers shelf also *smooths* the coastline (a gentler
near-sea gradient is a less crenulated one), which returns shoreline-
development to ~baseline (6.8686, from iteration 1's 7.2334). The two open
bands are not independent: the very lever that closes shelf partly undoes
crenulation's shoreline gain. This is acceptable under the operating frame —
6.8686 sits comfortably inside the Earth-anchor band **[5.13, 13.14]**
(`D_earth` = 8.21), and shoreline's contested 9.51 legacy floor is a
band-supersession question reserved to Nathan at close (§7 / Task 11 G6), not
an amplitude chase (which, given the fit ceiling and the high-frequency-texture
anti-pattern, crenulation cannot win anyway). Shelf is the band this season
must *close*, and it is closed; shoreline is a band to *hand over*, and it
already passes the Earth-anchored replacement.

Iteration-1 reading. Crenulation is a real shoreline-development mover: at the
first-probe amplitude it closes roughly **14%** of the 6.8647 → 9.51 gap in one
step (+0.369), with every guard holding and reroute actually easing (0.0840 →
0.0806 — crenulation reshapes coastline geometry, not the fluvial network, as
expected). But it leaves shelf-fraction essentially untouched (+0.0004, still
52/100 seeds below the 0.08 floor). The mechanism explains this: crenulation
alternates land/ocean at cell scale along the margin, multiplying coastline
*perimeter* while conserving near-sea-level *area* (it flips cells in both
directions). Shelf-fraction counts the fraction of cells in a shallow band
around sea level — an area measure crenulation does not feed. So the
hypothesis that crenulation would recover the fit-clip's shelf regression is
**falsified**: shoreline and shelf are near-orthogonal to this lever. Shelf
recovery needs its own mechanism (a shelf-widening / wave-cut-style lever, as
Sculpting used), or the shelf-fraction floor — never Earth-anchored, and
missed here by 0.2% — is a band-definition question for the season close.
