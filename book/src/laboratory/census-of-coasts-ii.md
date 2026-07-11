# The Census of Coasts II

Crust's terrain overhaul lands in two measured stages on one branch (spec
§7), so that a single before/after across both a grid-resolution bump and a
generator change is never confounded: **Stage L** raises the canonical
geodesic grid from level 5 (10,242 cells) to level 6 (40,962 cells, ~110 km)
with generator v1 otherwise unchanged, and this page records that
measurement — the **v1@L6 interim baseline** — against Study 010's original
v1@L5 numbers; **Stage C** then lands the v2 crust generator and re-runs the
same census for the **v2@L6** after-photograph, judged against the
acceptance bands below. Both stages re-run `studies/census-of-coasts.study.json`
unpinned over the same 10,000 seeds; only the grid level (and, at Stage C,
the generator) differ between rows.

| metric | v1@L5 baseline (Study 010, median) | v1@L6 interim (median) | v2@L6 after (median) |
|---|---|---|---|
| shoreline-development | 6.00 | 7.32 | 11.36 |
| hypsometric-bimodality | 3.04 | 2.97 | 1.72 |
| shelf-fraction | 0.274 | 0.378 | 0.276 |
| continent-count | 14 | 22 | 54 |
| largest-continent-share | 0.79 | 0.85 | 0.746 |
| plate-size-gini | 0.26 | 0.26 | 0.709 |

v2@L6: 10,000 seeds, `studies/census-of-coasts.study.json` unpinned, run against
the tuned constants recorded in the Tuning iterations table below (iteration
2, the committed generator). 0 refusals. Iteration 1's medians are preserved
in the Tuning iterations table.

## The acceptance bands (spec §7)

Preregistered before any generator code lands; judged on **v2@L6** medians;
undershoot and overshoot both fail (every classic procgen failure is an
overshoot of a "good" direction, not just a miss of it):

```
metric                    band              rationale
------------------------  ----------------  ----------------------------------------
shoreline-development     9.51 - 21.95      estimator is resolution-bound; band is
                                             relative to the same-grid interim (1.3x -
                                             3.0x of the v1@L6 interim median, 7.315138,
                                             displayed as 7.32)
hypsometric-bimodality    2.0 - 8.0         retain Earth-like separation; above 8
                                            means two delta functions, no shelf
shelf-fraction            0.08 - 0.22       Earth's true shelf+coastal-plain share;
                                            SUPERSEDES Study 010's "up" direction
                                            (see note)
continent-count           3 - 12            Earth: 6-8 major landmasses
largest-continent-share   0.25 - 0.65       Afro-Eurasia is ~0.57 of Earth's land
plate-size-gini           0.45 - 0.75       ~7 giants + microplate fringe
```

## Supersession note: shelf-fraction

Recorded openly, per the preregistration discipline: Study 010 preregistered
"shelf fraction up" from a baseline median of 0.274. That baseline number
reflects decay-slope coasts — many cells *near* sea level without shelf
structure — not a shelf. A true shelf is a *mode near sea level inside an
otherwise bimodal distribution*; Earth's within-±200 m share is roughly
0.10–0.15 of the surface. The band above (0.08–0.22) supersedes the
direction, not just the baseline it was measured from — a true shelf could
legitimately score *lower* than either measured baseline, if the decay-slope
cells it must replace were themselves inflating the number. Changing a
preregistered target is permitted only in the open, which is what this note
is.

## v2@L6 verdicts (iteration 2, the committed generator)

| metric | v2@L6 median | band | verdict |
|---|---|---|---|
| shoreline-development | 11.36 | 9.51 – 21.95 | **inside band** |
| hypsometric-bimodality | 1.72 | 2.0 – 8.0 | outside band, low |
| shelf-fraction | 0.276 | 0.08 – 0.22 | outside band, high |
| continent-count | 54 | 3 – 12 | outside band, high |
| largest-continent-share | 0.746 | 0.25 – 0.65 | outside band, high |
| plate-size-gini | 0.709 | 0.45 – 0.75 | **inside band** |

**shoreline-development — inside band.** 11.36 sits comfortably inside
9.51–21.95 (1.55x the v1@L6 interim median), essentially unchanged from
iteration 1's 11.09: the lobed craton margins and boundary-uplift arcs add
real coastline complexity, and the smooth tanh rims did not remove it.

**hypsometric-bimodality — outside band, low, moved toward the band.**
1.72 is still below the 2.0 floor but moved the right way from iteration
1's 1.59 (the interim was 2.97): the tanh rims and separated cratons
sharpened the land/ocean separation somewhat. The residual flattening
shares its cause with continent-count — a swarm of near-sea-level island
fragments and wide isostatic taper margins still populate the histogram's
middle range.

**shelf-fraction — outside band, high, moved dramatically toward the
band.** 0.276 is barely over the (superseded, see above) 0.08–0.22 band's
ceiling, less than half iteration 1's 0.581, and back to the v1@L5
baseline's level (0.274) — the smooth tanh rim taper replaced the hard
clamp's near-sea-level plateau shelves with steeper margins. What remains
above the ceiling tracks the same fragment swarm as continent-count.

**continent-count — outside band, high, by a wide margin, essentially
unmoved.** 54 against a ceiling of 12, marginally up from iteration 1's
50.5 — and this is the iteration's clearest negative result, because the
tanh lobing remap was *predicted* to cut this sharply by eliminating
clamp-plateau pinch-off fragments. Re-sampled directly (seeds 0–2, level
6, default pins): 59, 40, and 51 connected components per world, of which
40, 17, and 31 respectively are smaller than 5 cells (iteration 1 measured
50/31/49 components, 27/14/34 tiny) — the long tail of tiny islands beside
one dominant landmass (16,066 / 9,882 / 6,265 cells) is intact. The
fragments therefore do NOT primarily come from the lobing clamp's
pinch-off plateaus: the surviving suspect is noisy plate-boundary uplift
(`EDGE_AMP` arcs crossing sea level in open ocean), which no authorized
knob in either iteration has touched.

**largest-continent-share — outside band, high, moved slightly away.**
0.746 against a 0.65 ceiling, up from iteration 1's 0.730 (the interim was
0.85). The craton repulsion pass was predicted to move this *down* by
separating overlap-merged landmasses; the measured move is small and in
the wrong direction. The plausible mechanism: the tanh rims stopped
carving fringe fragments *off the dominant landmass* (those cells rejoined
the largest component) faster than repulsion split merged landmasses
apart, and repelled cratons — pushed to exactly rim-touching distance —
still merge through their overlapping 1.5x-radius lobing skirts.

**plate-size-gini — inside band.** 0.709 against 0.45–0.75, in from
iteration 1's 0.767 overshoot, exactly as the `WEIGHT_TAIL` 0.95 → 0.92
trim predicted (~0.70): the softened heavy tail keeps a few giants per
world without letting them swallow the globe. This was the one failure
with a clean single-knob cause, and the knob worked.

## Tuning iterations

| # | constants | census run? | shoreline-dev | bimodality | shelf | continent-count | largest-share | plate-gini | verdict |
|---|---|---|---|---|---|---|---|---|---|
| 0 | pre-Task-9 (budget 0.15–0.40; `PEAK_MIN_KM` 30 == `ISOSTASY_REF_KM`) | **not run** — predicted fail from Task 8's structural evidence (single-craton test: 2/20 default-seed pass rate on briefed shelf/bimodality bounds at canonical L6) | — | predicted fail | predicted fail | — | — | — | predicted fail (disclosed, not measured) |
| 1 | budget 0.20–0.30·u (0.20–0.50), post-draw area-normalization rescale to match budget (new, capped 0.6 rad); `PEAK_MIN_KM` 30→33 | **run**, 10,000 seeds, 0 refusals | 11.09 — inside | 1.59 — outside, low | 0.581 — outside, high | 50.5 — outside, high | 0.730 — outside, high | 0.767 — outside, high | 1/6 inside |
| 2 | tanh lobing remap (`0.5·tanh(GAIN·(n−0.5))` replaces the hard `clamp(−0.5, 0.5)`; `REBALANCE_GAIN` 6→15, recalibrated by sweep — see note); `WEIGHT_TAIL` 0.95→0.92 (plate weights [1, 12.5], median ~1.85); one deterministic craton-center repulsion pass (id order, zero draws, skipped under `--supercontinent`) | **run**, 10,000 seeds, 0 refusals | 11.36 — inside | 1.72 — outside, low (predicted up: **right**) | 0.276 — outside, high (predicted down: **right**, 0.581→0.276) | 54 — outside, high (predicted sharply down: **wrong**, 50.5→54) | 0.746 — outside, high (predicted toward ≤0.65: **wrong**, 0.730→0.746) | 0.709 — **inside** (predicted ~0.70: **right**) | 2/6 inside |

Iteration 2 note — the gain recalibration, recorded per the sweep
discipline: under the tanh map the rim-spread calibration (≥95% of 2000
seeds with spread > 0.2 on the test's ring geometry) fails everywhere in
the first-authorized 6–12 range (6 → 39.45%, 7 → 57.45%, 8 → 69.40%, 9 →
78.35%, 10 → 84.80%, 11 → 88.55%, 12 → 90.85%); the controller extended
the sweep to 13–16 (13 → 93.15%, 14 → 94.50%, **15 → 95.40%**, 16 →
95.95%) and the smallest clearing gain, 15, was taken. tanh saturates only
asymptotically where the clamp cut off exactly, so it needs a higher gain
for the same rim contrast — and its output is strictly inside (−0.5, 0.5),
so the rim bound tightens from a closed to an open interval.

Iteration 1 is the directed, pre-authorized fix for Task 8's diagnosed
area-quota mismatch (craton footprint budget vs. `ocean_fraction`'s land
quota, and `PEAK_MIN_KM` exactly equalling `ISOSTASY_REF_KM`) — see
`domains/terrain/src/crust.rs`'s `draw_cratons` doc comment and
`domains/terrain/src/elevation.rs`'s module doc for the mechanism. It
measurably worked: shoreline-development moved inside its band, and
largest-continent-share moved substantially toward its band (0.85 → 0.730).

Iteration 2 is the Nathan-authorized three-knob package aimed at the
remaining failures. Two of its three predictions came true (plate-size-gini
into band on the `WEIGHT_TAIL` trim; shelf-fraction more than halved and
bimodality up on the tanh rims) — but its central prediction, that the
lobing clamp's pinch-off plateaus were the fragment factory, was **refuted
by measurement**: continent-count did not fall (50.5 → 54), and the direct
re-sample (seeds 0–2: 59/40/51 components, 40/17/31 under 5 cells) shows
the tiny-island swarm intact. The craton repulsion pass likewise failed to
move largest-continent-share down (0.730 → 0.746). The fragment evidence
now points at noisy plate-boundary uplift (`EDGE_AMP` arcs surfacing in
open ocean), which no knob authorized in either iteration touches.

## STOP: iteration 2 recorded; four bands still outside

Per the tuning protocol (bands are immutable; every iteration is recorded
openly; a failing verdict stops the loop rather than iterating ad hoc),
iteration 2 is committed as measured and no third iteration was attempted.
The verdict stands at **2/6 inside** (shoreline-development,
plate-size-gini), with the four remaining failures sharing, on the current
evidence, one dominant unaddressed mechanism: `EDGE_AMP` boundary-uplift
arcs surfacing as open-ocean island chains, inflating continent-count
directly, holding shelf-fraction and hypsometric-bimodality just outside
their bands via the near-sea-level fragment swarm, and inflating
largest-continent-share's denominator asymmetry. Whether to authorize an
`EDGE_AMP`-side remedy (or accept this as Crust's hand-off state) is the
controller's and Nathan's call, not this page's.

This v2@L6 census — two bands inside range, four outside — is nonetheless
the number Sculpting inherits as its *before* photograph: Sculpting layers
modulated relief, erosion, and hotspot trails on top of whatever Crust
hands it, and this page is where that starting point is recorded, bands
met or not. The supersession note above stays in force regardless of the
disposition here — it documents why the band itself reads 0.08–0.22, not
whether v2 hits it.
