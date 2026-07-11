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
| shoreline-development | 6.00 | 7.32 | 11.09 |
| hypsometric-bimodality | 3.04 | 2.97 | 1.59 |
| shelf-fraction | 0.274 | 0.378 | 0.581 |
| continent-count | 14 | 22 | 50.5 |
| largest-continent-share | 0.79 | 0.85 | 0.730 |
| plate-size-gini | 0.26 | 0.26 | 0.767 |

v2@L6: 10,000 seeds, `studies/census-of-coasts.study.json` unpinned, run against
the tuned constants recorded in the Tuning iterations table below (iteration
1). 0 refusals.

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

## v2@L6 verdicts

| metric | v2@L6 median | band | verdict |
|---|---|---|---|
| shoreline-development | 11.09 | 9.51 – 21.95 | **inside band** |
| hypsometric-bimodality | 1.59 | 2.0 – 8.0 | outside band, low |
| shelf-fraction | 0.581 | 0.08 – 0.22 | outside band, high |
| continent-count | 50.5 | 3 – 12 | outside band, high |
| largest-continent-share | 0.730 | 0.25 – 0.65 | outside band, high |
| plate-size-gini | 0.767 | 0.45 – 0.75 | outside band, high |

**shoreline-development — inside band.** 11.09 sits comfortably inside
9.51–21.95 (1.51x the v1@L6 interim median), up from the interim's 7.32:
the lobed craton margins and boundary-uplift arcs both add real coastline
complexity without tipping into the pathological over-jaggedness the band's
ceiling guards against.

**hypsometric-bimodality — outside band, low.** 1.59 is *below* the 2.0
floor and has *moved the wrong way* from the v1@L6 interim (2.97): rather
than sharpening the land/ocean elevation separation the Crust generator
was meant to produce, the distribution flattened. Diagnosed cause (below):
a swarm of small islands near sea level, plus wide isostatic taper margins
on every craton, populate the histogram's middle range instead of leaving
it empty between two sharp modes.

**shelf-fraction — outside band, high.** 0.581 is nearly 3x the
(superseded, see above) 0.08–0.22 band's ceiling, and worse than either
prior measurement (v1@L5 0.274, v1@L6 interim 0.378) that motivated the
supersession in the first place — this is not "a true shelf scoring lower
than the decay-slope baseline" as the supersession note anticipated, it is
a larger near-sea-level share than ever measured on this branch. Same root
cause as bimodality: with `land_component_sizes` showing dozens of small
components near sea level, "within ±200 m of sea level" now covers a much
larger share of the globe than a genuine continental shelf would.

**continent-count — outside band, high, by a wide margin.** 50.5 is over
4x the band's ceiling (12) and more than double the v1@L6 interim (22).
`continent-count` counts *every* connected land component with no size
floor (`hornvale_terrain::shape::land_component_sizes`), so this number
is a direct, uncushioned readout of fragmentation. Sampled directly (seeds
0–2, level 6, default pins): 50, 31, and 49 connected components per world,
of which 27, 14, and 34 respectively are smaller than 5 cells — a long tail
of tiny islands sitting alongside one dominant landmass (16,066 / 10,640 /
6,005+5,064 cells) and a handful of medium ones. This is the **fragmented-
island symptom**: noisy plate-boundary uplift (`EDGE_AMP`) breaking the
surface as scattered arcs and the lobing kernel's rim noise (`LOBE_AMP`)
carving detached fringe fragments off the main cratons, independent of the
craton area-quota fix this iteration made.

**largest-continent-share — outside band, high, but improved.** 0.730 is
still above the 0.65 ceiling, but it *moved toward* the band from the
v1@L6 interim's 0.85 — the area-normalization fix is doing its intended
job of assembling fewer, appropriately-sized landmasses instead of one
oversized "guitar pick." The residual overshoot is consistent with the
same fragmentation mechanism as continent-count: one dominant landmass
plus a fringe of tiny islands inflates both the dominant share and the
component count simultaneously; they are not independent failures.

**plate-size-gini — outside band, high, and unrelated to this iteration.**
0.767 is a narrow miss over the 0.75 ceiling, but it moved from the v1@L6
interim's 0.26 to here entirely on Task 6's heavy-tailed plate weights
(`WEIGHT_TAIL`) and noisy plate edges (`EDGE_AMP`), which landed *between*
the interim measurement and this one — `crust.rs`'s craton budget and
`PEAK_MIN_KM` (this iteration's only two authorized knobs) do not touch
plate generation at all. This band's movement predates Task 9 and is not
diagnosable from this iteration's evidence.

## Tuning iterations

| # | constants | census run? | shoreline-dev | bimodality | shelf | continent-count | largest-share | plate-gini | verdict |
|---|---|---|---|---|---|---|---|---|---|
| 0 | pre-Task-9 (budget 0.15–0.40; `PEAK_MIN_KM` 30 == `ISOSTASY_REF_KM`) | **not run** — predicted fail from Task 8's structural evidence (single-craton test: 2/20 default-seed pass rate on briefed shelf/bimodality bounds at canonical L6) | — | predicted fail | predicted fail | — | — | — | predicted fail (disclosed, not measured) |
| 1 | budget 0.20–0.30·u (0.20–0.50), post-draw area-normalization rescale to match budget (new, capped 0.6 rad); `PEAK_MIN_KM` 30→33 | **run**, 10,000 seeds, 0 refusals | 11.09 — inside | 1.59 — outside, low | 0.581 — outside, high | 50.5 — outside, high | 0.730 — outside, high | 0.767 — outside, high | 1/6 inside |

Iteration 1 is the directed, pre-authorized fix for Task 8's diagnosed
area-quota mismatch (craton footprint budget vs. `ocean_fraction`'s land
quota, and `PEAK_MIN_KM` exactly equalling `ISOSTASY_REF_KM`) — see
`domains/terrain/src/crust.rs`'s `draw_cratons` doc comment and
`domains/terrain/src/elevation.rs`'s module doc for the mechanism. It
measurably worked: shoreline-development moved inside its band, and
largest-continent-share moved substantially toward its band (0.85 → 0.730).
It is not, however, sufficient — four of six bands still fail, and one of
those failures (plate-size-gini) traces to a change (Task 6's plate
weights) outside this iteration's scope entirely.

## STOP: BLOCKED on the lobing-clamp contingency

No second iteration was attempted. `continent-count` fails **high by more
than 4x the band ceiling** (50.5 against 3–12), and the direct evidence
above (`land_component_sizes` sampled at seeds 0–2: dozens of sub-5-cell
components alongside one dominant landmass) confirms this is the
**fragmented-island symptom**: noisy plate-boundary uplift and lobing-rim
noise scattering small detached landmasses across the ocean, not a craton
count or size miscalibration. Per the authorized dispatch, this specific
failure signature — shoreline-development or continent-count failing high
with fragmented-island symptoms — is the pre-identified **lobing-clamp
contingency**, explicitly out of this task's authorized scope (the two
tunable knobs, craton-area budget and `PEAK_MIN_KM`, do not touch plate-
boundary noise or the lobing kernel that produce it). It is reported here,
not silently patched: the controller holds a banked remedy for this
symptom that needs separate authorization before a second iteration can
run.

Two of the remaining failures (hypsometric-bimodality low, shelf-fraction
high) share the same root cause as continent-count — a swarm of near-sea-
level fragments dilutes the elevation histogram's separation and inflates
the near-sea-level share — so they are expected to move together with
whatever remedies the lobing-clamp contingency. `plate-size-gini`'s
failure is independent (Task 6, not diagnosable further from this
iteration's evidence).

This v2@L6 census — one band inside range, four outside for reasons tracing
to the lobing-clamp contingency, one outside for an unrelated, pre-existing
reason — is nonetheless the number Sculpting inherits as its *before*
photograph: Sculpting layers modulated relief, erosion, and hotspot trails
on top of whatever Crust hands it, and this page is where that starting
point is recorded, bands met or not. The supersession note above stays in
force regardless of the disposition here — it documents why the band
itself reads 0.08–0.22, not whether v2 hits it.
