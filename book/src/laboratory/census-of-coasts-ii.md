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

| metric | v1@L5 baseline (Study 010, median) | v1@L6 interim (median) |
|---|---|---|
| shoreline-development | 6.00 | 7.32 |
| hypsometric-bimodality | 3.04 | 2.97 |
| shelf-fraction | 0.274 | 0.378 |
| continent-count | 14 | 22 |
| largest-continent-share | 0.79 | 0.85 |
| plate-size-gini | 0.26 | 0.26 |

## The acceptance bands (spec §7)

Preregistered before any generator code lands; judged on **v2@L6** medians;
undershoot and overshoot both fail (every classic procgen failure is an
overshoot of a "good" direction, not just a miss of it):

```
metric                    band              rationale
------------------------  ----------------  ----------------------------------------
shoreline-development     9.51 - 21.95      estimator is resolution-bound; band is
                                             relative to the same-grid interim (1.3x -
                                             3.0x of the v1@L6 interim median, 7.32)
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

The v2 rows and verdicts land with the Crust epoch.
