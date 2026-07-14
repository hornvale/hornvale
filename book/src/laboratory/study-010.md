# Study 010: The Census of Coasts

Ten thousand tier-0 worlds, unpinned — the same seeds 0 through 9,999 every
prior census walks — measured against six new metrics that quantify
continental *shape* rather than continental *quantity*. Every earlier
census counted things (plates, biomes, settlements, names); none asked
whether a continent's outline looks like Earth's. Campaign 25, The Measured
Coast, exists to ask exactly that, as the first of a three-campaign
roadmap: this campaign only measures and improves the *lens* (no terrain
epoch); **Crust** then rebuilds the elevation field itself (cratons,
margins, per-cell hypsometry); **Sculpting** layers modulated relief,
erosion, and hotspot trails on top. Measurement comes first on principle —
the shape improvements of Crust and Sculpting must be judged against
preregistered numbers, not vibes, and this study is where those numbers are
recorded.

**A note on scale and provenance.** The headline numbers and every embedded
chart below come from one 10,000-seed run of
`studies/census-of-coasts.study.json`, executed once by hand at author
time, in the same arrangement Studies 001, 002, 006, 007, and 008 use for
their own 10,000-seed headline runs. It is **not** part of CI —
`.github/workflows/ci.yml`'s "Artifacts are current" step regenerates only
`the-census`, `census-of-the-meeting`, and `census-of-skies`
(all three select `"metrics": "all"`, so they already carry these six
columns from Task 4's metrics commit onward). The campaign spec originally
proposed a fourth CI-drift study, `census-coasts-drift.study.json`, but that
bullet was found redundant during planning — double-running the same
six-metric measurement CI already exercises — and is amended out; drift
coverage of these metrics rides the three existing `all`-metric studies.
Unlike Study 002's or Study 008's two-population arrangement, there is no
smaller drift twin whose chart might read a few tenths of a point off this
chapter's prose: the artifacts embedded below and the numbers quoted in the
prose are the *same* 10,000-row run, committed together in one commit.

**A note on preregistration.** Per ADR 0016, a study commits its expected
direction before the sweep that could confirm or refute it runs. This
study's whole purpose is that commitment: four of the five target
directions below are drawn directly from the campaign spec, §2
(`docs/superpowers/specs/2026-07-09-the-measured-coast-design.md`) (two
verbatim, two with the metric's full name substituted), written and ratified
before this census's 10,000-seed run executed; the fifth (hypsometric
bimodality) is a synthesis consistent with the spec's own framing of that
metric rather than a literal quote (see below). All five
are recorded here **before Crust or Sculpting change one line of the
elevation field** — so that when a later campaign re-runs this same study
against a new generator, the comparison is a genuine before/after, not a
target quietly redrawn to fit whatever the new generator happens to
produce.

## The six metrics

**Shoreline development**, `D = L / (2 √(π A))` — coastline length over the
circumference of the circle enclosing the same land area, the standard
limnological compactness index. `L` sums the great-circle length of every
cell edge whose two cells straddle sea level (approximated on the geodesic
grid as center distance over `√3`, the regular-hexagon dual); `A` sums land
cell areas under the equal-area approximation `4π/N`. A single compact
landmass scores near 1; a fjorded or fractal coast scores several times
that. `Absent` when a world has no land or no shoreline at all.

**Hypsometric bimodality** — Ashman's `D = |μ_land − μ_ocean| /
√((σ²_land + σ²_ocean)/2)` between the land and ocean elevation
populations (population variance). Earth's own hypsometric curve is
strongly bimodal — continental platform and abyssal ocean floor sit at two
well-separated elevation bands with little in between. `Absent` when either
population is empty, or both are degenerate (zero combined variance).

**Shelf fraction** — the share of all cells within ±200 m of sea level (the
`SHELF_BAND_M` constant, Earth-informed: the order of magnitude of Earth's
own continental shelf depth). This is bimodality's complement: a world can
score highly bimodal (land and ocean cleanly separated on average) while
still holding almost no cells *near* the boundary between them, i.e. no
populated shelf — the two metrics together distinguish "two modes with a
graded margin" from "two modes with a cliff."

**Continent count** — the number of connected land components, found by a
deterministic breadth-first search seeded in ascending cell-id order so the
count never depends on iteration order. Zero is a valid count (a landless
world); the metric is never `Absent`.

**Largest-continent share** — the largest component's fraction of all land
cells, from the same component search. `Absent` on a landless world (there
is no largest component to take a share of).

**Plate-size Gini** — the Gini coefficient over plate cell counts,
`G = 2 Σ i·x_i / (n Σx) − (n+1)/n` over ascending counts with 1-based rank
`i`. 0 is perfect equality; Earth's own plates are famously heavy-tailed
(seven giants carrying most of the surface, a long tail of microplates),
which scores high. A first-order Voronoi partition — plates as
same-sized-on-average cells of a random tessellation — scores low.
`Absent` only if there are no plates or all plate counts are zero, which
does not occur in a built world.

## The baseline, at 10,000 seeds

{{#include generated/census-of-coasts/census-of-coasts-summary.md}}

Across every one of the six metrics, all 10,000 rows are present — no
world in the sample lacks land, ocean, a shoreline, or a plate roster. That
is itself worth noting: unlike the settlement- and pantheon-facing
censuses, which lose a handful of seeds to zero-habitable-land or
zero-settlement worlds (Studies 002, 003, 006, 007, and 008 all name the
same three or so seeds), these are pure-geometry metrics computed directly
over the derived globe, and every one of the 10,000 tier-0 worlds this
generator draws has *some* land and *some* ocean to measure.

{{#include generated/census-of-coasts/census-of-coasts-default-shoreline-development.svg}}

**Shoreline development** is high and tightly one-sided: mean **6.06**,
median **6.00** (range 1.45–12.47), with 50.1% of worlds at `D ≥ 6` and a
further 33.5% at `[4, 6)` — 83.6% of worlds sit at four to twelve times a
compact circle's perimeter-to-area ratio. Read alongside continent count
below, this is not evidence *against* the near-convex diagnosis; it is the
aggregate signature *of* it. The metric sums shoreline over a world's
*entire* land surface against its *total* land area, and a typical world
fragments into a dozen-plus separate landmasses (median 14, next chart) —
many individually near-convex polygons combine into a high aggregate index
the same way scattering one compact disk into a dozen smaller ones inflates
total perimeter per unit area, without any single piece becoming less
convex. The instrument is measuring fragmentation compounding compactness,
not contradicting it.

{{#include generated/census-of-coasts/census-of-coasts-default-hypsometric-bimodality.svg}}

**Hypsometric bimodality** averages **3.63** (median 3.04, range
0.70–15.83), with the bulk of worlds — 86.2% — landing between 1 and 6.
This is the one metric where the current generator already matches Earth's
qualitative shape: elevation is built from two flat plateaus (a +400 m
continental base and a −4000 m oceanic base, `elevation.rs`) offset by
plate-boundary contributions, so land and ocean populations separate
cleanly by construction. The preregistered direction for this metric is
**retained**, not increased — Crust and Sculpting must not flatten this
separation while fixing the shelf and shoreline numbers below.

{{#include generated/census-of-coasts/census-of-coasts-default-shelf-fraction.svg}}

**Shelf fraction** averages **0.288** (median 0.274), with 42.5% of worlds
at `≥ 0.3` and a long tail down to near-zero. The band is populated mainly
by cells inside a plate boundary's decay zone — the graded slope from one
flat plateau to the other — rather than by a genuine, geologically modeled
continental margin; Crust's per-cell crust field and margin anatomy is the
replacement mechanism the target direction anticipates.

{{#include generated/census-of-coasts/census-of-coasts-default-continent-count.svg}}

**Continent count** averages **15.7** (median 14, range 1–71), with 57.1%
of worlds carrying 12 or more separate landmasses. A single first-order
Voronoi plate partition, with one continental flag per plate and only
locally decaying boundary relief, produces many small disconnected
fragments as readily as it produces a few large ones — quantity of
continents is not itself the shape defect this campaign targets, but it is
the context the shoreline-development number above must be read against.

{{#include generated/census-of-coasts/census-of-coasts-default-largest-continent-share.svg}}

**Largest-continent share** averages **0.737** (median 0.790), and 37.4%
of worlds put more than 90% of all land in one component — a single
dominant landmass is the modal outcome even though the *count* of
landmasses (above) is usually in the double digits: most worlds hold one
large supercontinent-scale blob plus a scatter of small islands, rather
than several comparably sized continents.

{{#include generated/census-of-coasts/census-of-coasts-default-plate-size-gini.svg}}

**Plate-size Gini** averages **0.2612** (median 0.2611), overwhelmingly
concentrated in `[0.2, 0.3)` — 64.7% of worlds — with 12.3% below (0.3% at
`[0, 0.1)`, 12.0% at `[0.1, 0.2)`) and 23.1% above (22.4% at `[0.3, 0.4)`,
0.7% at `[0.4, 0.5)`). This is a tight, low-inequality distribution: exactly what a
first-order Voronoi tessellation (plates as same-sized-on-average random
cells) predicts, and the most direct of the six numbers to Crust's
plate-size work — a heavy-tailed plate-size distribution (a few giants, a
long microplate tail) is the specific target Crust is charged with
producing.

## The preregistered target directions

Ratified before this census ran and recorded here, before any change to the
elevation field, so the later comparison is honest and not a target quietly
redrawn to fit whatever Crust and Sculpting produce. Four of the five are
drawn directly from the campaign spec §2 (two verbatim, two with the
metric's full name substituted); the fifth (bimodality) is this chapter's
own synthesis, spelled out rather than quoted, of the spec's observation
that the current generator "is expected to score bimodal but shelf-poor":

- **Shoreline development index up** — from the measured 6.06 mean / 6.00
  median baseline. (Read together with the fragmentation note above: a
  future measurement should show the *individual*-continent geometry
  driving the number up — finer, non-convex coastal detail — not merely a
  change in how many pieces a world's land breaks into.)
- **Shelf fraction up** — from the measured 0.288 mean / 0.274 median
  baseline, moving toward a genuinely modeled continental margin rather
  than a plate-boundary decay slope standing in for one.
- **Plate-size Gini up** — from the measured 0.2612 mean / 0.2611 median
  baseline, toward Earth's heavy-tailed few-giants-plus-microplates
  distribution and away from near-uniform Voronoi cells.
- **Hypsometric bimodality retained** (synthesis, not a spec quote) — from
  the measured 3.63 mean / 3.04 median baseline; this is the one metric the
  current generator already gets qualitatively right, and Crust/Sculpting
  must not trade it away while fixing the other four.
- **Largest-continent share more dispersed** — from the measured 0.737
  mean / 0.790 median baseline (37.4% of worlds at `≥ 0.9`), toward several
  comparably sized continents rather than one dominant supercontinent-scale
  blob plus scattered islands.

## The diagnosis this census quantifies

Every number above is a different face of the same root cause: a
first-order Voronoi partition with one continental flag per plate produces
**near-convex, single-scale continents** — the "guitar-pick" shapes the
continental-rendering brainstorm diagnosed as the generator's abstraction
made visible. A plate is a Voronoi cell; a continent is one or more whole
plates; the only relief inside a plate's interior is a boundary
contribution decaying with graph distance from the edge. There is no
crust field, no craton, no per-cell margin, and no relief at any scale
finer than "whole plate" — so shorelines are polygon edges, shelves are
narrow boundary-decay slivers, plate sizes cluster near a Voronoi tessellation's
uniform mean, and one plate (or a merged handful) dominates a world's land
budget as often as not. Crust exists to give a plate an interior; Sculpting
exists to give that interior relief at more than one scale. This census is
the number these two campaigns must move, measured honestly, before either
writes a line of code.
