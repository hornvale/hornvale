# The Gazetteer

The Watershed named the landscape and shipped a null. This campaign is not
that campaign's repeat — it is what survives once the null's own boundary is
read correctly.

## The null that does not bind

The Watershed sought landscape names as a cure for settlement-name
collisions. It predicted below 15% and measured a floor of **44.8%** across
an eight-seed, 1837-settlement battery, before writing a line of code. The
reason is structural and does not change with more effort: a settlement's
landscape is *shared by construction* — being on the same river, the same
landmass, is what those concepts mean — so the discriminator's effective
cardinality is a tenth of its nominal cardinality. Seed 42's settlements
occupy five landmasses and thirteen rivers out of many named ones; no amount
of finer landscape naming widens that bottleneck.

That result closes one question and leaves another completely open. This
campaign's payoff was never collision rate — decision 0024 had already
ratified that uniqueness is a reference-time property, and no future work
fixes a collision rate by adding entropy. The payoff is a toponymic
*knowledge* layer: a name is a thing a character can learn, lack, or be told,
and an eventual map's fogged layer is names, with terrain the given. Reading
the Watershed's number as "landscape naming is closed" is the cheapest
available inference and the wrong one; it is closed for that criterion only.

## Individuating what the terrain already computes

Every piece of the machinery this campaign needed already existed, thrown
away at the last step. The connected-components walk that measures a
landmass's size discarded its cell set and kept only a count. The flow forest
that gives every land cell a downhill pointer was never read as a tree of
rivers. `NameKind::Landform` already carried the doctrine — one identity,
many names — with a single caller, `volcano_name`.

Five classes fall out of graph traversal over already-committed fields, with
zero stream draws: **landmass** and **sea** are connected components of the
elevation field against sea level; **salt lake** is a connected component of
`WaterKind::SaltBasin`; **river** is a maximal subtree of the downhill flow
forest, keyed on its terminal cell; **volcano** was already individuated by
`volcano_at` and simply had no surface to name it from. Identity is drawn
from the terrain alone — the lowest cell id in a component, or a river's own
terminal cell — canonical, integer, and requiring no tie-break. It is also
not stable under anything that moves a coastline: a sea-level change can
renumber every landmass. That fragility is safe only because a name is never
committed. A landscape name is a pure function of `(seed, identity, species)`,
re-derived on read exactly as `volcano_name` always worked, so a terrain
change that renumbers a feature can never leave a saved world carrying a name
its own terrain contradicts.

Measured on seed 42: **10 landmasses, 1 sea, 80 salt lakes, 106 rivers, 208
volcanoes** — 405 individuated, named features, none of them existing in any
committed artifact a week ago.

## A floor is a claim about what kind of predicate it thresholds

The Watershed's inherited size floors did not survive contact with the
current tree. Landmass moved to a proportional rule already shipped and
censused elsewhere in the repo — 0.5% of total land cells, Earth-calibrated
so Greenland (~1.4%) qualifies and Iceland (~0.07%) does not — rather than
the fixed 20-cell floor it inherited, because a fixed count has no relation
to a world's own land fraction. The same rule, applied to ocean cells, gives
sea exactly one feature: this world's ocean is one connected body over 72%
of its surface, and the other eight ocean components are 1-3-cell
below-sea-level pockets that are not seas by any honest reading.

The salt-lake floor was the sharper case. The spec's inherited floor —
the same 20 cells landmass used — yields **zero** salt lakes: the largest
salt-basin component on this world is two cells. That is not evidence the
class is empty; it is evidence the floor was borrowed from the wrong kind of
predicate. Landmass and sea are thresholds on a *continuous field* —
elevation against sea level — where a one-cell component really is a
quantization artifact, a rock breaking the surface by construction noise.
`WaterKind::SaltBasin` is a *classification* — a terminal endorheic sink,
already decided by the water model rather than thresholded from it — so a
one-cell salt basin is a real ~112 km salt pan at this resolution, Great Salt
Lake or Etosha scale, not an artifact of where a line was drawn. Salt lakes
ship at floor 1, yielding 80, because the predicate that decides membership
is a different kind of thing than the predicate that decides a landmass, and
one floor cannot serve both honestly.

## An ocean does not fragment the way a river's catchments do

Individuating rivers by flow-forest subtree suggested an obvious dual: could
the single connected ocean be partitioned the same way, downhill to an
abyssal minimum, into something resembling separate seas? Measured directly:
**1,081 basins**, the largest 1.14% of ocean area, no scale break anywhere in
the distribution, and a floor of 500 cells yields zero. The physical story —
that mid-ocean ridges separate real basins — was plausible and did not
survive measurement. At ~112 km cells the seafloor's local minima are
everywhere; an abyssal catchment fragments exactly the way any noisy height
field fragments, which is bathymetric noise, not ridge-driven structure.
Sea ships as the single connected component it already is. A cartographically
honest partition into named seas needs ridge-aware merging, a clustering
pass outside this campaign's scope — flagged for whoever attempts a map with
more texture than "the ocean."

## One landform, many names — and the measurement that was a tautology

Landscape names draw through the existing `language/<species>/name/landform`
leg, keyed on feature identity and species, with no new stream label and no
epoch: decision 0083 already classifies this as the same algorithm on a
different subject, and nothing about the draw path moves.

The campaign's second preregistered hypothesis (H2) predicted that two
peoples with different phonologies would name a shared feature differently.
Measured at world scale, across 105 people-pairs and 405 features: 42,525 of
42,525 pairs diverge — **1.0000**. That number is a tautology, not a
finding. `Namer::name` derives `seed -> ROOT -> species -> NAME -> kind ->
salt`, and species is a leg in that derive path: two distinct peoples draw
from entirely different streams for the same feature and their names differ
by construction, independent of phonology or salt. The measurement's floor
is structurally 1.0; it cannot return anything else short of two peoples
being one people, and reports only that a hash produces different outputs
for different inputs. What actually carries H2 is a narrower unit control —
one phonology held fixed so species is the sole variable — which is
mutation-shaped and would fail if `feature_name` ever dropped the species
leg. The control carries the claim; the world-scale figure is retained only
as a sanity check on the derive chain, correctly attributed.

## The finding nobody specified

Interrogating why the world-scale number sat at an exact 100% surfaced the
question the hypothesis should have asked in the first place: not whether
peoples diverge from each other, but whether a single people's names collide
with themselves. Measured over 405 features x 15 peoples, 6,075 draws:

```
pooled distinct 5519/6075   collision rate 0.0915
min  0.0000  gnoll, high-elf, wood-elf   (fully distinct)
max  0.6247  kobold — only 152 of 405 features carry a distinct name
```

Three peoples name every feature distinctly. One collides on nearly two
thirds of them: the kobold name `Rara` covers fifteen different places —
seven volcanoes, a salt lake, and seven rivers. Decision 0024 already ruled
that committed names may collide and that no future work should chase
uniqueness by spending entropy, so this is not treated as a defect and
nothing was tuned to close it. It is reported because the variance, not the
mean, is the honest description of what a gazetteer feels like to its
readers: a kobold's landscape reads as generically named in a way a
gnoll's does not, and that difference is a fact about this world's phonology
draw rather than a flaw in the naming mechanism.

## A five-year-old guard, satisfied on its own terms

`no_rendered_artifact_names_a_geohazard` — The Repose's guard against a
committed artifact naming a hazard concept — refuses any rendered page that
mentions `"volcano"`. This campaign's gallery artifact needs to. The guard's
own failure message anticipated exactly this moment: *"If a later campaign
deliberately surfaces one, move the word out of `GEOHAZARD_VOCABULARY` in
the same commit and say so in the chronicle."* `"volcano"` was removed from
`GEOHAZARD_VOCABULARY` in the commit that shipped the gallery artifact, and
this paragraph is that chronicle line. The change was checked against the
guard's actual concern rather than merely its letter: the artifact renders
cell, magnitude and per-culture names only — no eruption style, no
knownness stock, no walk-scale perception — so the property The Repose
protects (hazard *consequences* reaching a committed artifact undisclosed)
remains untouched. Only the word collided with the vocabulary; the guard's
substance holds.

## What the world can say now

`explain` and the almanac name places without any map existing to draw
them on. A fifteen-people world puts fifteen names on a well-attested
feature, so the artifacts show, per class, features ordered by magnitude —
the total, deterministic, integer-only ordering that is also the placement
channel a later map's label gate will read — capped at a stated number per
class, with the cap printed rather than silently truncating. The committed
gallery, `book/src/gallery/gazetteer-seed-42.md`, is the first artifact in
this repository where a river or a landmass has a name at all.
