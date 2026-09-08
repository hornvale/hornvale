# The Newel

A newel is the post a spiral stair turns about — the thing that makes a
vertical passage navigable. The campaign took its name from its first
report and kept it through a split, because what the six reports turned out
to share was not a mechanism but a posture: in every one, the simulation
knew something true and the client did not say it.

## Six reports, and three of them were not the bug reported

The reports arrived as plain complaints from play. Reproducing each one
against real output before writing a line of specification is what turned
half of them into different problems.

**`enter Doaba` already worked.** So did `enter banana`, and so did
`enter Nenotata` — a settlement on the far side of the planet. Out of
doors, `enter` never read its argument at all; the branch that handles it
contains no reference to the word the player typed. Every one of those
three lines answered *"A small room in Doaba, holding a doorway, a screen,
a bench and a stone ledge."* The report asked for a feature that existed.
What was owed was the refusal — and decision 0788 had already ruled on it
in another context: *the footer's own words must be typable*, and the
walk-band prose prints "You can enter the settlement of Doaba."

**The terrain report's written confession was a red herring.** A comment in
the map's drawing code reads *"wire's own per-facet colour is not used
here"*, which looks like an admission of exactly the reported defect. It
sits on the constant that colours the observer's own marker, and it is
correct: tinting your own mark with the ground's colour would say the mark
was ground. The real defect was one layer down, where the facet's
reflectance takes its biome and its lithology from a single nearest
vertex. Measured at the shipped zoom: **twelve thousand distinct facets
render as one colour**, because they share five vertices.

**And "I press the left arrow, sometimes I move southwest" was not a
malfunction at all** — see below.

## What the arrow key measures

The walk band is a lattice of quadrilateral facets on a cube inflated to a
sphere. A compass word names one of a facet's eight neighbours, chosen by a
one-to-one assignment that minimises the worst angular error. At seed 42's
flagship the neighbour named *west* bears **267.28 degrees** — the closest
the lattice offers, and 2.7 degrees short of due west.

The map is a north-up Mercator raster with the observer pinned at centre,
so the window scrolls beneath the mark. Forty `west` presses scroll it
`(-1, 0)` thirty-eight times and `(-1, +1)` twice: west and south together,
which on screen is a step to the south-west. Each press carries 0.047 of a
row southward; every twenty-first press, that debt comes due as a whole
row.

The first framing this campaign reached for was that the movement is an
approximation and the picture is the truth, and that the fix was therefore
to say so. The project owner rejected the framing and supplied a better
one:

> movement [is] a transition on a graph that usually but not always agrees
> with the compass directions that we use as shorthand to describe those
> movements

That inverts the question from *does west go west* to *does the key take me
to the box I was looking at* — which is the contract a player actually
holds, and which is measurable. Across 3,448 facets at the walk rung, a
left-press lands in the box to the left **87.3%** of the time on the four
equatorial faces and **14.9%** on the polar caps; 1.13% of all moves land
on the box the observer already occupies, so the map does not move at all.
One press in eight is not where you looked.

The raster is already trying to be the graph — its width is set from the
count of facet edges around a great circle, so the horizontal axis is one
tile to one facet by construction. The vertical axis cannot follow, because
a lattice row is not a line of constant latitude.

## A preregistered hypothesis that was never run

Tracing that led somewhere else. The Pavement, which laid this lattice,
had frozen a prediction before writing its code:

> **H1 — a held heading walks true.** From 200 distinct seed-42 start
> cells, walking `n` for 500 steps leaves the walker within 0.5 cell of the
> starting meridian at every step.

Re-run over 216 start cells with the same call `Session::go` makes, and
cross-checked against a real forty-step walk to six decimal places: on the
four equatorial faces, north and south are **exact** — 0.00 cross-track,
none of 144 failing. East and west fail **144 of 144**, drifting 0.1445
step-lengths per step, against both the parallel and the great circle.
That is 1.68 times the figure The Pavement quotes as the defect it
repaired, and about 78 km off course over 563 km walked.

H1 chose the one direction that cannot fail. On a cube face the
constant-`a` lines lie in planes containing the polar axis, so they cut the
sphere in meridians — for `n`, the meridian and the great circle are the
same line, and the lattice column is exactly it. Written on `e`, the
hypothesis would have failed on its first start cell. No probe file exists,
and neither the chronicle nor the retrospective of that campaign reports an
H1 result, though both report its H2 and H3.

Nothing about that discovery required this campaign to fix it. Under the
graph framing the drift stops being a defect — you are following a row of
the world's own grid, which curves against north the way a road does. The
unrun hypothesis is a finding in its own right, and it went to the campaign
that inherited the report.

## The split

Two of the six reports outgrew the campaign at its specification review and
became their own: **The Sett**, which will draw the walk rung as the
adjacency graph rather than a projection of it, and **The Stipple**, which
must decide how a 1.1 km map box may speak about a field sampled every
110 km. Both were measured here and specified here; neither was built here.

The raster question was settled by measurement rather than argument. Two
candidate drawings — the face lattice directly, or a neighbourhood carried
through the graph by a transported local frame — are **exactly north-up on
the four equatorial faces**, 0.000 degrees mean and maximum alike. They
part company at the seams: the transported frame crosses a face edge with
no repeated box, where lattice space blanks up to 621 of 1,431 boxes, 43%
of the screen. Lattice space's failure covers 0.950% of a face against the
graph's 0.00201% — 473 times more reachable, and a blank screen is a worse
failure than a repeated box. A cube has real curvature at its eight
corners, so some failure there is unavoidable in any flat picture; the
question was only which one, and how often.

## What shipped

`>` and `<` reach the band changes. The sim had four descents and four
ascents — into a structure, into a cave, into water, down a stair — and the
keys reached only the stair pair, which refuses everywhere but underground.
Two verbs now dispatch to whichever applies, refusing and naming the
choices where more than one does. A measurement taken before the refusing
arm was written found no seed-42 facet offering two, and reported the null
with its two different reasons separated: cave-and-water is structurally
impossible, while site-plus-cave is merely absent from two thousand
samples.

`enter` resolves its argument or refuses it, naming what is actually here.
Tab completion sees the names the chart already carries, behind the same
discovery gate that withholds a placed site's name from the map — because
completion is a name surface, and a completion that offers what the map
withholds is a way to read the unexplored.

Open water answers to the clock. The two wet arms of the map's colour
function called a routine that takes no illuminant, so ocean was
byte-identical from a sun 80 degrees up to one 60 degrees below the
horizon, across a window three-quarters ocean. They carry spectra now, and
take the same light as everything else.

And the map strip describes the tile under the cursor. It carried 188
characters, of which 25 were about that tile and 160 about the map itself —
at 40 columns and one column per 300 ms, the last clause arrived 44.4
seconds after you looked. The map's own facts are not deleted, because a
map may state its own resolution; they are recognised as facts about a
gesture you just made rather than about where the cursor is, so they appear
when they change and decay after one full pass of the strip.
