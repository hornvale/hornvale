# The Sett

A sett is a single paving block. The Pavement laid the walk band's lattice;
this campaign fixed the one thing it left wrong — which was not the lattice
but the picture drawn of it.

The report was one sentence of ordinary play: *"I press the left arrow,
sometimes I move southwest."* The Newel reproduced it, established that no
component was malfunctioning, and split it off. What follows is what the
sentence turned out to be about.

## Two tessellations of one surface

The walk band is a lattice of quadrilateral facets on a cube inflated to a
sphere. Two entirely separate things were being computed about that surface,
and both were correct.

**Movement** is an edge of a graph. A compass word names one of a facet's
eight neighbours by a one-to-one assignment that minimises the worst angular
error, so *west* means "the neighbour whose bearing is closest to 270
degrees" — never exactly 270, and the assignment is a property of the local
neighbourhood, not of the world.

**The picture** was a projection. The plate was a clamped Mercator raster:
each character box was a rectangle of latitude and longitude, filled with
whatever facet contained its centre, and the observer's window panned over
that chart as they walked.

Neither is wrong. But a projection tessellates the sphere by *coordinates*
and the movement graph tessellates it by *adjacency*, and those are two
different tilings that only coincide where the coordinate grid happens to
run along the graph's own edges. Where they part, the box you looked at and
the box the key takes you to are simply different boxes, and no amount of
correctness in either half repairs it.

Measured over 3,456 facets at the walk rung, every neighbour resolved
through the shipped compass and projected through the shipped Mercator:

```text
                                equatorial faces   polar caps
  W lands in the box to the left      83.0%          17.7%
  W lands one box up-left              8.9%
  W lands one box down-left            8.2%
  a neighbour landing on the
  observer's OWN box, equatorial       1.13%
```

One left-press in six did not go to the box on the left. One move in
eighty-eight landed on the box the observer already occupied — press a key,
and the map does not move at all. That last figure is the sharpest form of
the problem: it is not an error of a fraction of a box, it is a step that
the picture cannot represent as a step.

## The third drawing

The predecessor campaign measured two candidate rasters and chose between
them. Both drew the neighbourhood in *lattice* space — one directly on the
cube face's own grid, one by carrying a local frame through the graph — and
the choice between them turned on how each behaved at a face seam.

Re-measuring that campaign's own frozen prediction, as written rather than
as reported, separated them where the readout had not. It also exposed a
third candidate that neither measurement could see.

**Draw the compass rose.** Box `(j, k)` is the facet reached by taking `k`
steps along the observer's own north/south compass chain, and then `j` steps
along that facet's east/west chain. The picture is the movement rule
iterated outward. "The box to the left" is "what the left key does" by
construction, not by geometry, because it is built by asking the movement
rule and nothing else.

The reason nobody had considered it is worth stating plainly: **on the four
equatorial faces of the cube it is the same raster as the lattice one.**
Measured, they agree at 18,432 of 18,432 words. Every equatorial measurement
either campaign took returns an identical number for both candidates. The
option was invisible to the instrument that chose against it, because it is
indistinguishable from the loser on two thirds of the world.

They part on the polar caps, which are the other third:

```text
                                        lattice     rose chain
  arrow lands in its box, equatorial      100.0%       100.0%
  arrow lands in its box, POLAR CAPS       66.7%       100.0%
  picture unchanged under one step, caps   66.7%       100.0%
  up-error, caps (mean / max, degrees)  28.5/58.9     8.8/24.4
```

And a second question, which is about the picture's coherence rather than
the observer's own box: standing on a box that is *not* the mark, does the
right arrow reach the box drawn to its right?

```text
  rose chain, polar caps    95.95% row   76.55% column
  lattice,    polar caps     1.49% row    1.40% column
```

On the caps the lattice picture is one that no key follows anywhere except
at the mark itself — the reported bug, relocated from the observer's own box
to every other box on the screen.

The rose raster changes nothing in the simulation. The compass is untouched,
its measured error ceiling is untouched, no verb is added and no word
changes meaning. The client stops deriving a second frame of its own and
consults the one the world already has.

## What a fold costs

A cube inflated to a sphere has all of its curvature concentrated at eight
points. Gauss–Bonnet guarantees it: 720 degrees of angular defect has to go
somewhere, and on a cube it sits at the corners. No flat picture of that
surface is free of a defect; the only question is which defect, and how
often you meet it.

At a cube corner the compass refuses one bearing outright — the land folds
away to nothing that way — so a chain ends and the boxes past it cannot be
reached. They are drawn **blank**. Not repeated, not diced, not given a seam
glyph: a seeded draw at the one place the surface genuinely folds would
manufacture terrain the world does not have, and a blank is the local honest
mark for a local honest fold. The affected neighbourhood is ten facets wide
on an equatorial face and twenty-eight on a polar one, out of four hundred
million in the band.

At each pole the failure has the opposite shape. The meridian chains
converge, so the picture *repeats* rather than blanking — at the pole itself
456 boxes of 861 show ground already shown, with no blanks at all — and it
clears the moment the pole leaves the plate, sixteen facets out, about a
sixth of a degree.

That is worth comparing against what it replaces rather than against
perfection. Mercator refuses to project past 85 degrees, and the walk view's
centring propagated the refusal: today an observer within **five degrees** of
a pole cannot have the view centred on them at all, and the plate keeps
drawing wherever the window last was. Against the rose fold's 0.176 degrees
that is twenty-eight times the radius and some eight hundred times the area
— and it is a refusal rather than a degradation. Both pictures fail at the
pole. One of them fails over a region eight hundred times smaller, and fails
by showing the same ground twice instead of showing the wrong place
entirely.

## The mark is central by construction

Under a projection the walk view was centred by arithmetic: each turn
re-computed where the observer projected to and moved the window's origin so
that box landed in the middle. Under the rose raster the observer is the
anchor the chains are grown from, so the centre box holds them because the
construction says so. The window's origin stops meaning anything at the walk
rung, and the per-turn correction disappears rather than being made more
careful.

One consequence needed a rule of its own. A facet drawn more than once — at
a corner, or at a pole — has more than one box, so the inverse question
("which box holds this facet?") has more than one answer. Answering with the
first one found in scan order is not a rule at all; it is whatever order the
scan happened to use, and at the exact pole it answers *top of the centre
column* for the one facet everything else agrees is the middle. The inverse
map answers with the box **nearest the plate's centre**, which makes the
observer's own mark central by the same construction that centres the
picture, and for any other repeated facet names the instance they are
actually closest to.

## The map is not the walk

Mercator was not deleted. The map band keeps it at every zoom rung,
including the finest, and the choice of which raster to draw is made by
which view is open rather than by which rung is displayed.

That is a statement about what the two instruments are for. A map is a
geographic object with a cursor, a pan and a zoom, all of which are
coordinate gestures; a graph raster has no coordinates to pan over. A map
that cannot draw the pole is a map telling the truth about a projection. The
generalisation the design reached is the short one: **Mercator is the map;
the graph is the walk.**

## The cost, which was not where anyone looked

The campaign froze a cost prediction before writing the code: between one
and two milliseconds per keypress, against a measured baseline of 0.380 ms.

It measured **9.71 milliseconds** — five times the top of the predicted band
and twenty-five times the baseline. The prediction is falsified and stays
falsified.

What the falsification bought was worth more than the prediction would have
been. The band above five milliseconds had a response frozen with it: stop,
and find out what is being paid per frame before optimising anything. Two
within-binary controls — drawing the same world in the map view, and
switching the raster choice off — both came back at 0.415 ms, agreeing with
each other to one percent and with the frozen baseline to nine, which
established that nothing else had drifted and the whole delta was the
raster.

Then ablation found the cost, and it was not the raster. Building the rose
picture is 6% of the redraw. **65% of it was a single line** — a height
blend, added by an earlier campaign, resolving the nearest mesh vertices for
every character box on every frame without consulting the memo sitting
directly above it in the same function.

**The rose raster did not introduce that cost. Removing a cache is what made
it visible.** The projected path pays exactly the same call; a tile cache had
been amortising it to once per tile and then hiding it forever. The
licensed remedy for the band the measurement landed in — an incremental
fill, rebuilding only the newly exposed row or column — would have attacked
the raster build, which is to say six percent of the problem, and it would
have looked like it worked.

Routing that one line through the memo already in scope took the redraw from
9.71 ms to **3.98 ms**, a factor of 2.44, with no answer anywhere in either
client changing — the memo is a cache of a pure function, byte-identical to
recomputing by construction. The line itself fell from 6.82 ms to 0.82 ms,
a factor of 8.3.

Two committed assertions went red under that change, and neither was an
answer. Both counted mesh searches, and one of them had been quietly wrong
since the height blend was written: it asserted that a plate's search count
is bounded by the mesh rather than by the screen, and it was true of the
searches it could *see*. The blend's call bypassed the memo, and therefore
bypassed the counter the test read. The plate was running eight thousand two
hundred searches while the test observed eight. **The expensive path had not
stopped happening; it had stopped being counted**, and a test measuring work
through an instrument the work no longer passes through reports zero and
reads like a strong result.

## What the walk band draws now

The box to the left of the mark is the facet the left arrow takes you to, at
every facet on every face of the cube, including the caps. The picture
shifts by exactly one row or column when you step, so what you were looking
at is still what you are looking at. An observer at the pole is drawn at the
centre of their own view for the first time. And where the surface itself
folds — eight points on a cube, and the two poles — the picture says so, by
going blank or by repeating, rather than by inventing ground or by declining
to draw at all.
