# The Sett — design

**Spun out of The Newel at its G3 stop (Nathan, 2026-09-06).** A sett is a
single paving block; The Pavement laid the walk band's lattice, and this
campaign fixes the one thing it left wrong.

The full reproduction evidence, the falsified hypothesis, the reframing and
the raster comparison are in **The Newel's ledger**,
`docs/superpowers/ledgers/2026-09-06-the-newel.md`, entries R9, R10, R13,
R14 and decisions #1, #7, #8. Nothing in that ledger is restated here; this
document is the design that follows from it.

## 1. The report

> **Bug Report (Nathan):** I press the left arrow, sometimes I move
> southwest. What?

## 2. What it is, stated in the frame Nathan supplied

> movement [is] a transition on a graph that usually but not always agrees
> with the compass directions that we use as shorthand to describe those
> movements

The picture and the movement are two different tessellations. Movement is
an edge of the facet adjacency graph. The walk-band picture is a clamped
Mercator raster. Measured over 3,448 facets at walk rung 13:

| | equatorial faces | polar caps |
|---|---|---|
| a left-press lands in the box to the left | **87.3%** | **14.9%** |
| lands up-left / down-left | 6.60% / 6.08% | — |
| any move landing on the observer's own box | 1.13% | — |

**One left-press in eight does not go to the box on the left**, and at seed
42's flagship `S` lands on the observer's own box, so pressing down moves
the map not at all.

The raster is already trying to be the graph — `virtual_dims` takes its
width from `tiles_around_a_great_circle(depth)`, so the horizontal axis is
1 tile to 1 facet by construction. The vertical axis cannot follow, because
a lattice row is not a line of constant latitude.

## 3. The change

**At the walk rung, draw the graph.** One facet, one character box, placed
by carrying a local frame through the adjacency graph outward from the
observer. Then the box to the left IS the west neighbour, by construction.
Mercator keeps the coarser rungs, where nobody is walking.

Compass words become labels on edges. `look` may name a bearing; movement
stops promising one.

**The raster is the graph-neighbourhood one (option 2), decided on
measurement** (ledger R14). Against the lattice-space alternative:

- both are **exactly north-up on the four equatorial faces** — 0.000
  degrees mean and max;
- on the polar caps the graph raster rotates 23.3 degrees mean against
  lattice space's 90.0;
- at a face **seam** the graph raster repeats nothing, where lattice space
  blanks up to **621 of 1,431 boxes (43% of the screen)**;
- lattice space's failure covers **0.950%** of a face against the graph
  raster's **0.00201%** — 473x more reachable, and a blank screen is a
  worse failure than a repeated box.

The graph raster's own failure is at the eight cube corners, where the
surface has real curvature and **some** failure is unavoidable in any flat
picture: at most 170 of 1,431 boxes repeat.

## 4. What this campaign must decide, and does not inherit

1. **Cost.** Not measured. Transport is ~2 `neighbors()` calls per box for
   1,431 boxes, and a step shifts the window so only ~27 boxes are new —
   which argues it is CHEAPER than resampling every box from lat/lon each
   frame. That is an argument, not a measurement. Measure it against The
   Legend's warm-redraw baseline before relying on it.
2. **What the corner looks like.** A repeated box is the measured
   behaviour; whether it should instead be drawn as a visible seam, a
   blank, or something else is a presentation call nobody has made.
3. **What happens to the compass words.** They stay, as labels. Whether
   `look` names the bearing, and whether the endpaper carries it, is open —
   The Newel proposed both as disclosure and then discarded disclosure as a
   *substitute* for the fix, not as a complement.
4. **Whether `chart::draw` returns or goes.** It is unreachable in the
   shipped client (ledger R8) and it WAS the graph view, placed in bearing
   space — the same mistake in another coordinate system. This campaign
   either revives it in graph space or deletes it; leaving a broken
   unreachable third projection is the one outcome to refuse.

## 5. The separate finding, which survives whatever this campaign builds

**The Pavement's H1 is falsified and appears never to have been run.**

H1, verbatim (`docs/superpowers/specs/2026-08-30-the-pavement-design.md`
section 7): *"From 200 distinct seed-42 start cells, walking `n` for 500
steps leaves the walker within 0.5 cell of the starting meridian at every
step. This is the direct repair of The Rhumb's falsified H1 (unbounded
drift, ~0.086 step-lengths per step)."*

Re-run over 216 start cells (ledger R9), cross-track in step-lengths, on
the equatorial faces: **N and S are exact — 0.00, 0 of 144 failing. E and W
fail 144 of 144 at 0.1445 step-lengths per step**, against both the
parallel and the great circle. That is 1.68x the figure The Pavement quotes
as the defect it repairs, and ~78 km off course over 563 km walked.

H1 chose the one direction that cannot fail: on a cube face the constant-`a`
lines cut the sphere in meridians, so for `n` the meridian and the great
circle are the same line. And the probe file the plan's Step 1 names does
not exist; no H1 result is reported in the chronicle or the retrospective,
though H2 and H3 both are.

**Under this campaign's framing the drift stops being a defect to repair**
— you are following a row of the world's own grid, which curves against
north the way a road does, and the picture will now say so. **The unrun
hypothesis does not stop being a finding.** It wants a decision record of
its own, and its subject is process as much as movement: a preregistration
whose chosen axis could not fail, shipped as a repair.

## 6. Non-goals

- Cross-track correction / restoring `course.rs`. Still correct, still
  costs carried state and a decision superseding The Pavement section 3.4,
  and it repairs a promise the game need not make once the picture draws
  the graph.
- The coarse rungs' Mercator raster, which is a geographic map and should
  stay one.
- Everything in The Newel's remaining scope (B1, B2, B4, B6) and in The
  Stipple's (B5).
