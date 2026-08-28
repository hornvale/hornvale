# The Quadrat

Three defects were reported against the game client. The map zoomed about a
point nobody had chosen. The prose pane, on entering the map, displayed
something that looked like a left-aligned hex chart. And the walking view was a
fixed forty columns of sparse triangular lattice where a wide grid was wanted.

All three are fixed. The third one turned out to require rebuilding how the
client draws maps at all, and everything worth recording here follows from that.

## The keystone: a rung is a depth

The old zoom ladder was `virtual_w = plate_width << zoom`, clamped at a constant
called `MAX_VIRTUAL_WIDTH` whose value — 385 — was related to nothing in the
world. Powers of two are a fact about integers, not about terrain.

Replace it with the only ladder the data actually has: **a zoom rung is a
refinement depth of the facet tree.** Band B, the wilderness-travel scale, is
mesh depth 12 — 1.87 km per facet side. Coarsening runs 11, 10, 9, 8, 7 down to
globe level 6, where a facet is about 120 km across. Seven rungs, six steps.

This is a decision the project had already ratified, applied one level over.
Decision 0077 holds that *zoom in the room mesh is path truncation, never an
aggregation*; that was ratified about **addressing**, and this campaign applies
the identical rule to the **view**. What makes it a keystone rather than a
refactor is that three separate properties stop being rules to enforce and
become facts about the construction:

1. **A tile can never be finer than its datum**, because a rung *is* a data
   resolution. Decision 0196 — a view may disclose its own resolution but may
   never invent detail below it — now holds structurally. The campaign's first
   spec draft called this a "pitch floor" and proposed to check it.
2. **The projection frame never re-pegs.** An allocentric Mercator frame is
   fixed once at load from the world's own rotation, so the shimmer a prior
   campaign measured when a frame re-zeroes mid-drag cannot occur here. The
   first draft's observer-centred frame would have had to defend against it.
3. **Seams and pentagons are unreachable.** Sampling runs latitude/longitude →
   facet with no lattice traversal, so the base-face seams and the twelve
   pentagon points are not touched by any code path in this campaign.

A fourth consequence arrived free: six zoom-out steps is exactly the bound the
sim's own `map out N` already enforced, so the client's ladder and the sim's
stopped being two ladders that disagreed.

## What was measured

**H1 — mesh alignment removes the search — is supported, and it is
rung-conditional.** The hypothesis, frozen before the code existed: if a rung is
a mesh depth then a tile *is* a facet, so terrain comes from that facet's three
corner vertices by direct addressing, and the spatial search from an arbitrary
point to a nearest vertex disappears.

The mechanism holds at every rung, and it is not a small effect. Drawing a
200×200 plate, the number of vertex candidates scanned falls from 1,960,000 to
**90** at band B, and to 88,986 — a factor of 22 — at the coarsest rung, where
the memo has almost no reuse to offer.

The *budget* is what varies with the rung, and the spec's own success line was
the defect: it said "200×200 under 50 ms uncached" and named no rung, so it was
first measured at the single most favourable one. A full sweep on a quiet
machine:

| rung | uncached 200×200 | memo misses / 40,000 tiles |
|---|---|---|
| 12 (band B) | 31.3 ms | 30 |
| 10 | 30.5 ms | 333 |
| 8 | 44.5 ms | 4,717 |
| 7 | 66.4 ms | 15,756 |
| 6 (globe) | 91.5 ms | 29,662 |

**The uncached boundary is left unresolved on purpose, and that is the honest
result rather than a gap in the work.** Four measurements of the same code path
read 65.3, 65.6, 70.3 and 91.5 ms — a 1.40× spread — with byte-identical memo
miss counts, which is to say the same computation over the same ground on a
machine in four different states. Rung 6 settles at about 65.5 ms on a genuinely
idle box; rung 7 has no clean replicate. Three attempts produced three different
answers about where the boundary sits, and this project's own working notes
record two prior campaigns anchoring on a committed cost figure and
extrapolating wrong from it. So the campaign records the boundary as
load-sensitive and unresolved rather than picking a fifth number, and separates
out what does **not** depend on the machine: the scan counts above, which are
deterministic, and the cached figure below, which is three orders of magnitude
clear of the question.

**The cache is where the bar is actually met.** At 200×200 on the coarsest rung
— the worst case in the table — a warm redraw is **0.056 ms, 893× under the
50 ms bar**; a full keystroke round trip is 0.119 ms, 419× under. A cold fill is
79 ms and is paid once per `(frame, rung)` pair. That the warm figure is real
and not an artefact of a harness re-reading one tile was established by area
scaling: 100×100, 200×200 and 300×300 measure in the ratio 1 : 3.53 : 7.87
against areas of 1 : 4 : 9, and a harness skipping the composition would have
been flat or step-shaped instead.

**H2 dissolved rather than passing, and the distinction is the point.** The
hypothesis was that a byte-for-byte pin between the client's chart renderer and
the sim's would survive the reprojection this campaign was going to perform. It
is green. It is green because there is no reprojection: the design that needed
one was measured false and never built (below), so nothing touched the pin. A
hypothesis whose subject has been removed is not a hypothesis confirmed, and
recording it as a pass would have banked a free green for work that was never
done.

**H3 — the cursor invariant — holds**, asserted as an exact containment across
all twelve ladder steps in both directions rather than at a tolerance. An
earlier draft of that assertion would have used a tolerance equal to half a tile
at the coarsest rung, which is blind to a full one-tile error.

## The reprojection that was not written

The spec's original architecture had the client's chart renderer reproject the
per-turn perception packet onto the square grid, with the sim's own ASCII
renderer moving identically so the byte pin between them stayed green.

The packet describes each perceived facet as a **relative polar offset** —
a bearing and a distance from the observer. The raster describes everything as
an **absolute Mercator tile**, reached by flooring a projected coordinate.
Converting one into the other means recovering each facet's absolute coordinate
first, and the chart renderer cannot: its parsed mirror of the wire document
keeps no observer at all, and the simulation offers it no inverse of the
bearing-and-distance construction. Reprojecting the offsets *without* that
recovery leans instead on where the observer sits within its own tile — a
sub-tile phase — and swept over 200 phases on the fixture's own observer that
gives, at best, 0 marks misplaced, at worst **24 of 31**, mean 11.5, and only 2
of the 200 phases exact. The mandated design produces precisely the defect the
task's own agreement test existed to catch, and that test could only have passed
by being weakened to "within one tile".

**The campaign first recorded that measurement as evidence the wire does not
carry the phase. It is not, and the correction belongs where the claim was
made.** The document names the observer's own centroid latitude and longitude,
and bearing and distance run centroid to centroid, so the spherical direct
problem recovers every facet's absolute position exactly — at eight significant
digits, centimetres against a tile 1.87 km across. The sweep measures the
shortcut. What is genuinely absent is narrower and less interesting: one crate's
mirror of the document, and a piece of arithmetic nobody has written.

The design that shipped instead was already half-present in the tree. Each wire
cell carries `room`, a packed facet identifier. The client crate that owns the
chart cannot use it — it depends on no simulation crate and so has no mesh — but
the binary crate can, and already did, in a function whose own doc records that
no new geometry was written for it. So the perception layer is drawn there, by
unpacking each cell's facet to a coordinate and projecting it through the *same*
projection the raster uses. The two pictures agree by construction rather than by
two independent computations arriving at the same answer.

Five things dissolved with it: the byte pin stayed green untouched, the sim's
"31 of 31 cells drawn" golden survived (any raster-agreeing reprojection would
have occluded about a third of the band), a third replica of the projection in
another client stayed consistent, the pane compositor needed no knowledge of the
rung, and two byte goldens never moved.

## Layers, and what they bought

The plate used to draw terrain and point sites in one pass. It now draws three
layers with three different cache keys: terrain on `(frame, rung, tile)`,
features on the discovery version, perception on the turn. The reason is
measured rather than aesthetic — a tile keyed on discovery is invalidated by
every discovery, the whole pyramid for one settlement, so no cache is viable
under a shared key.

The split had a second effect nobody designed for. Two registry rows had
recorded, with numbers, that the world map could not draw what it claimed to.
Both were re-measured against the new ladder at this campaign's close.

**The vertical axis no longer undersamples the mesh.** The old measurement:
sweeping the whole chart at the finest zoom reached only 70.5% of seed 42's
40,962 mesh vertices, with 330 of 874 cave vertices undrawable even then, and a
real plate at its coarsest rung reached 5.7%. The cause was `virtual_h =
virtual_w / GLYPH_ASPECT` — a *terminal* fact (a character is about twice as
tall as it is wide) governing a *projection* height, where clamped Mercator is
very nearly square. Height is now derived from the projection's own aspect. Swept
the same way: at rung 7 and every finer shipped rung the chart reaches **40,848
of 40,962 vertices (99.7%) and 874 of 874 cave vertices (100%)**. Every one of
the 114 vertices it does not reach lies outside the ±85° latitude clamp — all
114 of 114 — which is to say the residual is the deliberate polar clamp the map
already tells the reader about, and not undersampling at all. Even the coarsest
rung, the old 5.7% case, now reaches 96.4%.

**A settlement glyph is reachable at every shipped rung.** The old measurement
found seed 42's flagship settlement undrawable at every rung the game shipped,
appearing only past a chart width more than three times the old ladder's
ceiling. That was a consequence of drawing sites by *sampling* — a settlement
appeared only if an area-majority happened to land on its vertex. The feature
layer projects instead: each site's own committed coordinate is resolved once to
its nearest terrain vertex, and that vertex's coordinate is what the layer
draws. All **389** of seed
42's settlements project inside the chart at all seven shipped rungs, onto 386
distinct tiles at rung 7 and finer (three pairs share a tile) and 377 at the
globe rung. The row is dissolved by construction rather than by resolution: the
constant it cited, `MAX_VIRTUAL_WIDTH`, no longer exists.

## The three defects, briefly

**The zoom.** The old code changed the rung and then merely *clamped* the window
origin, so the same origin column named a different longitude afterwards and the
view lurched by half the visible span; the cursor was never consulted. The rule
now is that the geographic point under the cursor does not move across a zoom
step, and the implementation moves whichever of the origin or the cursor it
must — at the coarsest rung the whole planet fits the plate and the origin is
pinned, so there the cursor moves. Two limits are declared rather than hidden:
the invariant is exact on longitude, which wraps, and holds on latitude except
where the polar clamp binds. The rung is now named on screen, which was the
other half of "criteria I have not identified".

**The prose pane.** Typing bare `map` sent `map` to the sim *and* entered the
map focus, so the sim answered a mode change with a picture — and the pane's
wrapper split paragraphs on newlines and rejoined them on whitespace, collapsing
every run of spaces and left-flushing the chart. Both halves are fixed: a mode
gesture sends no verb, and a line that fits the pane is preserved verbatim while
one that exceeds it is clipped or scrolled, never re-flowed. The direction is the
rule — prose may grow downward, a picture may not grow at all — and an earlier
draft that said "wrap what does not fit" would have reproduced the defect at a
wider pane. `map out N` still returns the sim's picture, deliberately: it is the
diagnostic path that once caught a wrong client projection by putting the two
renderings side by side.

**The width.** The walking view claimed `max(40, w/2)` columns instead of a flat
40. That change is only *visible* because of everything above it — the old
one-glyph-per-facet render would have answered a wider pane with blank columns.

## What is left, honestly

The campaign found a latent bug in the kernel's windowed nearest-vertex scan,
incidentally, while cross-checking two lookup methods against each other. On the
shipped ladder the two agree over roughly 2.2 million samples with zero
mismatches. Below the ladder they disagree 37 times, and brute force over the
full vertex set says the mesh-addressed method is right 37 times out of 37 —
every disagreement at latitude exactly 0.0000. That contradicts the scan's own
documented claim of bit-identity with a full band scan "pinned by an all-levels
equality test"; that test evidently does not sample the equator. No shipped path
reaches it, so it is registered rather than fixed here.

There is no observer marker at coarse rungs. The reader now has a way home —
re-entering the map re-centres at any rung — without being able to see where
home is. That is a narrower gap than the perception layer's deliberate refusal
to draw off band B, and it is registered as its own row precisely because the
two are different things.

And the walking loop got 24× more expensive per redraw — 0.089 ms to 2.124 ms —
which is imperceptible in a terminal and is *not* the plate. It is
size-independent, and it is a full snapshot parse plus a purview call on every
keypress, including keypresses that are just typing.
