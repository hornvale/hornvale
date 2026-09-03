# The Hachure

Hachures are the short strokes an old surveyor drew down a slope: no numbers,
no contour, just marks that get denser where the ground falls away faster. They
are what a cartographer reaches for when the terrain is real and the notation
is not yet equal to it.

This campaign began with a picture. The game client's world map, opened at its
default view, drew the planet as a scatter of flat hundred-kilometre slabs —
one glyph repeated across a field, then an abrupt edge, then another field.
The complaint was aesthetic and the diagnosis looked obvious: the renderer must
be too coarse.

It was not. The renderer was drawing exactly what it had been ratified to draw,
and the campaign nearly filed that as a regression before a lint about
vocabulary stopped it.

## A view that could not be finer than its data

Two ratified decisions stood behind the slabs. [Decision
0196](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0196-a-map-is-a-fact-about-the-world-and-a-view-is-a-lens.md)
had settled that a map "may disclose its own resolution but may never invent
detail below it." [Decision
0287](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0287-a-zoom-rung-is-a-mesh-depth.md)
then made that clause *structural* rather than policed: a zoom rung **is** a
refinement depth of the facet tree, so a tile at rung `d` is a facet at depth
`d` and there is no rung at which a view could render finer than its datum,
because the ladder has no such rung on it.

The slabs were that guarantee, seen from the reader's chair.

What the campaign had actually found was a different thing wearing the same
appearance. The map was opening at the walk band's own rung — seven rungs below
the terrain mesh — where a whole screen falls inside one or two samples. The
data was not too coarse for the picture. The picture was pitched seven rungs
below the data, and every rung in between was one the ladder already had.

Moving the entry rung to where the mesh can fill the screen is the whole of
Stage 0, and it is a smaller change than the one the campaign first proposed,
which was to re-derive the chart's width from the terrain lattice. That
proposal was illegal. It would have broken 0287's identity of tile and facet
and forfeited the three properties 0287 buys by construction: no tile finer
than its datum, no re-pegging of the projection frame, and no lattice traversal
(so the base-face seams and the twelve pentagon points stay unreachable).

**Nothing in the design review caught that.** The commit gate did — and not
even the part of it that has opinions about design. A ratchet on the word
*cell* refused two probe files, and chasing the refusal led to the lexicon of
place, which cites 0287 in its second paragraph. A lint about vocabulary
surfaced a constitutional error in the design, because the campaign had read
the implementation and its comments and inferred intent from them. Comments
describe what a fix round did. They do not say what was ratified. The decision
log is the record of intent, and it had not been consulted.

## Reading between samples is not inventing

Stage 1 asks the map to read elevation *between* the mesh's samples rather than
snapping to the nearest one — a bilinear blend over the facet's own four
corners. That is a refinement below the datum's resolution, which is exactly
what 0196's sentence forbids, so it needed the sentence amended rather than
quietly stretched.

The amendment ([decision
0676](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0676-a-view-may-interpolate-between-its-samples-but-still-may-not-invent-below-them.md))
separates two operations the original had bundled. A view may **interpolate**:
a refined reading must lie inside the convex hull of the samples it is read
from, so it can never assert a value its own data do not bracket. A view may
not **invent**: a reading carrying information not derivable from those samples
needs its own ratification. The hull condition is what makes the first half
safe and checkable — interpolation never leaves the hull of its inputs, and
where the consumer bands the result, banding is monotone, so a blended tile's
band is bracketed by the least and greatest band of its own corners. That is
"coarse constrains fine," the Constitution's provider rule, said about a view.

The bound is not the one the campaign first asserted. Its first attempt quoted
decision 0121's phrase that a blend moves a value at most one band, and
measured a move of **two** at row 60, column 9. The phrase was never a
blend-versus-snap bound; at globe rung a tile *is* its facet, so the blend is
the plain mean of four corners, and a mean sits more than one band from the
nearest of them whenever the four span three bands. Over mountains, they do.
0121's ruling is untouched. What did not survive was a reading of it that it
had never made.

## The headline prediction was false

Stage 1 preregistered that blending would strictly increase the number of
distinct relief bands drawn on a plate at the rungs below the grid. Measured
over eight inland locations, seed 42, a 120×40 plate:

| | snapped | blended |
|---|---|---|
| distinct grid vertices | 1–4 | — |
| distinct relief bands | 1–2 | **1–2** |
| distinct heights | 1–4 | **612–3,860** |

The refinement is enormous and entirely invisible. `relief_band`'s rungs are
hundreds of metres wide, and within one ~110 km sample a real height ramp
almost never crosses one, so the quantizer discards everything the blend
recovered. The gain at the middle rungs is real and was measured — at rung 11 a
block edge becomes a gradient, `^` going from 461 to 1,212 — but at the rungs
where the reported defect lives, the prediction is false.

The floors were not retuned to rescue it. They are load-bearing for a shipped
wire field, whose own source says in terms: *do not retune the floors below to
make a picture look better.* What the null actually points at is that the
height field is present and rich and the **band** is what throws it away — a
colour ramp within a band, not a finer band ladder. That is filed, not built.

Four test drafts passed against unfixed code before one discriminated, and each
failure had the same cause: every draft asserted about the band, and the band
is a lossy quantization of the very thing being refined. Each test was
measuring the quantizer.

## A river is a line, and a line does not conserve area

Stage 2 draws rivers. The spec called for a per-tile query — is this tile's
footprint on a channel — and the built version produced river *scatter*.
Following a trunk downstream, the nearest line flips to a small tributary and
back, and the trunk breaks into dashes. The general statement is the useful
one: **connectivity is a property of the line, not of any point on it**, so no
per-tile query can guarantee it however finely the query is refined. Rivers are
rasterized from the polylines instead, which gives connectivity by
construction.

Choosing between candidate rules exposed something worse than a wrong rule — a
wrong *invariant*. Two rules were measured against the existing rung-6 raster:
one gave −96.1% river tiles, the other +1383%. Both numbers are alarming and
both are meaningless, because the raster area of a polyline is
resolution-dependent by construction. A rasterized line must cover `O(N)` of an
`N × N` chart, so the fraction of cells it occupies has to halve with every
doubling of resolution. Today's rule draws ~0.98% of tiles at *every* rung —
0.98, 0.99, 0.99, 0.96 — dead flat. That flatness is the signature of the
defect, so conserving against it would have preserved it. The rejected +1383%
rule gives 16.47, 3.94, 1.00, 0.14: halving per rung, as a line must.

[Decision
0677](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0677-a-line-carried-feature-conserves-its-length-not-its-rasterized-area.md)
records the rule that would have prevented both readings: a line-carried
feature conserves its length and its connectivity, never its rasterized area.

The rivers ride the terrain layer's own cache key, which is not a compromise
but the correct layering — the channel network is fixed at genesis and
selection is a pure function of the rung, so a river has exactly the terrain
layer's never-invalidated lifetime. Rasterizing costs river length in view;
sampling costs screen area and is flat at every rung. The reader zooming in
makes the first cheaper and the second no better.

A wrap bug survived both of Stage 2's purpose-built tests and was caught by an
unrelated invariant: the tile cache's byte-identity check, comparing a composed
plate against an uncached one. Both new tests drew a full-width plate at origin
zero, where the wrap never arises. They shared a blind spot, and the older test
did not have it.

## What was not built

The map now reads between its samples and draws its rivers as lines. Below the
mesh's ~110 km floor it still has nothing to say, and the answer to that —
a coherent noise field, read by both the map's height perturbation and the room
prose so that "you are standing in a hollow" and the dip on the map are *one*
hollow — is deferred to its own campaign.

That deferral is Nathan's, and the reason is not cost. The epoch it would
require turned out cheaper than the spec implied: `micro.wetness` reaches nine
committed fixtures and the room prose, but no census metric reads the micro
field at all, so no golden moves and no refresh is forced. Invention below the
datum is simply a larger question than a rendering fix, and 0676's second
sentence deliberately does not license it.

One ruling from the deferred design was recorded anyway. [Decision
0678](https://github.com/hornvale/hornvale/blob/main/docs/decisions/0678-one-noise-field-per-discrete-dimension-each-with-its-own-stream-label.md)
holds that each discrete dimension of a generated field gets its own noise
field with its own stream label — never one field feeding two axes. The failure
it prevents is not cosmetic and not local: shared fields would make every
hollow damp and every rise dry, everywhere, forever, which reads as a law of
physics nobody put in the sim. It is recorded ahead of its implementation
because it is a ruling rather than a campaign's choice, and because the
existing micro field violates its spirit today — four sequential draws off one
stream, independent but *ordered*, so a fifth axis would shift the existing
four.

## The lesson the lint taught

The campaign's own near-miss is the thing worth carrying. It read the code and
the code's comments, formed a confident account of why the map looked wrong,
and proposed a change that a ratified decision forbade. The account was
plausible at every step. The comments it rested on were accurate about what
they described. Nothing in the design was internally inconsistent.

Registry-first is already the documented habit: grep the idea registry before
proposing anything, because a `rejected` row is a closed question. This
campaign is the argument for its sibling. **Decisions-first** — grep
`docs/decisions/` for the mechanism you are about to call a mistake, before you
call it one. The near-miss cost nothing here only because a ratchet on a single
word happened to sit in the way.
