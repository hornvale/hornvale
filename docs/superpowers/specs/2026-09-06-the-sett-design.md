# The Sett — design

**Spun out of The Newel at its G3 stop (Nathan, 2026-09-06).** A sett is a
single paving block; The Pavement laid the walk band's lattice, and this
campaign fixes the one thing it left wrong.

The full reproduction evidence, the falsified hypothesis, the reframing and
the raster comparison are in **The Newel's ledger**,
`docs/superpowers/ledgers/2026-09-06-the-newel.md`, entries R9, R10, R13,
R14 and decisions #1, #7, #8. Nothing in that ledger is restated here; this
document is the design that follows from it.

## AMENDMENT 1 — the raster decision is overturned (The Sett, 2026-09-07)

**Sections 3 and 4 below are superseded by this amendment. They are kept
verbatim beneath it, unedited, because the reasoning they record is sound
on the evidence it had and the correction is only legible against it.**

The evidence is `docs/superpowers/ledgers/2026-09-06-the-sett.md`, entries
S1-S8 and decisions #1-#4. Nothing in that ledger is restated here beyond
the figures a reader needs to follow the argument.

### A0. What re-verification found

The Newel's three probes were never committed and no longer exist, so every
inherited figure was re-derived from scratch (ledger preamble). Three
results:

- **R13 and R14's arithmetic reproduce.** Self-collision 1.13% exactly;
  seam reachability 0.9501% and corner reachability 0.00201% exactly.
- **One figure is misattributed.** R14 takes its window as "53 x 27 boxes,
  the plate's size at an 80x24 terminal". The plate at 80x24 is **40 x 20**;
  53 x 27 is the plate at 93x31. No conclusion depends on it — every
  measurement below was taken at both — but the reachability percentages
  scale with the window, and at the enforced 80x24 floor they are 0.7312%
  and 0.00119%.
- **R14's decisive prediction P1 was cashed out at readout as a weaker
  property than the one it froze, and the property as written separates the
  options.** P1 defined agreement over `(facet, word)` pairs and was
  reported held on the strength of a duplicate-free, hole-free window. Run
  as written, option 2's word agreement is 100% on the four equatorial
  faces and **43.1%** on the polar caps. See A1.

That last one is the same defect this document's own section 5 reports of
The Pavement's H1 — a preregistered check settled on the axis that could
not fail. It is now the second instance, so the pattern is recorded as
decision 0907 rather than left in a ledger.

### A1. The change: draw the compass rose, not the lattice

**At the walk rung, box `(j, k)` is the facet reached by `k` steps along
the observer's own N/S `heading_rose` chain and then `j` along that facet's
E/W chain.** The picture is the movement rule iterated outward, so "the box
to the left" and "what the left key does" are the same object by
construction rather than by geometry. Mercator keeps the coarser rungs.

This is a third option The Newel did not consider, and the reason it did
not is exact: **on the four equatorial faces it is the same raster as
option 2** — the lattice frame and the compass rose agree there at
18,432/18,432 words over 2,304 facets, 100.0% — so every equatorial
measurement R14 took returns the identical number for both. They part
company only on the polar caps, which is a third of the world.

Measured over 288 facets at a 41 x 21 plate, both step axes:

| | option 2 (lattice) | option 3 (rose chain) |
|---|---|---|
| arrow keys land in their box, equatorial | 100.0% | 100.0% |
| arrow keys land in their box, **polar caps** | **66.7%** | **100.0%** |
| typed diagonals, polar caps | 66.7% | 66.7% |
| picture unchanged under one step, polar caps | **66.7%** | **100.0%** |
| up-error, polar caps (mean / max) | 28.5 / 58.9 deg | 8.8 / 24.4 deg |
| duplicate or blank boxes at 41 x 21 | 0 / 0 | 0 / 0 |

And the picture's own coherence — for every drawn box, is the box to its
right the one the right arrow reaches *from there*:

| | row | column |
|---|---|---|
| rose chain, equatorial | 100.00% | 100.00% |
| lattice, equatorial | 100.00% | 100.00% |
| rose chain, **polar caps** | **95.95%** | **76.55%** |
| lattice, **polar caps** | **1.49%** | **1.40%** |

On the polar caps a lattice-space picture is one no key follows: from any
box that is not the observer's own, the right arrow reaches the box drawn
to its right 1.49% of the time. That is the reported bug, moved off the
observer's box and onto every other box on the screen.

**Option 3 changes nothing sim-side.** `heading_rose` is untouched,
`ROSE_WORST_DEG` is untouched, no movement verb is added, no decision is
superseded, and no compass word changes meaning. It also satisfies decision
0117 more directly than option 2 would: the client *consults* the sim's own
movement decision rather than deriving a second frame of its own.

Option 2's remaining advantage is confined to the eight cube corners — it
shows 100 repeated boxes there against option 3's 39 blanks and 243
repeats — and that region is **0.0012% of a face**, decaying to a clean
picture 20 steps out. This is the same trade R14 itself made in preferring
option 2 to option 1, at a larger ratio.

### A2. What is actually built: the raster AND its inverse

Every overlay the walk band draws — point sites, rivers, the perception
layer, the observer's mark — is placed today by `mercator::project`. Under
a graph raster each becomes a lookup in a `FacetId -> (col, row)` map built
in the same pass as the raster. **The inherited design names neither the
map nor the overlays**, and an overlay left projecting through Mercator
onto a graph raster lands in the wrong box silently.

The change decomposes into four independently shippable parts, only the
first of which section 3 describes: the addressing; the inverse map; the
centring (`Window::origin_row`/`origin_col` stop meaning anything at the
walk rung, because the observer is central by construction); and the cache
(the chart-tile key cannot survive, and is replaced by a facet-keyed memo).

### A3. Cost: measured, and the inherited argument falsified in both halves

Section 4 item 1 argues the graph raster is *"very likely CHEAPER than the
status quo … where the Mercator raster resamples every box from lat/lon
each frame."* Measured, addressing 861 boxes at the walk rung:

| | per redraw |
|---|---|
| mercator (today) | 0.2183 ms |
| lattice transport (option 2) | 0.9066 ms |
| rose chain, no memo (option 3) | 114.1571 ms |
| rose chain, memoised (option 3) | 11.3099 ms cold, **1.0376 ms warm** |

Both graph rasters cost more, not less. **And the premise is wrong as well
as the conclusion:** the Mercator raster does not resample every box each
frame. `clients/game/bin/src/tiles.rs`'s `TileCache` serves the walk band,
keyed on `(pole bits, depth, tile row, tile col, light)` over 32-box-square
tiles, so a warm redraw resamples nothing. Today's whole walk-band redraw
is **0.3801 ms per keypress** at 104x56.

So the graph raster costs roughly **1 ms more per keypress**. That is small
in absolute terms on a client whose keypress already runs a sim turn, and
it is a regression, named as one rather than absorbed. The headroom is
known and not spent in this campaign unless the whole-redraw measurement
asks for it: because the rose-chain picture after a step is exactly the old
picture shifted (100.00% of the overlap unchanged, both bands, both step
axes), only the newly exposed 21 or 41 boxes need building. That is section
4's own "only ~27 boxes are new" argument, which turns out to be true of
option 3 and false of option 2.

### A4. The four inherited open questions, closed

1. **Cost** — measured (A3). Falsified, reported, accepted.
2. **What the corner looks like** — a blank, never a seeded draw and never a
   new glyph. R12's ruling on The Stipple's biome boundary governs the same
   shape: a seeded draw is honest per value and dishonest per pattern, and
   a diced corner would manufacture terrain. This **overturns** section 4's
   stated preference for a repeated box over a blank one: under option 3 the
   corner is one refused bearing at one facet, which is a local honest mark
   at the one place the surface actually folds, and the sim already has the
   sentence for it (`CORNER_BEARING_REFUSAL`: "The land folds away to
   nothing that way").
3. **The compass words** — they stay exactly as they are, and `look` does
   not name a bearing. This is the strongest single argument for option 3:
   it is the only candidate that closes the report while changing nothing
   the player types. Disclosure was discarded by The Newel as a substitute
   for the fix and is not needed as a complement either, because the picture
   now says it.
4. **`chart::draw`** — deleted, with its bearing-space projection. Under
   option 3 the plate is the graph view drawn correctly, so reviving
   `chart.rs` would be a second implementation of it. Its unreachability is
   re-confirmed independently: one non-test call site
   (`clients/game/core/src/spread.rs`, the `Spatial::Walk` arm), in a branch
   that cannot be taken on the walk band because `world_plate` is `Some`
   exactly when the band is `Walk`.

### A5. What this amendment does not change

Section 5 (The Pavement's unrun H1) stands unaltered and still wants its
own decision record. Section 6's non-goals stand, and A1 strengthens the
first of them: cross-track correction repairs a promise the game need not
make, and option 3 makes that concrete rather than arguable, because the
picture now draws the row of the world's own grid that a held heading
follows.

---

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

## 3. The change — SUPERSEDED BY AMENDMENT 1 (kept verbatim)

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

## 4. What this campaign must decide, and does not inherit — SUPERSEDED BY AMENDMENT 1 (kept verbatim; all four are closed in A4)

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
