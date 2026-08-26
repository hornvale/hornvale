# The Quadrat — a square grid over a triangular world

**Branch:** `campaign/the-quadrat`, from `origin/main` @ `7576eca00` ·
**Decision block:** 0286–0295 · **Drafted:** 2026-08-26 ·
**Status:** G3 package pending.

*A quadrat is the square frame a field surveyor lays over irregular ground in
order to sample it. The frame is not a claim about the ground.*

---

## 0. What this is, and the correction that shaped it

Three defects were reported against the game client: the map zooms about a
point nobody chose, the prose pane displays a mangled ASCII chart on entering
the map, and the walk-band view is a fixed 40 columns of sparse hex lattice
where a wide grid is wanted. This campaign fixes all three, and the third one
turns out to require rebuilding how the client draws maps at all.

**The design is not the one this campaign started with, and the record of why
is load-bearing.** The first draft treated the walk band as an egocentric,
knowledge-limited view — fog of war, sensed-versus-remembered, an
observer-centred projection — and on that basis raised a fidelity/cost
carve-out about widening the per-turn perception packet from 31 cells to ~976.
That premise was wrong, and it was the drafting session's own invention rather
than anything the project had ratified. Nathan's correction:

> A is really the only scale at which the "Fog of War" really matters […] For
> B, C, D, and E, I don't think the "Fog of War" is remotely important. "Fog of
> Ignorance?" yes, absolutely, we shouldn't have all of the features of a given
> grid position available to us until we have navigated to them via A or B. But
> we don't need to worry about whether to render it or not.

A creature native to this world knows its own geology and physical geography.
What it does not know is *features* — cities, interesting places, place names.
That is exactly the model the world plate already implements ("terrain unfogged
at every rung, labels gated by discovery"), so bands B through E share **one**
epistemic model, and the objection to unifying them did not exist. The
carve-out is withdrawn: the client already holds `terrain`, `geo` and `nearest`
(`clients/game/bin/src/driver.rs:269-277`) and derives the world plate from them
with no wire traffic, so the 31-cell packet was never a limit on the terrain
layer at all.

**The transferable lesson, recorded here because §11 of every spec in this
project is where such things go to be forgotten:** the drafting session
invented an epistemic model, presented it as a constraint, and built a cost
question on top of it. Nothing mechanical caught that — the premise was
plausible, internally consistent, and cited real code. It was caught by the
owner restating what the game *is*.

## 1. The travel-scale model

| band | what it feels like | tile | shape today | epistemics | who moves |
|---|---|---|---|---|---|
| **A** | a house, a small grocery store | feet | square 4-neighbour ✓ | real fog of war | the character |
| **B** | driving on a highway | **1.87 km** (facet, depth 12) | **triangular ✗** | terrain known, features gated | the character |
| **C** | from an airplane | depth 11 → 9 | square raster ✓ | terrain known, features gated | a map cursor |
| **D** | a whole country | depth 8 → 7 | square raster ✓ | terrain known, features gated | a map cursor |
| **E** | the globe | depth 6 (globe level) | square raster ✓ | terrain known, features gated | a map cursor |

A and B are travelled at; C, D and E are consulted. That distinction already
exists in the client as `Focus::Walk` versus `Focus::Map` (decision 0160).

**Band A is already correct and this campaign does not touch it.** The chamber
band is a square 4-neighbour lattice (`CHAMBER_SIDE = 8`,
`windows/vessel/src/lattice/mod.rs:214`; orthogonal headings only, per The
Illumination spec §4.3) with its own sight radius. It is already the roguelike
view with real fog of war. **Band B is the only odd one out**, and every
structural change below exists to move it into line with C/D/E.

The scale figures are measured, not assumed. `walk_depth = globe_level + 6`
(`windows/vessel/src/agent.rs:16`) with `globe_level = 6`, so band B is mesh
depth 12: 335,544,320 facets over a globe of Earth's area, **1.87 km per side**,
adjacent facet centres **1.08 km** apart. `globe_level = 6` gives 40,962
vertices at **119.9 km** spacing, which agrees with the repo's own
`domains/terrain/src/features.rs:242` ("~110 km across […] on a globe of Earth's
area").

## 2. The keystone: a zoom rung is a mesh depth

The current plate's ladder is `virtual_w = plate_width << zoom`, clamped at
`MAX_VIRTUAL_WIDTH = 385` — powers of two, unrelated to the data's own
resolution. Replace it: **a rung is a refinement depth of the facet tree.**

This is decision 0077 already ratified — *"zoom in the room mesh is path
truncation, never an aggregation"* — applied to the view rather than to
addressing. Band B is depth 12; coarsening runs 11, 10, 9, 8, 7 to globe level
6 — seven rungs in all, and **six zoom-out steps**, which is exactly the bound
`Session::map` already enforces as `depth - globe_level`
(`windows/vessel/src/session.rs`, the `max_zoom` clamp). Stated as both numbers
because "six rungs" and "six steps" differ by one and this spec is the place
that discrepancy would otherwise be minted. The client's zoom ladder and the sim's `map out N` therefore stop being
two ladders that disagree and become one, which closes
`CLIENT-snapshot-chart-cannot-zoom` as a consequence rather than as a task.

Three properties follow by construction rather than by enforcement, and each
replaces a rule an earlier draft of this spec proposed to police:

1. **A tile can never be finer than its datum**, because a rung *is* a data
   resolution. Decision 0196's *"may disclose its own resolution but may never
   invent detail below it"* holds structurally. The earlier draft called this a
   "pitch floor" and proposed to check it.
2. **The frame never re-pegs.** An allocentric Mercator frame is fixed once at
   load from the world's own rotation (0196 clause 1) and does not move as the
   player does, so the shimmer hazard MAP-70 (The Excursion) recorded —
   *"collapsing the two re-zeros the frame mid-drag"* — cannot occur. The
   earlier draft's observer-centred frame would have had to defend against it.
3. **Seams and pentagons need no special case.** Sampling runs lat/lon → facet
   with no lattice traversal, so the base-face seam and the twelve pentagon
   points are not reached by any code path here.

## 3. The layer stack

The plate currently draws terrain and point sites in one pass. Split it into
three layers with distinct cache keys and distinct owners:

| layer | source | **drawn by** | key | fogging |
|---|---|---|---|---|
| **terrain** | client-side from `terrain`/`geo`; no wire traffic | `bin/plate.rs` | `(frame, rung, tile)` | none — a native knows the geography |
| **features / discovery** | the ledger's settlements and caves, gated on discovery | `bin/plate.rs` | discovery version | features hidden until visited |
| **perception** | the per-turn `scene/surrounds` packet (31 cells) | **`core/chart.rs`** | the turn | what the eye resolves now |

**The third column is load-bearing and was missing from the first draft of this
table.** Task 4 found the omission: the perception layer cannot be drawn by
`bin/plate.rs`, because the plate has no perception input at all — that packet
is rendered by `clients/game/core` into the *band* plate which the world plate
is drawn **instead of**. Adding a `draw_perception_layer` beside the other two
would have been a no-op with an untestable body. This is the same boundary
Ruling 2 fixed for Task 6: `core` carries no hornvale crate, so terrain lives in
`bin` and the wire packet lives in `core`, and the layers meet at
`spread::compose` rather than inside one function.

The split is not tidiness. `CLIENT-tiles-need-the-overlay-split` records the
measured reason: a tile keyed on the discovery version is invalidated by every
discovery — *the whole pyramid, for one settlement*. Terrain must depend only on
`(frame, rung)`, which change rarely, while discovery is a handful of sites
drawn on top.

**`PURVIEW_RADIUS` is not touched** (`windows/vessel/src/purview.rs:18`, value
4). The perception layer is a different job from the map and is correct at
radius 4; its `Body::perception` seam (EXP-3) stays reserved for a later
campaign.

## 4. The three reported defects

### 4.1 Zoom anchors on the cursor

`Driver::apply_zoom` (`clients/game/bin/src/driver.rs:1308`) changes
`window.zoom` and then only *clamps* the origin (`reclamp_window`, :1225).
Because the virtual chart's size changes under an unchanged origin, the same
`origin_col` names a different longitude after the step, so the view lurches by
half the visible span. The cursor is never consulted. Worse at the ladder's
ends: entering the world view resets to `Window { zoom: 0, origin_col: 0,
origin_row: 0 }` outright (:1314), which is an arbitrary point in the ocean.

**The invariant to implement: the geographic point under the cursor does not
move across a zoom step.** `recentre()` (:1361) already contains the machinery —
unproject, re-project, re-anchor the origin. Zoom needs the same three calls
with the new rung's dimensions instead of a new frame. Extract one helper both
call rather than a second copy; this file's own `content_height` doc records
what two independent copies of one computation cost here last time.

Anchoring has two knobs and the fix needs both. Normally the **window origin**
moves. At the coarsest rung the whole planet fits the plate and the origin is
pinned to `(0,0)` by an existing invariant, so there the **cursor** moves
instead. Same guarantee, different knob.

**Two limits are declared rather than hidden**, in the shape decision 0142 set
for a lost axis: the invariant holds exactly on longitude (which wraps), and
holds on latitude except where Mercator's polar clamp binds.

**The rung is named on screen.** Part of "zooming based on criteria I have not
identified" is that the same two keys enter the world view at one end of the
ladder and drop back to the walk band at the other, with nothing saying which
rung is showing. The map strip beneath the plate already exists and carries the
text.

**The test already exists in template form**:
`recentre_keeps_the_same_geographic_point_under_the_cursor` (`driver.rs:2293`).
The zoom assertion is that one with a zoom step substituted for the recentre.

### 4.2 The prose pane

Reproduced, not inferred. Bare `map` calls `self.handle("map")` **before**
`enter_map()` (`driver.rs:1075`), so the sim answers a mode gesture with a
picture; `Session::map` returns `render_surrounds_ascii(...)` as prose
(`windows/vessel/src/session.rs:4315`); and `entry::wrap`
(`clients/game/core/src/entry.rs:125`) splits paragraphs on `\n` and rejoins on
`split_whitespace()`, collapsing every run of spaces. Measured output at seed
42, turn 0, rendered through the real pane:

```
plate (correct)          prose pane (mangled)
     +                   +
  + +   +                + + +
+ +   + +   +            + + + + +
+ +   @ +   +            + + @ + +
```

**A latent second defect, verified in the same probe.** The reply carried *no*
ANSI escapes — but only because seed 42 turn 0 reports "0 tinted, 31 withheld".
`surrounds_ascii.rs:224` emits `\x1b[38;2;r;g;bm` per tinted glyph and `Cell`
holds a single `char`, so the first tinted chart renders escape bytes as
literal glyphs. Not currently reproducible; real.

Three changes:

1. **Bare `map` that enters the map focus stops sending the verb.** The
   acknowledgement is the focus changing — the precedent `recentre`'s own doc
   states for itself ("the acknowledgement is the map redrawing"). `driver.rs`'s
   existing comment already argues `map` is a mode gesture and not a fetch; the
   code simply does not do what the comment says.

   **Only the BARE form changes, and that is load-bearing rather than
   conventional.** `map out N` still returns the sim's own picture — which is
   the diagnostic path that caught The Quire's wrong projection, where the
   client and the sim were rendered side by side over the identical thirty-one
   cells and only one was right. Removing every route to the sim's picture would
   delete the comparison that guards §5's H2. The bare/argument split preserves
   it for free.
2. **`wrap` stops collapsing whitespace, and never re-flows a picture.** A line
   that fits the pane is preserved verbatim. A line that *exceeds* it is
   **clipped or horizontally scrolled, never word-wrapped** — prose may grow
   downward, a picture may not grow at all, and re-flowing it is precisely the
   defect. `strip.rs`'s existing marquee is the precedent for the scrolling
   half. This fixes `map out N` too, which returns the same pre-formatted
   picture and does *not* enter map focus, so fixing only the gesture would
   leave the defect reachable.

   **Named failure mode:** the wire carries no marker distinguishing a
   pre-formatted block from prose, so this rule classifies *per line*, not per
   block. A prose line long enough to exceed the pane is wrapped (correct); a
   picture line long enough to exceed the pane is clipped (correct); a prose
   line that happens to be short is preserved verbatim (identical to wrapping
   it, so harmless). The residual risk is a picture narrower than the pane
   sitting beside prose — which is the actual case today, and which the rule
   handles.
3. **The client requests the `terrain` lens**, which `surrounds_ascii.rs`
   guarantees is escape-free. The client applies its own ink from the wire's
   `color` field and has no use for SGR.

### 4.3 Pane width

`PLATE_WIDTH = 40` (`clients/game/core/src/spread.rs:43`) is used
unconditionally unless `focus == Map && world_plate.is_some()` (:176). At
200×50 that is a ~13-column picture inside a 40-column pane.

The plate claims `max(PLATE_WIDTH, w / 2)`, floored so the entry pane keeps a
legible minimum. With §2 and §3 in place the raster fills whatever pane it is
given, which is what makes the width change visible at all — the earlier
one-glyph-per-facet render would have answered a wider pane with blank columns.

## 5. Performance: the bar, the hypothesis, and how it is settled

**The bar, in the owner's words:** *"this is an ASCII roguelike (currently), we
need to be able to render 200x200 ASCII grids and if we can't, we've failed
already."*

**Today we fail it.** Recorded measurement (`perf/world-plate-memo`,
2026-08-23): a 104×52 plate is **130 ms**, `nearest()` is **88%** of it,
264,992 samples for 5,408 cells (49 per cell, `SUBSAMPLES_PER_AXIS` squared),
cost linear in samples. 200×200 is 7.4× the cells, extrapolating to **~960 ms**.

**HYPOTHESIS H1 — mesh alignment removes the search, not merely the cache
miss.** `nearest()` is a spatial *search* from an arbitrary point to a vertex.
If a rung is a mesh depth then a tile *is* a facet, so terrain comes from that
facet's three corner vertices by direct addressing, and the finest rung needs
no subsampling at all. Predicted: **~17 ms at 200×200 before any caching.**

> **This is an extrapolation from a recorded measurement, not a measurement.**
> It is preregistered here (decision 0016) and must be measured before any
> performance claim ships. **A null is a result:** if mesh alignment does not
> remove the cost, the finding is that the tile cache — which is being built
> either way — is doing the work alone, and this spec's §5 says so in the
> chronicle rather than being quietly retuned.

**Three blockers, all already measured and registered**, in the order they must
be taken:

1. `CLIENT-draw-with-cannot-render-a-subrect` — `virtual_dims` derives the
   projection from the plate width, so no caller can render *part* of a plate.
   This blocks tiling outright and goes first. The registry row states the shape
   of the fix: explicit virtual dims plus a sub-rectangle to fill.
2. `CLIENT-tiles-need-the-overlay-split` — §3's layer split, without which the
   pyramid is invalidated by every discovery.
3. `CLIENT-render-tile-cache` — the cache itself. Measured: one column is
   1.208 ms against 130 ms for a full redraw, ~108× cheaper, so a scroll is
   near-free once tiles exist.

## 6. What this does NOT change

- **The mesh.** Facets, vertices, adjacency and the three-edge graph are
  untouched. Decision 0141 already established that a player-facing heading is
  an overlay resolved against the adjacency graph and never a modification of
  it; this campaign is the same move applied to the *picture*. A square grid is
  a presentation, not a claim about the ground.
- **Band A**, which is already square and already fogged.
- **`PURVIEW_RADIUS`**, the perception packet, and `Body::perception`.
- **The wire schemas.** `scene/surrounds/v2` already carries `radius` and
  `depth` and the client already reads both. No epoch, no cross-repo
  consequence. Decision 0196 clause 2 licenses the whole of §2 and §3 as a lens.
- **Determinism.** Everything here is in `clients/` and `windows/scene`'s
  renderer. No seed label, no stream order, no committed float moves.

## 7. Refusals

- A rung finer than band B is refused: the data stops at depth 12 and 0196
  forbids inventing below it.
- A rung coarser than globe level is the whole-planet view; there is nothing
  further out, and the existing refusal text stands.
- A pane narrower than the entry pane's legible minimum refuses to widen the
  plate further, rather than squeezing prose to nothing.

## 8. What is unverified, and how each is settled

| claim | status | how it is settled |
|---|---|---|
| mesh alignment removes the `nearest()` search | **hypothesis** | H1, §5 — measured before any perf claim ships |
| 200×200 renders inside a keystroke budget | **hypothesis** | falls out of H1 plus the tile cache; measured |
| the client's projection pin can be kept | **unverified** | `clients/game/core/tests/chart.rs:180` pins the client byte-for-byte to `render_surrounds_ascii`; the sim's renderer moves to the same raster and the pin is kept, not retired — it is the only thing that has ever caught a wrong projection here (The Quire) |
| `wrap`'s new rule leaves ordinary prose unchanged | **unverified** | a short prose line preserved verbatim is identical to word-wrapping it; assert both directions |
| the tinted-chart escape defect | **verified latent** | probe showed 0 tinted at seed 42 turn 0; construct a tinted chart in a test |

## 9. Preregistered measurement

- **H1** (§5): mesh-aligned terrain lookup removes the spatial search. Success:
  200×200 terrain raster under 50 ms uncached. Null: the tile cache carries it
  alone, reported as the headline.
  **DISPOSED 2026-08-26 — SUPPORTED, AND RUNG-CONDITIONAL.** The bar as written
  named no rung, which was a scoping defect in this line: the answer depends on
  one. Measured (Task 3, before any cache existed):
  the full sweep, on a quiet box (an earlier sweep read ~2× high at every rung
  under another campaign's suite at load 22–24; it was discarded and the ledger
  says so, so it cannot later read as a mystery 2× win):

  | rung | 200×200 | memo misses | scans | vs bar |
  |---|---|---|---|---|
  | 6 (`GLOBE_RUNG`) | **91.5 ms** | 29,662 | 88,986 | **1.8× over** |
  | 7 | **66.4 ms** | 15,756 | 47,268 | **1.3× over** |
  | 8 | 44.5 ms | 4,717 | 14,151 | under |
  | 10 | 30.5 ms | 333 | 999 | under |
  | 12 (`BAND_B_RUNG`) | 30.0 ms | 30 | 90 | under |

  **The bar holds at rungs ≥ 8 and fails at 6 AND 7.** An earlier revision of
  this line said "≥ 7", interpolated from a three-point sweep (6, 8, 12) that
  skipped rung 7 — rung 7's own 66.4 ms is 1.3× over, and the correction came
  from measuring the point the interpolation had assumed. Recorded because the
  error was in this spec, not in the measurement.

  Both failing rungs are **shipped** — a player reaches them by holding `-` —
  and they are where the memo has almost no reuse. The **mechanism** claim holds
  at every rung: vertex scans fall 1,960,000 → 90 at band B and → 88,986 (22×)
  at `GLOBE_RUNG`. So the search really does disappear; the 50 ms budget is
  what is rung-conditional. **§5's task-5 cache budget is therefore sized
  against 91.5 ms, not 31.3 ms, and takes `GLOBE_RUNG` as its acceptance
  criterion.**
- **H2:** the byte-for-byte client/sim projection pin survives the reprojection
  — i.e. the sim and the client still draw the identical picture afterwards.
  Failure here is a design failure, not a test failure.
- **H3:** the zoom invariant holds — the geographic point under the cursor is
  unchanged across every rung transition, except where the polar clamp binds.

## 10. Out of scope, carried forward

- `Body::perception` as a per-species sense radius (EXP-3).
- Band A's own renderer, and whether an *outdoor* A scale should exist at all —
  there is currently a ~1000× gap between feet (A) and 1.87 km (B), which is the
  Dragon Warrior model deliberately, but is worth an idea-registry row.
- `MAP-vertical-axis-undersamples-the-mesh` and
  `MAP-settlement-glyph-may-be-unreachable-at-any-shipped-zoom` — both should be
  re-measured *after* §2, since rungs-as-depths may dissolve them; neither is
  assumed dissolved.
- Place-name and feature gating rules beyond what discovery already does.

## 11. Flagged for Nathan at G3

1. **No schema move, and that is a claim worth checking.** §6 asserts this
   campaign changes no wire schema because `scene/surrounds/v2` already carries
   `radius` and `depth`. If the layer split needs the perception packet to carry
   anything new, that becomes an epoch question and returns here.
2. **The sim's ASCII renderer moves.** This is not a `clients/`-only campaign:
   `windows/scene/src/surrounds_ascii.rs` reprojects too, to keep the pin at
   `chart.rs:180`. That is sim-side code in service of a client-side picture,
   and it deserves an explicit yes.
3. **The performance claim is a hypothesis** (§5, H1) and the ~17 ms figure is
   an extrapolation. Flagged so it is never read as a measurement.
4. **Scope grew, deliberately.** This started as three defects and now absorbs
   three measured registry rows and rebuilds the zoom ladder. Ratified at the
   scope fork, recorded here.

## 12. Decisions to promote (0286–0295)

- **0286** — a zoom rung is a mesh depth (supersedes the power-of-two ladder).
- **0287** — bands B through E share one epistemic model: terrain known,
  features gated. Fog of war is band A's alone.
- **0288** — the map is three layers with distinct cache keys, and terrain never
  shares an invalidation key with discovery.
- **0289** — the geographic point under the cursor is invariant across a zoom
  step; the implementation moves whichever of window-origin or cursor it must.
- **0290** — a mode gesture is not a fetch: entering a client focus does not
  send a verb.
- **0291** — pre-formatted sim output survives the prose pane; the renderer
  wraps, and never re-flows.

## 13. Task outline

1. Decouple `virtual_dims` from plate width; explicit virtual dims + subrect.
2. Rungs become mesh depths; the ladder's ends and refusals follow.
3. Mesh-aligned terrain lookup; **measure H1 here**, before the cache exists.
4. The layer split: terrain / features / perception, distinct keys.
5. The tile cache.
6. Band B joins the raster ladder; `windows/scene`'s renderer moves with it;
   the `chart.rs:180` pin is kept (**H2**).
7. Cursor-anchored zoom, the shared helper, the rung indicator (**H3**).
8. The prose pane: the mode gesture, `wrap`, the `terrain` lens.
9. Pane width.
10. Artifacts, book chapter, chronicle, retrospective.
