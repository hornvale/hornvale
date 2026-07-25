# The Selvage — Design

**Ticket:** hornvale/orrery MAP-71 (idea-registry row added at close, following MAP-65..68 and MAP-70's convention)
**Date:** 2026-07-24
**Status:** Shipped 2026-07-24 (campaign *The Selvage*, registry MAP-71)
**Parent contracts:** hornvale-repo idea-registry rows MAP-67 (The Diorama — the `MapStyle` switch, the fixed-isometric camera, `buildVoxelHeightfieldGeometry`) and MAP-70 (The Excursion — the same-face neighbour ring, the stable-origin coordinate frame, the pan clamp and recenter hysteresis this campaign must keep intact). Both in `book/src/frontier/idea-registry.md` in the `hornvale` repo, not the orrery one. Also `windows/scene/src/region.rs` (`RegionAddr::node_units`, `param`) for the tile-node parameterisation this campaign's central argument rests on.
**Upstream work required: none.** Orrery-only. No wasm change, no producer change, no save-format surface. `world-wasm` stays at v12.

## 1. Problem

In the Map rung's Voxel-2.5D style, the neighbour-tile ring The Excursion
introduced shows a wide black band along every tile boundary running in one
direction — the viewer sees straight through the world to the page
background. The Excursion's own close named this as a follow-up and
attributed it to `buildVoxelHeightfieldGeometry` never drawing a wall face on
a tile's own outer edge.

That attribution is **half right, and it is the smaller half.** A visual pass
run at the start of this campaign — before any design work — found the black
band is dominated by a second, independent defect: an inverted sign that glues
the wrong two tile edges together.

### 1.1 The sign inversion

Two axis conventions disagree.

**The producer.** `windows/scene/src/region.rs` builds a region's `(N+1)²`
nodes as

```rust
fn param(index: u32, offset: f64, level: u32) -> f64 {
    -1.0 + 2.0 * (f64::from(index) + offset) / (1u64 << level) as f64
}
// node_units(): for row in 0..=n { let b = param(self.iy, row as f64 / n as f64, self.level); ... }
```

so within one tile, increasing `row` moves in the **increasing-`iy`**
direction. Two consequences follow. First, any consistent world layout must
run the within-tile `row` axis and the across-tile `dy` axis the same way.
Second, `param(iy, 1.0, l)` and `param(iy+1, 0.0, l)` are the same
floating-point expression — `-1 + 2·(iy+1)/2^l` — so a tile's last node row
and its neighbour's first node row are bit-identical positions, and the
elevation sampled there is identical too.

**The client.** `worldMesh.ts`'s heightfield builder lays corner `(row, col)`
at `cornerZ = (row/N)·extent − extent/2`: increasing `row` runs toward **+z**.
But `mapView.ts`'s `worldPointForOffset` places tile offset `(dx, dy)` at
`[dx·E, 0, −dy·E]`: increasing `dy` runs toward **−z**.

They disagree. Every `iy`-direction seam therefore joins a tile's `row = N`
edge to the wrong neighbour's wrong edge, producing an arbitrary elevation
discontinuity where continuity should hold to within one 250 m band.

The Pixel style is unaffected because it is internally consistent:
`mapTexture.ts` sets `flipY = true`, putting node row 0 at the top, so
increasing `row` runs toward `−y` — the same direction as its `+dy → −y`.
**The two styles legitimately hold opposite signs**, because they legitimately
use different axis conventions. A future "unification" of the two sign rules
would reintroduce this bug.

### 1.2 Evidence

Screenshots taken at brainstorm time, seed 42, the default map region, the
3×3 ring at minimum zoom:

- **As shipped** — a wide black wedge along the `\` screen diagonal only.
  Under the fixed camera, a constant-`z` seam (the `dy` seams) projects to
  `\` and a constant-`x` seam (the `dx` seams) projects to `/`. Gaps appear
  on `\` and never on `/`, which is exactly what a `dy`-only inversion
  predicts, and is *not* what a general missing-wall defect predicts.
- **With the sign flipped** (throwaway build, reverted) — the band is gone
  and coastlines and elevation terraces flow continuously across all nine
  tiles. What remains is the genuine missing-wall defect at its true scale:
  scattered specks a band or two deep at real cliffs.

The screenshots also rule out a transposition (`row`/`col` swapped) rather
than an inversion: a transposition would misalign *both* seam directions, and
the `/` seams are clean.

### 1.3 The residual defect

After the sign fix, two things remain, both genuine and both cosmetic:

- Small see-through gaps where a real elevation step falls on a tile boundary
  and no wall geometry fills it.
- The whole ring has no thickness. Its outer silhouette is a hard cut, so the
  diorama reads as a sheet of paper rather than a block on a table.

## 2. Goal

The Map rung's voxel diorama reads as one continuous piece of world with real
mass: no seam is locatable by eye, and the ring's outer edge is a finished
slab side. Panning, the pan clamp, and the ring recenter keep working exactly
as The Excursion left them.

Explicitly **seamless, not tile-legible** — the alternative aesthetic (tiles
as visibly separate finished objects, a tray of specimens) was surfaced,
considered, and rejected on purpose. It is recorded in the idea registry
rather than left unnamed.

## 3. The sign fix is three changes, not one

The forward mapping `(dx, dy) → world` lives in `worldPointForOffset`. Its
**inverse** is open-coded twice more, each time with the sign inlined:

| Site | Today | Role |
|---|---|---|
| `mapView.ts` `worldPointForOffset` | `[dx·E, 0, −dy·E]` (voxel), `[dx·E, −dy·E, 0]` (pixel) | forward: mesh + `controls.target` placement |
| `mapView.ts` `clampPan` | `minSecond = −bounds.maxDy·E`, `maxSecond = −bounds.minDy·E` | inverse: the active pan bound |
| `mapView.ts` `maybeRecenter` | `localY = −secondAxis / E` | inverse: which tile the camera is over |

All three currently hardcode `−dy`. All three are **style-aware on the axis**
(`z` for voxel, `y` for pixel) but **style-agnostic on the sign**. Since the
correct sign now differs by style, flipping only the emitter would leave the
ring recentring and clamping in the wrong direction under the voxel style —
a defect no screenshot can show, because it is a behaviour, not a look.

**Design:** make the world↔tile mapping a single invertible pair, both
style-aware, and route all three sites through it.

```
worldPointForOffset(dx, dy) -> [x, y, z]      // exists; sign corrected for voxel
tileOffsetForWorldPoint(target) -> {dx, dy}    // new; the stated inverse
```

`clampPan` and `maybeRecenter` stop open-coding the second-axis sign.
`clampPan` still needs the forward direction to convert tile bounds to world
bounds; it takes them from `worldPointForOffset` applied to the bound
corners, so a min/max swap is a consequence of the mapping rather than a
hand-maintained comment.

This is the campaign's structural improvement: the convention is stated once
and read everywhere, instead of stated once and re-derived by hand twice.

## 4. The plinth

`buildVoxelHeightfieldGeometry` gains one option and changes one branch.

```ts
opts: { extent, heightScale, bandM, floorY }

const neighborHeight = (ownIdx, row, col) =>
  row < 0 || row >= N || col < 0 || col >= N
    ? floorY                       // was: cellHeight[ownIdx] — "no wall at the boundary"
    : cellHeight[row * N + col]!;
```

Every boundary cell now emits a wall down to a shared floor, on all four
edges, unconditionally. The builder stays a **pure function of one
`RegionScene`**: no neighbour data, no ring awareness, no rebuild when a
neighbour arrives or when the ring recenters.

### 4.1 Why an unconditional plinth is sufficient

This is the load-bearing argument, and it depends on the camera being fixed.

The camera sits at `(d, d, d)` looking at the origin with rotation disabled
(`MapControls`, The Excursion). The only faces it can ever see are those whose
normals have a positive dot product with `(1,1,1)` — the `+x` and `+z` facing
ones.

**Interior seams.** Take two adjacent tiles, `P` nearer the camera and `Q`
farther. A gap is visible exactly when `Q` is higher than `P`; when `Q` is
lower, the face that would fill the step points away and is culled. The fill
needed is `Q`'s `+edge` wall — and `Q`'s plinth emits it, running from `Q`'s
edge height down past `P`'s height to the floor. The surplus below `P`'s
height sits behind `P`'s own terrain, which is nearer the camera, and is
occluded.

**Equal heights.** Where the two tiles agree (the common case after the sign
fix), the plinth is emitted and lies exactly behind the neighbour's top
surface, sharing only the boundary line. The prototype found no hairline and
no z-fighting; the implementation's visual pass re-checks this at mid zoom,
since it is the one claim here that rests on rasterisation rather than
geometry.

**The ring's outer boundary.** Nothing is behind the plinth, so it becomes the
slab's side — which is the desired outcome, not a tolerated one.

**Correction, added at close.** The clause *"the camera is fixed"* is false as
implemented, and the final whole-branch review caught it. `MapControls` keeps
position and aim in step by measuring their separation, moving the aim, then
restoring the separation — but `clampPan` moves the aim directly and leaves
the position alone, so every frame a drag presses against the pan limit, the
separation grows and the isometric angle shears, without bound. The argument
above therefore does not stand on its own premise. The plinth is nonetheless
safe, and only because of the belt-and-braces choice recorded in the next
paragraph: all four edges are emitted, not the two a fixed camera can see. If
that is ever economised down to two, the camera anchoring must be fixed
first. The shear itself is pre-existing (The Excursion) and is carried as a
follow-up, reframed there as one defect with three symptoms.

**On "unconditional".** Only a tile's `+x` and `+z` edge plinths are ever
visible; the `−x` and `−z` ones face away and are culled every frame. Emitting
all four anyway is deliberate: it keeps the builder free of any ring-membership
or camera knowledge, and it means the geometry stays correct if the isometric
azimuth is ever changed. The cost is roughly `4·N` hidden quads per tile — a
few thousand against the heightfield's own tens of thousands.

### 4.2 Why not the exact selvage read

Because a tile's `col = N` / `row = N` node line **is** its neighbour's first
cell representative, bit-identically (§1.1), a tile could compute the true
neighbour wall from its own data — exact depth, exact colour, and *no*
geometry at all where heights match. Strictly more precise than the plinth.

It closes no case the plinth does not. It costs a second code path through the
builder for a difference nothing renders. The shared-node fact stays in this
spec because it is what proves seams *should* be continuous to within one band
after §3 — it is the reasoning, not the mechanism.

Also rejected: **coping** (pass the builder its neighbouring `RegionScene`s —
correct on all four edges, but forces a rebuild whenever an asynchronously
arriving neighbour lands, against The Excursion's deliberately per-tile mount
path); **fusion** (build the 3×3 as one heightfield — no seams by
construction, but it destroys the per-tile mount/unmount model and makes any
one tile's arrival rebuild all nine); **bleed** (a fixed-depth skirt — fails
whenever the real step exceeds the depth); **backdrop** (a dark plane behind
the ring — cosmetic only, the gap survives).

### 4.3 The floor value

```
floorY = min(MAP_VOXEL_FLOOR_Y, minCellHeight − MAP_VOXEL_SLAB_MARGIN)
```

where `MAP_VOXEL_FLOOR_Y` derives from a fixed elevation constant through the
existing `heightScale · elevation / REFERENCE_RADIUS_M` formula, and
`minCellHeight` is this tile's own minimum banded cell height.

A bare constant is not safe: the builder's wall guard is a strict `<`, so a
tile whose cells sit at or below the floor would emit **no** wall and reopen
the gap. The producer does not state a single global minimum elevation
(`TRENCH_DEPTH_M` is −2800 m, but isostasy, sculpting, and carve all
contribute independently), so the correct-by-construction `min` form is used
rather than a researched bound. In the normal case every tile clears the
constant and all tiles share it, so the outer silhouette does not step.

### 4.4 Plinth colour

The cell's own terrain colour under the existing `VOXEL_CLIFF_DARKEN`, exactly
like every other cliff wall. No new constant, no wall/plinth distinction in the
builder. The slab's sides therefore band by biome, which reads as strata.
(A neutral rock tone and a depth gradient were both considered; both are
follow-up candidates, not this campaign's work.)

## 5. Write the invariant down

The root cause is not a typo. It is that **no client-side file states that the
producer's `row` axis runs in the increasing-`iy` direction** — the fact is
derivable from `region.rs` and was never written where the two conventions
meet.

The campaign adds a doc comment at that meeting point (`worldPointForOffset`,
alongside the corrected sign) stating the invariant, why the two map styles
hold opposite signs for it, and that the inverse is `tileOffsetForWorldPoint`
rather than an open-coded negation. This is a required deliverable, not a
nicety: without it the next person to touch either style has exactly the
information the last one lacked.

## 6. Testing

Three levels, and the first one matters most.

**The continuity invariant (unit, `worldMesh.test.ts` / a mapView test).**
A test asserting `worldPointForOffset(0,1) === [0,0,E]` would restate the
implementation and pass whichever sign is in the file — a test that measures
nothing and ships green. The honest assertion is
behavioural: build two `dy`-adjacent tiles from elevation data that varies
with `row`, place them through the real mapping, and assert the geometry is
continuous across the shared boundary plane — the last cell row of the tile
at `dy` and the first cell row of the tile at `dy+1` meet at adjacent `z` with
matching heights. This fails on the current code and passes after §3.
Mutation-verify it: flip the sign back and confirm it goes red.

**The round-trip (unit).** `tileOffsetForWorldPoint(worldPointForOffset(dx, dy))
=== {dx, dy}` for both styles, over a small grid of offsets including
negatives. This is what stops §3's three sites drifting apart again.

**The plinth (unit).** A flat single-elevation region emits no interior walls
today; with a `floorY` below it, it emits exactly one wall quad per boundary
cell and none in the interior. Count the quads. Separately, assert no
degenerate quads (the existing `hasWallBetweenEqualCells` helper) so a
`floorY` equal to a cell height cannot silently pass.

**The visual pass (controller, not delegated).** A subagent cannot see
rendered frames. The controller re-shoots the same three framings used in this
spec's evidence — zoomed-out ring, mid zoom, and a close-up straddling an
interior seam — and confirms: no seam band, no hairline, slab thickness
present. The close-up is the one that settles §4.1's rasterisation claim.

**Existing e2e.** The Excursion's pan, zoom, clamp, and recenter tests must
stay green unchanged — they are the regression guard for §3's inverse-mapping
rework.

## 7. Non-goals

- **Cross-cube-face-boundary panning.** Still out of scope, unchanged from The
  Excursion. No adjacency remapping exists for the six-face cube topology.
- **The Pixel style.** Internally consistent already (§1.1); untouched.
- **The globe's voxel style.** `buildVoxelBlocks` has the same "no wall at the
  grid boundary" rule, but on the sphere neighbouring tiles are stitched by
  skirts and the case does not arise the same way. Out of scope; noted as a
  follow-up to check.
- **Retuning `MAP_VOXEL_HEIGHT_SCALE`, `MAP_RING_RADIUS`, or the zoom bounds.**
  A thicker slab may argue for revisiting them; that is aesthetic tuning for a
  later pass, per the same merge-as-foundation reasoning The Overworld used.
- **Map URL-addressability.** Still open from The Vantage.

## 8. Flagged for G3

1. **No epoch, no save-format, no determinism-contract implications.**
   Orrery-only; no wasm rebuild, no producer change, no census exposure, no
   AWS spend. `world-wasm` stays v12.
2. **A behaviour change no screenshot can verify.** §3's clamp/recenter
   rework changes pan and ring behaviour under the voxel style, not just its
   look. The Excursion's existing e2e suite is the guard, and this is the
   part of the campaign most worth a careful review — it is the same shape as
   The Excursion's own final-review bug (a seam no single per-task review
   owned).
3. **One claim rests on rasterisation, not geometry.** §4.1's "no hairline at
   an equal-height seam" was confirmed by prototype at two zoom levels but is
   not provable from the geometry alone. The controller visual pass at close
   is the verification, and a hairline, if it appears, is fixed by insetting
   the plinth rather than by abandoning the approach.
4. **Two first-pass tunable constants** — `MAP_VOXEL_FLOOR_Y` and
   `MAP_VOXEL_SLAB_MARGIN`. Their *relationship* is load-bearing and specified
   in §4.3; their numeric values are visual-pass tuning, consistent with every
   prior view-remake campaign.
5. **Every claim in §1 was verified before it was written**, not reasoned to:
   the producer parameterisation was read from `region.rs`, the three sign
   sites were grepped, and both the diagnosis and the proposed fix were
   prototyped in throwaway builds and screenshotted, then reverted. The
   working tree is clean.
6. **Reached via two ideonomy passes, zero overturns, four material folds.**
   Pass 1 (dimension-identification + abstraction-lift, spectrum organon,
   axis = how much a tile must know about its neighbours) produced the
   approach spectrum in §4.2 and identified **rebuild cadence** rather than
   triangle count as the disqualifying cost for coping and fusion; its
   polarity flip surfaced the tile-legible aesthetic, rejected in §2. Pass 2
   (combination, over {sites declaring an axis convention} × {failure modes
   of an axis convention}) produced §3 — the finding that the sign is encoded
   in three places, two of them inverses — and §5, the unstated-convention
   root cause. Neither fold was visible before its pass; §3 in particular
   would have shipped as a one-line change that left panning wrong.
