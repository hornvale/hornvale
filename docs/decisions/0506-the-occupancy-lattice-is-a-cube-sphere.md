# 0506. The occupancy lattice is a cube-sphere; the icosphere stays the field substrate

**Status:** Accepted (2026-08-31) · **Decider:** Nathan (autopilot, spec §2) ·
**Supersedes:** [0141](0141-compass-navigation-is-an-overlay.md) ·
**Amends:** [0287](0287-a-zoom-rung-is-a-mesh-depth.md) (its corollary only;
its core survives untouched) · **Relates:**
[0077](0077-zoom-in-the-room-mesh-is-path-truncation.md),
[0189](0189-a-pre-flip-world-file-does-not-load-and-that-is-the-point.md),
[0196](0196-a-map-is-a-fact-about-the-world-and-a-view-is-a-lens.md)

In the context of a walk band whose ground was triangular icosphere faces —
three edges per room against a compass offering eight — while every other
lattice in the project (interiors, the underground level, every drawn raster)
was already square, we decided that **the occupancy lattice is a tangent-warped
cube-sphere, six base faces subdividing into quads, while `Geosphere` remains
the substrate every field is sampled on**, accepting that `Facet` values change
meaning across the whole repository and every address-carrying artifact
rebaselines.

## Context

The mismatch was not cosmetic. `Facet::neighbors` returned `[Facet; 3]`; the
compass offers eight bearings; and 0141 resolved that by making the compass an
overlay — a dead-reckoned rhumb course snapped to whichever of the three real
edges landed nearest an ideal point. Its own Consequences recorded the residual
as its headline finding: the walked cell's error from the ideal rhumb is
**unbounded**, 172.6 step-lengths at 2,000 steps, growing ~0.086 per step, with
the mechanism being the local triad's alignment rather than latitude — so the
behaviour was qualitatively different on different ground. Six days later The
Stride made arrow keys the startup default, which put the mismatch in a
player's hands.

**0141's blocking argument does not survive checking.** It rejected a graph
edit because "every non-player consumer would inherit it silently: path costs
move, ecology's spread changes, settlement fitting sees a different
neighbourhood." At `92a4b059e` — the commit that added 0141 — `git grep
'\.neighbors()' -- domains/` returns nothing, and it still returns nothing.
Ecology is `VertexMap`/`Vertex`; `domains/settlement` contains no mesh type at
all; `hops_between` is `Geosphere`'s; `kernel/src/astar.rs` is generic over its
state space and says so in its own doc. Face adjacency has never had a domain
consumer, so the cost 0141 refused to pay was not on the table.

## What was decided

- **Two meshes, with one job each.** The occupancy lattice — where a body
  stands and what it can step to — is the cube-sphere. The field substrate —
  where terrain, climate, hydrology and ecology are sampled — stays
  `Geosphere`, unchanged, at the same globe level, drawing the same streams in
  the same order.
- **The address space survives.** `Facet { face, path }`, `pack` (`(pathword
  << 5) | face`, five face bits, of which a cube uses six of the 32 encodable
  values), `child`, `parent`, `depth` and prefix containment are all unedited.
  0077's "zoom is path truncation, never aggregation" carries over verbatim.
- **0287 is AMENDED, not superseded.** Its core — *a tile at rung `d` is a
  facet at depth `d`*, and zooming out truncates the path rather than averaging
  the picture — is exactly as true on a quad tree as on a triangle tree, and is
  what this campaign's `Facet::corners` still leans on. What retires is a
  COROLLARY that 0287's own text never states (`grep -cEi
  'vertex|vertices|corner'` over the record returns **0**) but which the code
  around it relied on: that a room's corners are `Geosphere` vertices. A
  cube-sphere quad's corners are not geosphere vertices at any level, and
  cannot be.
- **0141 is SUPERSEDED.** With eight real edges the overlay's mapping is the
  identity and its subject is gone: `windows/vessel/src/course.rs` and
  `course_properties.rs` are deleted rather than ported. This is a hypothesis
  whose SUBJECT was removed, not one confirmed, and must be reported that way.
- **The world itself does not move.** No seed label, no stream label, no draw,
  no stream consumption order. Verified rather than asserted: `hornvale new
  --seed 42` built at `origin/main` (`f047ae36d`) and on this branch produces
  **byte-identical** world files, sha256 `e70ca3d0…`, 21,635 facts each.

## Consequences

- **An epoch, authorised explicitly.** Every committed `agent-at` place
  changes; `vessel/session/v2` fixtures, the seed-42 gallery transcripts, the
  almanacs and the `scene/surrounds` goldens all rebaseline. A pre-flip room id
  does not decode — `FacetId::unpack` refuses any face `>= 6`, which is 0189's
  shape and is deliberate.
- **Eight singular points instead of twelve.** Three faces meet at each of the
  cube's eight corners, so a corner room has seven neighbours. That replaces
  the icosphere's twelve pentagons, and the singular points are now *corners of
  the world* rather than points scattered across inhabited ground.
- **What we give up:** the corner-is-a-vertex identity, which several fixtures
  and one property test were built on. `Facet::corner_weights` now returns four
  NEAREST geosphere vertices with bilinear weights rather than an exact
  identification, and any test that read a room's geometry off the field mesh
  needed a new ruler (see `domains/terrain/tests/suite/rill_properties.rs`).

## See also

`docs/superpowers/specs/2026-08-30-the-pavement-design.md` §1, §1.1, §2, §2.1,
§4; `kernel/src/room.rs`, `kernel/src/cube.rs`.
