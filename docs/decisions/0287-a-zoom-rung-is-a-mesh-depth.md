# 0287. A zoom rung is a mesh depth

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (autopilot, spec §2) ·
**Relates:** [0077](0077-zoom-in-the-room-mesh-is-path-truncation.md) (the same
rule applied to addressing rather than to the view),
[0196](0196-a-map-is-a-fact-about-the-world-and-a-view-is-a-lens.md)
(clause 1, the frame; clause 2, the lens); [The Quadrat](../../book/src/chronicle/the-quadrat.md)

In the context of a client zoom ladder built as `virtual_w = plate_width <<
zoom`, clamped at a `MAX_VIRTUAL_WIDTH` unrelated to anything in the world, we
decided that **a zoom rung is a refinement depth of the facet tree** —
band B is depth 12, coarsening 11, 10, 9, 8, 7 down to globe level 6 — accepting
a ladder with exactly seven rungs and no continuous zoom.

## Context

The power-of-two ladder was never ratified; it was the shape the first world
plate happened to grow. It had three costs and all three were structural rather
than incidental: the projection was derived from the plate's own width (so no
caller could render part of a chart — `CLIENT-draw-with-cannot-render-a-subrect`),
a rung could ask for detail finer than the datum underneath it, and the client's
ladder and the sim's `map out N` ladder were two different ladders that
disagreed.

Decision 0077 already holds that *"zoom in the room mesh is path truncation,
never an aggregation."* That was ratified about **addressing**. This record
applies the identical rule to the **view**: a tile at rung `d` is a facet at
depth `d`, and zooming out truncates the path rather than averaging the picture.

## Consequences

- **Three properties hold by construction, each replacing a rule an earlier
  draft of the spec proposed to police.** A tile can never be finer than its
  datum (0196's "may never invent detail below it" is now structural, not
  checked). The projection frame never re-pegs, so the shimmer hazard MAP-70
  recorded cannot occur. Sampling runs lat/lon → facet with no lattice
  traversal, so the base-face seams and the twelve pentagon points are not
  reached by any code path here.
- **The client's ladder and `Session::map`'s become one ladder.** Six zoom-out
  steps is exactly the bound `Session::map` already enforced as
  `depth - globe_level`. `CLIENT-snapshot-chart-cannot-zoom` closes as a
  consequence rather than as a task.
- **`MAX_ZOOM` and `MAX_VIRTUAL_WIDTH` are deleted.** Any later reader citing
  either is citing a ladder that no longer exists.
- **What we give up:** a continuous or arbitrary-factor zoom. The ladder has
  seven rungs and each is a real data resolution; there is no half-step between
  them and there will not be one, because a half-step is a resolution the world
  does not have.

## See also

Spec §2; `clients/game/bin/src/plate.rs` (`virtual_dims`, `BAND_B_RUNG`,
`GLOBE_RUNG`).
