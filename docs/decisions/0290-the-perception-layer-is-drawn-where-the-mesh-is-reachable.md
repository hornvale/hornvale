# 0290. The perception layer is drawn where the mesh is reachable

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (autopilot; the campaign
controller's Ruling 20, retracting its own Ruling 19 on the implementer's
measurement) · **Relates:**
[0289](0289-the-map-is-layers-with-distinct-cache-keys.md) (the layer this one
places), [0022](0022-sim-emits-data-clients-render.md),
[0142](0142-a-channel-carries-one-axis-and-a-lost-axis-is-declared.md) (a
channel that does not carry an axis cannot be asked for it);
[The Quadrat](../../book/src/chronicle/the-quadrat.md)

In the context of overlaying the per-turn perception packet onto a Mercator
raster, we decided that **the perception layer is drawn in the crate that can
reach the mesh (`clients/game/bin`), by unpacking each wire cell's own `room`
FacetId and projecting it through the same `mercator::project` the raster uses**
— not by reprojecting the packet's polar offsets in `clients/game/core` —
accepting that the overlay exists in one client only.

## Context

The spec's original architecture had `core/chart.rs` reproject the packet onto
the square grid, with `windows/scene/src/surrounds_ascii.rs` moving identically
so the byte pin at `chart.rs:180` stayed green. **That design was measured false
and never built.**

`core` works from the wire's `(bearing_deg, distance_rad)` — a *relative polar
offset*. The raster works from *absolute* Mercator tile coordinates via `floor`.
Converting one into the other depends on the observer's **sub-tile phase**,
which the wire does not carry. Sweeping 200 phases on the fixture's own
observer: **0 misplaced marks at best, 24 of 31 at worst, mean 11.5, and only 2
of 200 phases exact.** The mandated design produces exactly the defect the
task's own agreement test existed to catch, and that test could only have passed
by being weakened to "within one tile".

The alternative was already in the tree. The wire's chart cells each carry
`room`, a packed `FacetId`; `core` cannot use it (it depends on no hornvale
crate and so has no mesh), but `bin` can, and already did —
`driver.rs:1695-1696` unpacks exactly that field, and that module's own doc
records "No new geometry was written for this."

## Consequences

- **The two pictures agree by construction rather than by coincidence**, because
  both sides call one projection. An arithmetic agreement between two
  independent reprojections is the thing this decision refuses.
- **Five problems dissolve rather than being solved.** `core/chart.rs` and
  `surrounds_ascii.rs` never move, so the client/sim byte pin stays green
  untouched; `windows/scene`'s "31 of 31 cells drawn" golden survives (any
  raster-agreeing reprojection would have occluded about a third of the band,
  undoing a prior campaign's headline); the third replica of the projection in
  `clients/vessel/src/pane_chart.ts` stays consistent; `spread::compose` needs no
  knowledge of the rung; and two byte goldens never move.
- **It also made the campaign `clients/`-only**, which withdrew a flagged
  sim-side change from the owner's review list.
- **What we give up:** the overlay lives in `bin`, so a future non-Rust client
  reimplements it. That is already true of the projection itself — there are
  three replicas of it today — so this decision adds a fourth thing to that
  list rather than opening a new category.
- **H2 dissolved with the design it tested.** The preregistered hypothesis was
  "the byte-for-byte pin survives the reprojection"; there is no reprojection, so
  the pin is green because nothing touched it. That is vacuous satisfaction and
  is reported as a dissolution, never as a pass.

## See also

Spec §5a; `clients/game/bin/src/plate.rs` (`draw_perception_layer`),
`clients/game/bin/src/driver.rs` (`perceived_cells`).
