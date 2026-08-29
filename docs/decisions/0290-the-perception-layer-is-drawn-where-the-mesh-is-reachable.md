# 0290. The perception layer is drawn where the mesh is reachable

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (autopilot; the campaign
controller's Ruling 20, retracting its own Ruling 19 on the implementer's
measurement) · **Relates:**
[0289](0289-the-map-is-layers-with-distinct-cache-keys.md) (the layer this one
places), [0022](0022-sim-emits-data-clients-render.md);
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
so the byte pin at `chart.rs:180` stayed green. **That design was replaced before
it was built**, on grounds narrower than the campaign first stated.

`core` works from the wire's `(bearing_deg, distance_rad)` — a *relative polar
offset*. The raster works from *absolute* Mercator tile coordinates via `floor`.
Converting one into the other means reconstructing each wire cell's absolute
coordinate first, and **`core` as it stands cannot**: its parsed mirror of the
document drops the `observer` block outright — `schema.rs`'s `Chart` carries
`radius`, `depth`, three legends, `cells`, `legend` and `sight`, and no
`observer` — so the centre's centroid is not in the crate's model at all; and
`kernel::room` has no inverse of `bearing_to`/`distance_rad_to`, so the
spherical direct problem would have to be newly written. Reprojecting the
offsets *without* that reconstruction — what the mandated design amounted to —
depends instead on the observer's **sub-tile phase**. Swept across 200 phases on
the fixture's own observer: **0 misplaced marks at best, 24 of 31 at worst, mean
11.5, and only 2 of 200 phases exact.** It produces exactly the defect the
task's own agreement test existed to catch, and that test could only have passed
by being weakened to "within one tile".

**What that sweep does not show, stated because this record first said it did.**
It does not show the wire is missing the phase. `SurroundsObserver` carries the
centre's centroid `latitude`/`longitude` as `pub`, unskipped fields, and
`bearing_deg`/`distance_rad` are centroid-to-centroid great-circle quantities,
so a consumer that writes the spherical direct problem recovers every cell's
absolute coordinate exactly, and 8-SIGNIFICANT-digit quantization does not
stand in the way: ~1 cm of latitude near the equator, ~11 cm near the ±85°
clamp, and at worst — a longitude of magnitude ~145, where eight significant
digits is only five decimals — ~1.1 m between storable values, so sub-metre
rounding, against a facet 1.87 km across. The sweep measures the shortcut, not
the schema. The claim "the wire does not carry the sub-tile phase" is false and
this record no longer rests on it. The ruling is unchanged, because even with
that reconstruction written, `core` and the raster would be two independent
arithmetics obliged to agree — which is the thing the first consequence below
refuses.

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
