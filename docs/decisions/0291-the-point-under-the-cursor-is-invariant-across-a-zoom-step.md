# 0291. The geographic point under the cursor is invariant across a zoom step

**Status:** Accepted (2026-08-27) · **Decider:** Nathan (the founding defect
report) · **Relates:** [0287](0287-a-zoom-rung-is-a-mesh-depth.md) (what a step
moves between),
[0142](0142-a-channel-carries-one-axis-and-a-lost-axis-is-declared.md) (the
shape a declared limit takes);
[The Quadrat](../../book/src/chronicle/the-quadrat.md)

In the context of a zoom that changed the chart's size under an unchanged window
origin — so the same `origin_col` named a different longitude after the step and
the view lurched by half the visible span — we decided that **the geographic
point under the cursor does not move across a zoom step**, and that the
implementation moves whichever of the window origin or the cursor it must to
keep that true.

## Context

The defect as reported: *"The game map appears to zoom in and out based on
criteria I have not identified. Rather, the focal point of the zoom should be
the position of the cursor."* The old `apply_zoom` changed the rung and then only
*clamped* the origin; the cursor was never consulted at all.

**Anchoring has two knobs and the guarantee needs both.** Normally the window
origin moves. At the coarsest rung the whole planet fits the plate and the origin
is pinned to `(0,0)` by an existing invariant, so there the *cursor* moves
instead. One guarantee, two mechanisms, chosen by which one is free to move.

## Consequences

- **Asserted as an exact containment across all twelve ladder steps in both
  directions** (H3, preregistered and holding) — not as a tolerance. An earlier
  draft of that test would have asserted at a tolerance equal to half a tile at
  the coarsest rung, which is blind to a full one-tile error.
- **Two limits are declared rather than hidden**, in the shape decision 0142 set
  for a lost axis: the invariant holds exactly on longitude, which wraps, and
  holds on latitude except where Mercator's polar clamp binds.
- **The rung is named on screen.** Part of "criteria I have not identified" was
  that the same two keys enter the world view at one end of the ladder and drop
  back to the walk band at the other with nothing saying which rung is showing.
- **What we give up:** the cursor is now an input to zoom, so a zoom is no longer
  a pure function of the rung. Any later caller changing the rung without going
  through the shared anchor helper reintroduces the defect, which is why there is
  one helper and not a second copy of the three calls.

## See also

Spec §4.1; `clients/game/bin/src/driver.rs` (`apply_zoom`, `recentre`, and the
anchor helper both call).
