# 0508. A diagonal costs √2, because the movement clock is flat

**Status:** Accepted (2026-08-31) · **Decider:** Nathan (autopilot, spec §3.2) ·
**Relates:** [0507](0507-every-lattice-in-the-project-is-eight-connected.md),
[0186](0186-an-instant-is-an-exact-tick-count.md)

In the context of `windows/vessel/src/clock.rs` charging `Action::MoveTo` a
flat 10,000 ticks whatever room it leads to, on a lattice that
[0507](0507-every-lattice-in-the-project-is-eight-connected.md) has just made
8-connected, we decided that **a diagonal step costs `√2` orthogonal steps at
both movement scales**, accepting one geometry multiplier at the two sites that
already modulate by body mass and climb.

## Context

A flat charge on an 8-connected lattice is a travel-speed exploit, not a UX
preference: a diagonal buys `√2 ≈ 1.414` times the ground for the same time, so
a creature or a player travels ~41% faster by zigzagging than by walking
straight. In a project that already charges movement against body mass and
terrain, that is a physics falsehood, and it becomes latent the instant
diagonals exist rather than being a defect anyone introduced.

**`√2` is the ideal, and the real lattice was measured against it rather than
assumed to match.** The cube-sphere's tangent warp distorts a quad, so there
was no a-priori reason for the diagonal to sit at exactly `√2` edges. Over
4,000 interior rooms at walk depth spread across all six faces, the mean
diagonal centroid separation is **1.411786** edge separations (−0.172% against
`√2`), spread 1.366086–1.434180 per room. The exploit was therefore **41.18%**
on the ground the project actually walks, against the ideal 41.42%.

## What was decided

- **`DIAGONAL_STEP_FACTOR = √2`**, applied at both `MoveTo` (walk band) and
  `MoveWithin` (interiors and the underground level).
- **The multiplier is the IDEAL, not the measured 1.411786.** The measurement
  exists to show the ideal is honest on this mesh — a 0.17% residual — not to
  be the constant. A per-room lookup would make a step's cost depend on where
  on a cube face it was taken, which is a distinction no player can perceive
  and every test would have to carry.
- **The two groups are averaged SEPARATELY.** A mean over all eight neighbours
  is not the quantity: the deleted `course.rs`'s `step_length_rad` divided by
  `ns.len()` and so returned ~1.21 edge steps once the mesh went 8-connected —
  a number that looks like one step and is not.

## Consequences

- **The planner and the clock must price a diagonal alike**, or a route's
  quoted cost and its charged cost diverge. Held by
  `the_planner_and_the_clock_price_a_diagonal_alike`.
- **A pair that is not a step at all charges the orthogonal unit**, and says so
  loudly in a debug build. Every caller passes a real step, so the arm is
  unreachable in practice — and "unreachable in practice" is an argument, so
  the `debug_assert!` turns it into something a test run enforces.
- **Every authored duration calibrated against the flat charge stays honest**,
  because the orthogonal step's price did not move; only the diagonal gained
  one. This is the same reason
  [0511](0511-walk-depth-is-globe-level-plus-seven.md) chose the depth it did.
- **What we give up:** a movement clock that can be reasoned about without
  knowing the geometry of the step. `base_cost` alone no longer answers "what
  does moving cost".

## See also

`docs/superpowers/specs/2026-08-30-the-pavement-design.md` §3.2;
`windows/vessel/src/clock.rs` (`DIAGONAL_STEP_FACTOR`, `step_factor`);
`windows/vessel/tests/suite/octile_cost.rs` (H2 and its positive control).
