# 0515. A diagonal step REACHES √2, as well as costing it

**Status:** Accepted (2026-09-01) · **Decider:** Nathan (ruling, fix round 1) ·
**Relates:** [0508](0508-a-diagonal-costs-root-two.md),
[0507](0507-every-lattice-in-the-project-is-eight-connected.md),
[0506](0506-the-occupancy-lattice-is-a-cube-sphere.md)

In the context of `hornvale_locale::LocaleContext::crossing_between` judging a
water crossing against `room_edge(a).min(room_edge(b))` — a LENGTH, the stride
available to clear the channel — with no diagonal factor, we decided that
**crossing water, land, or anything else diagonally costs and reaches `√2`
times the orthogonal equivalent, uniformly**, accepting a consistent
simplification over exact per-crossing geometry.

## Context

[0508](0508-a-diagonal-costs-root-two.md) made the movement clock charge `√2`
for a diagonal step, because a diagonal covers `√2` times the ground. The
water-crossing gate priced the same step with no such factor, so a diagonal
step was judged against the same channel width an orthogonal one was — charged
for the extra distance and refused credit for it. The two halves of one
geometric fact were modelled inconsistently.

The whole-branch review of The Pavement found this and Nathan ruled on it
directly: the factor is uniform, and the reason is the same reason 0508 gave.

## What was decided

- **The stride is `room_edge(a).min(room_edge(b)) · DIAGONAL_STEP_FACTOR`** on
  a corner-adjacent pair, and the bare minimum edge on an edge-adjacent one.
- **`DIAGONAL_STEP_FACTOR` has ONE definition, and it moved down a crate.**
  `hornvale_locale::DIAGONAL_STEP_FACTOR` is now the definition;
  `windows/vessel`'s `clock::DIAGONAL_STEP_FACTOR` — the name every
  movement-cost caller uses, and 0508's own — is an alias of it. `windows/
  locale` may not depend on `windows/vessel`, and locale is the lower of the
  two crates that need the factor, so this is the only home the constitutional
  layering allows for a shared one. Two copies of `SQRT_2` would have been the
  alternative, and a constant that can drift from its twin is not shared.
- **The PREDICATE is single-sourced too**, for the same reason:
  `hornvale_locale::is_diagonal_step` is the one answer to "is this step a
  diagonal", derived from `Facet::neighbor_steps`'s edge-first prefix rather
  than from a restated `4`, and `clock::step_factor` now asks it instead of
  indexing its own copy. Two copies of that predicate would let the clock
  charge for a diagonal the reach priced as orthogonal.
- **`√2` is a consistent simplification, not exact geometry, and this record
  says so.** Water width is measured PERPENDICULAR to the channel, and the
  channel's bearing is arbitrary relative to the lattice, so a diagonal step is
  not reliably more oblique to a stream than an orthogonal one is. The exact
  model divides the stride by the sine of the angle between the step and the
  channel — which needs a channel bearing this clause does not consult and
  `BankReading` does not carry. The uniform factor is accepted for the reason
  0508 accepted it: one number a reader can hold, applied everywhere a diagonal
  is priced, against a per-crossing trigonometric correction with no
  measurement behind it and a tuning surface nobody would maintain.

## Consequences

- **The direction is monotone, and it was verified empirically rather than
  argued.** The stride only grows, so a verdict can move `Impassable` →
  `Fordable` and never the reverse: nothing crossable before this change is
  uncrossable after it. Measured over the same 394 sampled transects at five
  depths on seed 42, before and after:

  | depth | Fordable before | after |
  | --- | --- | --- |
  | 12 | 110 | 110 |
  | 13 (walk) | 185 | 185 |
  | 14 | 232 | **248** |
  | 15 | 272 | **305** |
  | 16 | 141 | **229** |

  Every change is an increase; 0 of 394 transects moved the other way at any
  depth.

- **NOTHING MOVED AT WALK DEPTH, and that is a fact about this world rather
  than about the rule.** On seed 42 the widest full channel is 1.6566e-4 rad
  against a 1.6311e-4 room edge, so the width clause is very nearly inert at
  depth 13 — `2·b0 < step` already held at 394 of 394 transects — and a longer
  stride has nothing left to admit. The verdict counts at depth 13 are
  identical before and after (185 Fordable / 2 Impassable / 207 NotACrossing),
  so **no committed artifact drifts from this change**. It binds below walk
  depth, where the step has halved and the clause decides, and it will bind at
  walk depth on any world whose channels are wider relative to its rooms.

- **A test's model of the gate had to move with the gate.**
  `Transect::width_pricing` in `windows/locale/tests/suite/water_reading.rs`
  re-derives the gate's operands, and it now applies the same factor through
  the same public predicate. Its doc previously recorded the `√2` diagonal
  hypothesis as REFUTED; that refutation was about the *cause of one failure*
  (`Vertex(2656)`, where the factor could not have flipped the verdict either
  way) and not about the rule, and the paragraph now separates the two —
  otherwise the record reads as forbidding what this decision ratifies.

- **What we give up:** a crossing gate whose threshold can be stated without
  knowing which of a room's eight neighbours the step goes to. This is the
  same cost 0508 accepted for the clock, and the symmetry is the point.

## See also

`windows/locale/src/lib.rs` (`DIAGONAL_STEP_FACTOR`, `is_diagonal_step`,
`LocaleContext::crossing_between`); `windows/vessel/src/clock.rs`
(`step_factor`); `windows/locale/tests/suite/water_reading.rs`
(`the_width_clause_binds_when_the_step_shrinks`, `Transect::width_pricing`).
