# 0507. Every lattice in the project is 8-connected

**Status:** Accepted (2026-08-31) · **Decider:** Nathan (autopilot, spec §3.1) ·
**Relates:** [0506](0506-the-occupancy-lattice-is-a-cube-sphere.md),
[0141](0141-compass-navigation-is-an-overlay.md) (superseded by 0506)

In the context of three occupancy bands — interiors, the underground level and
the walk band — that were each square-drawn but 3- or 4-connected underneath,
so a compass offering eight bearings could resolve at most four of them
anywhere, we decided that **all three bands are 8-connected: a cell's
neighbourhood is its four edge-adjacent cells plus its four corner-adjacent
ones**, accepting eight singular points on the walk band where a cube corner
offers only seven.

## Context

`Session::go` accepted all eight compass tokens at every band and could
dispatch at most four of them. On the walk band the bound was three
(`0141`); on the two `Cell`-addressed bands `cell_delta` returned `None` for
all four diagonals. The verb over-admitted its own vocabulary — the mirror of
the parity contract The Blocking shipped, and the shape The Handle recorded as
structurally invisible to it.

Making one band 8-connected and leaving two at four would have been worse than
either uniform answer: a player crossing from a chamber onto open ground would
gain and lose four of their bearings with nothing in the fiction to explain it,
and every rule downstream (the corner rule, the octile charge, pathing) would
need a per-band exception.

## What was decided

- **One neighbourhood definition, three bands.** `Facet::neighbors` returns up
  to 8; `windows/vessel`'s `lattice::HEADINGS` and the underground level's
  `CellGrid` both enumerate the same eight offsets in the same order.
- **Seven at a cube corner, and no special case leaks upward.** Three faces
  meet at each of the cube's eight corners, so one diagonal does not exist
  there. `Facet::neighbors` returns seven; a step into the absent diagonal is
  refused with the geometry as the reason. This follows 0141's own treatment of
  the pole: *"No termination rule was invented, because inventing one would be
  inventing a defect to fix."*
- **The face seams are a table, not an approximation.** The twelve cube-map
  edge adjacencies each carry a fixed rotation between the two faces' `(u, v)`
  bases; the walk is integer-only and exhaustively testable
  (`kernel/tests/suite/cube_adjacency.rs`).

## Consequences

- **A radius-`r` neighbourhood is a square block, not a ring.** The seed-42
  gallery's seam chart went from 31 cells to **81** at radius 4 — a 9x9 block.
  Every count in every fixture that measured a neighbourhood moved with it, and
  that is the campaign's thesis rather than corruption.
- **Two rules become necessary that were not before.** A diagonal must be
  priced ([0508](0508-a-diagonal-costs-root-two.md)) and refused through a
  two-walled corner ([0509](0509-a-diagonal-through-a-two-walled-corner-is-refused.md)).
  Both are consequences of this decision and neither is optional once it holds.
- **What we give up:** the ability to reason about the walk band as a graph
  whose every edge is the same length. It is not one any more, and 0508 is the
  price of saying so honestly.

## See also

`docs/superpowers/specs/2026-08-30-the-pavement-design.md` §2.2, §3.1;
`kernel/src/room.rs` (`neighbors`, `neighbor_steps`);
`windows/vessel/src/lattice/mod.rs` (`HEADINGS`, `neighbours`).
