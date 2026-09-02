# 0509. A diagonal through a two-walled corner is refused; one open flank permits it

**Status:** Accepted (2026-08-31) · **Decider:** Nathan (autopilot, spec §3.3) ·
**Relates:** [0507](0507-every-lattice-in-the-project-is-eight-connected.md),
[0508](0508-a-diagonal-costs-root-two.md)

In the context of diagonals becoming walkable at every band
([0507](0507-every-lattice-in-the-project-is-eight-connected.md)), facing the
question of what a diagonal step means when the two orthogonal cells flanking
it are walls, we decided that **a diagonal is refused when BOTH flanking
orthogonal cells are impassable and permitted when either is open**, accepting
that a body can brush a single corner.

## Context

The sentence already existed in the tree and was already phrased as geometry
rather than as a rule — `UNDERGROUND_DIAGONAL_REFUSAL`: *"There is no slipping
through a corner down here either."* What did not exist was anything that
decided WHEN it fired, because no band had diagonals to refuse.

The physical reading settles it: passing through the point where two walls meet
is not a way through a building; brushing one corner is.

## What was decided

- **Both flanks impassable → refused.** Either flank open → permitted.
- **Refused with the geometry as the reason, not with a parse complaint** —
  the standard the indoor twin's own doc already set.
- **The rule is a passability CLOSURE, not a `Lattice` method.** Two bands with
  two different representations need it — `Lattice`/`CellKind` for interiors,
  `CellGrid<LevelCellKind>`/`movement_mode` for the underground level — and a
  signature taking one of them serves one of them.

**Two alternatives, recorded as rejected.** Refusing when EITHER flank is
impassable is stricter than the physical claim, and would make a body unable to
round the outside corner of a wall. Permitting always reintroduces the exploit
0508 exists to close, in the one place where a wall should have stopped it.

## Consequences

- **Every case is asserted in both directions.** A test asserting only refusal
  is structurally blind to over-refusal, so each case in
  `windows/vessel/tests/suite/corner_rule.rs` pairs a refusal with a permission
  that differs from it by exactly one flank.
- **The walk band does not consult this rule**, and that is not an oversight:
  `Session::go` performs no passability check at all — water was already
  walkable (0141) — so the band has no walls to ask the question about. The
  rule governs the two bands that have them.
- **What we give up:** a diagonal corridor one cell wide is unwalkable. Every
  step along such a corridor has both flanking cells as walls, so the rule
  refuses each of them; the corridor must be drawn orthogonally or widened.
  `windows/vessel/src/lattice/grow.rs` says so where the corridors are made.

## See also

`docs/superpowers/specs/2026-08-30-the-pavement-design.md` §3.3, §6;
`windows/vessel/src/lattice/mod.rs` (`diagonal_is_blocked`);
`windows/vessel/tests/suite/corner_rule.rs`.
