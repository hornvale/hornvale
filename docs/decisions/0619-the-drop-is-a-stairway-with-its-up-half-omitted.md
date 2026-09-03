# 0619. The drop is a stairway with its up half omitted, and `Fly` is its key

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (G3) / autopilot ·
**Relates:** [0567](0567-stairs-pair-by-coordinate.md) (stairs pair by
coordinate — this is its closing Consequence made real),
[0347](0347-an-affordance-is-a-relation-not-a-property.md) (an affordance is a
relation, not a property),
[0398](0398-a-capability-nothing-can-reach-is-not-a-capability.md),
[0616](0616-a-gate-is-a-requirement-on-a-way.md) ·
[The Brattice](../../book/src/chronicle/the-brattice.md)

In the context of wanting Dormans' cross-floor valve — the ledge you can drop
from and not climb — facing the question of whether it is a new kind of edge or
a variation on an existing one, we decided that **a drop is a stairway with its
up half omitted: the lip is written on the upper rung in place of a
`StairsDown`, no `StairsUp` is written beneath it, `down` takes it free and
`up` from beneath it needs `Fly`** — accepting that whether it reads as a valve
or as an asymmetric edge is a fact about the BODY and never about the edge, and
accepting that this adds no verb.

## Context

Decision 0567 settled that a stairway's two ends share a coordinate, and closed
by naming its own consequence: "the asymmetric twin — a drop, down with no up —
is now expressible as *omitting the up half of a pair*, which is what makes it
a valve rather than a special case. It belongs to The Brattice." This record is
that sentence taken literally.

The alternative shapes were both worse. A `Shut` way on the plan — a true
one-way edge — is a variant nothing in a cave can construct, the reserved seam
decision 0398 refuses. A new verb (`jump`, `climb`) would have put the
asymmetry in the grammar the player types rather than in the world, and would
need a second one the day a second asymmetric substance exists.

## Decision

On the plan, a chute is a `Stair` edge whose downward way is `Open` and whose
upward way is `Needs(Mode(Fly))` — the only place `DownFreeUpNeeds` is legal,
because only a stair edge has two ends on different floors.

In the rock, the realizer writes `Drop` at the stair coordinate on rung ℓ in
place of `StairsDown`, and on ℓ+1 writes **no** `StairsUp`: the landing is made
standable and its region reconnected exactly as any stair foot is. The pairing
test is amended rather than deleted — every `StairsDown` below the last rung
still pairs with a `StairsUp` at the same coordinate, every `Drop` pairs with a
standable non-stair cell one rung down, and no `StairsUp` ever sits under a
`Drop`.

In the walk, `down` on a lip lands at the same coordinate one rung below by
0567's own coordinate pairing. `up` from a cell whose twin one rung above is a
`Drop` is refused unless the body flies, and then it lands on the lip. The
asymmetry is entirely in what `up` will do.

## Consequence

- **Valve and asymmetric are the same gate.** To a dragon the chute is
  asymmetric — free down, costly up. To everything else it is a valve — free
  down, closed. Decision 0347's object × body relation, carried into traversal;
  the taxonomy's axis is a reading, not a stamp.
- **A chute makes the return path differ, which is what it is for.** Dormans'
  "unknown return path" follows from a chute by construction, so the readout
  that counts it is report-only and was never predicted. It also forces
  solvability to be measured as a ROUND TRIP rather than a reach (decision
  0620's invariant), because a chute costs nothing on the way down and
  everything on the way back.
- **The side a chute is placed on is named by GEOMETRY, not by length.** Naming
  it by a realm's short side put it on the same-floor path under the commonest
  class, where there is no stairway: 717 of the row's 823 draws were refused for
  want of one. `Side::Descending` names the path that leaves the anchor level,
  whatever its length, and the row applies 823 of 823.
- **The refusal names its own physical reason** — no way up beneath a chute for
  a body that cannot fly — beside the deep-water and locked-door refusals, at
  the one place every underground refusal already happens.
- A same-floor valve is still not expressible, because no same-floor substance
  exists for it.

## See also

- [The Brattice design](../superpowers/specs/2026-09-02-the-brattice-design.md)
  §3.1, §3.5, §3.6.
- [The Brattice ledger](../superpowers/ledgers/2026-09-02-the-brattice.md) #10
  (ruling D).
- [The Brattice chronicle](../../book/src/chronicle/the-brattice.md).
