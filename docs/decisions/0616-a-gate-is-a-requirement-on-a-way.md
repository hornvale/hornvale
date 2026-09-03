# 0616. A gate is a requirement on a way, realized as four parts kept apart

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (G3) / autopilot ·
**Relates:** [0347](0347-an-affordance-is-a-relation-not-a-property.md)
(an affordance is a relation between object and body),
[0396](0396-a-passage-is-a-thing-and-openness-is-its-fold.md) (a passage is a
thing and openness is its fold),
[0398](0398-a-capability-nothing-can-reach-is-not-a-capability.md),
[0566](0566-a-place-is-a-graph-before-it-is-a-map.md),
[0567](0567-stairs-pair-by-coordinate.md),
[0583](0583-the-witness-limits-list-is-open-not-closed.md) (the realization
witness) · [The Brattice](../../book/src/chronicle/the-brattice.md)

In the context of putting locks, valves and capability gates on a
series-parallel descent plan whose realms already carry a length class, facing
the question of where a gate *lives* when it is at once a rule, a place, an
object and a refusal, we decided that **a gate is a `Requirement` on a WAY —
one per direction of an edge — stamped on the FINISHED plan by a pass that runs
after growth, and realized as four co-located parts the design keeps apart: the
requirement on the plan, the place as a cell kind, the object as a Thing
carried on the wire as a mark, and the judgment in the walk** — accepting that
the walk never reads the plan, so a realization witness test is required rather
than optional, and accepting that "valve" and "asymmetric" are not properties
of a gate at all but one gate seen by two bodies.

## Context

Three facts about the shipped code decided the shape before any preference did.

**The plan has no attribute slot, and its grammar rewrites its own edges.** The
lengthening move deletes a passage and splices a chain into every realm path
that carried it, so an attribute keyed on an edge *during* growth is orphaned
by the next move. Gates are therefore stamped on the finished plan, by a pass
that touches no edge the grammar might still rewrite. A plan with every gate
ignored is exactly decision 0566's plan, which is what let this be added
without migrating anything.

**A door is an object at a threshold, never a cell kind.** The first draft of
the design proposed a `Door` variant of the level's cell kind, which is a
*place* named after an *object*. The building lattice already draws that line —
`CellKind::Threshold` is a designed opening and "a window is an ANCHOR at a
wall cell, never `CellKind::Window`" — and a `Door` glyph would have painted
built doorways across a wild cave. So EVERY passage records a crossing cell,
gated or not, and that cell is a `Threshold` — in a cave a squeeze, in a
building a doorway — except where the passage is a sump, whose crossing is
`Deep`, since one cell has one kind and a drowned squeeze is drowned (execution
amendment, ledger #9). A door is a Thing anchored at a threshold, and it travels
on the wire as a mark rather than as a palette kind. A threshold with no door is
a squeeze and needs no Thing.

**Valve and asymmetric are one gate seen by two bodies.** Decision 0347 already
says an affordance is a relation between an object and a body. A chute is
asymmetric to a dragon (free down, `Fly` up) and a pure valve to everything
else. Stamping "valve" on the edge would record a relation as a property, and
would have to be re-decided for every future body.

## Decision

`Edge` gains `gate: Option<Gate>`; a `Gate` carries a `Way` per direction
(`Open` or `Needs(Requirement)`), plus `hazard` and `persistence` stamps that
nothing reads. `Requirement` has exactly two kinds — `Key(NodeId)` (what you
hold) and `Mode(Capability)` (what you are). There is no `Shut` way: nothing in
a cave is impassable one way for *every* body, so the variant would be a
reserved seam with no constructor, the shape decision 0398 refuses.

The four parts and their homes:

| part | home | this campaign |
|---|---|---|
| requirement | the plan's edge | `Gate { toward_a, toward_b, … }` |
| place | the level's cell kind | `Threshold`, `Deep`, `Drop` |
| object | the ledger | a `door` Thing, a `key` Thing; a mark on the wire |
| judgment | the walk | an actor-aware seam beside `movement_mode` |

## Consequence

- **The realization witness is mandatory** (the discipline of 0577's successor
  line, 0583). Requirement and judgment are the one non-independent pair — the
  walk reads the rock and the ledger, never the plan — so solvability proved on
  the plan is a proof about the walked level *only* while a test asserts, in
  both directions, that each edge realizes exactly what its requirement names
  and that nothing of the kind exists that no requirement asked for.
- **`hazard` and `persistence` are stamped and read by nothing**, exactly as
  decision 0566's plan exported a length class it never read. Danger needs a
  creature to place; secrecy needs a render seam; a collapsing gate needs a
  crossing event no fact records. Each is a captured idea, not an empty variant.
- **A door onto rock stays admissible.** The Thing's anchor rule is deliberately
  no tighter than the building lattice's — an anchor may sit at a wall cell — so
  the tomb's false door needs no new class later.
- **Two seams, not one widened.** `movement_mode(kind)` keeps its signature and
  answers "how does this cell want to be crossed"; a second, actor-aware seam
  answers "may THIS body cross from here to there". The corner rule keeps asking
  the geometric one: a threshold is an opening in the wall whether a door in it
  is shut or not.
- The pass cannot gate an edge outside a realm (a gate on a bridge is
  unsolvable for a body holding nothing by definition), place two keys for one
  lock, or make a same-floor passage one-way.

## See also

- [The Brattice design](../superpowers/specs/2026-09-02-the-brattice-design.md)
  §3.1, §3.5, §3.6, §3.7.
- [The Brattice ledger](../superpowers/ledgers/2026-09-02-the-brattice.md) #2, #7.
- [The Brattice chronicle](../../book/src/chronicle/the-brattice.md).
