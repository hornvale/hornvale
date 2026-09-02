# 0567. Stairs pair by coordinate: a stairway's two ends share a cell

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (autopilot) ·
**Relates:** [0102](0102-one-per-cell-was-an-index-artifact.md) (a place, never
an ordinal), [0566](0566-a-place-is-a-graph-before-it-is-a-map.md) ·
[The Crosscut](../../book/src/chronicle/the-crosscut.md)

In the context of a level gaining several stairways instead of exactly one down
and one up, facing the question of how a descent verb finds the landing at the
far end of the stairway it is using, we decided that **a stairway's two ends
share a coordinate: a `StairsDown` at cell `c` on rung ℓ pairs with the
`StairsUp` at the same cell `c` on rung ℓ+1, and that is how a landing is
found** — accepting that the realizer must draw every stair inside the
intersection of the two rungs' extents, and must make a landing cell walkable
even where the carve left it rock.

## Context

The shipped code relied on there being exactly one stair of each kind per
level: `place_connections` cut one `StairsDown` in the first partition leaf and
one `StairsUp` in the last, a test asserted that count across 200 seeds, and
`peek_stairs` used the count to know where a stairway landed. Several stairways
per level breaks all three at once.

The rejected alternative was an **ordinal** pairing — the k-th stair down on ℓ
pairs with the k-th stair up on ℓ+1. It is the id-as-offset bug one scale up,
the exact defect decision 0102 was minted for: inserting a stairway anywhere
re-rolls every landing after it, so a change to one part of a level silently
relocates arrivals everywhere else. A coordinate is a place, and a place does
not move when a neighbour is added.

`peek_stairs`'s own doc comment already described its result as "the same
physical stairway, named from its other end". Coordinate pairing is that
sentence made true, rather than a new convention.

Rung extents grow with depth but share their origin, so the intersection of two
adjacent rungs' extents is non-empty and contains the whole shallower rung.
That is asserted for every rung pair the ladder admits, as a test, not assumed.

## Consequence

- `stairs_down_and_stairs_up_never_share_a_cell` is **replaced, not deleted**:
  its successor asserts that every `StairsDown` on rung ℓ has a `StairsUp` at
  the same coordinate on rung ℓ+1 and the converse, over the whole habitation
  ladder rather than a two-rung stub. (An earlier draft of this bullet also
  claimed the successor asserts "no cell carries both"; that assertion was
  vacuous — a cell holds exactly one kind — and the final review deleted it.)
- **A coordinate belongs to at most one stairway.** Pairing by coordinate only
  identifies a landing if the coordinate identifies a stairway, and a node on a
  middle rung is the upper end of one stairway and the lower end of another. So
  a stair coordinate is ONE draw into the two regions' intersection MINUS the
  coordinates the endpoints' other stairways already hold. Drawn as two
  independent `x`/`y` indices, as the first implementation did, two stairways
  collided on ~9% of five-rung descents and the realizer wrote one over the
  other.
- The stair coordinate is drawn at plan time, so the level realizer spends no
  draw of its own placing one.
- A landing is set walkable even if the carve left it rock — a stairway has a
  foot. Writing a stair onto a cell can sever the region it lands in, so the
  realizer repairs connectivity *within that region's own rectangle* after
  every stair write; repairing at level scope would carve through walls the
  plan drew deliberately (see the chronicle).
- The deepest rung keeps its dangling `StairsDown` and
  `STAIRS_LEAD_NOWHERE_REFUSAL` keeps its one firing case, unchanged.
- The asymmetric twin — a drop, down with no up — is now expressible as
  *omitting the up half of a pair*, which is what makes it a valve rather than
  a special case. It belongs to The Brattice.

## See also

- [The Crosscut design](../superpowers/specs/2026-09-01-the-crosscut-design.md)
  §3.3.
- [The Crosscut chronicle](../../book/src/chronicle/the-crosscut.md).
