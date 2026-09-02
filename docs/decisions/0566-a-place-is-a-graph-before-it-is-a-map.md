# 0566. A place is a graph before it is a map, and the grammar is series-parallel

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (autopilot) ·
**Relates:** [0069](0069-fine-position-is-never-serialized.md) (a plan is
FRAME-tier, derived on entry and never stored),
[0011](0011-studies-are-data-metrics-are-code.md) (rules are authored, instances
are not), [0016](0016-studies-preregister-hypotheses.md) ·
[The Crosscut](../../book/src/chronicle/the-crosscut.md)

In the context of an underworld that was a tree or a path at every scale — a
branch is a line of levels, a level is a binary partition tree whose siblings
are joined by exactly one passage, and neither embedder can realize a cycle —
facing Dormans' observation that a branching dungeon forces constant
backtracking, we decided that **the structural primitive is the cycle, that a
descent's whole plan is grown as a graph before any level is carved, and that
the grammar is series-parallel: exactly two operations, lengthen a path and
split an edge into two paths** — accepting that the grammar can never express a
shortcut between two different cycles (a K4 minor), and accepting a *third*
rule the two operations alone could not supply, the capability invariant below.

## Context

Growing the plan first, at the scope of a whole walked descent rather than one
level, is what lets a cycle put one of its two paths on the floor below: a
stairway down, a run along the next rung, a different stairway back up. A
level generator that builds one floor at a time cannot do that at all, which is
why the plan precedes the carve rather than annotating it.

Series-parallel is not a stylistic preference. Because every operation is a
series or a parallel composition, the derivation tree *is* the realm tree, so
"which cycle owns this room" has exactly one answer; the graph is planar with a
grid embedding by construction, so there is no crossing to resolve and no
embedding that can fail; and every node stays reachable from the entrance,
because growth only ever adds paths between nodes already joined.

**The plan is keyed to the walked descent, not to a branch of the chamber
lattice.** The design as approved keyed it to a run address and said it refined
`passages_from`. Reading `Underground::enter` while writing the plan showed the
walked object is different: it builds one level per habitation rung under one
vertex and reads no run, no branch and no entrance. A plan keyed to a run would
have described a place nobody stands in. Wiring the walk to the lattice is a
real gap and a separate campaign.

**The capability invariant.** The design claimed "every level has at least one
realm of its own" held *by construction* from the two operations. It did not: a
400-seed sweep found 24 of 72,000 levels with no cycle at all, because a
cross-floor cycle anchored on level ℓ spends cells on level ℓ+1 that ℓ+1's own
loop then cannot use. A count reserve cut that to 9 — the residue was
geometric enclosure, not scarcity, two landings walling a level's passages away
from its free cells. What made the claim true is a *capability* test carried by
every move that spends a level's cells: a level with no realm of its own must
keep at least one same-floor cycle still feasible. With it, the exhaustive
fallback pass succeeds by construction and the starved count is 0 of 72,000.

## Consequence

- Cycle density, region shape, passage placement and stair placement all become
  consequences of one plan. Nothing about a level is drawn that the plan did
  not say, and two adjacent regions with no edge between them keep a solid wall
  — the property a partition tree could not express, because siblings were
  always joined.
- **Per-level connectivity is no longer a guarantee.** A cross-floor cycle's
  landing corridor on the floor below joins the rest of that floor only through
  its stairs. Whole-*descent* connectivity is the guarantee, and the tests that
  asserted the per-level form were rescoped to it rather than weakened.
- The invariant costs about 2.5 points of cross-floor realm share (29.8% →
  27.3%) and nothing else measurable. The cross-floor readout has roughly four
  times the headroom it needs, so the trade is cheap.
- `LengthClass`, `depth` and `realm` are computed and exported and read by
  nothing here. The Brattice reads the length class to select a gate pattern;
  The Plat reads depth and realm as Alexander's intimacy gradient and
  circulation realm.
- A campaign wanting a shortcut between two different cycles, or a cycle
  through a junction into another cave system, needs a different grammar or a
  third composition rule — not a bigger budget.

## See also

- [The Crosscut design](../superpowers/specs/2026-09-01-the-crosscut-design.md)
  §2, §3.1–3.4.
- [The Circuit metaplan](../superpowers/specs/2026-09-01-the-circuit-metaplan.md)
  §2 — the three commitments this decision is the first of.
- [The Crosscut chronicle](../../book/src/chronicle/the-crosscut.md).
