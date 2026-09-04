# 0646. The inhabited reading is a function of the plan, drawn by nothing and stamped on nothing

**Status:** Accepted (2026-09-03) · **Decider:** Nathan (G3) / autopilot ·
**Relates:** [0566](0566-a-place-is-a-graph-before-it-is-a-map.md) (a place is
a graph before it is a map — this reads the `depth` and `realm` it exported),
[0568](0568-cycle-density-is-derived-not-authored.md) (cycle density is derived
from rock and workmanship),
[0618](0618-a-descent-key-makes-the-plan-a-save-format-contract.md) (the plan
grammar is a save-format contract — not engaged, and §Context says why),
[0620](0620-the-cycle-pattern-inventory-is-a-frozen-corpus.md) (the pattern
inventory is frozen — a heart PATTERN row would have been an epoch under it),
[0011](0011-studies-are-data-metrics-are-code.md) ·
[The Plat](../../book/src/chronicle/the-plat.md)

In the context of wanting Alexander's intimacy gradient, common heart and
staircase-as-a-stage on the underworld's descent plan, facing the choice
between *making* those shapes occur (a pattern row that the grammar draws) and
*reading* them off the shape that already occurs, we decided that **the
inhabited reading is a pure function over a finished `DescentPlan` — one
`Role` per node (`Entry`, `Heart`, `Sanctum`, `Chamber`), a `landing` flag on
some, and a realm-nesting `rank` — computed for every plan, wild or worked,
reading no seed, no vertex, no terrain and no ledger** — accepting that the
project therefore never guarantees a level *has* a good hall, only that it can
name the best one the rock and the draws happened to leave.

## Context

Decision 0566 exported `Node.depth` and `Node.realm` and read neither; its own
text says "The Plat reads depth and realm as Alexander's intimacy gradient and
circulation realm." Two shapes could have discharged that.

The first is a **pattern row**: add a hub-and-ring row to the frozen cycle
inventory so the grammar draws a central hall. That is a change to pattern
selection, which decision 0620 makes an epoch, and it is unnecessary on the
evidence: a probe over 1,200 plans found that *every* level already has a node
of degree ≥ 3 and the typical level has three to six. The row would have named
what already occurs while costing a version suffix.

The second is a **reading**, which is what this record ratifies. It adds no
draw, consumes no stream and rewrites no edge, so the plan is byte-identical
and 0618's epoch is not engaged. The cost is that a level with a poor center
of gravity gets a poor hall named rather than a good one built.

The heart's rule needed a measurement and then a correction of the
measurement. "The hub" was refused because it underdetermines (three to six
candidates per level). Alexander's own criterion for pattern 129 — "the center
of gravity of all the spaces the group occupies" — has an exact graph reading,
the **median**, and that is what shipped. The first draft summed distances
*within* a level; drawing one real level showed a four-node island reachable
only through the floor above, which that metric crowns by arithmetic. Measured
after the correction: a level's within-level graph is one connected piece on
only 23% of karst levels, 33% of fracture and 58% of lava tube.

## Decision

`windows/worldgen/src/plat.rs` exposes `read(plan: &DescentPlan) -> Reading`,
total and deterministic, with three vectors parallel to `plan.nodes`.

Per level, over that level's nodes:

- **Entry** — least `depth`, ties to the lower `NodeId`.
- **Sanctum** — greatest `depth`, ties to the lower `NodeId`. Never the Entry:
  a level's depth range was ≥ 2 on 100% of 6,000 levels measured.
- **Heart** — among the level's nodes **with the Entry excluded**, the node
  minimizing the sum of shortest-path distances to the level's other nodes,
  **where paths run through the whole plan, stairs included**. Ties: higher
  degree, then lower depth, then lower id. `None` where the median is the
  Sanctum, or where no candidate remains.
- **Chamber** — every other node. Its intimacy is its own `depth`; the reading
  adds no second number.
- **landing** — true where a node is the lower end of one stair and the upper
  end of another (Alexander 133).
- **rank** — the nesting depth of `node.realm` through `Realm.parent`, 0 on a
  spine node (Alexander 98).

**The Entry is excluded by rule, not by prediction.** Alexander 112 says there
is always a transition between the outside and the heart. Without the
exclusion the median lands on the arrival node on 22–29% of levels.

**The metric is the plan's, not the level's.** A resident crossing between two
islands of one floor does walk up a stair and down again.

The reading is computed for **every** plan; only its *vocabulary* is keyed to
a made rung (0647, 0649). It reads nothing but the plan, which is what lets a
later campaign lift it over a surface district graph.

## Consequence

- **The plan grammar does not move, and 0618 is not engaged.** No draw, no
  selection change, no edge rewrite. The four readouts of 0566 and the four of
  0616–0620 are byte-identical across this campaign.
- **The heart is a measured thing, so it can be absent.** `Heart` is `None` on
  0.0–0.1% of levels overall and 6.9% of the small lava-tube levels. A level
  with an entry, a sanctum and no hall is a legal reading, and the prose says
  nothing rather than naming a hall that is really the innermost room.
- **A summary statistic could not have caught the metric error, and a drawing
  did.** The within-level metric produced unremarkable numbers. One rendering
  of one real level made the island visible. Recorded because the instrument
  is cheap and was nearly not used.
- **The pattern route stays available and stays an epoch.** If a future
  campaign wants to *guarantee* a hall rather than name one, that is a row in
  the frozen inventory and a version suffix, under 0620.
- The reading is `FRAME`-tier under 0069: derived on entry, discarded on exit,
  never a fact subject.

## See also

- [The Plat design](../superpowers/specs/2026-09-03-the-plat-design.md) §3.1,
  §4.1.
- [The Plat ledger](../superpowers/ledgers/2026-09-03-the-plat.md) #2, #2
  addendum, #3.
- [The Plat chronicle](../../book/src/chronicle/the-plat.md).
