# 0077. Zoom in the room mesh is path truncation, and the chart may show a scale the body cannot enter

**Status:** Accepted (2026-07-26) · **Decider:** Nathan

In the context of giving a situated chart a coarse rung, facing the question
of how to aggregate a neighbourhood to a larger scale, we decided that
**a coarser view is the same builder run at a shallower depth of the same
address space** — and, separately, that **a view may render a scale the
possession verb refuses to enter** — accepting that the chart and the body
therefore disagree about what is reachable.

**Context.** A `RoomAddr` is a base icosahedron face plus a path of child
indices; a room's parent is `path[..len - 1]`. So "zoom out one rung" is not
an aggregation of cells into a supercell — it is the *same* neighbourhood
query, centred on the observer's ancestor, at depth − 1. A planned
aggregation layer disappeared when this was noticed during design rather than
during implementation. The epistemic overlay follows the same shape: a coarse
cell counts as remembered when any walked room's path is a prefix-descendant
of it, an integer prefix test rather than a fold.

**The bound is not the depth.** Zooming out is limited by
`depth − globe_level`, not by `depth`: below the canonical grid a room has no
inherited data to describe. A first implementation clamped on `depth`, which
let an internal error about the canonical grid reach the player at
intermediate rungs. The refusal belongs at the real bound and must be phrased
in the player's terms.

**The chart may outrun the body.** `enter` and `exit` — the possession's
vertical verbs — remain refused: possessing a settlement, a culture, or a
civilization is a deferred arc of its own. But *rendering* a coarser rung is a
lens, not a possession, and nothing about showing a scale implies being able
to inhabit it. We accept the asymmetry deliberately: the chart displaying a
scale the verb will not enter is the honest statement of that deferral, not a
gap in it. A player can see the shape of the larger thing before the game can
let them be it.

**Consequence.** Scale views over the room mesh cost no aggregation code, and
any future consumer gets the rung ladder for free from the address space. The
price is that the chart's reachability and the verb's reachability are
different questions, and any interface built on top must not conflate them —
a cell drawn on a coarse chart is not a place you can walk to.

Ratified at *The Purview*'s merge gate.
