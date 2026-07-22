# The Connection Graph

*The routes were always there; this campaign only reads them off the map.*

The Living Community gave the world a history: settlements grown, not placed,
their peoples displaced across a raw cell-adjacency grid whenever the climate
turned against them. That grid answers *can you get from here to there at
all* — every step costs the same. It cannot answer the more interesting
question: *how easily*. A mountain range and a coastal plain are the same
single hop under bare adjacency, which is not how travel, trade, or
displacement actually work. This campaign — the second of the
living-community program's five — builds the structure that knows the
difference: a **connection graph** derived from the world's real geography,
legible on the map and in the almanac, that says which places are a short hop
apart despite the distance, and which are cut off behind a wall the world
itself put there.

This is a first slice, deliberately. It establishes and reveals the
substrate; it does not yet change how anything moves. The Living Community's
displacement bake is untouched, byte-identical, still walking raw adjacency.
The graph is additive legibility on top of an unmodified world — the next
campaign in the program is the one that routes displacement, trade, and
conflict over it.

## The graph itself

Three kinds of edge, each carrying a *conductance* — a dimensionless
ease-of-travel weight, higher meaning easier:

- **Adjacency** — the geosphere's bare mesh neighbours, weighted by the
  terrain's own traversal cost (flat and low is cheap, steep and high is
  dear, open ocean is impassable to a walker).
- **Water routes** — sailing lanes. Starting from a coastal cell, a trace
  follows the world's existing ocean-current field, current by current,
  until it reaches another coast; that pairing becomes an edge, its
  conductance the current's strength. The currents were already part of the
  world (the weather program's C2 slice, The Gyre); this campaign is the
  first thing that reads them as a travel network rather than a climate
  input.
- **Land routes** — natural overland corridors. Between settlement pairs
  close enough to be worth checking, a least-cost search over the terrain's
  traversal-cost field (the kernel's deterministic `astar`, the same
  pathfinder the drive-and-goal rung proved out) finds the cheapest path; if
  it beats a plausible-corridor ceiling, it becomes an edge. This is a pass
  through a range, a valley along a river — the easiest ground the land
  itself offers, not a road anyone built.

All three, plus the reachability computed over them, are **derived**, not
committed. Nothing about this campaign writes a new fact, mints a new
predicate, or opens a save-format epoch. The graph is a pure function of
already-committed geography — terrain, currents, settlement cells — computed
on demand exactly like any other field. Ask for it and it appears; never ask,
and the world is unaffected. Every world The Living Community already built
stays byte-identical.

## The keystone reframe: natural routes, not built roads

The most important decision this slice made was what to call its own land
edges. The registry had already sketched a road as "a baked-history event" —
something a society lays down over time, the residue of who travelled where
and how often. Calling this slice's land corridors "roads" would have quietly
smuggled in a claim the model does not support: that anyone built them. They
are not built. They are the land's own *conductance* — where the terrain and
the currents make travel easy, independent of whether anyone has ever walked
there. A land route through an empty, unpeopled valley is exactly as real
as one through a well-worn pass; the graph doesn't know the difference,
because there isn't one yet.

So the vocabulary throughout is deliberately narrower than "road": a
**sailing lane** where the sea carries you, a **natural route** where the
land lets you through, never "the road to X." This is not pedantry — it is
the same discipline The Living Community itself learned the hard way,
distinguishing a spine (real structure the sim measures) from an annotation
(prose narrated over nothing). Built roads, a genuine later layer, will need
history and the graph to co-evolve — society choosing to formalize the routes
it actually uses. That is explicitly deferred, not forgotten.

## What the terrain taught: ocean is the real separator, not elevation

Building the fixture that would prove a mountain range actually isolates two
settlements turned up a finding the design did not anticipate. The first
attempt put a ring of high elevation around a region with a single low gap —
a ridge with a mountain pass, the obvious way to model "hard to reach, but
not impossible." On the icosphere mesh, this does not work as a separator at
all: elevation's cost is a *symmetric* tax on crossing a boundary, so a cell
bordering the ridge costs about the same whether it sits just outside the
ring or just inside it. With one gap open anywhere in the ring, a
great-circle path can dip through it early and then travel entirely within
one hemisphere for the rest of its length — the crossing point costs nothing
extra wherever along the route it happens to fall. A ring with a single
breach is, path-length-wise, topologically identical to no ring at all.

The fix was to stop treating elevation as a separator and start treating
**open ocean** as the only hard one. Ocean cells are not merely expensive to
cross — the least-cost search excludes them from its search space entirely,
the same way a walker simply cannot step onto open water. A settlement
surrounded by ocean except for a single narrow, elevated isthmus really is
isolated, because there is exactly one route off it and it is expensive by
construction, not by search luck. This is, on reflection, the correct model
of the real world too: mountain ranges have passes drovers use every season;
oceans, before a hull exists to cross them, do not. The graph's isolation
findings below all trace back to this: it is the sea, not the peaks, doing
the separating.

## Isolation predicts divergence

Running the derivation over a real seed-42 world resolves it into ten real
naturally-connected regions (below two cells, a "region" is just an
otherwise-unreachable island — not counted). The largest holds thirty-five
hundred cells and contains the world's flagship settlement, itself linked to
two neighbours by direct land route: a well-connected capital in a
well-connected heartland, exactly as expected.

The more interesting case sits on a separate shore entirely. Mjoexaenoenoa, a
hobgoblin settlement of a little over a hundred souls, is *locally* thriving
under natural travel — three sailing-lane destinations and four land-route
neighbours, more connections than the flagship itself has. And it is
*globally* isolated: no natural route this graph finds crosses the gap
between its own eighteen-hundred-cell region and the world's largest. Both
things are true of the same place at once, and the distinction matters. A
site can look locally busy — a real local network of coasts and passes — and
still be cut off from the wider world in every direction that counts. That
is not a data artifact; it is the geography a real, sparsely-connected planet
actually produces, and it is exactly the structural precondition the
program's third campaign needs: a people stranded behind a gap like this one
is a people who will drift, culturally and genetically, from everyone on the
other side of it. Isolation revealed now is divergence predicted for later.

## What this slice is, and is not

It delivers the graph type and its pure operations (`domains/topology`,
kernel-only — the pathfinder, the reachability computation); the real-geography
derivation at the worldgen composition root (adjacency, sailing lanes, bounded
land routes); a measured, budgeted cost gate on the land-route search (the
size risk the design flagged up front, checked rather than assumed); and a
legibility surface — a site's connections and isolation status rendered in
the almanac and the CLI, with a seed-42 gallery page showing both a
well-linked capital and an isolated hub side by side.

It does **not** deliver built roads (a later, history-coupled layer), portals
(magic- or astronomy-gated, later still), conductance that changes with the
seasons or the sky, or any change to how displacement, trade, or conflict
actually move. The Living Community's bake runs exactly as it did before this
campaign started; every existing world stays byte-identical. What this slice
establishes is the substrate the program's remaining campaigns — routing
displacement over real distance, diffuse coupling for trade and disease,
conductance gated by time — will need before any of them can be built
honestly.
