# The Transport Topology of Seed 42

The connection graph's legibility surface: a site's natural sea-lanes and
overland routes, and which of the world's naturally-connected regions it
belongs to, read off the `connections` verb. Nothing here is authored
infrastructure -- a "route" is always a natural corridor the terrain and
currents make easy, never a built road (see `EdgeKind`). The graph itself
is purely derived (no epoch, no seed draw): the same world always yields
the same topology.

## A well-linked capital

The flagship settlement, on the world's largest connected landmass. Its
own overland routes reach two neighboring settlements directly.

```text
The connections of vertex 13980
===============================

vertex 13980 opens onto no sea-lane and no natural overland route of its own: whatever reaches it must cross open country, adjacency by adjacency.

vertex 13980's region holds only 1 vertex -- cut off from the wider world: no route this graph knows of crosses the gap that separates it from the largest region (5550 vertices). 25 real regions divide the known world in all.
```

## A hub on a different shore

Vertex 28435 sits on a *separate* landmass under natural travel -- close
enough to its neighbors to reach several by both sea-lane and land route,
but with no natural corridor at all bridging it back to the flagship's
larger region.

```text
The connections of vertex 28435
===============================

vertex 28435 opens onto no sea-lane and no natural overland route of its own: whatever reaches it must cross open country, adjacency by adjacency.

vertex 28435's region holds only 1 vertex -- cut off from the wider world: no route this graph knows of crosses the gap that separates it from the largest region (5550 vertices). 25 real regions divide the known world in all.
```

## The world, in sum

The world-level reachability summary: how many real regions natural
travel divides this world into, the largest, and the rest.

```text
The reach of the map
---------------------

Natural travel divides the known world into 24 real regions (below 2 vertices, a "region" is just an island vertex no sea-lane reaches -- not counted here). The largest spans 5550 vertices; the rest, smaller and cut off from it, run 1876, 1654, 914, 638, and 417 vertices -- plus 18 smaller still.
```
