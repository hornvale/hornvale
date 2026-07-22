# The Transport Topology — Seed 42

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
The connections of Qvooshtvoagootao
===================================

A natural route runs to Vngooshtvoavngoashgootao and Shngooshvngooshqvaagootao, by land, over the easiest ground the terrain allows -- a pass, never a paved road.

Qvooshtvoagootao sits within the largest connected stretch of the known world (3525 cells, the largest of 10 real regions the map resolves into) -- well-linked, nothing here is stranded.
```

## A hub on a different shore

Cell 28435 sits on a *separate* landmass under natural travel -- close
enough to its neighbors to reach several by both sea-lane and land route,
but with no natural corridor at all bridging it back to the flagship's
larger region.

```text
The connections of Mjoexaenoenoa
================================

Mjoexaenoenoa is linked by sea-lane to Nekxngadnoenoa, cell 28374, and Vngetxngetnoenoanoaboo -- a current-borne crossing, not a road.
A natural route runs to Njoeqteakboanoenoanoagoo, Vngetxngetnoenoanoaboo, Qgeavoonoagoo, and Tgeongjoenoenoanoagoo, by land, over the easiest ground the terrain allows -- a pass, never a paved road.

Mjoexaenoenoa's region holds only 1977 cells -- cut off from the wider world: no route this graph knows of crosses the gap that separates it from the largest region (3525 cells). 10 real regions divide the known world in all.
```

## The world, in sum

The world-level reachability summary: how many real regions natural
travel divides this world into, the largest, and the rest.

```text
The reach of the map
---------------------

Natural travel divides the known world into 10 real regions (below 2 cells, a "region" is just an island cell no sea-lane reaches -- not counted here). The largest spans 3525 cells; the rest, smaller and cut off from it, run 1997, 1977, 1281, 1280, and 831 cells -- plus 4 smaller still.
```
