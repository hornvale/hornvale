# The Connection Graph — Campaign 2, Slice 1: The Legible Transport Substrate

**Status:** design (G3 review)
**Program:** The Living Community engine (campaign 2 of ~5)
**Slice:** the transport graph, derived + legible — no epoch.

---

## 1. The payoff

The world's real transport topology, revealed. Where the sea's currents carry a
boat between two coasts, a **sailing lane**. Where the land offers a low pass or a
river valley between two regions, a **natural route**. Where mountains and open
ocean forbid passage, a barrier. You can look at the map and read how the world is
*connected* — which places are a short hop apart despite the distance (a coastal
highway), and which are isolated behind a wall of peaks. That isolation is not just
scenery: it is the seed of divergence — the peoples history stranded behind the
mountains are the ones who will drift apart.

This slice builds that graph from real geography and makes it legible. It does not
yet change the dynamics (C1's displacement still runs on raw adjacency); it
establishes and reveals the substrate the rest of the program will run on.

## 2. Context — the program, and the keystone reframe

The Connection Graph (MAP-61) is a multi-slice campaign: (1) *this* — the derived,
legible substrate; (2) the dynamics run on it (diaspora over graph-distance — an
epoch); (3) diffuse coupling (trade/culture/disease as a fixed-point over the
graph); (4) conductance gating (seasonal lanes, intermittent portals); (5)
built roads + portals + player travel. C1 (history-first placement, shipped) displaces
peoples over raw cell-adjacency; this campaign gives that displacement a real
topology to follow, later.

**THE KEYSTONE REFRAME (from the ideonomy pass, materiality):** this slice derives
the terrain's **conductance — its natural traversability**, NOT *built roads*. The
registry's "a road = a baked-history event" is a *later* layer (society's marks,
requiring history × graph co-evolution). A slice-1 land edge means *the land permits
travel here* (a pass, a valley), never *a society built a road here*. Framing them
as "roads" would be dishonest — implying a builder that does not exist yet. So the
edges are **natural routes**: water routes (the sea carries you) and land routes
(the land lets you through). Honest legibility of real geography — which is why it is
not the annotation trap C1 warned against (we reveal real structure, not narrate over
a fabricated one).

## 3. Architecture (constitutional layering)

- **`domains/topology`** (new; kernel-only) — owns the graph TYPE (`ConnectionGraph`:
  nodes = `CellId`, typed edges with a conductance weight) and the pure graph
  operations: graph-distance, reachability / connected-components (isolation), and
  least-cost route-finding via `hornvale_kernel::astar` (The Foresight). No terrain,
  no climate — a domain depends only on the kernel.
- **`windows/worldgen`** (composition root) — the **derivation** lives here, because
  it reads terrain (the land traversal-cost field) and climate (the ocean-current
  field): `connection_graph(geo, terrain, currents, settlements) -> ConnectionGraph`.
- **`windows/*` legibility** — the scene emits the graph's edges (the client draws
  lanes/routes); the almanac renders a site's connections + its reachability
  ("linked by sea-lane to …, by land route to …; isolated from … behind the peaks").

**Derived, never committed.** The graph is computed on demand from the world, like a
field — NOT written to the ledger. So every world stays **byte-identical** (no epoch,
no artifact or census churn). Slice 2's dynamics will consume the same derived graph.

## 4. The graph model

`ConnectionGraph` over `CellId` nodes, with typed edges each carrying a conductance
(a dimensionless ease-of-travel weight, higher = easier):

- **Adjacency** — the geosphere's local neighbours (baseline), conductance from
  terrain (flat land high, mountains low, open water impassable by land).
- **Water route (sailing lane)** — a long-range edge between two coastal cells the
  ocean-current field connects: follow the current from a coastal cell until it
  reaches another coastal cell; that pairing is a lane, conductance from current
  strength.
- **Land route (natural)** — a long-range edge between two nearby settlements whose
  least-cost `astar` path over the terrain traversal-cost field is cheap enough to be
  a real corridor (a pass, a valley); conductance from the path cost. NOT a built
  road.

### 4.1 The terrain traversal-cost field

A per-cell cost derived from terrain: low on flat/lowland cells, rising with slope
(elevation gradient to neighbours), very high on peaks, impassable on open ocean (for
land routes). Reuses committed terrain elevation; no new seed draw. This field is the
weight `astar` plans over.

### 4.2 Reachability / isolation

Connected-components over the graph (thresholded by a minimum conductance) partition
the world into regions reachable from one another. A region cut off by mountains/ocean
is *isolated* — surfaced in the legibility layer as the structural prediction it is.

## 5. Cost bound (the size risk — measure-don't-narrate)

Least-cost land-route finding is `astar` between settlement pairs — naively
O(settlements²). It MUST be bounded: only attempt routes between settlements within a
bounded hop/distance radius (nearby pairs), and cap the `astar` frontier. A
preregistered **cost gate** measures the graph-derivation wall-time and the route-
attempt count on the base world and asserts they stay within budget (the Sounding's
lesson: bound it and measure it, do not let it blow up).

## 6. Determinism

Same seed + pins → byte-identical graph (the derivation is a total function of the
committed world). `astar` route-finding is deterministic (BTree frontier, total
`f64::total_cmp` tie-breaks, no HashMap/RNG — The Foresight's keystone). No wall-clock;
graph weights quantize only at any emit boundary.

## 7. Non-goals (§9 — read before assuming scope)

NOT built roads (society's historical marks — a later slice; slice 1 is natural
traversability only). NOT portals (magic/astronomy-gated — later). NOT conductance
gating in time (seasonal lanes, intermittent portals — later). NOT dynamics use — C1's
bake is UNCHANGED and byte-identical; the diaspora-on-routes rewire is slice 2 (the
epoch). NOT diffuse coupling (trade/culture/disease fixed-point — a later slice). NOT
the relational social graph (SOC-9 — a different graph).

## 8. Success criteria / DoD

- `ConnectionGraph` derived deterministically from the base world (byte-identical
  across runs); nodes = cells, edges = adjacency + water routes + land routes with
  conductance.
- Reachability/isolation computed (connected components); at least one seed shows a
  genuinely isolated region.
- The cost gate passes (derivation within budget; route-attempts bounded).
- Legibility: the scene emits the edges (drawable) and the almanac renders a site's
  connections + isolation. A seed-42 gallery page shows the transport topology.
- C1's worlds remain byte-identical (no epoch; the census does not move).
- DoD per CLAUDE.md: chronicle, retrospective, book freshness sweep, registry flip
  (MAP-61 → elaborated/slice-1-shipped, repoint Where), full gate + artifact drift.
  No census regen (byte-identical), no keystone refreeze beyond drift.
