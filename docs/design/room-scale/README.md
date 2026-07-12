# Room-Scale Variety

Design exploration: how to give an Earth-sized Hornvale world enough
**room-scale** variety and value to defeat the "1,000 bowls of oatmeal" sameness
problem, favoring algorithms and combinatorial generators over hand-authored
content. Ideation altitude — pre-spec, feeds future campaigns.

Context: a world is ~Earth-sized (10,242 level-5 geodesic cells, each ~240 km
across); a "room" is a lazily-subdivided sub-tile of one cell, ranging from a
small enclosed space to a small patch of wilderness distinct from its neighbors.

## Read this first

- **[synthesis.md](synthesis.md)** — the consolidation. Reduces the 60 ideation
  mechanisms to **8 computational primitives** (+ 2 cross-cuts), each specified
  with data structure, algorithm, borrowed cross-domain affordances, and its fit
  to Hornvale's constitution (determinism, layering, trace protocol, std-only).
  Includes the build order, a confidence/value registry of all 60, and the open
  questions. **This supersedes the cycles as the working technical artifact.**

## Keystone design

- **[p2-subdivision-design.md](p2-subdivision-design.md)** — first-pass design for
  **P2**, the deterministic hierarchical subdivision that defines what a "room"
  is and gates every other primitive. Grounded in `kernel/src/geosphere.rs`: rooms
  are deeper triangles of the same icosphere, addressed by `(base face + child
  path)`, so adjacency stays inside one continuous mesh and the pentagon problem
  is confined to 12 known points. Covers the addressing scheme, lazy geometry, the
  neighbor-walk crux, adaptive depth (T-junctions, still float-free), integer-only
  seeding + confining transcendentals to presentation (a cross-platform
  determinism strategy), chunking/residency and the address-partitioned ledger
  (freeze only the address; adaptive event-sourced LSM segments), concurrency &
  write authority (the chunk as a DDD aggregate root; three near-term invariants;
  untrusted-client / handoff / saga / netcode reserved for the multiplayer future),
  constitutional fit, and a validation plan. The partitioned-ledger call is reserved
  as a `Proposed` decision
  (`docs/decisions/the-room-tier-ledger-is-chunk-partitioned.md`).

## The ideation cycles (raw material behind the synthesis)

- **[cycle-01-ideonomy.md](cycle-01-ideonomy.md)** — 20 core mechanisms: the
  stratum stack, the room across time, the generation procedure, the Underdark
  graph.
- **[cycle-02-biomes-and-palimpsest.md](cycle-02-biomes-and-palimpsest.md)** — 20
  biome mechanisms across every strangeness level (extreme → engineered →
  aetheric → faerie, plus a curse/blight/undeath negative wing) and a deep dive on
  the palimpsest as a place state-machine. ~70-entry bestiary.
- **[cycle-03-the-living-layer.md](cycle-03-the-living-layer.md)** — 20 mechanisms
  for inhabitants, encounters, culture, and events: the map of the living, the
  notation of encounters, the tree of the unalive/made, the encounter periodic
  grid, the lattice of belonging.

Method: each cycle ran the ideonomy picker (Grace Kind / Gunkel) for fresh
operator/organon/dimension tuples, so the twenty ideas per cycle come from
different ideational moves rather than one default routine.

## Status of the keystone

**P2 has a first-pass design** ([p2-subdivision-design.md](p2-subdivision-design.md)).
The former hard sub-problem — neighbor adjacency across cell seams — is largely
dissolved by treating rooms as deeper triangles of the *same* icosphere (one
continuous mesh, never independent per-cell grids). The remaining first-order
risk is the integer-only triangle-quadtree neighbor walk; the recommended next
step is a throwaway kernel spike of that walk, validated against a fully-built
`Geosphere::new(7)` as the oracle.
