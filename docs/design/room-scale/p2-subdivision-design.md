# P2 — Deterministic Hierarchical Subdivision (first-pass design)

**Status:** Design proposal; **campaign 1 (The Room Mesh) shipped the kernel
substrate 2026-07-12** — `kernel/src/room.rs` (registry MAP-28). The remaining
sections (adaptive depth, §10 chunking, §11 concurrency, the locale window) stay
design. The keystone primitive from
[synthesis.md](synthesis.md) §2 — what a "room" *is*, and the thing every other
room-scale primitive is gated on.
**Grounded in:** `kernel/src/geosphere.rs` (read in full). Terms below match that
module: `Geosphere`, `CellId`, `CellMap`, `subdivide`, `build_neighbors`.
**Spike (2026-07-12):** the first-order risk — the neighbor walk + seam gluing
(§6) — is **validated** against a fully-built `Geosphere::new(7)` oracle: all
327,680 faces' lazy neighbor sets and geometry match byte-for-byte (7,680
cross-seam links = 30 edges × 128 × 2). It came out **simpler than §6/§14
feared** — O(1), not the O(depth) up-walk/mirror-down (see §6, §13, §14.1). P2 is
buildable. A second spike then validated **adaptive depth / T-junctions** (§6),
interior *and* across base-face seams (the "one hard corner"). Only a
two-platform libm-swap fixture (§13.3) remains unspiked.

---

## 1. The requirement

Turn one ~240 km level-5 geodesic cell into an **addressable, lazily-generated,
deterministic tree of rooms** down to ~room scale (a few km² to a few hundred m²),
such that we can, for any room:

1. **Address** it stably → derive its seed.
2. **Locate** it (unit-sphere position) to sample coarse fields (biome, climate,
   elevation live on the level-5 `CellMap`).
3. **Walk** to its neighbors — the MUD exits — *including across level-5 cell
   seams*.
4. Do all of the above **without materializing the global fine mesh** (a room
   every ~1 km is ~10⁹ rooms; we can hold thousands, not billions).
5. Map a room to its **containing coarse cell(s)** so it inherits their fields.
6. Obey the constitution: determinism, save-format stability, std-only + serde,
   no `HashMap`, no wall-clock.

Adjacency across cell seams was flagged in the synthesis as *the* hard problem.
The mesh reading below largely dissolves it.

## 2. What the existing mesh gives us (and why it changes the design)

`Geosphere` is a **geodesic dual (Goldberg polyhedron)**: cells are the
*vertices* of an icosahedron subdivided `level` times; adjacency is the
triangulation edges. Three properties matter enormously here:

- **Rooms are already latent in the same construction.** The globe *is* a stack
  of triangular faces refined 5 times. Refining further (levels 6, 7, …) is the
  same operation producing the same geometry. **A room is just a triangle of the
  icosphere at a deeper level** — not a new coordinate system bolted onto the old
  one. This is the pivotal realization: we do not stitch independent per-cell
  grids (the trap), so seams never misalign, because there is only ever *one*
  subdivision.
- **`subdivide()` has a fixed, deterministic child order.** For face `[a,b,c]`
  with midpoints `ab,bc,ca`, the four children are, in order:

  ```
    child 0 = [a,  ab, ca]   (corner at a)
    child 1 = [b,  bc, ab]   (corner at b)
    child 2 = [c,  ca, bc]   (corner at c)
    child 3 = [ab, bc, ca]   (center, inverted)
  ```

  That order is *already* a de-facto save-format contract (it fixes the coarse
  mesh's vertex numbering). We adopt it as the room **addressing alphabet** — the
  digits `0..4` — so room geometry is byte-identical to "just subdividing more."
- **Vertex indices are preserved across levels** (midpoints appended, old kept).
  So the 10,242 level-5 `CellId`s remain the corners of the deeper triangulation
  — a room's coarse parents are literally three level-5 `CellId`s (§6).

The mesh is also **never serialized** — recomputed on demand. Rooms inherit that
stance for free: nothing about rooms is persisted except, where a consequence
occurs, a Fact keyed by the room's address.

## 3. The core decision

Three families were considered; the mesh reading makes the choice clear.

```
  APPROACH                         ADJACENCY ACROSS SEAMS         VERDICT
  ------------------------------   ----------------------------   ----------------------------
  A. Independent per-cell hex      independent grids must be      REJECTED — this is the
     grids (H3-style, one per          stitched at every seam;       "seam trap" the synthesis
     level-5 cell)                     they do not align; pentagons  warned about. All pain, no
                                       everywhere.                   reuse of existing geometry.
  B. Lazy global re-numbering of   one continuous mesh, but the   REJECTED — indices are
     the existing vertex mesh          vertex indexing is emergent   emergent from global
                                       from global traversal; no     traversal; not addressable
                                       closed-form per-vertex         lazily; no clean nesting.
                                       address; no nesting.
  C. Rooms = deeper TRIANGLES of   one continuous mesh; adjacency  ADOPTED — nests cleanly
     the same icosphere, addressed     is triangle-quadtree          (triangle→4), lazy, reuses
     by (base face + child path)       neighbor-finding + a small    existing geometry exactly,
                                       FIXED face-edge table;         confines pentagons to 12
                                       pentagons only at 12 points.   known points.
```

**Adopted: Approach C.** Rooms are triangular faces of the icosphere at depth
`L = 5 + d`; the coarse cell is a *derived grouping*, not the addressing root.

## 4. Addressing

```rust
/// A room = a triangular face of the icosphere at refinement depth path.len().
/// face: which of the 20 base icosahedron faces (0..20).
/// path: child index (0..4) at each refinement, from the base face down.
struct RoomAddr { face: u8, path: Vec<u8> }   // canonical form

/// Packed form for keys/seeds. face(5b) | depth(5b) | 2 bits per path digit.
/// Depth is capped so this fits a u64 with room to spare (see §9).
struct RoomId(u64);
```

- Refinement depth `d` below the coarse cell = `path.len() - 5`. The **depth-5
  prefix** of `path` identifies the coarse (level-5) triangle; the tail refines
  it. Everything is self-contained in the address — no separate coarse-face
  enumeration needed.
- Room scale by depth (Earth radius; base-face edge ≈ 7,050 km, area ≈ 25.5 M
  km²):

  ```
    depth L    edge        area          note
    5          ~220 km     ~24,900 km²    == the coarse cell scale (sanity check)
    12         ~1.7 km     ~1.5 km²       "small patch of wilderness" room
    16         ~110 m      ~6 ha          "small area / large clearing" room
    ~17        cap                        RoomId still fits u64
  ```

`RoomId` pack/unpack is a pure bijection (a frozen save-format contract, §11).

## 5. Geometry (position, lazily)

A room's position is the centroid of its triangle, computed by **walking the path
from the base face**, slerp-refining midpoints exactly as `subdivide()` does —
O(depth) work, no global mesh:

```
  fn corners(addr) -> [[f64;3]; 3]:
      (v0,v1,v2) = base_icosahedron().face(addr.face)   // 3 unit vectors
      for digit in addr.path:
          (ab,bc,ca) = (slerp_mid(v0,v1), slerp_mid(v1,v2), slerp_mid(v2,v0))
          (v0,v1,v2) = select_child(digit, v0,v1,v2, ab,bc,ca)  // the fixed table
      return (v0,v1,v2)
  fn centroid(addr) = normalize(v0 + v1 + v2)
```

`slerp_mid` is the existing edge-midpoint-then-`normalize` step, so a room built
this way is **byte-identical** to the same triangle in a fully-built
`Geosphere::new(L)`. That equivalence is directly testable (§8) and is what
de-risks the whole approach.

## 6. Adjacency — the crux, and it is now tractable

A room-triangle has **exactly three edge-neighbors**. Two cases:

- **Interior to the base face** — pure triangle-quadtree neighbor-finding. The
  center child (digit 3) neighbors its three corner siblings; a corner child's
  inward edge neighbors the center sibling, and its two outer edges neighbor
  triangles reached by **walking up the path to the least common ancestor and
  back down the mirrored path** (the standard quaternary-triangular-mesh
  algorithm; O(depth) worst case, usually O(1)). No floats — pure path
  arithmetic, hence cross-platform-exact.
- **Across a base-face edge** — resolved by a **fixed 20-face / 30-edge / 12-
  vertex gluing table** derived once from `base_icosahedron()`'s face list. An
  outward walk that runs off face `F` across edge `e` continues onto the adjacent
  face `F'` with an **orientation flip** (the shared edge is traversed in
  opposite winding). 30 edges, precomputable, tiny.

**The 12 pentagons are confined to 12 points.** In this triangle scheme the only
irregularity is the 12 original icosahedron vertices, where **5** faces meet
instead of 6. A room touches this only if its triangle has a corner *at* one of
those 12 vertices — a vanishing fraction of rooms at any depth. Their neighbor
walk closes on a 5-face vertex figure (handled explicitly in the gluing table).
This is the whole of the "pentagon problem" here — versus Approach A, where it
infects every seam.

> **Spike finding: the pentagon problem does not touch edge-neighbor finding at
> all.** Every icosahedron edge borders *exactly two* faces, so an edge crossing
> is always unambiguous (the spike asserts this over all 30 edges). The 5-face
> vertex figure only matters for *vertex*-adjacency (going around a corner); the
> MUD's three exits are *edge*-adjacency. The spike exercises the
> corner-at-a-base-vertex rooms (all-`0` paths) and they pass with no special
> case. So pentagons cost nothing for movement — they would only surface if a
> later feature needed the dual-hex's six vertex-neighbors (§9, deferred).

> This up-walk/mirror-down neighbor algorithm is the one genuinely intricate
> piece. It is well-trodden (quaternary triangular meshes / geodesic DGGS), it is
> integer-only, and it is the thing to prototype and property-test *first* (§12).
>
> **Spike finding (2026-07-12): it is O(1), not O(depth).** Represent a
> sub-triangle as an ordered triple of integer barycentric coords `(a,b,c)`,
> `a+b+c = 2^d` (the exact dyadic coords of §7). The neighbor across edge `(U,V)`
> is the lattice apex `W'` satisfying `W' + W_self = U + V` (the two triangles on
> an edge have apexes summing to `U+V`). If any coord of `W'` is negative the
> edge is a seam — re-express `U,V` on the adjacent face (preserving the
> along-edge parameter, via the base-edge table) and take the one inward apex
> there. No up-walk, no recursion, no LCA — pure constant-time integer
> arithmetic. This is the algorithm the spike validated exhaustively.

**Adaptive depth (mixed resolution) is still exact and float-free.** Rooms need
not all sit at one depth. Where the world is more interesting — settlement, a
halo, high strangeness, expressed as a deterministic *target-depth field* over
the coarse mesh — subdivision runs deeper. The geometry stays exact because a
parent triangle's edge is tiled *precisely* by `2^Δ` of its descendants' edges
(child 0's `a–ab` and child 1's `ab–b` cover the parent's `a–b`, recursively). So
a coarse room borders exactly `2^Δ` finer rooms across a side where its neighbor
is `Δ` levels deeper — a **T-junction**, not a mismatch. This is the
*restricted-quadtree* adjacency structure: one-to-many, irregular, and **entirely
path arithmetic** — no floats, no re-negotiation. The world becomes a graph, not
a grid, which is exactly what a MUD wants. Two rules keep it clean:

- **Canonical depth is a pure function of the world** (the target-depth field), so
  both sides of any seam agree on who is subdivided without shared state. Player
  "zoom" (§9) is a *transient view* below canonical depth — never a change to the
  persistent graph.
- **Correctness is independent of what is loaded** — a room computes a neighbor's
  address and target-depth even if that neighbor is not resident, so adaptive
  depth survives chunking (§10) untouched.

> **Spike finding (2026-07-12): validated, interior and cross-seam.** A second
> throwaway prototype ran a deterministic per-coarse-cell target-depth field over
> the mesh and checked adaptive adjacency against a fine-mesh oracle: a coarse
> room across a T-junction sees exactly `2^Δ` finer rooms, each finer room sees
> the one coarse room, adjacency is mutual, and the lazy result matched the oracle
> on every leaf pair — including T-junctions that cross a base-face seam. Two
> resolution rules: (a) a room's canonical depth is `target_depth(coarse_cell)`,
> evaluated independently on both sides of a seam (no shared state); (b) across an
> edge, if the neighbour region is coarser, take the coarser ancestor (one
> neighbour); if finer, enumerate the `2^Δ` descendants of the same-depth mirror
> whose edge lies on the shared segment.
>
> **Implementation caveat the spike surfaced.** The target-depth field is a
> *discrete* classification (`hash(coarse_cell) mod k`), and coarse-cell corner
> coordinates are all multiples of the coarse step — hence `≡ 0` modulo small
> powers of two. A weakly-mixed hash (plain FNV) then leaves its **low bits**
> unperturbed by those coordinates, and `mod k` reads exactly those bits, silently
> collapsing the field to depend only on the base face. This is the same
> discrete-threshold-on-low-bits hazard §7/§8 warn about; the field's hash **must
> avalanche** (e.g. a splitmix64 finalizer) before the `mod`. A degenerate field
> still *passes* an adjacency-equality test (both sides share it) — so the field's
> non-degeneracy needs its own assertion.

## 7. Coarse-field inheritance

A room does not re-derive biome/climate from scratch — it **inherits from its
three level-5 corner cells** and refines locally:

- The room's **depth-5 ancestor triangle** has three corners that are level-5
  `CellId`s (vertex indices are preserved across levels, §2). Computing them is a
  by-product of the path walk (record the three vertex indices at depth 5).
- The room centroid's **barycentric coordinates** within that ancestor triangle
  give three weights — and, read straight off the integer path, they are **exact
  dyadic rationals** `(i, j, k)/2^d`, so the blend needs no transcendentals.
  Coarse fields sample as the weighted blend of the three `CellMap` values, e.g.
  `biome_field = Σ wᵢ · coarse.get(cellᵢ)` (scalar fields; categorical fields take
  the max-weight corner or blend then re-classify — §14). This is exactly a P1
  leaf field; room-depth noise, seeded by the integer address (§8), adds the
  sub-cell texture.

So the coarse mesh **constrains** the fine (the constitution's "coarse constrains
fine, higher fidelity refines never contradicts") for free: a room can only ever
be a refinement of the biome its three parents already agreed on.

**Field-refinement, not mesh-requantization (Crust's canonical-grid line).** Crust
ratified that *identity computes on the canonical grid; observation samples fields*
(decision 0038): pointwise `Field`s (elevation,
crust, biome-via-field) are resolution-free, and a room may sample them as finely as
it likes. But **mesh-bound quantities — sea level, drainage and basins, connected
land components, settlement placement — bear world identity and are computed once on
the canonical grid.** A room therefore *refines fields* yet *inherits* mesh-bound
truth: it must not re-derive a coastline, a lake, or a drainage basin at room scale,
because a finer re-quantization would *contradict* world identity, not refine it —
that is an epoch, not a tier (decision 0039). Room detail
adds *within* what the canonical grid already fixed; the constrain step (P2's kin to
The Walk §4.1 step 5) enforces it.

**Corollary — confine transcendentals to presentation.** Adjacency is integer
(§6), seeds are integer (§8), and these blend weights are exact dyadic rationals —
so the entire *content* pipeline is integer + rational and cross-platform-exact
without libm at all. The only transcendentals (`normalize`/slerp, §5) compute a
room's 3-D position, which feeds **rendering and coordinate display only**. Push
them to that boundary and the platform-libm hazard never touches what a room
*contains*. (A rule that genuinely needs metric geometry — "within X km of the
coast" — uses graph distance in rooms or a quantized value; it does not reach back
into content seeding.)

## 8. Seeds — integer-address-only (a determinism decision, not a detail)

**Room seeds derive from the integer address, never from the float position.**

```
  room_seed(addr) = fold over addr.path of  hash(parent_seed, digit),
                    rooted at hash(locale_world_seed, addr.face)
```

Two reasons, the second constitutional:

- **Hierarchical coherence.** A parent's seed conditions its children, so a river
  or road entering a triangle can be continued into the correct child rather than
  re-rolled — coherence across the zoom levels, cheaply.
- **Cross-platform byte-identity.** The centroid uses `sqrt`/`sin`/`cos`/`acos`,
  which route to the platform libm and differ in the last ULP. Our own CI is
  *already* red on exactly this failure mode — discrete thresholds landing on
  full-precision libm values that disagree between macOS and Linux. If room seeds
  (and thus the bulk of generated content — grammar draws, inhabitants, names)
  came from float positions, every room's content would inherit that hazard.
  Sourcing seeds from the **integer address** makes all stochastic room content
  **platform-exact by construction**. The residual risk is confined to
  *continuous field samples* (§7), which must be **quantized before any discrete
  classification** (`hornvale_kernel::quantize`) — the same discipline the rest
  of the codebase already applies at emit boundaries.

## 9. Movement model (how it presents as a MUD)

- **Three edge-exits per room** (the triangle's neighbors, §6). Naming them by
  bearing (the centroid→neighbor azimuth) gives human directions
  (N/NE/… or "toward the ridge"). Alternating up/down triangle orientation gives
  a natural, non-grid texture to travel.
- **Zoom as a vertical verb** ("enter" / "look closer") descends the path one
  digit — Cycle 01 #9 (fractal zoom) *is* appending a path digit; "step back"
  pops one. This is orthogonal to the 2D triangle tiling.
- **The Underdark** (Cycle 01 #16, P6) is a *separate* labeled graph keyed by
  surface `RoomAddr` + depth-below — not part of this 2D tiling; cave mouths link
  the two graphs.
- Optional later: expose the **dual hexagon** (6 neighbors) if triangle-3-exits
  feels sparse in play. The triangle tiling stays the substrate; hexes are a
  presentation choice. Deferred.

## 10. Chunking, residency & the partitioned ledger

Adaptive depth (§6) means detail concentrates locally and without bound; chunking
keeps that tractable. It composes cleanly here **because a room is a pure function
of its global address**, which dissolves the usual chunk-seam traps (boundary
ownership, cross-chunk negotiation): any chunk that needs a boundary room computes
the identical room. Chunks are therefore a **persistence + indexing + residency**
concern, never a generation-correctness one, and *correctness is independent of
what is loaded*.

> **Scope (reconciled with The Walk §3.6).** The *near-term* room-tier ledger is The
> Walk's **in-memory** model — a `Vec<Fact>` log + `BTreeMap` indexes, bounded by the
> memory economy (frontier MEM-1), persisted as today's single-JSON `World`. No LSM
> machinery and no on-disk partitioning is built now. Everything in the rest of this
> section — address-prefix chunks, event-sourced segments, split/compact/snapshot —
> is the **deferred out-of-RAM form** that The Walk §3.6 explicitly sets aside, kept
> here so the addressing stays partition-ready and the eventual design is on record.
> The *shape* (append-only log + derived indexes) is shared; only scale and machinery
> differ. See decision 0037.

**The key is two layers — freeze only the address.** A first pass framed this as
"pick a chunk-prefix length `k`" and freeze it; that optimizes the wrong quantity.
A fixed prefix makes chunks uniform in *area*, but a ledger's cost scales with
*weight* (delta count), so a metropolis chunk and an ocean chunk — same size on the
map — are wildly unequal files. Every mature spatial-log system (spatial B-trees,
R-trees, LSM stores, even ZIP+4 and cadastral lots) instead splits on *density*.
So:

- **Logical key = the full room address** — already frozen by `RoomId` (§4,
  decision 0006). room → chunk-of-any-prefix is just truncation; a chunk is a big
  room; it nests for free. **Do not** add a second coordinate system (a lat/long
  grid): that re-imports the seam-mismatch trap Approach C exists to escape.
- **Physical segmentation = adaptive, and explicitly *not* a contract** — group
  addresses into files by a runtime policy (below), rebuildable from the ledger and
  never referenced by content, so it is free to evolve.

The thing that felt like it needed a frozen number is exactly the thing that must
*not* be frozen; the thing that must be frozen is the address we already have.

**A chunk is a namespace + index, not a pre-generated block.** "Load chunk C"
loads its coarse fields and its *delta index* — not its millions of latent rooms,
which stay lazy and LOD'd by activity. A pristine chunk nobody has touched costs a
seed and nothing else.

**Two-tier state keeps memory bounded:**

```
  REGENERABLE CACHE  (rooms you generated)  -> evict freely (LRU); pure fn of address
  DURABLE DELTAS     (Facts: what happened) -> never evict; the only precious bytes
```

Camping in one spot deepens a chunk and accretes generated rooms, but those are
disposable (regenerate in microseconds). Only *consequences* persist, and they go
in the ledger.

**The physical segment is an event-sourced LSM store** (LevelDB/Bitcask/Datomic
lineage) — which fits, because the ledger is *already* append-only and
contradiction-checked. A segment grows by appending; at a **save-boundary
structural trigger** (never wall-clock — constitutional) it does one of three
things, chosen by *why* it grew:

```
  many distinct rooms touched  -> SPLIT spatially (the region is genuinely busy)
  one room re-touched a lot     -> COMPACT in place (supersession); you CANNOT
                                   split below a single room
  old, settled deltas           -> SNAPSHOT: fold history into a base state
```

"Split at N" is really that three-way decision. Constitutional edges: compaction
is **local, never a global rewrite**; and a *lossy* snapshot becomes authoritative,
so it must be **full-precision** — never a quantized checkpoint re-derived forward
from (the Lorenz guard-rail). Reverts are **appends** (a retracting fact), not
mutations, so history stays replayable. Segments **split but never merge** — a
cooled region compacts and rests; storage is cheap, merge is complexity. The **hot
single room is the true floor**: below one room, splitting stops and in-room
snapshotting takes over — a genuinely storied room earning its own store is a
feature (cycle 03's rare deep places), not an exception. Emergent bonus: the layout
self-organizes into a legible *map of consequence* — dense where a world was
played, empty elsewhere, zero tuning.

**The one real trap: the monolithic ledger.** Today a `World` is one ledger
serialized to one JSON. At planetary room scale, consequence counts outgrow a
load-everything blob — and the save format is exactly the kind of contract the
constitution treats as catastrophic to change late. So **design the ledger to be
chunk-partitionable now**, even if v1 still serializes monolithically: deltas
keyed/shardable by address prefix. A world then reads as two tiers —

```
  planetary tier: seed + the small canonical ledger (astronomy/terrain/climate/
                  settlements) — stays ~today's size, ~monolithic
  room tier:      sparse per-chunk delta files — exist ONLY where play touched;
                  most chunks are empty and cost zero
```

— still "a world is a seed plus a ledger," just spatially partitioned; an
untouched world remains just a seed. Because **only the address is a contract** (no
chunk size is), the sole frozen surface is one we already own; the physical
segmentation evolves freely (decision 0037).

**Span features cross chunks; derive them top-down.** Rivers, roads, migration
routes, a territory's extent — simulate none of them room-by-room (that forces
loading the path). Plan them at the coarse tier and let each room compute its own
participation locally ("does the river cross my triangle?" from coarse routing +
my address) — "coarse constrains fine" again. Wandering agents use `position =
f(seed, time)` (P8), computable without walking the path. *Stateful*
border-crossing agents are handled by the authority-handoff protocol (§11), not in
the mesh.

## 11. Concurrency & write authority

A MUD is multi-writer, so concurrent deltas to the same region need deterministic
reconciliation. Partitioning makes this **spatially local**: disjoint rooms write
disjoint delta streams and never conflict, so the whole question reduces to the
rare **contended hot room**.

**The chunk is the write-authority unit** — a Domain-Driven-Design *aggregate
root*: one authority serializes a chunk's writes; within it invariants hold
synchronously, across chunks consistency is eventual. That is quadruple duty — the
prefix-chunk is residency + storage-segment + index + **authority** — and it is the
classic MMO server-per-zone pattern; cross-chunk play is disjoint by construction.

**The near-term sim needs almost none of this.** With one trusted authority (the
sim itself) every write is already serialized and the machinery below is dormant —
The Walk's Milestone 1 is a *single-agent frozen slice* where "no one lies," so the
reconciliation, validation, and handoff machinery is not even reachable yet. This
whole section is **parked as reserved-for-multiplayer**, past The Walk's Milestone 2
(liveness) and into Living-Globe territory.
What matters now is preserving **three cheap invariants** so the multiplayer future
stays reachable without a rewrite:

1. **Model a consequence as *proposed action → validated committed fact*** — even
   single-player. Costs nothing now; is the entire basis of safe *untrusted* play
   later (a client proposes; the authority validates against rules + prior state
   and only then commits — anti-cheat falls out, because the authority re-derives
   the result and never trusts the client's claim).
2. **Keep the chunk as the aggregate / authority boundary** (it already is).
3. **Keep determinism absolute** (constitutional) — it gifts cheap forking and
   rollback (below).

**Reconciling the genuinely-contended, when it arises.** Maximize commutative
deltas (CRDT-style: two players dropping *different* items never contend — the
ledger's contradiction-check is the non-commuting detector); order the remainder by
a **deterministic total order that respects causality** — key `(WorldTime.day,
fair-tiebreak)`, reusing the constitutional sim-clock (no wall-clock) and the
existing `total_cmp`-with-tiebreak discipline. The tiebreak is seeded by a per-tick
hash of the *contending set* (not raw `agent-id`, which would let a low id always
win), so ties are fair and still reproducible. The reconciled state is **invented
convention, not discovered truth** — pick the cheap fair rule and stop.

**Reserved for the multiplayer / Living-Globe future** — nothing to build yet, but
foreclosed if the three invariants are violated:

```
  NEED                       OFF-THE-SHELF ANSWER              TRIGGER
  ------------------------   ------------------------------    -----------------------
  untrusted clients          authority VALIDATES, not just     in-browser clients
                             orders (action vs fact)
  agent crosses a seam       ATC-style handoff: one owner      stateful cross-chunk
                             at a time, clean transfer         agent
  action spans chunks        saga / 2-phase commit +           drag item A->B across
                             idempotency keys                  a seam
  real-time responsiveness   client prediction + rollback      human play on a
                             (netcode) — FREE here, as the     sim clock
                             world re-derives deterministically
  varying contention         consistency is a modular          wilderness vs an
                             SPECTRUM per chunk (eventual       auction house
                             for wilderness, strong for hot)
```

The one hard corner is **untrusted × cross-chunk** (a browser client dragging an
item across a seam): validation at both authorities + a saga + a handoff at once —
rare, nameable, the concurrency analogue of the hot-room floor. True peer-to-peer
with *no* authority (needing consensus / vector clocks) is out of scope;
per-chunk authority is required for any multi-writer deployment.

## 12. Constitutional fit — checklist

- **Determinism.** Geometry is a pure path-walk (byte-identical to a full
  `Geosphere::new(L)`); seeds are integer-address-only (§8); field samples
  quantized before classification; concurrent deltas fold by a deterministic
  causal order (§11), not wall-clock.
- **Save-format contracts (new, frozen).** (a) the `subdivide` child-order as the
  addressing alphabet; (b) the base-face numbering from `base_icosahedron()`;
  (c) the 30-edge gluing table; (d) `RoomId` packing — which is *also* the
  room-tier ledger's only frozen key (§10): physical segmentation is adaptive and
  explicitly **not** a contract (decision
  0037). Changing any of (a)–(d) is an
  epoch-suffix regeneration, never an edit.
- **std-only + serde.** All of it — hashing, slerp, the neighbor walk, the gluing
  table — is std math and fixed data. No new crates (no H3/S2 crate; we borrow
  their *addressing math*, not their code).
- **No `HashMap`.** The gluing table is a fixed array; any caches are `BTreeMap`.
- **Layering.** Lives in the **kernel**, beside `Geosphere` (it *is* the
  Geosphere extended). Domains consume rooms the way they consume cells; the new
  **locale window** (synthesis §4) composes the P1 stack over rooms. No domain
  depends on another.

## 13. Validation plan (prototype order)

> **Spike result (2026-07-12).** Steps 1–3 below were run as a throwaway
> out-of-workspace prototype (borrowing `Geosphere` as the oracle) at level 7,
> uniform depth. All passed: lazy neighbor set == oracle for **all 327,680
> faces** (7,680 cross-seam links, count-confirmed as 30 edges × 128 × 2);
> adjacency mutual/symmetric; independent lazy path-walk geometry byte-identical
> to the authoritative mesh (`max|Δ| = 0`); integer-address seeds deterministic.
> The replica was first proven byte-identical to `Geosphere::new(7)` so the
> oracle is trustworthy. A **second spike** then covered adaptive depth /
> T-junctions (step 3's mixed-resolution case) against a fine-mesh oracle —
> interior and cross-base-seam, `2^Δ` fan-out confirmed, mutual (see §6). **Still
> not covered:** a two-platform libm-swap fixture (low-risk; all content-path
> outputs are integer-derived by construction, §8).

1. **Neighbor walk first** — it is the risk. Implement §6 integer-only.
2. **Cross-check lazy vs authoritative.** Build a *full* `Geosphere::new(7)`
   (163,842 cells — cheap) and assert: every room address's lazy `centroid`
   equals the corresponding fully-built vertex/face geometry; every lazy neighbor
   set equals the fully-built adjacency. This turns the existing, trusted mesh
   into the oracle for the lazy one — the single most important test.
3. **Property tests.** Adjacency is mutual and symmetric; every non-vertex room
   has exactly 3 edge-neighbors; the 12 vertex figures close on 5 faces;
   `RoomId` pack/unpack round-trips; same address → byte-identical
   centroid+seed; two platforms (or a libm-swap fixture) agree on all
   integer-derived outputs.
4. **Bench.** Room + neighbors at depth 16 should be microseconds (O(depth)
   hashes and slerps). Confirm no global allocation.

## 14. Open questions & risks

1. **The up-walk/mirror-down neighbor algorithm** (§6) is the one intricate part.
   Known-solvable, integer-only, but it carries the implementation risk — hence
   "prototype first." **RESOLVED (2026-07-12 spike).** Reformulated on the
   barycentric lattice it is O(1) (apex `W' = U+V−W_self`; negative coord ⇒ seam),
   not the O(depth) up-walk this item feared, and it validates exhaustively
   against the level-7 oracle (§13). No longer the first-order risk.
2. **Area distortion.** Icosphere triangles are *not* equal-area (center children
   larger than corner children); room areas vary ~1.5–2×, roughly the coarse
   5-vs-6 valence ratio. Almost certainly fine at room scale; an equal-area
   variant (ISEA/Snyder) would break geometry-reuse and is deferred unless play
   demands it.
3. **Depth cap.** Fix a maximum depth (≈17) so `RoomId` fits `u64` and rooms
   don't shrink absurdly. Decide the exact cap when the locale window's minimum
   room size is chosen.
4. **Coarse categorical blending** (§7): blending biome *categories* across three
   corners needs a rule (max-weight vs blend-then-reclassify vs a P3 grammar
   conditioned on the three). Small, but a real choice — likely a P3 concern.
5. **Triangle vs dual-hex rooms** (§9) is a gameplay-feel decision that can be
   deferred; the substrate is unaffected.
6. **Segment split threshold & policy** (§10). The three-way split/compact/snapshot
   decision and its trigger size are **tunable runtime policy, not a contract** —
   they need measuring against real play density, not fixing now.
7. **Multi-writer, only if/when it happens** (§11). Untrusted validation, the
   ATC-style handoff, cross-chunk sagas, and rollback netcode are *reserved* for a
   multiplayer/Living-Globe future; nothing to build for the single-authority sim
   beyond preserving the three near-term invariants. The one hard corner when it
   arrives is untrusted × cross-chunk.

## 15. Recommended next step

~~Spike the neighbor walk (§6) + the lazy-vs-`Geosphere::new(7)` oracle test
(§13.2) as a throwaway prototype in the kernel.~~ **Done 2026-07-12 — it held**
(§13 result). P2 has graduated from "keystone risk" to "buildable."

The next steps, in order of remaining risk:

1. ~~**Spike adaptive depth / T-junctions** (§6)~~ **Done 2026-07-12 — it held**
   (§6 finding). Interior and cross-seam, `2^Δ` fan-out, mutual.
2. ~~**Stand up P2 for real** — a kernel `room` module beside `Geosphere`~~
   **Done 2026-07-12 — The Room Mesh (campaign 1)** shipped `kernel/src/room.rs`:
   addressing, lazy geometry (byte-identical to `Geosphere`), the O(1) neighbor
   walk + seam gluing, vertical verbs, integer-address seeding, and coarse-field
   inheritance hooks. The two spike caveats (quantize before discrete
   classification §7; avalanche the target-depth hash before its `mod` §6) carry
   forward to the still-deferred adaptive-depth and variety layers (P1/P3, the
   locale window).
3. ~~**Promote P2 into the frontier registry**~~ **Done 2026-07-12** — P2 is
   registry **MAP-28** (variety/bestiary MAP-29, palimpsest MAP-30); cycle-03's
   folds carry back-links; The Walk §4.1 names P2 as its spatial instantiation.
