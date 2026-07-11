# P2 — Deterministic Hierarchical Subdivision (first-pass design)

**Status:** Design proposal, first pass. The keystone primitive from
[synthesis.md](synthesis.md) §2 — what a "room" *is*, and the thing every other
room-scale primitive is gated on.
**Grounded in:** `kernel/src/geosphere.rs` (read in full). Terms below match that
module: `Geosphere`, `CellId`, `CellMap`, `subdivide`, `build_neighbors`.

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

`RoomId` pack/unpack is a pure bijection (a frozen save-format contract, §7).

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

> This up-walk/mirror-down neighbor algorithm is the one genuinely intricate
> piece. It is well-trodden (quaternary triangular meshes / geodesic DGGS), it is
> integer-only, and it is the thing to prototype and property-test *first* (§8).

## 7. Coarse-field inheritance

A room does not re-derive biome/climate from scratch — it **inherits from its
three level-5 corner cells** and refines locally:

- The room's **depth-5 ancestor triangle** has three corners that are level-5
  `CellId`s (vertex indices are preserved across levels, §2). Computing them is a
  by-product of the path walk (record the three vertex indices at depth 5).
- The room centroid's **barycentric coordinates** within that ancestor triangle
  give three weights. Coarse fields sample as the weighted blend of the three
  `CellMap` values, e.g. `biome_field = Σ wᵢ · coarse.get(cellᵢ)` (for scalar
  fields; categorical fields take the max-weight corner or blend then
  re-classify). This is exactly a P1 leaf field; room-depth noise (P1) adds the
  sub-cell texture.

So the coarse mesh **constrains** the fine (the constitution's "coarse constrains
fine, higher fidelity refines never contradicts") for free: a room can only ever
be a refinement of the biome its three parents already agreed on.

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

## 10. Constitutional fit — checklist

- **Determinism.** Geometry is a pure path-walk (byte-identical to a full
  `Geosphere::new(L)`); seeds are integer-address-only (§8); field samples
  quantized before classification. No wall-clock.
- **Save-format contracts (new, frozen).** (a) the `subdivide` child-order as the
  addressing alphabet; (b) the base-face numbering from `base_icosahedron()`;
  (c) the 30-edge gluing table; (d) `RoomId` packing. Changing any is an
  epoch-suffix regeneration, never an edit.
- **std-only + serde.** All of it — hashing, slerp, the neighbor walk, the gluing
  table — is std math and fixed data. No new crates (no H3/S2 crate; we borrow
  their *addressing math*, not their code).
- **No `HashMap`.** The gluing table is a fixed array; any caches are `BTreeMap`.
- **Layering.** Lives in the **kernel**, beside `Geosphere` (it *is* the
  Geosphere extended). Domains consume rooms the way they consume cells; the new
  **locale window** (synthesis §4) composes the P1 stack over rooms. No domain
  depends on another.

## 11. Validation plan (prototype order)

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

## 12. Open questions & risks

1. **The up-walk/mirror-down neighbor algorithm** (§6) is the one intricate part.
   Known-solvable, integer-only, but it carries the implementation risk — hence
   "prototype first."
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

## 13. Recommended next step

Spike the neighbor walk (§6) + the lazy-vs-`Geosphere::new(7)` oracle test (§11.2)
as a throwaway prototype in the kernel. That single spike resolves the only
first-order risk in this design; everything after it (positions, seeds,
inheritance, the locale window) is straightforward and already de-risked by the
mesh reading. If the spike holds, P2 graduates from "keystone risk" to "buildable,"
and the P1/P3 layers on top can start.
