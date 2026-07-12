# The Room Mesh — Design (P2, campaign 1)

**Date:** 2026-07-12
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-05-hornvale-longterm-plan-design.md` (Constitution §2 governs)
**Grounded in:** `docs/design/room-scale/p2-subdivision-design.md` (the full P2 design) and
`docs/design/room-scale/reconciliation.md`. Implements registry **MAP-28**; the room-scale
variety (MAP-29) and palimpsest (MAP-30) layers build on top of it.
**De-risked by:** two throwaway kernel spikes (2026-07-12) — the O(1) neighbour walk + seam
gluing against a level-7 `Geosphere` oracle (all 327,680 faces), and adaptive depth /
T-junctions (interior and cross-seam). Recorded in p2 §6/§13.
**Provenance:** four ideonomy passes over the design — loose-ends, room-*reference*, ecological
psychology, and world-*change* — each folded in below. They mostly *confirmed* the layering and
sharpened a handful of contracts and non-goals.

---

## 1. What this is

The first P2 campaign: a new kernel module `kernel/src/room.rs` — the **addressable, lazily
generated, deterministic room substrate** that sits below the canonical globe grid. It defines
what a "room" *is* (identity, geometry, adjacency, seeding, coarse-field inheritance) and gates
every room-scale layer above it (variety, the locale window, the game's Local-space).

A room is a **triangular face of the same icosphere as `Geosphere`, refined deeper** — *not* a
new coordinate system. Refining further (levels 6, 7, …) is the identical `subdivide` operation
producing the identical geometry, so seams never misalign and the pentagon irregularity is
confined to the 12 original icosahedron vertices — where it never even touches edge-neighbour
finding, because every icosahedron edge borders exactly two faces.

**The value proposition (why this and not a finer canonical grid):** raising the canonical grid
(L5→L6 already cost dearly) re-runs *mesh-bound, global, identity-bearing* computation for the
whole planet. P2 is the opposite: lazy, per-address, local, **zero global cost**. It is the
vehicle for arbitrarily fine *local* detail — a room at refinement depth 7 literally *is* an L7
triangle, computed only when someone looks — without ever paying the global bill. The discipline
that makes this sound is Crust's ratified line (`identity-computes-on-the-canonical-grid`):
resolution-free **fields** are sampled as finely as desired anywhere; **mesh-bound structure**
(sea level, drainage, connectivity, placement) is fixed once on the canonical grid and only
*rendered* finer, never re-derived.

## 2. Scope

**In (this campaign — "The Room Mesh"):**

- Addressing: `RoomAddr` + the `RoomId` u64 save-format contract (pack/unpack).
- Lazy geometry: corner positions, centroid, geo-coordinate, bearing — byte-identical to a
  fully-built `Geosphere`.
- Adjacency: the O(1) integer-barycentric neighbour walk (interior + base-face seam), **uniform
  depth**, with a documented neighbour-order contract.
- Vertical verbs: `parent` / `child` / `ancestor` (the zoom primitive and the containment backbone).
- Integer-address seeding (float-free), with the seed labels declared as frozen stream labels.
- Coarse-field **inheritance hooks**: a room's three canonical-grid corner cells + exact dyadic
  barycentric weights (the mechanism; consumers blend).

**Deferred (named, with a home):**

- **Adaptive depth / T-junctions** — spike-validated; ships later as an *additive*
  `neighbors_adaptive(addr, &target_depth_field)` beside the stable uniform `neighbors`. The
  uniform API does not change.
- **Categorical field-blend policy** (biome max-weight vs blend-then-reclassify — p2 §14 Q4);
  belongs with the variety layer (MAP-29).
- **Chunking / partitioned ledger** (p2 §10) and **concurrency / write authority** (p2 §11) —
  the multiplayer / out-of-RAM future.
- **The locale window** — the P1/P3 stack composed over rooms.

## 3. Addressing and the `RoomId` contract

```rust
/// A room — a triangular face of the icosphere at refinement depth `path.len()`.
/// Keyed to the base icosahedron face (level 0), so the address is INDEPENDENT of the
/// world's canonical globe level; the coarse cell is a path prefix (§7).
pub struct RoomAddr { pub face: u8, pub path: Vec<u8> }   // face 0..20, digits 0..4

/// Packed, serialized form — a FROZEN save-format contract.
pub struct RoomId(pub u64);
```

`RoomAddr` is the working form (unbounded `Vec`); `RoomId` is the storage/key form (u64, capped).
The digit alphabet **is** `Geosphere::subdivide`'s child order (child 0/1/2 = the corner children
at `a`/`b`/`c`; child 3 = the inverted centre), so a room's geometry is byte-identical to "just
subdividing more."

**`RoomId` bit-grammar (self-delimiting; the frozen packing):**

```
  bits [0, 5)    face      = 5 bits, valid iff < 20
  bits [5, 64)   pathword  = a leading-1 SENTINEL, then 2 bits per digit, ROOT digit first:
                             pathword = 1; for digit in path (root..leaf): pathword = (pathword<<2)|digit
  len(path)      = highest_set_bit_index(pathword) / 2         (pathword = 1 followed by 2*len bits)
  cap            = 29 digits                                   (1 + 2*29 = 59 bits available)

  VALID(u64) iff face < 20 AND pathword != 0
  pack   : &RoomAddr -> Result<RoomId, DepthExceedsCap>        # RoomAddr(Vec) can out-run u64
  unpack : RoomId    -> Result<RoomAddr, MalformedRoomId>      # not total; validate face + sentinel
```

Every u64 bit is either `face` or `pathword` — there are no unused bits, so "malformed" means only
`face >= 20` or `pathword == 0` (2-bit digits are always valid child indices). `pathword == 1`
encodes the empty path — a whole base face. The cap of **29** is the natural u64 limit; useful room
scale is ~L16–20 (an L18 room ≈ 27 m edge). pack∘unpack is the identity on the valid subset; both
directions are checked (never infallible) — important for rejecting a forged/corrupt key in the
untrusted-save future.

## 4. Geometry (lazy, byte-identical)

```rust
pub fn corners(addr: &RoomAddr)  -> [[f64; 3]; 3];              // the 3 unit-sphere corners
pub fn centroid(addr: &RoomAddr) -> [f64; 3];                  // normalize(v0 + v1 + v2)
pub fn coord(addr: &RoomAddr)    -> GeoCoord;                  // lat/long of the centroid
pub fn bearing(from: &RoomAddr, to: &RoomAddr) -> f64;        // great-circle azimuth, degrees
```

Corners are computed by walking `path` from the base face, `slerp`-refining edge midpoints exactly
as `subdivide` does — O(depth) work, no global mesh, **byte-identical** to the same triangle in a
fully-built `Geosphere::new(path.len())` (spike: `max|Δ| = 0`). `bearing` is included in v1 (not
deferred): the vista, wayfinding, and human exit-naming layers all need it, and it is a few lines
of great-circle azimuth.

Transcendentals (`sqrt`/`sin`/`cos`/`acos`) live **only** in these geometry functions — they feed
rendering and coordinate display. Identity, adjacency, seeding, and inheritance weights are all
integer/rational (§§5–7), so the platform-libm hazard never touches what a room *contains*.

## 5. Adjacency — the neighbour walk

```rust
/// The three edge-neighbours. neighbor[i] is the room across the edge OPPOSITE corner i
/// (the edge between corners (i+1)%3 and (i+2)%3). Geometric BASE adjacency only.
pub fn neighbors(addr: &RoomAddr) -> [RoomAddr; 3];
```

The algorithm the spikes validated, integer-only:

- Represent the room's triangle as an ordered triple of integer **barycentric** coordinates
  `(a,b,c)`, `a+b+c = 2^depth` (the exact dyadic coordinates of §7).
- Neighbour across edge `(U,V)` = the lattice apex `W' = U + V − W_self`. If every coordinate of
  `W'` is in range, it is an interior neighbour `[U, V, W']`. If any is negative, the edge lies on
  a base-face boundary — re-express `U,V` on the adjacent face (a fixed **30-edge gluing table**
  derived once from `base_icosahedron`, preserving the along-edge parameter) and take the one
  inward apex there. O(1), no recursion, no floats.

**Contracts:** the neighbour *order* is fixed (opposite-corner index `i`), so consumers can name
and cache exits stably. `neighbors` returns the **immutable geometric base graph** and nothing
else — passability, blocked paths, and overlay edges (roads, tunnels, rivers, flight, portals) are
higher-layer overlays keyed by `RoomId`, and must never leak into this API (§9, §10).

## 6. Vertical verbs and seeding

```rust
pub fn parent(addr: &RoomAddr)             -> Option<RoomAddr>;                 // None at a base face
pub fn child(addr: &RoomAddr, digit: u8)   -> Result<RoomAddr, DepthExceedsCap>;
pub fn ancestor(addr: &RoomAddr, depth: u32) -> Option<RoomAddr>;              // the containing room at a coarser depth

pub fn seed(addr: &RoomAddr, world: Seed)  -> Seed;                            // hierarchical, integer-address, float-free
```

`parent`/`child` are the "zoom" verb (append/pop a digit); `ancestor` truncates to a coarser depth.
They are the **containment backbone** — a room's ancestors are exactly its address prefixes (room ∈
locale ∈ region ∈ base face), which is the hierarchy geographic reference and residency hang on.

`seed` folds `hash(parent_seed, digit)` down the path from `hash(world_seed, face)`, using
`Seed::derive`. Because it is sourced from the **integer address, never the float position**, all
stochastic room content is cross-platform-exact by construction. The derivation labels are a
save-format contract: declared as constants and published into the generated stream manifest via
the existing `stream_labels()` mechanism (changing one silently corrupts every world).

## 7. Coarse-field inheritance hooks

```rust
/// The room's three canonical-grid corner cells, each with its integer blend weight
/// at the room centroid. Weights are NUMERATORS over the denominator
///   D = 3 << (addr.path.len() - geo.level())          // = 3 * 2^(depth - globe_level)
/// and sum to D. (The cells alone are the `.0`s.)
///
/// Precondition: addr.path.len() >= geo.level() (the room is at or below the canonical grid).
/// Returns None above it — a room coarser than the grid spans many cells; inheritance undefined.
pub fn corner_weights(addr: &RoomAddr, geo: &Geosphere) -> Option<[(CellId, u64); 3]>;
```

A room inherits coarse fields from its **three canonical-grid corner cells** and refines locally.
The room's depth-`geo.level()` ancestor triangle has three corners that are canonical-grid vertices;
their `CellId`s are resolved by computing the three ancestor-corner positions (the same `slerp`
walk, so byte-identical to the mesh vertices) and matching them in the **already-resident**
`Geosphere` via `NearestCellIndex` (exact match) — no new global mesh. The weights are the room
**centroid's** barycentric coordinates within that ancestor triangle: exact integer rationals over
`D = 3 · 2^(depth − globe_level)`, computed entirely by integer path arithmetic, so
cross-platform-deterministic. (The three *corners* are dyadic, `/2^d`, as p2 §7 states; the
*centroid* averages them, which is where the factor of 3 in the denominator comes from — the design
doc's "dyadic" phrasing is for the corners.) A consumer blends
`Σ (wᵢ / D) · cellmap.get(cellᵢ)` for a scalar field; the `wᵢ/D` division is IEEE-deterministic, and
field values are `f64` anyway (quantized before any discrete classification, per §8/Crust).

**Globe-level correctness.** The canonical grid is `GLOBE_LEVEL` (default 6, pinnable 4–7); these
hooks read `geo.level()` rather than assuming 5, so they are correct for any pinned world.

**Time is clean.** The weights are **time-invariant**; the *field values* are `f(space, time)`. The
consumer samples the (possibly time-varying — MAP-24) field at the corner cells, so seasonal and
climatic variation is derived, never stored, and never disturbs the room's identity.

**Dependency split.** Only this function takes `&Geosphere`; everything in §§3–6 is a pure function
of the address and depends on nothing. The module is organized along that seam so the pure core is
testable in isolation.

## 8. Module structure

`kernel/src/room.rs`, beside `geosphere.rs`. The base-icosahedron data, `slerp` midpoint, and the
child-selection order are **extracted to a shared `pub(crate)` surface** both `geosphere` and `room`
consume — a single source of truth for the child-order contract, which is exactly right since
`RoomAddr`'s alphabet *is* that contract. This is a small refactor of `geosphere.rs`; it must leave
`Geosphere`'s public API and output byte-identical.

## 9. What P2 is a substrate *for* (deliberate non-goals)

Four ideonomy passes converged on the same boundary: P2 is the **umwelt-neutral locomotion
substrate**, and everything experiential rides above it. Recording these keeps naming/perception/
change machinery *out* of the kernel and tells the next campaigns exactly which handles to hang on.

- **Reference.** "Get the spoon from the kitchen" = resolve *the kitchen* → `RoomId`(s) [epistemic +
  linguistic layer] + resolve *the spoon* → object [the unbuilt object-scale layer, MAP-19/20]. P2
  owns neither. It provides the three handles the *place* half stands on: **canonical identity**
  (`RoomId`), **geographic containment** (`ancestor`/`parent` = the address prefix), and
  **adjacency** (`neighbors`, for deictic reference). Naming, toponyms, and descriptive resolution
  are generative/cultural layers (LANG, settlement, MAP-2) — most rooms are unnamed wilderness,
  referred to by derived features or relative position.
- **Perception & wayfinding.** Experience is not room-local — it is a *vista* out to the horizon
  across many rooms (Gibson's ambient array; The Walk's vantage query). P2 exposes the substrate
  (`neighbors` + positions + `bearing`); line-of-sight, affordances (which are body- and
  goal-relative — MAP-27 × PSY-6), place-knowledge, and getting lost are layers above. Room
  *variety* (MAP-29) is what makes the world *navigable* (Lynch imageability), not merely
  un-boring — a reframe worth carrying into that campaign.
- **Change.** Geometry and identity are **immutable**; every world-change is a **delta overlay**
  keyed by `RoomId` (a point change), a `RoomId`-pair (an added overlay edge — a tunnel, a road),
  or a coarse prefix (a span — a burn, a flood, a territory). Most change isn't even stored: cyclic
  and climatic change is *derived* from time-varying fields, and a consequence unfolds as a
  *deterministic trajectory evaluated at any time* from a single stored **event** (abandonment →
  succession; construction → decay), the room-scale analogue of the astronomy ephemeris — no
  stepping. The delta *store* is the deferred ledger (p2 §10) + The Walk's store-irreversible/
  derive-reversible seam. **v1 ships the immutable substrate and the stable delta keys, nothing
  more.**

## 10. Frozen save-format contracts (new)

Changing any of these is an epoch-suffix regeneration (`…/v2`), never an edit:

1. the `subdivide` child-order, as the addressing alphabet (shared with `geosphere`);
2. the base-face numbering from `base_icosahedron`;
3. the 30-edge gluing table;
4. the `RoomId` packing (§3);
5. the room seed-derivation labels (§6).

## 11. Constitutional fit

- **Determinism:** geometry byte-identical to `Geosphere`; seeds integer-address-only; no code path
  derives identity or seed from a float (distinct deep rooms *may* share an f64 centroid — identity
  is always the integer address).
- **No `HashMap`:** the gluing table is a fixed array; any caches are `BTreeMap`.
- **std + serde only;** no new crates.
- **No wall-clock;** time is `WorldTime` where it appears (field sampling).
- **Layering:** a kernel module; no domain dependency (the inheritance hooks take `&Geosphere`,
  also kernel).
- **Quantization:** `RoomId` is integer-exact; v1 serializes nothing float-valued about a room.
  Should room geometry ever be serialized, it quantizes at the emit boundary per existing discipline.
- **Typed quantities:** new pub-boundary primitives (`[f64;3]` positions, the `bearing` angle) carry
  `type-audit:` tags matching `geosphere.rs`'s existing conventions.

## 12. Testing

The two spikes *are* the test design; they port into `kernel/tests/` and `kernel/src/room.rs`.

- **Oracle (the load-bearing test):** build `Geosphere::new(L)` and assert, for every face at L:
  lazy `corners`/`centroid` == the mesh vertex geometry byte-for-byte; the lazy `neighbors` set ==
  the fully-built face adjacency; adjacency mutual/symmetric; exactly three edge-neighbours; the
  seam-crossing count equals `30 · 2^L · 2`.
- **Oracle ceiling:** `Geosphere::new(L)` is buildable to ~L8 (L9 ≈ 2.6 M cells). Validate
  exhaustively there; beyond it, trust by **induction** (every level is the identical `subdivide`
  op) plus oracle-free spot-checks — `RoomId` round-trip and neighbour symmetry — at deep random
  addresses.
- **`RoomId`:** pack/unpack round-trips over all faces to some depth and random deep addresses;
  malformed-u64 rejection; `DepthExceedsCap` past the cap.
- **Determinism:** same address → identical geometry + seed.
- **Inheritance:** weight numerators sum to `D = 3 · 2^(depth − globe_level)`; a constant field
  blends to the constant; the corner cells' positions exactly equal the computed ancestor corners.
  Targeted cases: *at* the 12 pentagon vertices, and *across* a canonical land/sea boundary (weights
  stay well-defined even though the categorical policy is deferred).
- **`bearing`:** known cardinal directions between a room and a chosen neighbour.

## 13. Open questions / risks

1. **Inheritance precondition surface** — `Option` (not-applicable above the grid) vs `Result` vs a
   `debug_assert`. Chosen: `Option`, precondition documented. Revisit only if a caller wants the
   error reason.
2. **Depth cap = 29** (the u64 limit). Frozen; useful range ~L16–20. Confirm no near-term need for a
   deeper floor (there is none — an L20 room is ~7 m).
3. **`bearing` / position type-audit tags** — settle bare-`f64`-with-tag vs a newtype during
   implementation, matching whatever `geosphere.rs` does for `position`.
4. **Area distortion is a feature, not a wart.** Icosphere triangles are not equal-area (centre
   children ~1.5–2× the corner children); read positively that is free room-size variety feeding
   MAP-29. An equal-area variant would break geometry-reuse and stays deferred (p2 §14 Q2).

## 14. Implementation shape (the plan follows via writing-plans)

Natural build order, each step compiling and testing green before the next:

1. Shared icosphere-primitive extraction (refactor `geosphere`, prove byte-identical) → `RoomAddr` /
   `RoomId` / pack / unpack + round-trip tests.
2. Lazy geometry (`corners`/`centroid`/`coord`/`bearing`) + the oracle geometry test.
3. The neighbour walk (interior + the 30-edge gluing table) + the oracle neighbour test + property
   tests (symmetry, exactly-three, seam count).
4. Vertical verbs + integer-address seeding (with stream labels) + determinism tests.
5. Inheritance hooks (`corner_cells` / `barycentric_weights` via `NearestCellIndex`) + inheritance
   and targeted (pentagon / coast) tests.

**Definition of Done** (per campaign discipline): the book — a chronicle entry and a freshness sweep
(including a Confidence Gradient re-score if this moves a bet), a `docs/retrospectives/` page, and
the MAP-28 registry row flipped to `spec'd` / repointed at this spec. The two throwaway spikes are
superseded by the real module and deleted.
