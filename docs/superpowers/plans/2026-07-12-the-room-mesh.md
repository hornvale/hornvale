# The Room Mesh Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `kernel/src/room.rs` — the addressable, lazily-generated, deterministic room substrate below the canonical globe grid (registry MAP-28).

**Architecture:** Rooms are triangular faces of the same icosphere as `Geosphere`, refined deeper and addressed by `(base face + child-digit path)`. Geometry is a lazy `slerp` path-walk, byte-identical to a fully-built `Geosphere`. Adjacency is an O(1) integer-barycentric apex rule plus a fixed 30-edge base-face gluing table. Identity, adjacency, and seeding are integer/rational (cross-platform-exact); transcendentals live only in position geometry.

**Tech Stack:** Rust (edition 2024), `std` + `serde` only. No new crates, no `HashMap`/`HashSet`, no wall-clock. The kernel's `Seed`, `Geosphere`, `CellId`, `GeoCoord`, `NearestCellIndex`.

**Provenance:** ports the two validated throwaway spikes (neighbour walk + adaptive depth), which passed against a level-7 `Geosphere` oracle. Spec: `docs/superpowers/specs/2026-07-12-the-room-mesh-design.md`.

## Global Constraints

- **Determinism is constitutional.** Same address → byte-identical geometry and seed. Geometry byte-identical to `Geosphere::new(path.len())` for the same face. No code path derives identity or seed from a float.
- **No `HashMap`/`HashSet`.** `BTreeMap`/`BTreeSet`/`Vec` only (enforced by `clippy.toml`).
- **No wall-clock.** Time is `WorldTime` where it appears.
- **`std` + `serde` only.** No new dependencies (enforced by `cli/tests/architecture.rs`).
- **Layering.** `room` lives in the kernel; it may use other kernel modules and nothing else.
- **`#![warn(missing_docs)]`** is on: every public item, field, and variant gets a one-line doc comment.
- **Frozen save-format contracts** (changing any is an epoch-suffix regeneration, never an edit): the `subdivide` child-order (addressing alphabet), the base-face numbering, the 30-edge gluing table, the `RoomId` packing, the room seed labels.
- **The gate** (run before every commit, cost-ordered — fmt/clippy first, then scoped tests): `cargo fmt`, `cargo clippy -p hornvale-kernel --all-targets -- -D warnings`, `cargo test -p hornvale-kernel`.

---

### Task 1: Extract shared base-icosphere primitives in `geosphere`

Give `room` a single, drift-proof source for the base data, `normalize`, and the sphere-midpoint op — without changing `Geosphere`'s behaviour or output.

**Files:**
- Modify: `kernel/src/geosphere.rs`

**Interfaces:**
- Produces: `pub(crate) fn normalize(v: [f64;3]) -> [f64;3]`; `pub(crate) fn slerp_mid(a: [f64;3], b: [f64;3]) -> [f64;3]`; `pub(crate) fn base_data() -> &'static (Vec<[f64;3]>, Vec<[u32;3]>)` (cached base vertices + 20 faces).

- [ ] **Step 1: Change `normalize` visibility and add `slerp_mid`**

In `kernel/src/geosphere.rs`, change `fn normalize` to `pub(crate) fn normalize`, and add below it:

```rust
/// Edge midpoint projected onto the unit sphere — the subdivision step
/// (average, then normalize). The one place the sphere-midpoint op is defined.
pub(crate) fn slerp_mid(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    normalize([(a[0] + b[0]) / 2.0, (a[1] + b[1]) / 2.0, (a[2] + b[2]) / 2.0])
}
```

- [ ] **Step 2: Route `subdivide`'s midpoint through `slerp_mid`**

In `subdivide`, replace the body of the `midpoint` closure's computation

```rust
        let [ax, ay, az] = positions[a as usize];
        let [bx, by, bz] = positions[b as usize];
        let mid = normalize([(ax + bx) / 2.0, (ay + by) / 2.0, (az + bz) / 2.0]);
```

with

```rust
        let mid = slerp_mid(positions[a as usize], positions[b as usize]);
```

- [ ] **Step 3: Add the cached `base_data` accessor**

Add near `base_icosahedron` (keep `base_icosahedron` as-is; `Geosphere::new` still needs an owned, mutable copy):

```rust
/// The base icosahedron (12 vertices, 20 faces), computed once. `room` reads
/// this immutably; `Geosphere::new` keeps taking its own owned copy to mutate.
pub(crate) fn base_data() -> &'static (Vec<[f64; 3]>, Vec<[u32; 3]>) {
    use std::sync::OnceLock;
    static BASE: OnceLock<(Vec<[f64; 3]>, Vec<[u32; 3]>)> = OnceLock::new();
    BASE.get_or_init(base_icosahedron)
}
```

- [ ] **Step 4: Run the existing geosphere tests to prove no behavioural change**

Run: `cargo test -p hornvale-kernel geosphere`
Expected: PASS (all existing geosphere tests, including `geosphere_is_deterministic` and the cell-count/valence tests — the refactor is output-preserving).

- [ ] **Step 5: fmt + clippy + commit**

```bash
cargo fmt && cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add kernel/src/geosphere.rs
git commit -m "refactor(kernel): expose base-icosphere primitives for the room module"
```

---

### Task 2: `RoomAddr`, `RoomId`, and the packing contract

**Files:**
- Create: `kernel/src/room.rs`
- Modify: `kernel/src/lib.rs` (register the module + re-exports)

**Interfaces:**
- Produces: `RoomAddr { face: u8, path: Vec<u8> }`; `RoomId(u64)`; `RoomAddr::depth(&self) -> u32`; `RoomAddr::pack(&self) -> Result<RoomId, RoomAddrError>`; `RoomId::unpack(&self) -> Result<RoomAddr, RoomIdError>`; `const MAX_DEPTH: usize = 29`.

- [ ] **Step 1: Create the module with types and write the failing round-trip test**

Create `kernel/src/room.rs`:

```rust
//! The Room Mesh: rooms are triangular faces of the same icosphere as
//! `Geosphere`, refined deeper and addressed by `(base face + child path)`.
//! Lazy, deterministic, and byte-identical to a fully-built `Geosphere`.
//! Identity, adjacency, and seeding are integer/rational; transcendentals live
//! only in position geometry (registry MAP-28).

/// The deepest path a `RoomId` can pack: 5 face bits + 1 sentinel + 2*29 digit
/// bits = 64. Useful room scale is ~L16-20 (an L18 room edge is ~27 m).
pub const MAX_DEPTH: usize = 29;

/// A room — a triangular face of the icosphere at refinement depth
/// `path.len()`. Keyed to the base icosahedron face (level 0), so the address
/// is independent of the world's canonical globe level.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RoomAddr {
    /// Which of the 20 base icosahedron faces (0..20).
    pub face: u8,
    /// Child index (0..4) at each refinement, from the base face down.
    pub path: Vec<u8>,
}

/// Packed, serialized form of a `RoomAddr` — a frozen save-format contract.
/// Layout: bits `[0,5)` = face; bits `[5,64)` = a leading-1 sentinel then 2
/// bits per digit, root digit first.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RoomId(pub u64);

/// Why a `RoomAddr` could not be packed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RoomAddrError {
    /// `path.len()` exceeds `MAX_DEPTH`, so it will not fit a `u64`.
    DepthExceedsCap,
    /// A path digit was not a child index in `0..4`, or `face >= 20`.
    Invalid,
}

/// Why a `u64` is not a valid `RoomId`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RoomIdError {
    /// The face field is not a base face (`>= 20`), or the sentinel is missing.
    Malformed,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roomid_round_trips() {
        let cases = [
            RoomAddr { face: 0, path: vec![] },
            RoomAddr { face: 19, path: vec![3] },
            RoomAddr { face: 7, path: vec![0, 1, 2, 3, 0, 1, 2] },
            RoomAddr { face: 3, path: vec![3; MAX_DEPTH] },
        ];
        for addr in cases {
            let id = addr.pack().expect("packs");
            assert_eq!(id.unpack().expect("unpacks"), addr, "round-trip for {addr:?}");
        }
    }
}
```

- [ ] **Step 2: Register the module and run the test to see it fail**

In `kernel/src/lib.rs`, add `pub mod room;` (alphabetically, after `pub mod refine;`) and add to the re-export block:

```rust
pub use room::{MAX_DEPTH, RoomAddr, RoomAddrError, RoomId, RoomIdError};
```

Run: `cargo test -p hornvale-kernel room::tests::roomid_round_trips`
Expected: FAIL to compile — `pack`/`unpack` not defined.

- [ ] **Step 3: Implement `pack`, `unpack`, and `depth`**

Add to `kernel/src/room.rs` (above the `tests` module):

```rust
impl RoomAddr {
    /// Refinement depth = path length.
    pub fn depth(&self) -> u32 {
        self.path.len() as u32
    }

    /// Pack to the `u64` `RoomId` contract. Fails past `MAX_DEPTH`.
    pub fn pack(&self) -> Result<RoomId, RoomAddrError> {
        if self.path.len() > MAX_DEPTH {
            return Err(RoomAddrError::DepthExceedsCap);
        }
        if self.face >= 20 || self.path.iter().any(|&d| d >= 4) {
            return Err(RoomAddrError::Invalid);
        }
        let mut pathword: u64 = 1; // sentinel
        for &d in &self.path {
            pathword = (pathword << 2) | u64::from(d);
        }
        Ok(RoomId((pathword << 5) | u64::from(self.face)))
    }
}

impl RoomId {
    /// Unpack to a `RoomAddr`. Validates the face and the sentinel; not total.
    pub fn unpack(&self) -> Result<RoomAddr, RoomIdError> {
        let face = (self.0 & 0x1F) as u8;
        if face >= 20 {
            return Err(RoomIdError::Malformed);
        }
        let pathword = self.0 >> 5;
        if pathword == 0 {
            return Err(RoomIdError::Malformed);
        }
        let top = 63 - pathword.leading_zeros(); // index of the sentinel bit = 2*len
        let len = (top / 2) as usize;
        let mut path = Vec::with_capacity(len);
        for i in (0..len).rev() {
            path.push(((pathword >> (2 * i)) & 0b11) as u8);
        }
        Ok(RoomAddr { face, path })
    }
}
```

- [ ] **Step 4: Run the round-trip test to verify it passes**

Run: `cargo test -p hornvale-kernel room::tests::roomid_round_trips`
Expected: PASS.

- [ ] **Step 5: Add validity tests (cap and malformed)**

Add to the `tests` module:

```rust
    #[test]
    fn pack_rejects_over_cap() {
        let too_deep = RoomAddr { face: 0, path: vec![0; MAX_DEPTH + 1] };
        assert_eq!(too_deep.pack(), Err(RoomAddrError::DepthExceedsCap));
    }

    #[test]
    fn unpack_rejects_malformed() {
        assert_eq!(RoomId(20).unpack(), Err(RoomIdError::Malformed)); // face 20, no path
        assert_eq!(RoomId(0).unpack(), Err(RoomIdError::Malformed)); // face 0, pathword 0
    }
```

Run: `cargo test -p hornvale-kernel room::tests`
Expected: PASS (all three).

- [ ] **Step 6: fmt + clippy + commit**

```bash
cargo fmt && cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add kernel/src/room.rs kernel/src/lib.rs
git commit -m "feat(kernel): RoomAddr/RoomId addressing and the u64 packing contract"
```

---

### Task 3: Lazy geometry — `corners`, `centroid`, `coord`, `bearing`

**Files:**
- Modify: `kernel/src/room.rs`

**Interfaces:**
- Consumes: `geosphere::base_data`, `geosphere::slerp_mid`, `geosphere::normalize`; `GeoCoord`.
- Produces: `RoomAddr::corners(&self) -> [[f64;3];3]`; `RoomAddr::centroid(&self) -> [f64;3]`; `RoomAddr::coord(&self) -> GeoCoord`; `RoomAddr::bearing_to(&self, other: &RoomAddr) -> f64`.

- [ ] **Step 1: Write the failing oracle geometry test**

The oracle is a fully-built `Geosphere`. Because the replica in the spike proved byte-identity, we assert the lazy corners of every face at a modest level equal the mesh geometry. To connect a `RoomAddr` to a mesh vertex we walk *both* the same way. Add to the `tests` module:

```rust
    use crate::geosphere::{Geosphere, base_data};

    // Enumerate every RoomAddr at depth `level` by DFS over child digits.
    fn all_addrs(level: u32) -> Vec<RoomAddr> {
        let mut out = Vec::new();
        for face in 0..20u8 {
            let mut stack = vec![RoomAddr { face, path: vec![] }];
            while let Some(a) = stack.pop() {
                if a.path.len() as u32 == level {
                    out.push(a);
                } else {
                    for d in 0..4u8 {
                        let mut p = a.path.clone();
                        p.push(d);
                        stack.push(RoomAddr { face: a.face, path: p });
                    }
                }
            }
        }
        out
    }

    #[test]
    fn lazy_geometry_is_byte_identical_to_geosphere() {
        // Build a reference mesh by replicating subdivide while tracking, per
        // face, its (RoomAddr, [global vertex index; 3]); then compare lazy
        // corners against the mesh's own vertex positions. See helper below.
        let level = 5u32;
        let (positions, face_addrs, face_gverts) = reference_mesh(level);
        let geo = Geosphere::new(level);
        // 1. the replica equals Geosphere byte-for-byte (trust transfer)
        assert_eq!(positions.len(), geo.cell_count());
        for id in 0..positions.len() {
            assert_eq!(positions[id], geo.position(crate::CellId(id as u32)));
        }
        // 2. lazy corners equal the mesh vertices for every face
        for (addr, gverts) in face_addrs.iter().zip(face_gverts.iter()) {
            let lazy = addr.corners();
            for i in 0..3 {
                assert_eq!(lazy[i], positions[gverts[i] as usize], "corner drift at {addr:?}");
            }
        }
    }
```

- [ ] **Step 2: Add the reference-mesh helper (oracle) to the tests module**

This mirrors `subdivide` while carrying each face's `RoomAddr` and global vertex indices. It is test-only.

```rust
    // Test oracle: replicate subdivide carrying (RoomAddr, [gvert;3]) per face.
    fn reference_mesh(level: u32) -> (Vec<[f64; 3]>, Vec<RoomAddr>, Vec<[u32; 3]>) {
        use crate::geosphere::slerp_mid;
        use std::collections::BTreeMap;
        let (verts, faces) = base_data().clone();
        let mut positions = verts;
        let mut addrs: Vec<RoomAddr> =
            (0..faces.len() as u8).map(|f| RoomAddr { face: f, path: vec![] }).collect();
        let mut gverts: Vec<[u32; 3]> = faces;
        for _ in 0..level {
            let mut cache: BTreeMap<(u32, u32), u32> = BTreeMap::new();
            let mut midpoint = |a: u32, b: u32, pos: &mut Vec<[f64; 3]>| -> u32 {
                let key = (a.min(b), a.max(b));
                if let Some(&idx) = cache.get(&key) {
                    return idx;
                }
                let mid = slerp_mid(pos[a as usize], pos[b as usize]);
                let idx = pos.len() as u32;
                pos.push(mid);
                cache.insert(key, idx);
                idx
            };
            let mut na = Vec::with_capacity(addrs.len() * 4);
            let mut ng = Vec::with_capacity(gverts.len() * 4);
            for (addr, &[a, b, c]) in addrs.iter().zip(gverts.iter()) {
                let ab = midpoint(a, b, &mut positions);
                let bc = midpoint(b, c, &mut positions);
                let ca = midpoint(c, a, &mut positions);
                for (digit, gv) in [(0u8, [a, ab, ca]), (1, [b, bc, ab]), (2, [c, ca, bc]), (3, [ab, bc, ca])] {
                    let mut p = addr.path.clone();
                    p.push(digit);
                    na.push(RoomAddr { face: addr.face, path: p });
                    ng.push(gv);
                }
            }
            addrs = na;
            gverts = ng;
        }
        (positions, addrs, gverts)
    }
```

Run: `cargo test -p hornvale-kernel room::tests::lazy_geometry_is_byte_identical_to_geosphere`
Expected: FAIL to compile — `corners` not defined.

- [ ] **Step 3: Implement `corners`, `centroid`, `coord`, `bearing_to`**

Add to `kernel/src/room.rs`:

```rust
use crate::GeoCoord;
use crate::geosphere::{base_data, normalize, slerp_mid};

impl RoomAddr {
    /// The three unit-sphere corner positions of the room's triangle, in the
    /// mesh's corner order. Byte-identical to the same face in
    /// `Geosphere::new(self.path.len())`.
    pub fn corners(&self) -> [[f64; 3]; 3] {
        let (verts, faces) = base_data();
        let g = faces[self.face as usize];
        let mut v = [
            verts[g[0] as usize],
            verts[g[1] as usize],
            verts[g[2] as usize],
        ];
        for &d in &self.path {
            let ab = slerp_mid(v[0], v[1]);
            let bc = slerp_mid(v[1], v[2]);
            let ca = slerp_mid(v[2], v[0]);
            v = match d {
                0 => [v[0], ab, ca],
                1 => [v[1], bc, ab],
                2 => [v[2], ca, bc],
                _ => [ab, bc, ca],
            };
        }
        v
    }

    /// The room centroid — `normalize(v0 + v1 + v2)`.
    pub fn centroid(&self) -> [f64; 3] {
        let [a, b, c] = self.corners();
        normalize([a[0] + b[0] + c[0], a[1] + b[1] + c[1], a[2] + b[2] + c[2]])
    }

    /// The geographic coordinate of the centroid (matches `Geosphere::coord`).
    pub fn coord(&self) -> GeoCoord {
        let [x, y, z] = self.centroid();
        GeoCoord {
            latitude: z.asin().to_degrees(),
            longitude: y.atan2(x).to_degrees(),
        }
    }

    /// Great-circle initial azimuth (degrees, clockwise from north) from this
    /// room's centroid to `other`'s. For rendering and exit-naming only.
    pub fn bearing_to(&self, other: &RoomAddr) -> f64 {
        let a = self.coord();
        let b = other.coord();
        let (lat1, lat2) = (a.latitude.to_radians(), b.latitude.to_radians());
        let dlon = (b.longitude - a.longitude).to_radians();
        let y = dlon.sin() * lat2.cos();
        let x = lat1.cos() * lat2.sin() - lat1.sin() * lat2.cos() * dlon.cos();
        let deg = y.atan2(x).to_degrees();
        (deg + 360.0) % 360.0
    }
}
```

Note the `_ => [ab, bc, ca]` arm handles digit 3 and keeps the match exhaustive; `pack`/`child` already reject digits `>= 4`.

- [ ] **Step 4: Run the oracle geometry test to verify it passes**

Run: `cargo test -p hornvale-kernel room::tests::lazy_geometry_is_byte_identical_to_geosphere`
Expected: PASS.

- [ ] **Step 5: Add a determinism test and a bearing sanity test**

```rust
    #[test]
    fn geometry_is_deterministic() {
        let a = RoomAddr { face: 11, path: vec![0, 3, 1, 2, 3, 0] };
        assert_eq!(a.corners(), a.clone().corners());
        assert_eq!(a.centroid(), a.centroid());
    }

    #[test]
    fn bearing_is_in_range() {
        let a = RoomAddr { face: 0, path: vec![0, 1, 2] };
        let b = RoomAddr { face: 0, path: vec![3, 3, 3] };
        for bear in [a.bearing_to(&b), b.bearing_to(&a)] {
            assert!((0.0..360.0).contains(&bear), "bearing {bear} out of range");
        }
    }
```

Run: `cargo test -p hornvale-kernel room::tests`
Expected: PASS.

- [ ] **Step 6: fmt + clippy + commit**

```bash
cargo fmt && cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add kernel/src/room.rs
git commit -m "feat(kernel): lazy room geometry byte-identical to Geosphere"
```

---

### Task 4: Barycentric coordinates — forward walk and decode

The neighbour walk (Task 5) computes a neighbour's integer barycentric triple; converting it back to a path needs a decode. Both are integer-only and validated (the spike's replica + `ancestor`).

**Files:**
- Modify: `kernel/src/room.rs`

**Interfaces:**
- Produces (module-private): `type Bary = [i64;3]`; `fn bary_triple(path: &[u8]) -> (i64, [Bary;3])` (scale, triple); `fn decode(face: u8, tri: [Bary;3], scale: i64) -> RoomAddr`. Plus helpers `add`, `dbl`, `orient`, `strictly_inside`, `child_at_scale`.

- [ ] **Step 1: Write the failing path↔bary round-trip test**

```rust
    #[test]
    fn bary_forward_decode_round_trips() {
        for addr in all_addrs(6) {
            let (scale, tri) = bary_triple(&addr.path);
            let back = decode(addr.face, tri, scale);
            assert_eq!(back, addr, "bary round-trip for {addr:?}");
        }
    }
```

- [ ] **Step 2: Run it to see it fail**

Run: `cargo test -p hornvale-kernel room::tests::bary_forward_decode_round_trips`
Expected: FAIL to compile — `bary_triple`/`decode` not defined.

- [ ] **Step 3: Implement the barycentric primitives, forward walk, and decode**

Add to `kernel/src/room.rs` (module-private, above `impl RoomAddr`):

```rust
/// Integer barycentric coordinate within a base face; components sum to the
/// current scale `2^depth`.
type Bary = [i64; 3];

fn add(a: Bary, b: Bary) -> Bary {
    [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
}
fn dbl(a: Bary) -> Bary {
    [a[0] * 2, a[1] * 2, a[2] * 2]
}
fn mid_i(a: Bary, b: Bary) -> Bary {
    [(a[0] + b[0]) / 2, (a[1] + b[1]) / 2, (a[2] + b[2]) / 2]
}

/// Forward walk: a path to its ordered barycentric triple and its scale
/// `2^path.len()`. Child order matches `subdivide`.
fn bary_triple(path: &[u8]) -> (i64, [Bary; 3]) {
    let mut tri: [Bary; 3] = [[1, 0, 0], [0, 1, 0], [0, 0, 1]];
    let mut scale = 1i64;
    for &d in path {
        let [p0, p1, p2] = tri;
        let (m01, m12, m20) = (add(p0, p1), add(p1, p2), add(p2, p0));
        tri = match d {
            0 => [dbl(p0), m01, m20],
            1 => [dbl(p1), m12, m01],
            2 => [dbl(p2), m20, m12],
            _ => [m01, m12, m20],
        };
        scale *= 2;
    }
    (scale, tri)
}

/// 2x signed area in the (x,y) projection of the plane x+y+z=const.
fn orient(a: Bary, b: Bary, c: Bary) -> i64 {
    (b[0] - a[0]) * (c[1] - a[1]) - (c[0] - a[0]) * (b[1] - a[1])
}

/// Is `p` strictly inside triangle `t`? (all at the same scale)
fn strictly_inside(t: [Bary; 3], p: Bary) -> bool {
    let d = orient(t[0], t[1], t[2]);
    let s = [
        orient(t[1], t[2], p),
        orient(t[2], t[0], p),
        orient(t[0], t[1], p),
    ];
    s.iter().all(|&si| si != 0 && (si > 0) == (d > 0))
}

/// One subdivision at a FIXED scale (midpoint split), used by decode.
fn child_at_scale(r: [Bary; 3], digit: u8) -> [Bary; 3] {
    let [a, b, c] = r;
    let (mab, mbc, mca) = (mid_i(a, b), mid_i(b, c), mid_i(c, a));
    match digit {
        0 => [a, mab, mca],
        1 => [b, mbc, mab],
        2 => [c, mca, mbc],
        _ => [mab, mbc, mca],
    }
}

/// Decode a barycentric triple (at `scale = 2^depth`) on `face` back to a path,
/// by top-down containment: at each level pick the child whose triangle
/// contains the target centroid. Integer-only.
fn decode(face: u8, tri: [Bary; 3], scale: i64) -> RoomAddr {
    let depth = scale.trailing_zeros();
    let g3 = add(add(tri[0], tri[1]), tri[2]); // 3 * centroid, at scale `scale`
    let mut region: [Bary; 3] = [[scale, 0, 0], [0, scale, 0], [0, 0, scale]];
    let mut path = Vec::with_capacity(depth as usize);
    for _ in 0..depth {
        let digit = (0..4u8)
            .find(|&d| {
                let c = child_at_scale(region, d);
                // compare g3 (=3*centroid) against child scaled by 3
                let c3 = [
                    [c[0][0] * 3, c[0][1] * 3, c[0][2] * 3],
                    [c[1][0] * 3, c[1][1] * 3, c[1][2] * 3],
                    [c[2][0] * 3, c[2][1] * 3, c[2][2] * 3],
                ];
                strictly_inside(c3, g3)
            })
            .expect("centroid lies in exactly one child");
        region = child_at_scale(region, digit);
        path.push(digit);
    }
    RoomAddr { face, path }
}
```

- [ ] **Step 4: Run the round-trip test to verify it passes**

Run: `cargo test -p hornvale-kernel room::tests::bary_forward_decode_round_trips`
Expected: PASS.

- [ ] **Step 5: fmt + clippy + commit**

```bash
cargo fmt && cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add kernel/src/room.rs
git commit -m "feat(kernel): integer barycentric forward walk and decode for rooms"
```

---

### Task 5: The neighbour walk (interior + seam gluing)

The first-order risk, validated by the spike against the level-7 oracle.

**Files:**
- Modify: `kernel/src/room.rs`

**Interfaces:**
- Consumes: `bary_triple`, `decode`, `Bary`, `add`.
- Produces: `RoomAddr::neighbors(&self) -> [RoomAddr; 3]` (neighbor `i` is across the edge opposite corner `i`). Module-private: `VKey`, `vkey`, `base_edge`, `place_on_face`, `common_apex`, `base_edge_faces`.

- [ ] **Step 1: Write the failing oracle neighbour test**

The oracle derives face adjacency from shared global vertex *identity* (a face-independent key), a different mechanism than the apex rule. Add to `tests`:

```rust
    use std::collections::{BTreeMap, BTreeSet};

    // Canonical key of a face at uniform depth: (base face, sorted bary triple).
    fn fkey(face: u8, mut tri: [Bary; 3]) -> (u8, [Bary; 3]) {
        tri.sort();
        (face, tri)
    }

    #[test]
    fn neighbours_match_the_oracle() {
        let level = 5u32;
        let scale = 1i64 << level;
        let addrs = all_addrs(level);
        let (verts, faces) = base_data().clone();
        // oracle: map each global vertex key -> faces; adjacency = share 2 keys.
        let mut vk_faces: BTreeMap<super::VKey, Vec<usize>> = BTreeMap::new();
        let mut fkeys: Vec<(u8, [Bary; 3])> = Vec::new();
        let mut face_vkeys: Vec<[super::VKey; 3]> = Vec::new();
        for addr in &addrs {
            let (_s, tri) = super::bary_triple(&addr.path);
            let g = faces[addr.face as usize];
            let ks = [
                super::vkey(addr.face, g, tri[0], scale),
                super::vkey(addr.face, g, tri[1], scale),
                super::vkey(addr.face, g, tri[2], scale),
            ];
            let idx = fkeys.len();
            fkeys.push(fkey(addr.face, tri));
            face_vkeys.push(ks);
            for k in ks {
                vk_faces.entry(k).or_default().push(idx);
            }
        }
        let _ = &verts;
        for (idx, addr) in addrs.iter().enumerate() {
            let ks = face_vkeys[idx];
            let mut oracle: BTreeSet<(u8, [Bary; 3])> = BTreeSet::new();
            for (a, b) in [(0, 1), (1, 2), (2, 0)] {
                let sa: BTreeSet<usize> = vk_faces[&ks[a]].iter().copied().collect();
                let shared: Vec<usize> =
                    vk_faces[&ks[b]].iter().copied().filter(|o| sa.contains(o) && *o != idx).collect();
                assert_eq!(shared.len(), 1, "one neighbour per edge");
                oracle.insert(fkeys[shared[0]]);
            }
            let mine: BTreeSet<(u8, [Bary; 3])> = addr
                .neighbors()
                .iter()
                .map(|n| {
                    let (_s, t) = super::bary_triple(&n.path);
                    fkey(n.face, t)
                })
                .collect();
            assert_eq!(mine, oracle, "neighbour mismatch at {addr:?}");
        }
    }
```

- [ ] **Step 2: Run it to see it fail**

Run: `cargo test -p hornvale-kernel room::tests::neighbours_match_the_oracle`
Expected: FAIL to compile — `neighbors`, `VKey`, `vkey` not defined.

- [ ] **Step 3: Implement global vertex identity, the gluing table, and the apex rule**

Add to `kernel/src/room.rs` (module-private items `pub(crate)` so the test can name them via `super::`):

```rust
use std::collections::{BTreeMap, BTreeSet};

/// Face-independent identity of a triangle corner. Two rooms are edge-adjacent
/// iff they share two of these.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(crate) enum VKey {
    Interior(u8, i64, i64, i64),
    Edge(u32, u32, i64),
    Vertex(u32),
}

pub(crate) fn vkey(face: u8, g: [u32; 3], c: Bary, s: i64) -> VKey {
    match c.iter().filter(|&&x| x == 0).count() {
        0 => VKey::Interior(face, c[0], c[1], c[2]),
        1 => {
            let mut ends: Vec<(u32, i64)> =
                (0..3).filter(|&i| c[i] != 0).map(|i| (g[i], c[i])).collect();
            ends.sort_by_key(|&(gi, _)| gi);
            VKey::Edge(ends[0].0, ends[1].0, ends[0].1)
        }
        _ => VKey::Vertex(g[(0..3).find(|&i| c[i] == s).unwrap()]),
    }
}

fn base_edge(a: VKey, b: VKey) -> (u32, u32) {
    let mut set = BTreeSet::new();
    for k in [a, b] {
        match k {
            VKey::Edge(lo, hi, _) => {
                set.insert(lo);
                set.insert(hi);
            }
            VKey::Vertex(g) => {
                set.insert(g);
            }
            VKey::Interior(..) => unreachable!("interior key on a boundary edge"),
        }
    }
    let v: Vec<u32> = set.into_iter().collect();
    (v[0], v[1])
}

fn place_on_face(k: VKey, hg: [u32; 3], s: i64) -> Bary {
    let idx = |g: u32| hg.iter().position(|&x| x == g).expect("vertex on face");
    let mut c = [0i64; 3];
    match k {
        VKey::Edge(lo, hi, wlo) => {
            c[idx(lo)] = wlo;
            c[idx(hi)] = s - wlo;
        }
        VKey::Vertex(g) => c[idx(g)] = s,
        VKey::Interior(..) => unreachable!("interior key on a seam"),
    }
    c
}

const UNITS: [Bary; 6] = [
    [1, -1, 0], [-1, 1, 0], [0, 1, -1], [0, -1, 1], [1, 0, -1], [-1, 0, 1],
];

fn in_range(v: Bary, s: i64) -> bool {
    v.iter().all(|&x| (0..=s).contains(&x))
}

/// The unique in-range lattice point adjacent to both `u` and `v` with edge
/// step `h` — the inward apex on a base-face seam.
fn common_apex(u: Bary, v: Bary, s: i64, h: i64) -> Bary {
    let nbr = |p: Bary| -> BTreeSet<Bary> {
        UNITS
            .iter()
            .map(|e| add(p, [e[0] * h, e[1] * h, e[2] * h]))
            .filter(|&x| in_range(x, s))
            .collect()
    };
    let both: Vec<Bary> = nbr(u).intersection(&nbr(v)).copied().collect();
    debug_assert_eq!(both.len(), 1, "cross-face inward apex must be unique");
    both[0]
}

/// Base edge -> the two faces sharing it, computed once.
fn base_edge_faces() -> &'static BTreeMap<(u32, u32), [u8; 2]> {
    use std::sync::OnceLock;
    static T: OnceLock<BTreeMap<(u32, u32), [u8; 2]>> = OnceLock::new();
    T.get_or_init(|| {
        let (_v, faces) = base_data();
        let mut acc: BTreeMap<(u32, u32), Vec<u8>> = BTreeMap::new();
        for (i, g) in faces.iter().enumerate() {
            for (a, b) in [(g[0], g[1]), (g[1], g[2]), (g[2], g[0])] {
                acc.entry((a.min(b), a.max(b))).or_default().push(i as u8);
            }
        }
        acc.into_iter().map(|(k, v)| (k, [v[0], v[1]])).collect()
    })
}
```

- [ ] **Step 4: Implement `neighbors`**

Add to `impl RoomAddr`:

```rust
    /// The three edge-neighbour rooms, at the same depth (the geometric base
    /// graph). `neighbor[i]` is across the edge opposite corner `i` (between
    /// corners `(i+1)%3` and `(i+2)%3`). Integer-only; passability and overlay
    /// edges are higher layers and never enter here.
    pub fn neighbors(&self) -> [RoomAddr; 3] {
        let (scale, tri) = bary_triple(&self.path);
        let h = scale >> self.path.len(); // = 1; edge step at this depth
        let (_v, faces) = base_data();
        let g = faces[self.face as usize];
        let mut out: Vec<RoomAddr> = Vec::with_capacity(3);
        // neighbor[n] is across the edge OPPOSITE corner n, so the excluded
        // corner k == n: tuples ordered (i,j,k) with k = 0, 1, 2.
        for (i, j, k) in [(1usize, 2, 0), (2, 0, 1), (0, 1, 2)] {
            let (u, v, w) = (tri[i], tri[j], tri[k]);
            let apex = add(add(u, v), [-w[0], -w[1], -w[2]]); // U + V - W_self
            if in_range(apex, scale) && apex != w {
                out.push(decode(self.face, [u, v, apex], scale));
            } else {
                let (ku, kv) = (vkey(self.face, g, u, scale), vkey(self.face, g, v, scale));
                let (lo, hi) = base_edge(ku, kv);
                let pair = base_edge_faces()[&(lo, hi)];
                let fp = if pair[0] == self.face { pair[1] } else { pair[0] };
                let hg = faces[fp as usize];
                let (u2, v2) = (place_on_face(ku, hg, scale), place_on_face(kv, hg, scale));
                let apex2 = common_apex(u2, v2, scale, h);
                out.push(decode(fp, [u2, v2, apex2], scale));
            }
        }
        [out[0].clone(), out[1].clone(), out[2].clone()]
    }
```

- [ ] **Step 5: Run the oracle neighbour test**

Run: `cargo test -p hornvale-kernel room::tests::neighbours_match_the_oracle`
Expected: PASS.

- [ ] **Step 6: Add the property tests (symmetry, exactly-three, pentagon)**

```rust
    #[test]
    fn adjacency_is_mutual_and_ternary() {
        let addrs = all_addrs(4);
        let present: BTreeSet<RoomAddr> = addrs.iter().cloned().collect();
        for addr in &addrs {
            let ns = addr.neighbors();
            let uniq: BTreeSet<&RoomAddr> = ns.iter().collect();
            assert_eq!(uniq.len(), 3, "exactly three distinct neighbours at {addr:?}");
            for n in &ns {
                assert!(present.contains(n), "neighbour {n:?} not on the mesh");
                assert!(n.neighbors().contains(addr), "not mutual: {addr:?} -> {n:?}");
            }
        }
    }

    #[test]
    fn corner_at_pentagon_vertex_walks() {
        // The all-0 path keeps a corner at a base icosahedron vertex (5-valent).
        let addr = RoomAddr { face: 0, path: vec![0; 5] };
        let ns = addr.neighbors();
        assert_eq!(ns.iter().collect::<BTreeSet<_>>().len(), 3);
        for n in &ns {
            assert!(n.neighbors().contains(&addr), "pentagon neighbour not mutual");
        }
    }

    #[test]
    fn neighbor_order_matches_opposite_corner_contract() {
        // neighbor[n] is across the edge opposite corner n: it shares the OTHER
        // two corners' positions with self, and not corner n's.
        for addr in all_addrs(2) {
            let sc = addr.corners();
            let ns = addr.neighbors();
            for n in 0..3usize {
                let nc = ns[n].corners();
                let shares = |p: [f64; 3]| nc.iter().any(|&q| q == p);
                assert!(!shares(sc[n]), "neighbor {n} of {addr:?} should not share corner {n}");
                assert!(shares(sc[(n + 1) % 3]), "neighbor {n} of {addr:?} misses a shared corner");
                assert!(shares(sc[(n + 2) % 3]), "neighbor {n} of {addr:?} misses a shared corner");
            }
        }
    }
```

Run: `cargo test -p hornvale-kernel room::tests`
Expected: PASS (all).

- [ ] **Step 7: fmt + clippy + commit**

```bash
cargo fmt && cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add kernel/src/room.rs
git commit -m "feat(kernel): O(1) barycentric room neighbour walk with seam gluing"
```

---

### Task 6: Vertical verbs and integer-address seeding

**Files:**
- Create: `kernel/src/streams.rs` (the frozen room seed labels + `stream_labels()`)
- Modify: `kernel/src/room.rs`
- Modify: `kernel/src/lib.rs` (register `streams`, re-export `stream_labels`)

**Interfaces:**
- Consumes: `Seed`.
- Produces: `kernel::streams::{ROOM_FACE, ROOM_CHILD}` (label constants); `kernel::stream_labels() -> Vec<(&'static str, &'static str)>`; `RoomAddr::parent(&self) -> Option<RoomAddr>`; `RoomAddr::child(&self, digit: u8) -> Result<RoomAddr, RoomAddrError>`; `RoomAddr::ancestor(&self, depth: u32) -> Option<RoomAddr>`; `RoomAddr::seed(&self, world: Seed) -> Seed`.

- [ ] **Step 1: Write the failing verb + seed tests**

```rust
    use crate::Seed;

    #[test]
    fn vertical_verbs_compose() {
        let a = RoomAddr { face: 4, path: vec![1, 2, 3] };
        let child = a.child(0).unwrap();
        assert_eq!(child.path, vec![1, 2, 3, 0]);
        assert_eq!(child.parent(), Some(a.clone()));
        assert_eq!(a.ancestor(1), Some(RoomAddr { face: 4, path: vec![1] }));
        assert_eq!(RoomAddr { face: 4, path: vec![] }.parent(), None);
        assert_eq!(a.child(4), Err(RoomAddrError::Invalid));
    }

    #[test]
    fn room_seed_is_deterministic_and_hierarchical() {
        let world = Seed(42);
        let a = RoomAddr { face: 3, path: vec![0, 1, 2] };
        assert_eq!(a.seed(world), a.seed(world));
        // a parent and its child derive different seeds
        assert_ne!(a.seed(world), a.parent().unwrap().seed(world));
    }
```

- [ ] **Step 2: Run to see it fail**

Run: `cargo test -p hornvale-kernel room::tests::vertical_verbs_compose`
Expected: FAIL to compile.

- [ ] **Step 3: Create `kernel/src/streams.rs` with the frozen labels**

Match the domain `streams.rs` convention (see `domains/astronomy/src/streams.rs`). Create `kernel/src/streams.rs`:

```rust
//! Seed-derivation labels for the kernel — permanent save-format contracts
//! (The Room Mesh spec §6/§10). Changing one silently corrupts every world.

/// Root label for a room's base face.
/// type-audit: bare-ok(identifier-text)
pub const ROOM_FACE: &str = "room/face";
/// Label for a room's child descent.
/// type-audit: bare-ok(identifier-text)
pub const ROOM_CHILD: &str = "room/child";

/// Every kernel seed label, for the generated stream manifest.
/// type-audit: bare-ok(artifact: return)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        (ROOM_FACE, "room base face"),
        (ROOM_CHILD, "room child descent"),
    ]
}
```

Register it in `kernel/src/lib.rs`: add `pub mod streams;` (after `pub mod seed;`) and `pub use streams::stream_labels;` in the re-export block.

- [ ] **Step 4: Implement the verbs and the seed**

Add to `kernel/src/room.rs`:

```rust
use crate::Seed;
use crate::streams::{ROOM_CHILD, ROOM_FACE};

impl RoomAddr {
    /// The containing room one level coarser, or `None` at a base face.
    pub fn parent(&self) -> Option<RoomAddr> {
        if self.path.is_empty() {
            return None;
        }
        let mut path = self.path.clone();
        path.pop();
        Some(RoomAddr { face: self.face, path })
    }

    /// Descend into child `digit` (0..4). Fails past `MAX_DEPTH` or on a bad digit.
    pub fn child(&self, digit: u8) -> Result<RoomAddr, RoomAddrError> {
        if digit >= 4 {
            return Err(RoomAddrError::Invalid);
        }
        if self.path.len() >= MAX_DEPTH {
            return Err(RoomAddrError::DepthExceedsCap);
        }
        let mut path = self.path.clone();
        path.push(digit);
        Ok(RoomAddr { face: self.face, path })
    }

    /// The containing room at coarser `depth`, or `None` if `depth > self.depth()`.
    pub fn ancestor(&self, depth: u32) -> Option<RoomAddr> {
        let d = depth as usize;
        if d > self.path.len() {
            return None;
        }
        Some(RoomAddr { face: self.face, path: self.path[..d].to_vec() })
    }

    /// Deterministic per-room seed, derived from the integer address only —
    /// never the float position, so all room content is platform-exact.
    pub fn seed(&self, world: Seed) -> Seed {
        let mut s = world.derive(ROOM_FACE).derive(&self.face.to_string());
        for &d in &self.path {
            s = s.derive(ROOM_CHILD).derive(&d.to_string());
        }
        s
    }
}
```

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cargo test -p hornvale-kernel room::tests::vertical_verbs_compose room::tests::room_seed_is_deterministic_and_hierarchical`
Expected: PASS. (The cross-crate stream-manifest wiring — adding the kernel to `cli/src/streams.rs` and regenerating the artifact — is Task 8, Step 2b.)

- [ ] **Step 6: fmt + clippy + commit**

```bash
cargo fmt && cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add kernel/src/room.rs kernel/src/streams.rs kernel/src/lib.rs
git commit -m "feat(kernel): room vertical verbs and integer-address seeding"
```

---

### Task 7: Coarse-field inheritance hooks

**Files:**
- Modify: `kernel/src/room.rs`
- Modify: `kernel/src/geosphere.rs` (add `NearestCellIndex::nearest_to_position`)

**Interfaces:**
- Consumes: `Geosphere`, `NearestCellIndex`, `CellId`.
- Produces: `NearestCellIndex::nearest_to_position(&self, geo: &Geosphere, pos: [f64;3]) -> CellId`; `RoomAddr::corner_weights(&self, geo: &Geosphere, index: &NearestCellIndex) -> Option<[(CellId, u64); 3]>`.

- [ ] **Step 1: Add an exact position-based nearest lookup to `NearestCellIndex`**

The corner positions we look up are *exactly* mesh vertices, so a dot-product search finds them with no lat/lon round-trip. Add to `kernel/src/geosphere.rs`:

```rust
    /// The cell nearest a unit-sphere position, by maximum dot product. Because
    /// a room's ancestor-corner positions are byte-identical to mesh vertices,
    /// this returns that exact vertex (self-dot = 1.0 wins).
    pub fn nearest_to_position(&self, geo: &Geosphere, pos: [f64; 3]) -> CellId {
        let latitude = pos[2].asin().to_degrees();
        let band = (((90.0 - latitude) / BAND_DEGREES) as usize).min(BAND_COUNT - 1);
        let lo = band.saturating_sub(1);
        let hi = (band + 1).min(BAND_COUNT - 1);
        let mut best = CellId(0);
        let mut best_dot = f64::NEG_INFINITY;
        for cells in &self.bands[lo..=hi] {
            for &cell in cells {
                let d = dot3(geo.position(cell), pos);
                if d > best_dot {
                    best_dot = d;
                    best = cell;
                }
            }
        }
        best
    }
```

- [ ] **Step 2: Write the failing inheritance test**

Add to `kernel/src/room.rs` tests:

```rust
    use crate::{CellId, CellMap, NearestCellIndex};

    #[test]
    fn corner_weights_sum_and_blend() {
        let geo = Geosphere::new(3); // globe level 3 for a cheap test
        let index = NearestCellIndex::new(&geo);
        // a room several levels below the grid
        let addr = RoomAddr { face: 6, path: vec![0, 3, 1, 2, 3] };
        let denom: u64 = 3 << (addr.path.len() as u32 - geo.level());
        let ws = addr.corner_weights(&geo, &index).expect("below the grid");
        let sum: u64 = ws.iter().map(|&(_, w)| w).sum();
        assert_eq!(sum, denom, "weights sum to 3*2^(depth-globe)");
        // a constant field blends to the constant
        let field = CellMap::from_fn(&geo, |_| 5.0f64);
        let blended: f64 = ws.iter().map(|&(c, w)| (w as f64 / denom as f64) * field.get(c)).sum();
        assert!((blended - 5.0).abs() < 1e-9, "constant field blends to the constant");
        // above the grid -> None
        let coarse = RoomAddr { face: 6, path: vec![0, 3] };
        assert!(coarse.corner_weights(&geo, &index).is_none());
    }
```

- [ ] **Step 3: Run to see it fail**

Run: `cargo test -p hornvale-kernel room::tests::corner_weights_sum_and_blend`
Expected: FAIL to compile — `corner_weights` not defined.

- [ ] **Step 4: Implement `corner_weights`**

Add to `impl RoomAddr` (needs `Geosphere`, `NearestCellIndex`, `CellId` in scope):

```rust
use crate::{CellId, Geosphere, NearestCellIndex};

impl RoomAddr {
    /// The room's three canonical-grid corner cells, each with its integer
    /// blend weight at the room centroid (numerators over
    /// `D = 3 << (depth - globe_level)`, summing to `D`). `None` if the room is
    /// coarser than the grid (`depth < geo.level()`).
    pub fn corner_weights(
        &self,
        geo: &Geosphere,
        index: &NearestCellIndex,
    ) -> Option<[(CellId, u64); 3]> {
        let gl = geo.level();
        if self.depth() < gl {
            return None;
        }
        // Ancestor triangle at the globe level: its 3 corner positions (exact
        // mesh vertices) resolve to CellIds.
        let anc = RoomAddr { face: self.face, path: self.path[..gl as usize].to_vec() };
        let corner_pos = anc.corners();
        let cells = [
            index.nearest_to_position(geo, corner_pos[0]),
            index.nearest_to_position(geo, corner_pos[1]),
            index.nearest_to_position(geo, corner_pos[2]),
        ];
        // Centroid barycentric within the ancestor: numerators over 3*2^d.
        // Express this room's corners in the ancestor's scale (2^d, d=depth-gl),
        // then the centroid numerators are the summed corner coords.
        let (scale, tri) = bary_triple(&self.path[gl as usize..]);
        debug_assert_eq!(scale, 1i64 << (self.depth() - gl));
        let centroid_num = add(add(tri[0], tri[1]), tri[2]); // sums to 3*scale per axis-total
        Some([
            (cells[0], centroid_num[0] as u64),
            (cells[1], centroid_num[1] as u64),
            (cells[2], centroid_num[2] as u64),
        ])
    }
}
```

Note: `self.path[gl..]` re-walks the sub-path *within* the ancestor to get barycentric coordinates relative to the ancestor triangle — the ancestor's own corners are `(scale,0,0)`,`(0,scale,0)`,`(0,0,scale)`, so the centroid numerators `centroid_num[axis]` sum to `3*scale = D`. Each numerator pairs with the ancestor corner of that axis; `anc.corners()[axis]` and `bary_triple(sub)`'s axis share the same corner order (both inherit the base-face corner order), so `cells[axis]` is the correct corner for `centroid_num[axis]`.

- [ ] **Step 5: Run the inheritance test**

Run: `cargo test -p hornvale-kernel room::tests::corner_weights_sum_and_blend`
Expected: PASS.

- [ ] **Step 6: Add a pentagon-corner inheritance test**

```rust
    #[test]
    fn inheritance_at_a_pentagon_corner() {
        let geo = Geosphere::new(3);
        let index = NearestCellIndex::new(&geo);
        // all-0 path keeps a corner at a base vertex (a 5-valent pentagon cell)
        let addr = RoomAddr { face: 0, path: vec![0, 0, 0, 0, 0] };
        let ws = addr.corner_weights(&geo, &index).expect("below the grid");
        let denom: u64 = 3 << (addr.path.len() as u32 - geo.level());
        assert_eq!(ws.iter().map(|&(_, w)| w).sum::<u64>(), denom);
        // the three corner cells are distinct valid ids
        let ids: BTreeSet<CellId> = ws.iter().map(|&(c, _)| c).collect();
        assert_eq!(ids.len(), 3);
    }
```

Run: `cargo test -p hornvale-kernel room::tests`
Expected: PASS (all).

- [ ] **Step 7: fmt + clippy + commit**

```bash
cargo fmt && cargo clippy -p hornvale-kernel --all-targets -- -D warnings
git add kernel/src/room.rs kernel/src/geosphere.rs
git commit -m "feat(kernel): coarse-field inheritance hooks for rooms"
```

---

### Task 8: Type-audit tags, docs, and campaign Definition of Done

**Files:**
- Modify: `kernel/src/room.rs` (type-audit tags)
- Create: `book/src/chronicle/<n>-the-room-mesh.md` (chronicle entry)
- Create: `docs/retrospectives/<n>-the-room-mesh.md`
- Modify: `book/src/frontier/idea-registry.md` (flip MAP-28), the confidence-gradient chapter if a bet moved
- Modify: `docs/design/room-scale/p2-subdivision-design.md` §15 (mark campaign 1 shipped)

- [ ] **Step 1: Add type-audit verdict tags to new pub-boundary primitives**

Every primitive at a `pub` boundary needs a `type-audit:` tag matching `geosphere.rs`'s conventions. Tag the `[f64;3]` returns, the `bearing_to` angle, the `u8`/`u64` fields, etc. Match what `geosphere.rs` already does for `position`/`coord`/`CellId`.

Run: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`
Expected: PASS (no untagged pub-boundary primitives). Fix any it names.

- [ ] **Step 2: Wire exports and run the full kernel gate**

Confirm `kernel/src/lib.rs` re-exports the public room surface (`RoomAddr`, `RoomId`, `MAX_DEPTH`, the error enums) and `stream_labels`. Then:

Run: `cargo test -p hornvale-kernel && cargo clippy -p hornvale-kernel --all-targets -- -D warnings && cargo fmt --check`
Expected: PASS.

- [ ] **Step 2b: Wire the kernel into the stream manifest and regenerate**

The manifest in `cli/src/streams.rs` aggregates only domain crates today; add the kernel. Change the array type width and add the row:

```rust
    let sources: [(&str, Vec<(&'static str, &'static str)>); 9] = [
        ("hornvale-kernel", hornvale_kernel::stream_labels()),
        ("hornvale-astronomy", hornvale_astronomy::stream_labels()),
        // ...the existing seven domain rows, unchanged...
    ];
```

Then regenerate the committed stream manifest with the exact command from the "Artifacts are current" step of `.github/workflows/ci.yml` (of the form `cargo run -p hornvale -- streams > book/src/reference/streams.md`), and confirm only the new kernel section is added.

Run: `git diff --stat book/src/reference/`
Expected: the streams manifest gains a `hornvale-kernel` section with `room/face` and `room/child`.

- [ ] **Step 3: Write the book chronicle entry**

Create the next-numbered `book/src/chronicle/<n>-the-room-mesh.md` (check the directory for the current highest number and the `SUMMARY.md` for the pattern). Write, at the book's altitude: what a room is, the addressing, the O(1) neighbour walk and why seams dissolve, the spike-then-build path, and the deliberate non-goals. Add it to `book/src/SUMMARY.md`.

- [ ] **Step 4: Freshness sweep + Confidence Gradient**

Sweep `book/` for chapters that now lag reality (the geosphere/spatial chapters). If this campaign moved a bet on `book/src/open-questions.md` (the room-scale / coarse-constrains-fine bets), re-score that chapter (decision `the-confidence-gradient-is-re-scored-not-frozen`).

- [ ] **Step 5: Flip the MAP-28 registry row**

In `book/src/frontier/idea-registry.md`, change MAP-28's status from `elaborated` to `spec'd` (or `shipped` once merged) and repoint **Where** at this spec/plan. Run the drift-check:

Run: `cargo test -p hornvale --test docs_consistency`
Expected: PASS.

- [ ] **Step 6: Retrospective + delete the spikes**

Write `docs/retrospectives/<n>-the-room-mesh.md` (process lessons: the spike-first approach, the ideonomy passes, the dyadic-denominator catch). Delete the throwaway spike crate from the scratchpad (it is superseded by the real module).

- [ ] **Step 7: Full workspace gate + commit**

Run: `cargo test --workspace && cargo clippy --workspace --all-targets -- -D warnings && cargo fmt --check`
Expected: PASS.

```bash
git add -A
git commit -m "docs(kernel): The Room Mesh chronicle, retrospective, MAP-28 spec'd"
```

---

## Notes for the implementer

- **The oracle is the single source of truth for correctness.** Tasks 3 and 5 build a fully-materialised mesh and assert the lazy result matches it. The `Geosphere::new(L)` oracle is only buildable to ~L8 (L9 ≈ 2.6 M cells); the tests use L5 for speed. Beyond the oracle ceiling, correctness is inductive (every level is the identical operation) plus the oracle-free round-trip/symmetry properties.
- **i64 suffices at the cap.** Barycentric coords reach `2^29`; `orient`'s products on the `×3` centroid scale stay under `2^63`. Do not widen to i128.
- **Identity is always the integer address.** Distinct deep rooms may share an `f64` centroid; never derive identity or a seed from a position.
- **Keep `neighbors` purely geometric.** Passability, blocked paths, and overlay edges (roads/tunnels/rivers) are higher layers keyed by `RoomId` — they must never enter this module.
