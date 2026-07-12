//! The Room Mesh: rooms are triangular faces of the same icosphere as
//! `Geosphere`, refined deeper and addressed by `(base face + child path)`.
//! Lazy, deterministic, and byte-identical to a fully-built `Geosphere`.
//! Identity, adjacency, and seeding are integer/rational; transcendentals live
//! only in position geometry (registry MAP-28).

use crate::GeoCoord;
use crate::Seed;
use crate::geosphere::{base_data, normalize, slerp_mid};
use crate::streams::{ROOM_CHILD, ROOM_FACE};
use std::collections::{BTreeMap, BTreeSet};

/// The deepest path a `RoomId` can pack: 5 face bits + 1 sentinel + 2*29 digit
/// bits = 64. Useful room scale is ~L16-20 (an L18 room edge is ~27 m).
/// type-audit: bare-ok(count)
pub const MAX_DEPTH: usize = 29;

/// A room — a triangular face of the icosphere at refinement depth
/// `path.len()`. Keyed to the base icosahedron face (level 0), so the address
/// is independent of the world's canonical globe level.
/// type-audit: bare-ok(index: face), bare-ok(index: path)
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
/// type-audit: bare-ok(constructor-edge)
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

/// Face-independent identity of a triangle corner. Two rooms are edge-adjacent
/// iff they share two of these.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub(crate) enum VKey {
    /// Interior lattice point: `(face, a, b, c)` barycentric coordinates.
    Interior(u8, i64, i64, i64),
    /// Boundary-edge lattice point: `(low global vertex, high global vertex,
    /// weight toward the low vertex)`.
    Edge(u32, u32, i64),
    /// A base-mesh vertex, identified by its global vertex index.
    Vertex(u32),
}

/// The global identity of barycentric point `c` on `face` (whose corners are
/// global vertices `g`), at scale `s`.
pub(crate) fn vkey(face: u8, g: [u32; 3], c: Bary, s: i64) -> VKey {
    match c.iter().filter(|&&x| x == 0).count() {
        0 => VKey::Interior(face, c[0], c[1], c[2]),
        1 => {
            let mut ends: Vec<(u32, i64)> = (0..3)
                .filter(|&i| c[i] != 0)
                .map(|i| (g[i], c[i]))
                .collect();
            ends.sort_by_key(|&(gi, _)| gi);
            VKey::Edge(ends[0].0, ends[1].0, ends[0].1)
        }
        _ => VKey::Vertex(g[(0..3).find(|&i| c[i] == s).unwrap()]),
    }
}

/// The pair of global vertices bounding the base edge that keys `a`/`b` (which
/// must both lie on the same boundary edge or a shared vertex).
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

/// Re-express boundary key `k` as a barycentric coordinate on the face whose
/// global corners are `hg`, at scale `s`.
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
    [1, -1, 0],
    [-1, 1, 0],
    [0, 1, -1],
    [0, -1, 1],
    [1, 0, -1],
    [-1, 0, 1],
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

impl RoomAddr {
    /// Refinement depth = path length.
    /// type-audit: bare-ok(count)
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

impl RoomAddr {
    /// The three unit-sphere corner positions of the room's triangle, in the
    /// mesh's corner order. Byte-identical to the same face in
    /// `Geosphere::new(self.path.len())`.
    /// type-audit: pending(wave-1)
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
    /// type-audit: pending(wave-1)
    pub fn centroid(&self) -> [f64; 3] {
        let [a, b, c] = self.corners();
        normalize([a[0] + b[0] + c[0], a[1] + b[1] + c[1], a[2] + b[2] + c[2]])
    }

    /// The geographic coordinate of the centroid (matches `Geosphere::coord`).
    /// type-audit: pending(wave-1)
    pub fn coord(&self) -> GeoCoord {
        let [x, y, z] = self.centroid();
        GeoCoord {
            latitude: z.asin().to_degrees(),
            longitude: y.atan2(x).to_degrees(),
        }
    }

    /// Great-circle initial azimuth (degrees, clockwise from north) from this
    /// room's centroid to `other`'s. For rendering and exit-naming only.
    /// type-audit: pending(wave-1)
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
                let fp = if pair[0] == self.face {
                    pair[1]
                } else {
                    pair[0]
                };
                let hg = faces[fp as usize];
                let (u2, v2) = (place_on_face(ku, hg, scale), place_on_face(kv, hg, scale));
                let apex2 = common_apex(u2, v2, scale, h);
                out.push(decode(fp, [u2, v2, apex2], scale));
            }
        }
        [out[0].clone(), out[1].clone(), out[2].clone()]
    }
}

impl RoomAddr {
    /// The containing room one level coarser, or `None` at a base face.
    pub fn parent(&self) -> Option<RoomAddr> {
        if self.path.is_empty() {
            return None;
        }
        let mut path = self.path.clone();
        path.pop();
        Some(RoomAddr {
            face: self.face,
            path,
        })
    }

    /// Descend into child `digit` (0..4). Fails past `MAX_DEPTH` or on a bad digit.
    /// type-audit: bare-ok(index)
    pub fn child(&self, digit: u8) -> Result<RoomAddr, RoomAddrError> {
        if digit >= 4 {
            return Err(RoomAddrError::Invalid);
        }
        if self.path.len() >= MAX_DEPTH {
            return Err(RoomAddrError::DepthExceedsCap);
        }
        let mut path = self.path.clone();
        path.push(digit);
        Ok(RoomAddr {
            face: self.face,
            path,
        })
    }

    /// The containing room at coarser `depth`, or `None` if `depth > self.depth()`.
    /// type-audit: bare-ok(count)
    pub fn ancestor(&self, depth: u32) -> Option<RoomAddr> {
        let d = depth as usize;
        if d > self.path.len() {
            return None;
        }
        Some(RoomAddr {
            face: self.face,
            path: self.path[..d].to_vec(),
        })
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Seed;
    use crate::geosphere::{Geosphere, base_data};
    use std::collections::{BTreeMap, BTreeSet};

    #[test]
    fn vertical_verbs_compose() {
        let a = RoomAddr {
            face: 4,
            path: vec![1, 2, 3],
        };
        let child = a.child(0).unwrap();
        assert_eq!(child.path, vec![1, 2, 3, 0]);
        assert_eq!(child.parent(), Some(a.clone()));
        assert_eq!(
            a.ancestor(1),
            Some(RoomAddr {
                face: 4,
                path: vec![1]
            })
        );
        assert_eq!(
            RoomAddr {
                face: 4,
                path: vec![]
            }
            .parent(),
            None
        );
        assert_eq!(a.child(4), Err(RoomAddrError::Invalid));
    }

    #[test]
    fn room_seed_is_deterministic_and_hierarchical() {
        let world = Seed(42);
        let a = RoomAddr {
            face: 3,
            path: vec![0, 1, 2],
        };
        assert_eq!(a.seed(world), a.seed(world));
        // a parent and its child derive different seeds
        assert_ne!(a.seed(world), a.parent().unwrap().seed(world));
    }

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
                let shared: Vec<usize> = vk_faces[&ks[b]]
                    .iter()
                    .copied()
                    .filter(|o| sa.contains(o) && *o != idx)
                    .collect();
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
                        stack.push(RoomAddr {
                            face: a.face,
                            path: p,
                        });
                    }
                }
            }
        }
        out
    }

    // Test oracle: replicate subdivide carrying (RoomAddr, [gvert;3]) per face.
    fn reference_mesh(level: u32) -> (Vec<[f64; 3]>, Vec<RoomAddr>, Vec<[u32; 3]>) {
        use crate::geosphere::slerp_mid;
        use std::collections::BTreeMap;
        let (verts, faces) = base_data().clone();
        let mut positions = verts;
        let mut addrs: Vec<RoomAddr> = (0..faces.len() as u8)
            .map(|f| RoomAddr {
                face: f,
                path: vec![],
            })
            .collect();
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
                for (digit, gv) in [
                    (0u8, [a, ab, ca]),
                    (1, [b, bc, ab]),
                    (2, [c, ca, bc]),
                    (3, [ab, bc, ca]),
                ] {
                    let mut p = addr.path.clone();
                    p.push(digit);
                    na.push(RoomAddr {
                        face: addr.face,
                        path: p,
                    });
                    ng.push(gv);
                }
            }
            addrs = na;
            gverts = ng;
        }
        (positions, addrs, gverts)
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
        for (id, &pos) in positions.iter().enumerate() {
            assert_eq!(pos, geo.position(crate::CellId(id as u32)));
        }
        // 2. lazy corners equal the mesh vertices for every face
        for (addr, gverts) in face_addrs.iter().zip(face_gverts.iter()) {
            let lazy = addr.corners();
            for i in 0..3 {
                assert_eq!(
                    lazy[i], positions[gverts[i] as usize],
                    "corner drift at {addr:?}"
                );
            }
        }
    }

    #[test]
    fn geometry_is_deterministic() {
        let a = RoomAddr {
            face: 11,
            path: vec![0, 3, 1, 2, 3, 0],
        };
        assert_eq!(a.corners(), a.clone().corners());
        assert_eq!(a.centroid(), a.centroid());
    }

    #[test]
    fn bearing_is_in_range() {
        let a = RoomAddr {
            face: 0,
            path: vec![0, 1, 2],
        };
        let b = RoomAddr {
            face: 0,
            path: vec![3, 3, 3],
        };
        for bear in [a.bearing_to(&b), b.bearing_to(&a)] {
            assert!((0.0..360.0).contains(&bear), "bearing {bear} out of range");
        }
    }

    #[test]
    fn roomid_round_trips() {
        let cases = [
            RoomAddr {
                face: 0,
                path: vec![],
            },
            RoomAddr {
                face: 19,
                path: vec![3],
            },
            RoomAddr {
                face: 7,
                path: vec![0, 1, 2, 3, 0, 1, 2],
            },
            RoomAddr {
                face: 3,
                path: vec![3; MAX_DEPTH],
            },
        ];
        for addr in cases {
            let id = addr.pack().expect("packs");
            assert_eq!(
                id.unpack().expect("unpacks"),
                addr,
                "round-trip for {addr:?}"
            );
        }
    }

    #[test]
    fn pack_rejects_over_cap() {
        let too_deep = RoomAddr {
            face: 0,
            path: vec![0; MAX_DEPTH + 1],
        };
        assert_eq!(too_deep.pack(), Err(RoomAddrError::DepthExceedsCap));
    }

    #[test]
    fn unpack_rejects_malformed() {
        assert_eq!(RoomId(20).unpack(), Err(RoomIdError::Malformed)); // face 20, no path
        assert_eq!(RoomId(0).unpack(), Err(RoomIdError::Malformed)); // face 0, pathword 0
    }

    #[test]
    fn bary_forward_decode_round_trips() {
        for addr in all_addrs(6) {
            let (scale, tri) = bary_triple(&addr.path);
            let back = decode(addr.face, tri, scale);
            assert_eq!(back, addr, "bary round-trip for {addr:?}");
        }
    }

    #[test]
    fn pack_rejects_invalid() {
        assert_eq!(
            RoomAddr {
                face: 20,
                path: vec![]
            }
            .pack(),
            Err(RoomAddrError::Invalid)
        );
        assert_eq!(
            RoomAddr {
                face: 0,
                path: vec![4]
            }
            .pack(),
            Err(RoomAddrError::Invalid)
        );
    }

    #[test]
    fn adjacency_is_mutual_and_ternary() {
        let addrs = all_addrs(4);
        let present: BTreeSet<RoomAddr> = addrs.iter().cloned().collect();
        for addr in &addrs {
            let ns = addr.neighbors();
            let uniq: BTreeSet<&RoomAddr> = ns.iter().collect();
            assert_eq!(
                uniq.len(),
                3,
                "exactly three distinct neighbours at {addr:?}"
            );
            for n in &ns {
                assert!(present.contains(n), "neighbour {n:?} not on the mesh");
                assert!(
                    n.neighbors().contains(addr),
                    "not mutual: {addr:?} -> {n:?}"
                );
            }
        }
    }

    #[test]
    fn corner_at_pentagon_vertex_walks() {
        // The all-0 path keeps a corner at a base icosahedron vertex (5-valent).
        let addr = RoomAddr {
            face: 0,
            path: vec![0; 5],
        };
        let ns = addr.neighbors();
        assert_eq!(ns.iter().collect::<BTreeSet<_>>().len(), 3);
        for n in &ns {
            assert!(
                n.neighbors().contains(&addr),
                "pentagon neighbour not mutual"
            );
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
                let shares = |p: [f64; 3]| nc.contains(&p);
                assert!(
                    !shares(sc[n]),
                    "neighbor {n} of {addr:?} should not share corner {n}"
                );
                assert!(
                    shares(sc[(n + 1) % 3]),
                    "neighbor {n} of {addr:?} misses a shared corner"
                );
                assert!(
                    shares(sc[(n + 2) % 3]),
                    "neighbor {n} of {addr:?} misses a shared corner"
                );
            }
        }
    }
}
