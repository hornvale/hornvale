//! The Room Mesh: rooms are triangular faces of the same icosphere as
//! `Geosphere`, refined deeper and addressed by `(base face + child path)`.
//! Lazy, deterministic, and byte-identical to a fully-built `Geosphere`.
//! Identity, adjacency, and seeding are integer/rational; transcendentals live
//! only in position geometry (registry MAP-28).

use crate::GeoCoord;
use crate::geosphere::{base_data, normalize, slerp_mid};

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
/// current scale `2^depth`. Only the round-trip test calls these helpers so
/// far; the neighbour walk (Task 5) is their production caller, hence the
/// blanket `#[allow(dead_code)]` below until that lands.
#[allow(dead_code)]
type Bary = [i64; 3];

#[allow(dead_code)]
fn add(a: Bary, b: Bary) -> Bary {
    [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
}
#[allow(dead_code)]
fn dbl(a: Bary) -> Bary {
    [a[0] * 2, a[1] * 2, a[2] * 2]
}
#[allow(dead_code)]
fn mid_i(a: Bary, b: Bary) -> Bary {
    [(a[0] + b[0]) / 2, (a[1] + b[1]) / 2, (a[2] + b[2]) / 2]
}

/// Forward walk: a path to its ordered barycentric triple and its scale
/// `2^path.len()`. Child order matches `subdivide`.
#[allow(dead_code)]
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
#[allow(dead_code)]
fn orient(a: Bary, b: Bary, c: Bary) -> i64 {
    (b[0] - a[0]) * (c[1] - a[1]) - (c[0] - a[0]) * (b[1] - a[1])
}

/// Is `p` strictly inside triangle `t`? (all at the same scale)
#[allow(dead_code)]
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
#[allow(dead_code)]
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
#[allow(dead_code)]
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
}

#[cfg(test)]
mod tests {
    use super::*;
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
}
