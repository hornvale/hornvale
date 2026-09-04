//! The Room Mesh: rooms are quads of the tangent-warped cube-sphere
//! ([`crate::cube`]), refined deeper and addressed by `(base face + child
//! path)`. Lazy, deterministic; identity and seeding are integer/rational,
//! and transcendentals live only in position geometry (registry MAP-28).
//!
//! **The base geometry is no longer the icosphere** (The Pavement). Rooms
//! were triangular faces of the same icosphere as `Geosphere`, so a room's
//! corners WERE mesh vertices and "byte-identical to a fully-built
//! `Geosphere`" was the property this module advertised. A cube-sphere quad's
//! corners are not geosphere vertices, so that property is gone — deliberately
//! (spec section 7's H3a; decision 0287's core survives and only its
//! corner-is-a-vertex corollary is retired). The icosphere remains the FIELD
//! substrate: `Geosphere`/`Vertex` and every `domains/` crate are untouched,
//! and a room still reads terrain and climate through them.

use crate::GeoCoord;
use crate::Seed;
use crate::VertexMap;
use crate::cube;
use crate::derived::Derived;
use crate::math;
use crate::seed::StreamLabel;
use crate::streams::{ROOM_CHILD, ROOM_FACE};
use crate::{Geosphere, NearestVertexIndex, Vertex};

/// The deepest path a `FacetId` can pack: 5 face bits + 1 sentinel + 2*29 digit
/// bits = 64. Useful room scale is ~L16-20; an L18 room edge is ~35 m, derived
/// from the campaign's own measurement of 1.126 km at depth 13 (spec section
/// 2.3) halved five times, not from the icosphere figure (~27 m at L18) this
/// line carried before the base mesh changed.
/// type-audit: bare-ok(count)
pub const MAX_DEPTH: usize = 29;

/// A room — a quad of the tangent-warped cube-sphere at refinement depth
/// `path.len()`. Keyed to the base cube face (level 0), so the address is
/// independent of the world's canonical globe level.
/// type-audit: bare-ok(index: face), bare-ok(index: path)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Facet {
    /// Which of the 6 base cube faces (0..6), numbered as
    /// [`crate::cube::CUBE_FACES`].
    pub face: u8,
    /// Child index (0..4) at each refinement, from the base face down. A digit
    /// is a PAIR OF BITS, `(hi_x << 1) | hi_y`: which half of the parent quad
    /// this child takes on each face parameter. [`Facet::face_lattice`] is the
    /// inverse — it re-interleaves those bits back into integer `(x, y)`.
    pub path: Vec<u8>,
}

/// A room's exact position in its base face's square lattice: the quad's
/// integer `(x, y)` index at `scale = 2^depth`, both in `0..scale`.
/// **Integer-only, so two rooms on the same base face have an exact,
/// cross-platform-stable relative offset** — that is the whole reason this
/// type exists rather than a pair of `f64` face parameters, and it is what
/// lets a situated chart place things without a transcendental. It survives
/// the move off the triangle unchanged: interleaving path bits is integer
/// arithmetic exactly as accumulating a barycentric triple was.
///
/// **There is no orientation flag any more, and its absence is the point.** A
/// triangle's four children are three same-handed corner triangles and one
/// inverted centre triangle, so a triangle address could not be reconstructed
/// from a lattice base point alone — the `up` flag carried the missing bit. A
/// quad's four children are all the same handedness, so `(x, y, scale)` is a
/// complete address and there is no bit left over to carry.
/// type-audit: bare-ok(index: x), bare-ok(index: y), bare-ok(count: scale)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FaceLattice {
    /// Lattice index along the base face's first parameter (`a`), in
    /// `0..scale`.
    pub x: i64,
    /// Lattice index along the base face's second parameter (`b`), in
    /// `0..scale`.
    pub y: i64,
    /// The lattice scale at this depth, `1 << depth`.
    pub scale: i64,
}

/// Packed, serialized form of a `Facet` — a frozen save-format contract.
/// Layout: bits `[0,5)` = face; bits `[5,64)` = a leading-1 sentinel then 2
/// bits per digit, root digit first.
/// type-audit: bare-ok(constructor-edge)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FacetId(pub u64);

/// Why a `Facet` could not be packed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FacetError {
    /// `path.len()` exceeds `MAX_DEPTH`, so it will not fit a `u64`.
    DepthExceedsCap,
    /// A path digit was not a child index in `0..4`, or `face >= 6`.
    Invalid,
}

/// Why a `u64` is not a valid `FacetId`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FacetIdError {
    /// The face field is not a cube face (`>= 6`), or the sentinel is
    /// missing.
    ///
    /// **The bound is 6, matching `pack`'s, and the symmetry is the whole
    /// point.** It was 20 — the icosahedron's face count — until The
    /// Pavement's Task 3. A `FacetId` whose face field lands in `6..20` is
    /// precisely the shape a PRE-CUBE world file has, and while the bound
    /// stayed at 20 such an id decoded silently into a `Facet` that looked
    /// valid and addressed a room on the wrong mesh. Decision 0189 requires a
    /// pre-flip world to fail LOUDLY ("a world file written before this flip
    /// does not load, deliberately"); a silent decode is the exact inverse of
    /// that guarantee, and one that a drift check would call green forever.
    /// So a refusal now means one of two things, and a reader who hits it on
    /// a stored id should assume the second: the `u64` is not a packed
    /// `FacetId` at all, or it is one minted before the base mesh became the
    /// cube-sphere, in which case the world it came from must be regenerated
    /// from its seed and pins rather than loaded.
    Malformed,
}

/// The face parameter of lattice line `i` at `scale`: `-1 + 2i/scale`.
///
/// **Exact, and that exactness is load-bearing.** `scale` is always a power of
/// two and `i <= scale <= 2^29`, so every value this returns is a dyadic
/// rational representable in `f64` with no rounding whatever. Two quads that
/// share an edge compute that edge's parameter through this one expression and
/// therefore get BIT-IDENTICAL corner positions out of
/// [`crate::cube::face_unit`] — the mesh is watertight by construction rather
/// than to within a tolerance. `corners_are_watertight_across_the_lattice` and
/// `a_parent_shares_its_own_corners_with_its_children` pin both directions.
fn face_param(i: i64, scale: i64) -> f64 {
    -1.0 + 2.0 * i as f64 / scale as f64
}

/// One directed cube seam: what happens to a lattice coordinate that steps
/// off one of a base face's four sides.
///
/// **Derived, never written down.** [`seam_table`] computes all 24 of these
/// (the cube's 12 edges, each traversed both ways) from
/// [`crate::cube::CUBE_FACES`] alone, by the linear algebra of the face bases
/// — see that function's doc for the derivation. A hand-written rotation
/// table is exactly where an off-by-one-turn bug hides: it satisfies every
/// test that does not compare the two sides of a seam, and
/// `adjacency_is_symmetric_across_every_seam` (in
/// `kernel/tests/suite/cube_adjacency.rs`) is the test that does.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Seam {
    /// The base face the step lands on.
    to_face: u8,
    /// Which of the destination face's two lattice coordinates carries the
    /// along-seam coordinate: `false` = its `x` (the `a` parameter), `true` =
    /// its `y` (the `b` parameter).
    along_is_y: bool,
    /// The along-seam coordinate runs backwards on the destination face
    /// (`along -> scale - 1 - along`).
    reverse: bool,
    /// The destination's OTHER coordinate — the one measuring depth away from
    /// the shared seam — is the far edge (`scale - 1`) rather than the near
    /// one (`0`).
    high: bool,
}

/// The four sides of a base face's `(a, b)` parameter square, in the order
/// [`seam_table`] indexes them: `a = -1`, `a = +1`, `b = -1`, `b = +1`.
const SIDE_A_LO: usize = 0;
const SIDE_A_HI: usize = 1;
const SIDE_B_LO: usize = 2;
const SIDE_B_HI: usize = 3;

/// A `CUBE_FACES` basis vector as exact integers. Every component is one of
/// `0.0`, `1.0`, `-1.0`, so the cast is exact and no float comparison enters
/// the seam derivation.
fn axis(v: [f64; 3]) -> [i64; 3] {
    [v[0] as i64, v[1] as i64, v[2] as i64]
}

fn idot(a: [i64; 3], b: [i64; 3]) -> i64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

fn isub(a: [i64; 3], b: [i64; 3]) -> [i64; 3] {
    [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}

fn iadd(a: [i64; 3], b: [i64; 3]) -> [i64; 3] {
    [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
}

/// The 24 directed cube seams, `[face][side]`, DERIVED from
/// [`crate::cube::CUBE_FACES`].
///
/// # The derivation
///
/// A face `f` with basis `(n, u, v)` carries the cube point `n + a·u + b·v`
/// for parameters `(a, b) ∈ [-1, 1]²` (the tangent warp is a per-axis odd
/// function fixing `±1`, so it commutes with everything below and drops out —
/// see [`crate::cube`]). A point on one of `f`'s four sides is therefore
/// `P = c + t·e`, where `t` is the along-seam parameter and, per side,
///
/// | side | `c` | `e` (the along-seam axis) | outward axis `w` |
/// |---|---|---|---|
/// | `a = -1` | `n - u` | `v` | `-u` |
/// | `a = +1` | `n + u` | `v` | `+u` |
/// | `b = -1` | `n - v` | `u` | `-v` |
/// | `b = +1` | `n + v` | `u` | `+v` |
///
/// The neighbouring face is the one whose NORMAL is that outward axis: for
/// `a = +1`, `P·n' = 1` forces `v·n' = 0` and `(n + u)·n' = 1`, and since two
/// faces never share a normal the only solution is `n' = u`. So `to_face` is
/// a lookup of `w` in the `CUBE_FACES` normals — no table.
///
/// The destination parameters are then `a' = P·u' = c·u' + t·(e·u')` and
/// `b' = P·v' = c·v' + t·(e·v')`. Because `{n, u, v}` are three orthogonal
/// axes and `u = n' ⊥ u'`, exactly one of `c·u'`, `e·u'` is `±1` and the
/// other is `0` (likewise for `v'`). That is what makes the transition a
/// signed axis swap and nothing worse:
///
/// - if `e·u' = σ ≠ 0` the along-seam parameter lands on `a'` as `σ·t`
///   (`along_is_y = false`, `reverse = σ < 0`), and `b' = c·v' = ±1` is the
///   seam itself (`high = c·v' > 0`);
/// - otherwise it lands on `b'` and `a' = c·u' = ±1` is the seam.
///
/// In lattice terms (`t = -1 + 2T/scale`, so `T' = T` when `σ = +1` and
/// `T' = scale - T` when `σ = -1`, and the seam line is `0` or `scale`), an
/// identical along-seam map takes quad `k` to quad `k` and a reversed one
/// takes it to `scale - 1 - k`; the depth-away coordinate is quad `0` at the
/// `0` line and quad `scale - 1` at the `scale` line.
fn seam_table() -> &'static [[Seam; 4]; 6] {
    use std::sync::OnceLock;
    static T: OnceLock<[[Seam; 4]; 6]> = OnceLock::new();
    T.get_or_init(|| {
        let normals: Vec<[i64; 3]> = cube::CUBE_FACES.iter().map(|[n, _, _]| axis(*n)).collect();
        let face_with_normal = |w: [i64; 3]| -> u8 {
            normals
                .iter()
                .position(|&n| n == w)
                .expect("every cube axis is some face's normal") as u8
        };
        let derive = |face: usize, side: usize| -> Seam {
            let [n, u, v] = cube::CUBE_FACES[face];
            let (n, u, v) = (axis(n), axis(u), axis(v));
            let (c, e, w) = match side {
                SIDE_A_LO => (isub(n, u), v, [-u[0], -u[1], -u[2]]),
                SIDE_A_HI => (iadd(n, u), v, u),
                SIDE_B_LO => (isub(n, v), u, [-v[0], -v[1], -v[2]]),
                _ => (iadd(n, v), u, v),
            };
            let to_face = face_with_normal(w);
            let [_, up, vp] = cube::CUBE_FACES[to_face as usize];
            let (up, vp) = (axis(up), axis(vp));
            let (eu, ev) = (idot(e, up), idot(e, vp));
            let (cu, cv) = (idot(c, up), idot(c, vp));
            debug_assert_eq!(
                (eu == 0) as i32 + (ev == 0) as i32,
                1,
                "the along-seam axis must land on exactly one destination axis"
            );
            if eu != 0 {
                debug_assert_eq!(cu, 0, "a destination axis cannot carry both terms");
                Seam {
                    to_face,
                    along_is_y: false,
                    reverse: eu < 0,
                    high: cv > 0,
                }
            } else {
                debug_assert_eq!(cv, 0, "a destination axis cannot carry both terms");
                Seam {
                    to_face,
                    along_is_y: true,
                    reverse: ev < 0,
                    high: cu > 0,
                }
            }
        };
        let mut out = [[Seam {
            to_face: 0,
            along_is_y: false,
            reverse: false,
            high: false,
        }; 4]; 6];
        for (face, sides) in out.iter_mut().enumerate() {
            for (side, slot) in sides.iter_mut().enumerate() {
                *slot = derive(face, side);
            }
        }
        out
    })
}

/// The destination quad of a step that leaves `face` across `side`, where
/// `along` is the surviving in-range lattice coordinate along that seam (the
/// `y` coordinate for an `a` side, the `x` coordinate for a `b` side) and
/// `scale` is `1 << depth`.
fn seam_step(face: u8, side: usize, along: i64, scale: i64) -> (u8, i64, i64) {
    let s = seam_table()[face as usize][side];
    let a = if s.reverse { scale - 1 - along } else { along };
    let d = if s.high { scale - 1 } else { 0 };
    if s.along_is_y {
        (s.to_face, d, a)
    } else {
        (s.to_face, a, d)
    }
}

/// The `Facet` at integer face-lattice `(x, y)` and `depth` on `face` — the
/// forward direction of [`Facet::face_lattice`]'s decode, interleaving the
/// two coordinates' bits into `(hi_x << 1) | hi_y` digits.
fn lattice_facet(face: u8, x: i64, y: i64, depth: u32) -> Facet {
    let path = (0..depth)
        .rev()
        .map(|i| (((x >> i) & 1) as u8) << 1 | ((y >> i) & 1) as u8)
        .collect();
    Facet { face, path }
}

impl Facet {
    /// Refinement depth = path length.
    /// type-audit: bare-ok(count)
    pub fn depth(&self) -> u32 {
        self.path.len() as u32
    }

    /// This room's exact face-local lattice position. Integer-only — no
    /// transcendental enters it, so a chart placed from these coordinates is
    /// byte-identical across platforms. Face-LOCAL: two rooms on different
    /// base faces have no meaningful relative offset, and a consumer must
    /// compare `face` before differencing.
    ///
    /// The walk is the exact inverse of [`Facet::child`]'s digit encoding: a
    /// path digit is `(hi_x << 1) | hi_y`, so each level shifts both
    /// coordinates left one bit and appends that level's half-choice. Nothing
    /// here is a float, which is what preserves the exact-relative-offset
    /// property the type doc promises.
    pub fn face_lattice(&self) -> FaceLattice {
        let (mut x, mut y, mut scale) = (0i64, 0i64, 1i64);
        for &d in &self.path {
            scale <<= 1;
            x = (x << 1) | i64::from(d >> 1);
            y = (y << 1) | i64::from(d & 1);
        }
        FaceLattice { x, y, scale }
    }

    /// Pack to the `u64` `FacetId` contract. Fails past `MAX_DEPTH`.
    pub fn pack(&self) -> Result<FacetId, FacetError> {
        if self.path.len() > MAX_DEPTH {
            return Err(FacetError::DepthExceedsCap);
        }
        if self.face >= 6 || self.path.iter().any(|&d| d >= 4) {
            return Err(FacetError::Invalid);
        }
        let mut pathword: u64 = 1; // sentinel
        for &d in &self.path {
            pathword = (pathword << 2) | u64::from(d);
        }
        Ok(FacetId((pathword << 5) | u64::from(self.face)))
    }
}

impl FacetId {
    /// Unpack to a `Facet`. Validates the face against the CUBE's six faces
    /// and checks the sentinel; not total — see [`FacetIdError::Malformed`]
    /// for what a refusal means now that the bound matches [`Facet::pack`]'s.
    pub fn unpack(&self) -> Result<Facet, FacetIdError> {
        let face = (self.0 & 0x1F) as u8;
        if face >= 6 {
            return Err(FacetIdError::Malformed);
        }
        let pathword = self.0 >> 5;
        if pathword == 0 {
            return Err(FacetIdError::Malformed);
        }
        let top = 63 - pathword.leading_zeros(); // index of the sentinel bit = 2*len
        let len = (top / 2) as usize;
        let mut path = Vec::with_capacity(len);
        for i in (0..len).rev() {
            path.push(((pathword >> (2 * i)) & 0b11) as u8);
        }
        Ok(Facet { face, path })
    }
}

impl Facet {
    /// The four unit-sphere corner positions of the room's quad, wound
    /// counter-clockwise in the base face's `(a, b)` parameter space starting
    /// at the componentwise-minimum corner: `(a_lo, b_lo)`, `(a_hi, b_lo)`,
    /// `(a_hi, b_hi)`, `(a_lo, b_hi)`.
    ///
    /// **This is no longer byte-identical to a face of
    /// `Geosphere::new(self.path.len())`, and it cannot be** — a cube-sphere
    /// quad's corners are not geosphere vertices at all (spec section 7's H3a;
    /// decision 0287's corner-is-a-vertex COROLLARY is retired, its core
    /// untouched). What survives, and is what the old property was actually
    /// buying, is byte-identity ACROSS THE QUADTREE: every corner comes from
    /// [`face_param`] and [`crate::cube::face_unit`], so a corner shared by two
    /// quads — siblings, lattice neighbours, or a parent and a child — is the
    /// same bits computed the same way, not two values that agree to a
    /// tolerance.
    /// type-audit: pending(wave-1)
    pub fn corners(&self) -> [[f64; 3]; 4] {
        let l = self.face_lattice();
        let f = self.face as usize;
        let (a_lo, a_hi) = (face_param(l.x, l.scale), face_param(l.x + 1, l.scale));
        let (b_lo, b_hi) = (face_param(l.y, l.scale), face_param(l.y + 1, l.scale));
        [
            cube::face_unit(f, a_lo, b_lo),
            cube::face_unit(f, a_hi, b_lo),
            cube::face_unit(f, a_hi, b_hi),
            cube::face_unit(f, a_lo, b_hi),
        ]
    }

    /// The room centroid — the projection of the quad's PARAMETER-SPACE
    /// centre, not the normalized mean of its corners.
    ///
    /// The distinction is deliberate: the parameter centre is an exact dyadic
    /// rational ([`face_param`]), and it is the same quantity
    /// [`Facet::containing`]'s bisection converges on, which is what makes
    /// `containing(f.centroid(), f.depth()) == f` hold with a full half-quad
    /// of margin instead of resting on how a four-point mean happens to round.
    /// type-audit: pending(wave-1)
    pub fn centroid(&self) -> [f64; 3] {
        let l = self.face_lattice();
        // The centre of quad `i` at `scale` is line `2i + 1` at `2 * scale`.
        cube::face_unit(
            self.face as usize,
            face_param(2 * l.x + 1, 2 * l.scale),
            face_param(2 * l.y + 1, 2 * l.scale),
        )
    }

    /// The room at `depth` whose quad contains `position`.
    /// PRESENTATION-SIDE: resolves a float coordinate to an integer address
    /// (float→address, the same determinism class as `NearestVertexIndex::nearest`).
    /// Not an identity path — a boundary-straddling position may resolve to
    /// adjacent rooms on different platforms; a room's content stays
    /// integer-exact once addressed.
    ///
    /// [`crate::cube::locate`] answers the base face and the two face
    /// parameters in one step (it is the exact inverse of the projection
    /// `corners`/`centroid` use, warp included), and the descent is then a
    /// dyadic bisection of each parameter independently, emitting
    /// `(hi_x << 1) | hi_y` at each level — the same digit encoding
    /// [`Facet::face_lattice`] decodes. So
    /// `containing(r.centroid(), r.depth()) == r` holds for every room, at
    /// every depth up to [`MAX_DEPTH`], with half a quad of margin: a
    /// centroid sits at the exact centre of its quad and every bisection
    /// boundary is an exact dyadic rational, so the comparison is never close.
    ///
    /// **Total by construction.** A parameter outside `[-1, 1]` (a numerically
    /// on-seam position, or a `position` that is not quite a unit vector)
    /// simply keeps choosing the same side all the way down and lands in the
    /// edge quad, which is the honest answer rather than a panic; no
    /// nearest-face fallback is needed, because the cube-map face rule
    /// `locate` uses is total already.
    /// type-audit: pending(wave-1)
    pub fn containing(position: [f64; 3], depth: u32) -> Facet {
        let (face, a, b) = cube::locate(position);
        let (mut a_lo, mut a_hi) = (-1.0f64, 1.0f64);
        let (mut b_lo, mut b_hi) = (-1.0f64, 1.0f64);
        let mut path = Vec::with_capacity(depth as usize);
        for _ in 0..depth {
            let a_mid = 0.5 * (a_lo + a_hi);
            let b_mid = 0.5 * (b_lo + b_hi);
            let hi_x = u8::from(a >= a_mid);
            let hi_y = u8::from(b >= b_mid);
            if hi_x == 1 {
                a_lo = a_mid
            } else {
                a_hi = a_mid
            }
            if hi_y == 1 {
                b_lo = b_mid
            } else {
                b_hi = b_mid
            }
            path.push((hi_x << 1) | hi_y);
        }
        Facet {
            face: face as u8,
            path,
        }
    }

    /// The geographic coordinate of the centroid.
    ///
    /// The parenthetical this line used to carry — "matches
    /// `Geosphere::coord`" — is retired with the triangle: a cube-sphere quad's
    /// centroid is not a geosphere vertex, so there is no `Geosphere::coord`
    /// for it to match. The conversion itself is unchanged.
    /// type-audit: pending(wave-1)
    pub fn coord(&self) -> GeoCoord {
        let [x, y, z] = self.centroid();
        GeoCoord {
            latitude: math::asin(z).to_degrees(),
            longitude: math::atan2(y, x).to_degrees(),
        }
    }

    /// Great-circle initial azimuth (degrees, clockwise from north) from this
    /// room's centroid to `other`'s. For rendering and exit-naming only.
    /// type-audit: pending(wave-1)
    pub fn bearing_to(&self, other: &Facet) -> f64 {
        let a = self.coord();
        let b = other.coord();
        let (lat1, lat2) = (a.latitude.to_radians(), b.latitude.to_radians());
        let dlon = (b.longitude - a.longitude).to_radians();
        let y = math::sin(dlon) * math::cos(lat2);
        let x =
            math::cos(lat1) * math::sin(lat2) - math::sin(lat1) * math::cos(lat2) * math::cos(dlon);
        let deg = math::atan2(y, x).to_degrees();
        (deg + 360.0) % 360.0
    }

    /// Great-circle angular distance to `other`'s centroid, in radians.
    /// Pairs with [`Facet::bearing_to`]: together they are a polar
    /// coordinate for `other` about `self`, which is what a client needs
    /// to place a vertex without doing spherical trigonometry itself.
    /// type-audit: pending(wave-1)
    pub fn distance_rad_to(&self, other: &Facet) -> f64 {
        let a = self.centroid();
        let b = other.centroid();
        let dot = (a[0] * b[0] + a[1] * b[1] + a[2] * b[2]).clamp(-1.0, 1.0);
        math::acos(dot)
    }

    /// The angular length (radians) of this room's shortest edge — the
    /// minimum pairwise great-circle separation of [`Facet::corners`]'s four
    /// corners, in their own winding order.
    ///
    /// **Promoted from `windows/locale`'s `room_edge` (The Weft,
    /// Task 5 fix round 1)**, on the same argument [`blend_corner_weights`]'s
    /// own promotion doc states: a pure `&Facet -> f64` function had drifted
    /// into three character-for-character copies (`windows/locale::room_edge`,
    /// a private helper in `windows/worldgen::weft`, and an open-coded third
    /// in `windows/locale`'s own `site_address_agreement.rs` test) with no
    /// shared implementation, and no dependency edge stood in the way — this
    /// method reads only [`Facet::corners`], nothing locale- or worldgen-
    /// specific. `windows/locale::room_edge` now delegates here; the weft's
    /// duplicate and the test's open-coded copy are deleted.
    /// type-audit: pending(wave-1: return)
    pub fn edge_rad(&self) -> f64 {
        let [a, b, c, d] = self.corners();
        let sep = |u: [f64; 3], v: [f64; 3]| -> f64 {
            let dp: f64 = u[0] * v[0] + u[1] * v[1] + u[2] * v[2];
            math::acos(dp.clamp(-1.0, 1.0))
        };
        sep(a, b).min(sep(b, c)).min(sep(c, d)).min(sep(d, a))
    }

    /// The edge- and corner-adjacent rooms, at the same depth: the geometric
    /// base graph of the cube-sphere quad lattice. **Eight entries in the
    /// interior, seven at a cube corner, and four at a base face itself
    /// (depth 0).**
    ///
    /// The arity is honest at the type level rather than padded to a
    /// fixed-size array a caller would have to know to skip: a `Vec` says
    /// "however many there are", and the two short cases are real geometry,
    /// not degeneracies. The order is fixed and carries one invariant worth
    /// knowing before indexing into it — `[..4]` is always the edge-adjacent
    /// rooms; see [`Facet::neighbor_steps`].
    ///
    /// # The three cases
    ///
    /// 1. **Interior** — both stepped coordinates stay in `0..scale`. Same
    ///    face, same depth.
    /// 2. **Seam** — exactly one coordinate leaves range. [`seam_table`] says
    ///    which face the step lands on and how the surviving coordinate is
    ///    re-expressed there; that table is DERIVED from
    ///    [`crate::cube::CUBE_FACES`], never written by hand.
    /// 3. **Corner** — BOTH coordinates leave range, which happens if and only
    ///    if this room sits at a corner of its base face, i.e. at one of the
    ///    cube's eight corners, and the step is the outward diagonal. Only
    ///    three quads meet at a cube corner (the vertex has degree three,
    ///    unlike every other lattice vertex's four), and the other two are
    ///    already this room's seam neighbours — so there is no eighth room to
    ///    return, and this yields nothing rather than a sentinel.
    ///
    /// Integer-only: no transcendental and no float comparison enters the
    /// walk, so the graph is byte-identical across platforms. Passability and
    /// overlay edges are higher layers and never enter here.
    pub fn neighbors(&self) -> Vec<Facet> {
        let l = self.face_lattice();
        let depth = self.depth();
        let mut out = Vec::with_capacity(8);
        for (dx, dy) in Self::neighbor_steps() {
            let (nx, ny) = (l.x + dx, l.y + dy);
            let off_x = !(0..l.scale).contains(&nx);
            let off_y = !(0..l.scale).contains(&ny);
            match (off_x, off_y) {
                (false, false) => out.push(lattice_facet(self.face, nx, ny, depth)),
                (true, false) => {
                    let side = if nx < 0 { SIDE_A_LO } else { SIDE_A_HI };
                    let (f, x, y) = seam_step(self.face, side, ny, l.scale);
                    out.push(lattice_facet(f, x, y, depth));
                }
                (false, true) => {
                    let side = if ny < 0 { SIDE_B_LO } else { SIDE_B_HI };
                    let (f, x, y) = seam_step(self.face, side, nx, l.scale);
                    out.push(lattice_facet(f, x, y, depth));
                }
                // The cube corner: three quads meet, not four.
                (true, true) => {}
            }
        }
        out
    }

    /// The eight lattice steps [`Facet::neighbors`] walks, in order: **the
    /// four EDGE steps first, counter-clockwise from `+a`, then the four
    /// DIAGONAL steps, counter-clockwise from `(+a, +b)`.**
    ///
    /// # Why grouped by kind rather than interleaved by angle
    ///
    /// Interleaving the eight compass directions by angle reads more
    /// naturally and buys the caller nothing, because the arity is not fixed:
    /// a cube-corner room drops one step, and under an interleaved order a
    /// dropped step shifts every index after it, so `neighbors()[k]` has no
    /// stable meaning at all. Grouping edges first gives one invariant that
    /// survives the drop, because the dropped step is always a DIAGONAL:
    ///
    /// > **`neighbors()[..4]` is always exactly the four edge-adjacent rooms**
    /// > — the rooms sharing a full quad edge, two corner positions apiece —
    /// > and everything from index 4 on shares only a single corner.
    ///
    /// So a consumer that wants the 4-connected subgraph (a movement rule that
    /// forbids corner-cutting, say) takes the prefix, and one that wants all
    /// of 8-connectivity takes the whole `Vec`; neither has to ask which is
    /// which. `the_first_four_neighbours_are_always_the_four_edge_neighbours`
    /// (in `kernel/tests/suite/cube_adjacency.rs`) asserts the invariant in its
    /// own right, including that the step a cube corner drops always lands in
    /// the diagonal tail; `every_neighbour_physically_touches_the_room_it
    /// _neighbours` asserts it positionally as a side effect of checking
    /// adjacency.
    ///
    /// **The order is load-bearing, not incidental.** Roughly thirty
    /// `neighbors()[k]` call sites in `windows/vessel` and `windows/lab` index
    /// 0, 1 or 2 — inside this prefix — so none of them can panic at a cube
    /// corner, and none of them silently receives a diagonal. Reordering these
    /// steps breaks all thirty at once, which is measured rather than assumed:
    /// putting the diagonals first fails both tests named above.
    ///
    /// A base face (depth 0) returns only the prefix: all four of its corners
    /// are cube corners, so all four diagonals drop.
    /// type-audit: bare-ok(index: return)
    pub fn neighbor_steps() -> [(i64, i64); 8] {
        [
            // the four edges, counter-clockwise from +a
            (1, 0),
            (0, 1),
            (-1, 0),
            (0, -1),
            // the four diagonals, counter-clockwise from (+a, +b)
            (1, 1),
            (-1, 1),
            (-1, -1),
            (1, -1),
        ]
    }

    /// The room's FOUR canonical-grid corner vertices, each with its integer
    /// bilinear blend weight at the room centroid (numerators over
    /// `D = 4 << (2 * (depth - globe_level))`, summing to `D`). `None` if the
    /// room is coarser than the grid (`depth < geo.depth()`).
    ///
    /// The four vertices are the grid vertices nearest the ancestor quad's
    /// four corners, in [`Facet::corners`]' counter-clockwise winding:
    /// `(a_lo, b_lo)`, `(a_hi, b_lo)`, `(a_hi, b_hi)`, `(a_lo, b_hi)`. Two
    /// corners of a coarse ancestor may resolve to the same `Vertex`; the
    /// entries are NOT merged and NOT sorted, so a consumer that wants a
    /// single vertex must apply its own dominance rule.
    ///
    /// # Why bilinear, and why the denominator changed shape
    ///
    /// Until The Pavement's Task 3 this returned THREE corners weighted by
    /// [`bary_triple`] — triangle barycentrics, a leftover of the icosphere
    /// base mesh. Task 2 moved the base geometry to the cube and left this
    /// unreconciled, so for one campaign-internal tree state every terrain
    /// and climate field `windows/locale` reads came through triangle weights
    /// paired with three of a quad's four corners. Nothing went red, because
    /// the two properties the tests asserted — the numerators sum to `D`, and
    /// a constant field blends to the constant — are consequences of the
    /// weight arithmetic alone and neither looks at the mesh.
    ///
    /// A quad's four corners are not barycentric, so the three-corner
    /// invariant `D = 3 << (depth - globe_level)` could not survive. Bilinear
    /// is the natural analogue and it stays EXACT in integers: with
    /// `d = depth - globe_level`, `s = 1 << d` and the room's lattice position
    /// `(x, y)` inside the ancestor, the centroid sits at parameter
    /// `(u, v) = ((2x+1)/2s, (2y+1)/2s)`, so scaling by `S = 2s` gives
    /// numerators
    ///
    /// ```text
    /// w0 = (S-U)(S-V)   w1 = U(S-V)   w2 = U·V   w3 = (S-U)·V
    /// where U = 2x+1, V = 2y+1, and w0+w1+w2+w3 = S² = D
    /// ```
    ///
    /// all integers, no float and no transcendental — the same
    /// cross-platform-exactness the old triple had. `D` is largest at
    /// `depth = MAX_DEPTH` with `globe_level = 0`, where it is `2^60`, so a
    /// numerator always fits `u64`.
    ///
    /// # Ties, and the order that resolves them
    ///
    /// **This function makes no choice, so it breaks no tie: it returns all
    /// four entries, in [`Facet::corners`]' winding order, never sorted and
    /// never merged.** That ORDER is the contract a consumer's tie-break rests
    /// on, so it is stated here rather than left implicit: entry `i` is always
    /// the grid vertex nearest ancestor corner `i` in that winding, whatever
    /// the weights do. `corner_weights_pin_vertex_weight_pairing` pins it.
    /// Two corners of a coarse ancestor may resolve to the SAME `Vertex`, and
    /// their entries stay separate — a consumer that wants one vertex's total
    /// share must sum them itself.
    ///
    /// Where exact equalities arise (`U = 2x+1`, `V = 2y+1`, `S = 2s`):
    ///
    /// - `w0 == w2` exactly when `U + V == S`, and `w1 == w3` exactly when
    ///   `U == V`. Both are common, not coincidences.
    /// - **The ARGMAX is nevertheless unique for every `depth > globe_level`.**
    ///   `U` and `V` are odd and `S/2 = s` is even for `d >= 1`, so neither can
    ///   equal `S/2`; the four remaining pairwise comparisons reduce to
    ///   `U <=> S/2` and `V <=> S/2` and are therefore strict. If `U + V == S`
    ///   then one of `U`, `V` is below `S/2` and the other above, and the
    ///   argmax is the off-diagonal corner strictly; if `U == V` the argmax is
    ///   `w0` or `w2` strictly. The two cases cannot hold at once (that needs
    ///   `U == V == S/2`). Verified exhaustively over every sub-position at
    ///   `d = 1..4` as well as argued: zero tied argmaxes.
    /// - **`depth == globe_level` IS a four-way tie, and it is the one case a
    ///   tie-break decides.** There `s = 1`, `U = V = 1`, `S = 2`: all four
    ///   weights are `1` and `D = 4`. The room IS the ancestor and its centroid
    ///   is the quad's centre, equidistant from all four corners, so no corner
    ///   dominates and the answer is a property of the CONSUMER's rule, not of
    ///   the geometry. The two rules in this repository disagree there and both
    ///   are deterministic: `windows/locale`'s `dominant_corner` and
    ///   `windows/vessel`'s `containing_vertex` take the greatest weight then
    ///   the LOWEST `Vertex`, while `session.rs`'s
    ///   `corners.iter().max_by_key(|c| c.weight)` takes Rust's LAST maximum,
    ///   i.e. corner 3. That divergence is recorded at
    ///   `windows/locale`'s `expr_at_stratum` and is not new; what is new is
    ///   that four corners make an exact tie reachable at `d = 0` rather than
    ///   merely possible. `a_room_at_the_globe_level_is_an_exact_four_way_tie`
    ///   pins the tie and both rules' answers.
    ///
    /// # What a test must look at
    ///
    /// The weight↔corner PAIRING is the half that broke silently, so the
    /// assertion that guards it has to be geometric:
    /// `the_dominant_corner_weight_is_the_nearest_corner_of_its_ancestor_quad`
    /// checks that the maximum-weight entry is the ancestor corner the room's
    /// centroid is nearest — recovered by inverting the projection from the 3D
    /// position, not by re-running the weight arithmetic — and
    /// `hugging_a_corner_puts_the_weight_on_that_corner` walks the descent
    /// that converges on each of the four corners in turn. Neither passes on
    /// a triangle.
    /// type-audit: bare-ok(count: return)
    pub fn corner_weights(
        &self,
        geo: &Geosphere,
        index: &NearestVertexIndex,
    ) -> Option<[(Vertex, u64); 4]> {
        let gl = geo.depth();
        if self.depth() < gl {
            return None;
        }
        // The ancestor quad at the globe level: its four corner positions
        // resolve to the four grid vertices this room blends between.
        let anc = Facet {
            face: self.face,
            path: self.path[..gl as usize].to_vec(),
        };
        let corner_pos = anc.corners();
        let vertices = [
            index.nearest_to_position(geo, corner_pos[0]),
            index.nearest_to_position(geo, corner_pos[1]),
            index.nearest_to_position(geo, corner_pos[2]),
            index.nearest_to_position(geo, corner_pos[3]),
        ];
        // This room's lattice position WITHIN the ancestor: the tail of the
        // path re-interleaved at the sub-ancestor scale.
        let sub = Facet {
            face: self.face,
            path: self.path[gl as usize..].to_vec(),
        };
        let l = sub.face_lattice();
        let (big_s, u, v) = (2 * l.scale, 2 * l.x + 1, 2 * l.y + 1);
        let w = [
            (big_s - u) * (big_s - v),
            u * (big_s - v),
            u * v,
            (big_s - u) * v,
        ];
        debug_assert_eq!(
            w.iter().sum::<i64>(),
            big_s * big_s,
            "bilinear numerators must sum to D = (2 * 2^(depth - globe_level))^2"
        );
        Some([
            (vertices[0], w[0] as u64),
            (vertices[1], w[1] as u64),
            (vertices[2], w[2] as u64),
            (vertices[3], w[3] as u64),
        ])
    }
}

/// Corner-blend a per-vertex `field` using [`Facet::corner_weights`]'s output —
/// the weighted mean of the four corners, weighted by their bilinear numerators.
/// Promoted from `windows/locale`'s private `blend_with_weights` (The Weft, Task
/// 4) so `blend_at`'s bilinear read has exactly one implementation instead of
/// two; locale's helper now delegates here. The expression is a byte-identity
/// surface (float summation order decides the result, which flows into every
/// committed artifact `blend_at` feeds) and must not be reordered or rewritten
/// with `fold`/`zip` without re-proving neutrality against the seed-42 goldens.
/// type-audit: bare-ok(count: weights), bare-ok(ratio: field), bare-ok(ratio: return)
pub fn blend_corner_weights(weights: [(Vertex, u64); 4], field: &VertexMap<f64>) -> f64 {
    let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
    let sum: f64 = weights.iter().map(|&(c, w)| w as f64 * *field.get(c)).sum();
    sum / denom as f64
}

impl Facet {
    /// The containing room one level coarser, or `None` at a base face.
    pub fn parent(&self) -> Option<Facet> {
        if self.path.is_empty() {
            return None;
        }
        let mut path = self.path.clone();
        path.pop();
        Some(Facet {
            face: self.face,
            path,
        })
    }

    /// Descend into child `digit` (0..4). Fails past `MAX_DEPTH` or on a bad digit.
    /// type-audit: bare-ok(index)
    pub fn child(&self, digit: u8) -> Result<Facet, FacetError> {
        if digit >= 4 {
            return Err(FacetError::Invalid);
        }
        if self.path.len() >= MAX_DEPTH {
            return Err(FacetError::DepthExceedsCap);
        }
        let mut path = self.path.clone();
        path.push(digit);
        Ok(Facet {
            face: self.face,
            path,
        })
    }

    /// The containing room at coarser `depth`, or `None` if `depth > self.depth()`.
    /// type-audit: bare-ok(count)
    pub fn ancestor(&self, depth: u32) -> Option<Facet> {
        let d = depth as usize;
        if d > self.path.len() {
            return None;
        }
        Some(Facet {
            face: self.face,
            path: self.path[..d].to_vec(),
        })
    }

    /// Deterministic per-room seed, derived from the integer address only —
    /// never the float position, so all room content is platform-exact.
    pub fn seed(&self, world: Seed) -> Seed {
        let mut s = world
            .derive(ROOM_FACE)
            .derive(StreamLabel::dynamic(&self.face.to_string()));
        for &d in &self.path {
            s = s
                .derive(ROOM_CHILD)
                .derive(StreamLabel::dynamic(&d.to_string()));
        }
        s
    }
}

/// The `corner_weights` half's store shape: `(Facet, Geosphere::level())`
/// keys onto [`Facet::corner_weights`] results. A private alias only
/// because the raw nested type trips clippy's `type_complexity` lint on the
/// [`RoomMeshMemo`] field below — it names nothing beyond its own expansion.
type CornerWeightsStore = Derived<(Facet, u32), Option<[(Vertex, u64); 4]>>;

/// A session-lived cache of [`Facet::corner_weights`] and
/// [`Facet::neighbors`] results, keyed by the room they were computed for
/// (the-waymark, Task 3), backed by [`crate::derived::Derived`] (the-forebay
/// Task 3). Both are `Validity::Pure` — pure functions of a KEY that names
/// every parameter the derivation reads: geometry alone for `neighbors`
/// (key: `Facet`), and `(Facet, Geosphere::level())` for
/// `corner_weights`, since `Geosphere::new` takes a level and nothing else
/// (spec §2.1) — so two geospheres at the same level are byte-identical and
/// the level is the only thing that needs to join the key. Caching either is
/// exactly caching a pure function, byte-identical to the un-memoized read by
/// construction (the same argument [`PrimaryAfraidMemo`] makes in
/// `windows/vessel`). Caller-owned and threaded by `&mut` (the
/// `PrimaryAfraidMemo` shape): no `RefCell`, no global, no `OnceLock`.
/// Append-only for the memo's lifetime — nothing ever invalidates an entry,
/// because a `Pure` entry's key already carries everything its derivation
/// reads.
#[derive(Debug, Default, Clone)]
pub struct RoomMeshMemo {
    /// `(Facet, level) -> corner_weights(geo, index)`. The level enters
    /// the key (spec §2.1/§2.1b) precisely because `corner_weights` is a
    /// pure function of `(Facet, Geosphere::level())` and nothing else —
    /// so one memo may now legitimately serve more than one globe level:
    /// two different keys, two correct answers, never an aliasing footgun.
    /// That is a capability gain over the memo's earlier `Facet`-only
    /// key, which could not tell two levels apart at all.
    corner_weights: CornerWeightsStore,
    /// `Facet -> neighbors()`. Pure geometry, no external dependency, so
    /// this half never goes stale regardless of which world it is reused
    /// across.
    neighbors: Derived<Facet, Vec<Facet>>,
    /// The `Geosphere::level()` of the FIRST `corner_weights` entry ever
    /// inserted — never updated after that. `None` until the first insert.
    /// Read-path plumbing for [`Self::corner_weights_lookup`] (which takes
    /// only a `Facet` and so needs a level from somewhere to complete the
    /// key) and for a caller such as `windows/locale`'s own read-side parity
    /// check against its live `Geosphere`. No longer a write-path aliasing
    /// guard (the-forebay Task 3, spec §2.1b): completing the key retired
    /// that footgun rather than strengthening the guard against it, so
    /// nothing here forbids `corner_weights_memo` from inserting a second,
    /// different level — it did before, and that is exactly the mixing
    /// this campaign made legitimate.
    /// type-audit: bare-ok(count)
    corner_weights_geo_level: Option<u32>,
    /// How many `corner_weights_memo` calls, ever, found the address already
    /// cached — the scaling property's own deterministic witness (the-forebay
    /// Task 1), never a wall-clock proxy. A cached `None` (an above-the-grid
    /// room) counts as a hit here, matching `corner_weights_lookup`'s
    /// `Option<Option<_>>`: the outer `Some` is what "cached" means, and the
    /// inner value is irrelevant to hit/miss accounting.
    /// type-audit: bare-ok(count)
    corner_weights_hits: u64,
    /// How many `corner_weights_memo` calls, ever, filled a fresh entry —
    /// the complement of `corner_weights_hits`. Never reset.
    /// type-audit: bare-ok(count)
    corner_weights_misses: u64,
    /// How many `neighbors_memo` calls, ever, found the address already
    /// cached. Same deterministic-witness shape as `corner_weights_hits`.
    /// type-audit: bare-ok(count)
    neighbors_hits: u64,
    /// How many `neighbors_memo` calls, ever, filled a fresh entry — the
    /// complement of `neighbors_hits`. Never reset.
    /// type-audit: bare-ok(count)
    neighbors_misses: u64,
}

impl RoomMeshMemo {
    /// An empty memo — construct one per session/tick scope, matching
    /// [`crate::room`]'s sibling `PrimaryAfraidMemo` precedent in
    /// `windows/vessel`.
    pub fn new() -> Self {
        Self::default()
    }

    /// A read-only consult of the `corner_weights` half: `None` on a cache
    /// miss (the caller falls through to a fresh [`Facet::corner_weights`]
    /// call), `Some(inner)` on a hit, where `inner` is the memoized
    /// `corner_weights` result itself (which can legitimately be `None` for
    /// an above-the-grid room — that is a cached ABSENCE, distinct from "not
    /// looked up yet"). The read-only sibling of [`Facet::
    /// corner_weights_memo`]: this one takes `&self` (a caller who only
    /// holds a SHARED reference — e.g. a prefilled cache embedded in a
    /// `&self`-only reader — can still consult it, just never fill a miss).
    /// Its single argument stays a bare `Facet`, so the level half of the
    /// store's key comes from [`Self::corner_weights_geo_level`] instead —
    /// `None` (not-looked-up) when this memo has never been filled at all.
    /// Reads via [`crate::derived::Derived::peek`], never [`crate::
    /// derived::Derived::get`]: this is a shared-reference consult, not a
    /// cache hit, so it must count neither a hit nor a miss.
    /// type-audit: bare-ok(count: return)
    pub fn corner_weights_lookup(&self, addr: &Facet) -> Option<Option<[(Vertex, u64); 4]>> {
        let level = self.corner_weights_geo_level?;
        self.corner_weights.peek(&(addr.clone(), level)).copied()
    }

    /// The `Geosphere::level()` this memo's `corner_weights` half was FIRST
    /// filled against (the-waymark fix round, round 2) — `None` before any
    /// `corner_weights_memo` insert, and never updated after that first
    /// insert even if a later insert uses a different level (the-forebay
    /// Task 3: mixing levels through one memo is now legitimate — see the
    /// field doc). A read-side consumer that also holds the `(Geosphere,
    /// NearestVertexIndex)` it is ABOUT to read through (e.g. `windows/
    /// locale`'s `LocaleContext`) can still compare this against its own
    /// `geo.depth()` for its OWN parity check, and [`Self::
    /// corner_weights_lookup`] uses it to complete the store's key.
    /// type-audit: bare-ok(count: return)
    pub fn corner_weights_geo_level(&self) -> Option<u32> {
        self.corner_weights_geo_level
    }

    /// How many `corner_weights_memo` calls, ever, hit an already-cached
    /// entry (a cached `None` counts as a hit — see the field doc). Never
    /// reset; this campaign's instrument for whether the memo's reuse is
    /// real (the-forebay Task 1).
    /// type-audit: bare-ok(count: return)
    pub fn corner_weights_hits(&self) -> u64 {
        self.corner_weights_hits
    }

    /// How many `corner_weights_memo` calls, ever, filled a fresh entry.
    /// Never reset.
    /// type-audit: bare-ok(count: return)
    pub fn corner_weights_misses(&self) -> u64 {
        self.corner_weights_misses
    }

    /// How many `neighbors_memo` calls, ever, hit an already-cached entry.
    /// Never reset.
    /// type-audit: bare-ok(count: return)
    pub fn neighbors_hits(&self) -> u64 {
        self.neighbors_hits
    }

    /// How many `neighbors_memo` calls, ever, filled a fresh entry. Never
    /// reset.
    /// type-audit: bare-ok(count: return)
    pub fn neighbors_misses(&self) -> u64 {
        self.neighbors_misses
    }
}

impl Facet {
    /// [`Self::corner_weights`], consulting/filling a caller-owned
    /// [`RoomMeshMemo`] instead of recomputing the four
    /// [`NearestVertexIndex::nearest_to_position`] scans on every call
    /// (three until The Pavement made the stencil a quad). Byte-
    /// identical to `corner_weights` by construction (a cache of a pure
    /// function of `(self, geo.depth())` — spec §2.1: `Geosphere::new` takes
    /// only a level, so two geospheres at the same level are byte-identical
    /// and the level is the only extra key ingredient `corner_weights`
    /// needs) — pinned by `corner_weights_memo_bit_equals_recomputation`
    /// below. The level rides in the store's key rather than being asserted
    /// against a single recorded value (the-forebay Task 3, spec §2.1b): one
    /// memo may now legitimately answer for more than one globe level,
    /// pinned by `corner_weights_memo_serves_two_globe_levels_correctly`.
    /// type-audit: bare-ok(count: return)
    pub fn corner_weights_memo(
        &self,
        geo: &Geosphere,
        index: &NearestVertexIndex,
        memo: &mut RoomMeshMemo,
    ) -> Option<[(Vertex, u64); 4]> {
        let level = geo.depth();
        let key = (self.clone(), level);
        if let Some(&cached) = memo.corner_weights.get(&key) {
            memo.corner_weights_hits += 1;
            return cached;
        }
        memo.corner_weights_misses += 1;
        if memo.corner_weights_geo_level.is_none() {
            memo.corner_weights_geo_level = Some(level);
        }
        let computed = self.corner_weights(geo, index);
        memo.corner_weights.insert(key, computed);
        computed
    }

    /// [`Self::neighbors`], consulting/filling a caller-owned [`RoomMeshMemo`]
    /// instead of recomputing the cube-lattice/seam-crossing arithmetic
    /// on every call. Byte-identical to `neighbors` by construction (a cache
    /// of a pure function of `self`) — pinned by
    /// `neighbors_memo_bit_equals_recomputation` below.
    pub fn neighbors_memo(&self, memo: &mut RoomMeshMemo) -> Vec<Facet> {
        if let Some(cached) = memo.neighbors.get(self) {
            memo.neighbors_hits += 1;
            return cached.clone();
        }
        memo.neighbors_misses += 1;
        let computed = self.neighbors();
        memo.neighbors.insert(self.clone(), computed.clone());
        computed
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Seed;
    use crate::geosphere::Geosphere;
    use std::collections::BTreeSet;

    #[test]
    fn vertical_verbs_compose() {
        let a = Facet {
            face: 4,
            path: vec![1, 2, 3],
        };
        let child = a.child(0).unwrap();
        assert_eq!(child.path, vec![1, 2, 3, 0]);
        assert_eq!(child.parent(), Some(a.clone()));
        assert_eq!(
            a.ancestor(1),
            Some(Facet {
                face: 4,
                path: vec![1]
            })
        );
        assert_eq!(
            Facet {
                face: 4,
                path: vec![]
            }
            .parent(),
            None
        );
        assert_eq!(a.child(4), Err(FacetError::Invalid));
    }

    #[test]
    fn room_seed_is_deterministic_and_hierarchical() {
        let world = Seed(42);
        let a = Facet {
            face: 3,
            path: vec![0, 1, 2],
        };
        assert_eq!(a.seed(world), a.seed(world));
        // a parent and its child derive different seeds
        assert_ne!(a.seed(world), a.parent().unwrap().seed(world));
    }

    /// The derived seam table's own structure, checked at the table level
    /// rather than through the walk: 24 directed entries pairing into 12
    /// undirected cube edges, each face reaching its four lateral faces and
    /// never itself or its opposite.
    ///
    /// This is the cheapest witness that the derivation in [`seam_table`] is
    /// a derivation and not a transcription. It does not replace
    /// `adjacency_is_symmetric_across_every_seam` — that one exercises the
    /// coordinate re-expression, which is where a turn goes missing — but it
    /// pins the topology in one place a reader can check by eye against the
    /// cube.
    ///
    /// claim: structural(seed: none) — false-positive seed-loop flag: the loop
    /// is over the cube's six FACES and their four sides, a fixed enumeration
    /// of `cube::CUBE_FACES`. No world is built and no seed is drawn.
    #[test]
    fn the_derived_seam_table_is_twelve_edges_traversed_both_ways() {
        let t = seam_table();
        let mut undirected: BTreeSet<(u8, u8)> = BTreeSet::new();
        for (face, sides) in t.iter().enumerate() {
            let face = face as u8;
            let mut reached: BTreeSet<u8> = BTreeSet::new();
            for (side, s) in sides.iter().enumerate() {
                assert_ne!(s.to_face, face, "face {face} side {side} loops to itself");
                reached.insert(s.to_face);
                undirected.insert((face.min(s.to_face), face.max(s.to_face)));
            }
            assert_eq!(
                reached.len(),
                4,
                "face {face} must reach four DISTINCT faces, reached {reached:?}"
            );
            // The unreached face is the opposite one, derived from the
            // normals rather than named.
            let [n, _, _] = cube::CUBE_FACES[face as usize];
            let opposite = cube::CUBE_FACES
                .iter()
                .position(|[m, _, _]| (0..3).all(|i| (m[i] + n[i]).abs() < 1e-12))
                .expect("every cube face has an opposite") as u8;
            assert!(
                !reached.contains(&opposite),
                "face {face} must not seam to its opposite {opposite}"
            );
        }
        assert_eq!(undirected.len(), 12, "a cube has twelve edges");
    }

    /// Stepping across a seam and straight back must return the room you left
    /// — the table's own involution, independent of `neighbors`' three-case
    /// dispatch. A rotation that is off by one turn survives an arity check
    /// and dies here.
    #[test]
    fn a_seam_step_and_its_return_are_inverses() {
        for depth in [1u32, 2, 3, 5] {
            let scale = 1i64 << depth;
            for face in 0..6u8 {
                for side in 0..4usize {
                    for along in 0..scale {
                        let (f2, x2, y2) = seam_step(face, side, along, scale);
                        // Which side of the DESTINATION we arrived through:
                        // whichever coordinate sits on an edge of the
                        // destination's square, read off the landing quad
                        // rather than taken from the table.
                        let back_side = match (x2, y2) {
                            _ if seam_table()[face as usize][side].along_is_y => {
                                if x2 == 0 {
                                    SIDE_A_LO
                                } else {
                                    SIDE_A_HI
                                }
                            }
                            _ => {
                                if y2 == 0 {
                                    SIDE_B_LO
                                } else {
                                    SIDE_B_HI
                                }
                            }
                        };
                        let along_back = if seam_table()[face as usize][side].along_is_y {
                            y2
                        } else {
                            x2
                        };
                        let (f3, x3, y3) = seam_step(f2, back_side, along_back, scale);
                        assert_eq!(
                            f3, face,
                            "depth {depth}: {face}/{side}/{along} came back to face {f3}"
                        );
                        let expect = match side {
                            SIDE_A_LO => (0, along),
                            SIDE_A_HI => (scale - 1, along),
                            SIDE_B_LO => (along, 0),
                            _ => (along, scale - 1),
                        };
                        assert_eq!(
                            (x3, y3),
                            expect,
                            "depth {depth}: {face}/{side}/{along} came back to ({x3}, {y3})"
                        );
                    }
                }
            }
        }
    }

    // Enumerate every Facet at depth `level` by DFS over child digits, over
    // the CUBE's six base faces — i.e. over the whole address space `pack`
    // accepts.
    fn all_addrs(level: u32) -> Vec<Facet> {
        let mut out = Vec::new();
        for face in 0..6u8 {
            let mut stack = vec![Facet { face, path: vec![] }];
            while let Some(a) = stack.pop() {
                if a.path.len() as u32 == level {
                    out.push(a);
                } else {
                    for d in 0..4u8 {
                        let mut p = a.path.clone();
                        p.push(d);
                        stack.push(Facet {
                            face: a.face,
                            path: p,
                        });
                    }
                }
            }
        }
        out
    }

    // `lazy_geometry_is_byte_identical_to_geosphere` and its `reference_mesh`
    // oracle stood here. Both are DELETED, not ported: they asserted that
    // `Facet::corners()` returns the very vertex positions
    // `Geosphere::new(depth)` builds, which is precisely decision 0287's
    // corner-is-a-vertex corollary — the one thing The Pavement retires (spec
    // section 7's H3a). A cube-sphere quad's corners are not geosphere
    // vertices, so there is no version of that assertion left to make; the
    // subject dissolved rather than the property failing. What the old test
    // was actually buying — that lazily-computed corners agree bit-for-bit
    // with the same corners reached another way — is now bought by
    // `corners_are_watertight_across_the_lattice` and
    // `a_parent_shares_its_own_corners_with_its_children` below, which pin
    // agreement across the quadtree instead of against a second mesh.

    #[test]
    fn geometry_is_deterministic() {
        let a = Facet {
            face: 5,
            path: vec![0, 3, 1, 2, 3, 0],
        };
        assert_eq!(a.corners(), a.clone().corners());
        assert_eq!(a.centroid(), a.centroid());
    }

    #[test]
    fn bearing_is_in_range() {
        let a = Facet {
            face: 0,
            path: vec![0, 1, 2],
        };
        let b = Facet {
            face: 0,
            path: vec![3, 3, 3],
        };
        for bear in [a.bearing_to(&b), b.bearing_to(&a)] {
            assert!((0.0..360.0).contains(&bear), "bearing {bear} out of range");
        }
    }

    #[test]
    fn distance_to_self_is_zero_and_the_antipode_is_pi() {
        // The long note that stood here explained why `[1,0,0]`/`[-1,0,0]` were
        // AVOIDED: they sit on a rotational symmetry axis of the base
        // ICOSAHEDRON, where a point and its negation resolve to rooms related
        // by a z-rotation rather than a true point inversion, so their
        // centroids converged on exact antipodes only at depth 27. That
        // reasoning belonged to the icosphere and goes with it.
        //
        // On the cube base the antipodal map is exact and STRUCTURAL, which is
        // both why this holds at depth 4 for any point and why the assertions
        // below changed shape. Measured, this pair: `locate` puts the point on
        // face 4 (`n=+z, u=+x, v=+y`) at `(a, b) = (0.42460278214434916,
        // 0.5509315712412851)` and its negation on face 5 (`n=-z, u=-x,
        // v=+y`) at `(a, -b)`; the `y` index reflects to `scale - 1 - y`
        // (12 -> 3 at scale 16), whose quad centre is the exact negation of
        // `y`'s, and `face_unit` of the reflected parameters is the exact
        // componentwise negation of the original — same sum of squares, so
        // `normalize` divides by the same `m`. So the strong form of the
        // property is BIT equality of the negated centroids, and that is what
        // the middle assertion now states.
        //
        // NEITHER OF THE OLD TOLERANCES SURVIVES, AND BOTH WERE MEASURING
        // FLOATING POINT RATHER THAN GEOMETRY. `distance_rad_to` is
        // `acos(dot)`, whose derivative at `1 - eps` is `1/sqrt(2 eps)`, so a
        // one-ULP error in the dot product (1.1e-16) becomes ~1.5e-8 in the
        // ANGLE. That is a floor of the representation, not a property of the
        // mesh:
        //   - `assert_eq!(self_distance, 0.0)` passed only because the
        //     triangle centroid's `normalize(v0+v1+v2)` happened to give a
        //     self-dot of exactly 1.0 for this one room; `cube::face_unit`
        //     gives 1.0 - 1.1e-16, hence 1.49e-8. The neighbouring
        //     `self_distance_never_nans_from_an_unclamped_dot_product` already
        //     records the principle in its own comment ("what matters here is
        //     'small', not 'exactly zero'") and asserts 1e-6 over every room at
        //     depth 4; this line now agrees with it.
        //   - `< 1e-9` on the antipode passed for the same kind of reason — the
        //     old comment says so outright, that at depth 27 "the raw dot
        //     product rounds to exactly -1.0 there, collapsing the remaining
        //     gap rather than continuing to halve it". Here the dot is
        //     -0.9999999999999999 (one ULP off) and the angle is
        //     3.141592638688632, an error of 1.49e-8. 1e-9 is BELOW the floor
        //     for any angle reached through `acos`, so it is replaced by 1e-7
        //     (~5x the floor) and the exactness claim is made directly instead.
        let a = Facet::containing([0.3, 0.4, 0.866], 4);
        let b = Facet::containing([-0.3, -0.4, -0.866], 4);
        let self_d = a.distance_rad_to(&a);
        assert!(self_d.abs() < 1e-6, "self-distance was {self_d}");
        let (ca, cb) = (a.centroid(), b.centroid());
        for i in 0..3 {
            assert_eq!(
                cb[i], -ca[i],
                "antipodal centroids must negate EXACTLY on axis {i}"
            );
        }
        let d = a.distance_rad_to(&b);
        assert!(
            (d - std::f64::consts::PI).abs() < 1e-7,
            "antipodal distance was {d}"
        );
    }

    #[test]
    fn self_distance_never_nans_from_an_unclamped_dot_product() {
        // A unit vector dotted with itself can land fractionally above 1.0
        // through floating-point accumulation in `centroid`'s normalize step
        // (sum-of-squares then divide), and `acos` of anything above 1.0 is
        // NaN. Sweep a broad set of real rooms (not just one) so this would
        // actually catch a dropped `.clamp(-1.0, 1.0)` rather than passing
        // vacuously.
        for a in all_addrs(4) {
            let d = a.distance_rad_to(&a);
            assert!(!d.is_nan(), "self-distance NaN for {a:?}");
            // Not exact equality to 0.0: the raw dot product of a vector
            // with itself can land a hair *under* 1.0 too (normalize's own
            // rounding), which the clamp does not (and should not) correct
            // — only overshoot past +/-1.0 is a clamping concern. What
            // matters here is "small", not "exactly zero".
            assert!(d.abs() < 1e-6, "self-distance too large for {a:?}: {d}");
        }
    }

    #[test]
    fn distance_is_symmetric() {
        let level = 3u32;
        let addrs = all_addrs(level);
        // Every 37th pair (to keep the test fast) across the full address set.
        let pairs: Vec<(&Facet, &Facet)> = addrs
            .iter()
            .enumerate()
            .flat_map(|(i, a)| addrs.iter().skip(i + 1).step_by(37).map(move |b| (a, b)))
            .collect();
        assert!(!pairs.is_empty());
        for (a, b) in pairs {
            assert_eq!(
                a.distance_rad_to(b),
                b.distance_rad_to(a),
                "asymmetric distance for {a:?} <-> {b:?}"
            );
        }
    }

    #[test]
    fn roomid_round_trips() {
        let cases = [
            Facet {
                face: 0,
                path: vec![],
            },
            Facet {
                face: 5,
                path: vec![3],
            },
            Facet {
                face: 4,
                path: vec![0, 1, 2, 3, 0, 1, 2],
            },
            Facet {
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
        let too_deep = Facet {
            face: 0,
            path: vec![0; MAX_DEPTH + 1],
        };
        assert_eq!(too_deep.pack(), Err(FacetError::DepthExceedsCap));
    }

    #[test]
    fn unpack_rejects_malformed() {
        assert_eq!(FacetId(20).unpack(), Err(FacetIdError::Malformed)); // face 20, no path
        assert_eq!(FacetId(0).unpack(), Err(FacetIdError::Malformed)); // face 0, pathword 0
    }

    #[test]
    fn unpack_refuses_a_pre_cube_face_loudly() {
        // The bound that actually moved (The Pavement, Task 3, addendum 2):
        // `unpack` refused only `face >= 20`, so an id whose face field landed
        // in `6..20` — the shape a PRE-CUBE `FacetId` has, minted when the base
        // mesh was a 20-face icosahedron — decoded silently into a
        // valid-looking `Facet` that `pack` would have refused. Decision 0189
        // requires a pre-flip world to fail loudly; a silent decode is the
        // inverse of that.
        //
        // `FacetId(20)` above does NOT discriminate this: 20 was refused before
        // and after. These do, and they carry a VALID sentinel (pathword 1, a
        // depth-0 path) so the only thing that can refuse them is the face
        // bound itself.
        for face in 6..20u8 {
            let id = FacetId((1u64 << 5) | u64::from(face));
            assert_eq!(
                id.unpack(),
                Err(FacetIdError::Malformed),
                "face {face} is not a cube face and must not decode"
            );
        }
        // The six cube faces, same shape, still decode — the bound moved, it
        // did not close.
        for face in 0..6u8 {
            let id = FacetId((1u64 << 5) | u64::from(face));
            assert_eq!(
                id.unpack(),
                Ok(Facet { face, path: vec![] }),
                "face {face} is a cube face"
            );
        }
        // And the two bounds now agree in both directions: anything `unpack`
        // accepts, `pack` re-accepts.
        for face in 0..32u8 {
            let id = FacetId((1u64 << 5) | u64::from(face));
            if let Ok(f) = id.unpack() {
                assert!(
                    f.pack().is_ok(),
                    "unpack accepted face {face} that pack refuses"
                );
            }
        }
    }

    #[test]
    fn pack_rejects_invalid() {
        assert_eq!(
            Facet {
                face: 20,
                path: vec![]
            }
            .pack(),
            Err(FacetError::Invalid)
        );
        assert_eq!(
            Facet {
                face: 0,
                path: vec![4]
            }
            .pack(),
            Err(FacetError::Invalid)
        );
    }

    #[test]
    fn corner_weights_sum_and_blend() {
        use crate::{NearestVertexIndex, VertexMap};
        let geo = Geosphere::new(3); // globe level 3 for a cheap test
        let index = NearestVertexIndex::new(&geo);
        // a room several levels below the grid
        let addr = Facet {
            face: 5,
            path: vec![0, 3, 1, 2, 3],
        };
        let denom: u64 = 4 << (2 * (addr.path.len() as u32 - geo.depth()));
        let ws = addr.corner_weights(&geo, &index).expect("below the grid");
        assert_eq!(ws.len(), 4, "a quad has four corners");
        let sum: u64 = ws.iter().map(|&(_, w)| w).sum();
        assert_eq!(
            sum, denom,
            "bilinear weights sum to D = (2 * 2^(depth-globe))^2"
        );
        // a constant field blends to the constant
        let field = VertexMap::from_fn(&geo, |_| 5.0f64);
        let blended: f64 = ws
            .iter()
            .map(|&(c, w)| (w as f64 / denom as f64) * field.get(c))
            .sum();
        assert!(
            (blended - 5.0).abs() < 1e-9,
            "constant field blends to the constant"
        );
        // above the grid -> None
        let coarse = Facet {
            face: 5,
            path: vec![0, 3],
        };
        assert!(coarse.corner_weights(&geo, &index).is_none());
    }

    #[test]
    fn corner_weights_pin_vertex_weight_pairing() {
        use crate::NearestVertexIndex;
        // A constant field can't catch a transposition (any permutation of the
        // same weights still blends to the constant), so this pins the
        // vertex<->weight axis directly. THE PROPERTY IS UNCHANGED; THE ROUTE
        // TO IT HAD TO CHANGE, and the reason is worth recording because the
        // old route looked stronger than it was.
        //
        // It used to argue indirectly: "the room centroid's barycentric weights
        // ARE its proximity to each corner, so for an asymmetric room the
        // max-weight corner must be the corner vertex nearest the centroid",
        // and asserted `max_vertex == index.nearest_to_position(centroid)`.
        // Every step of that argument rests on decision 0287's
        // corner-is-a-vertex COROLLARY, which The Pavement retires (spec
        // section 7's H3a): it needs `nearest_to_position(corner_i)` to BE
        // corner i, which it was when a room was an icosphere triangle.
        //
        // Measured on the cube, with this fixture's `[0; 5]` path: the
        // ancestor's corner 0 is `(1, -1, -1)/sqrt(3)`, a CUBE CORNER, and its
        // three nearest geosphere vertices are 529, 527 and 528 at chords
        // 0.09516688089304542, 0.09516688089304551 and 0.09516688089304554 —
        // a three-way tie inside two ULPs. So `nearest_to_position` there is
        // decided by ULP-level tie-breaking in the icosphere's Voronoi diagram,
        // and the centroid (0.023331 away from that corner) falls to a
        // different member of the tie: 527 for the corner, 529 for the
        // centroid. Deepening the path does not help — checked at depths 5
        // through 10, the split persists at every one, down to a
        // corner-to-centroid chord of 0.000723 — because the tie is a property
        // of the point, not of the distance to it. The old assertion was
        // measuring the Voronoi diagram, not the weights.
        //
        // So the pairing is now asserted against its own contract, which is
        // what the doc on `corner_weights` states and what a caller relies on:
        // entry `i` carries the vertex nearest the ancestor's corner `i`. A
        // transposed zip — the bug class this test exists for — still fails it,
        // and it needs no proximity argument at all.
        //
        // **THE INDEPENDENT ROUTE IS RESTORED, ELSEWHERE, AND DELIBERATELY NOT
        // HERE.** The two sentences this replaces said the outside route was
        // lost until `corner_weights` was reconciled with the cube; Task 3
        // reconciled it, and the route came back one step short of the Voronoi
        // diagram:
        // `the_dominant_corner_weight_is_the_nearest_corner_of_its_ancestor_quad`
        // recovers the centroid's place inside its ancestor by inverting the
        // projection from the 3D POSITION, never by asking
        // `nearest_to_position` which vertex owns anything. That is
        // what dodges the tie measured above — the tie is in the icosphere's
        // Voronoi regions around a cube corner, and a comparison that never asks
        // which vertex owns a point never meets it.
        let geo = Geosphere::new(3);
        let index = NearestVertexIndex::new(&geo);
        // path biased toward corner 0 the whole way down -> distinct weights,
        // corner 0 dominates.
        let addr = Facet {
            face: 0,
            path: vec![0, 0, 0, 0, 0],
        };
        let ws = addr.corner_weights(&geo, &index).expect("below the grid");
        let (max_vertex, max_w) = *ws.iter().max_by_key(|&&(_, w)| w).expect("four corners");
        for &(vertex, w) in &ws {
            if vertex != max_vertex {
                assert!(
                    max_w > w,
                    "max weight must be strictly unique for this test to pin anything"
                );
            }
        }
        let anc = addr
            .ancestor(geo.depth())
            .expect("ancestor at the globe level");
        let corner_pos = anc.corners();
        for (i, &(vertex, _)) in ws.iter().enumerate() {
            assert_eq!(
                vertex,
                index.nearest_to_position(&geo, corner_pos[i]),
                "weight {i} is paired with the wrong corner's vertex"
            );
        }
        // And the dominant weight really is corner 0's, which is what makes the
        // fixture asymmetric rather than a permutation-blind constant.
        assert_eq!(
            max_vertex,
            index.nearest_to_position(&geo, corner_pos[0]),
            "the all-0 path must weight the ancestor's corner 0 most heavily"
        );
    }

    /// **THE GEOMETRY-LOOKING ASSERTION.** The whole reason
    /// `corner_weights`' triangle-on-a-quad defect could sit in the tree
    /// unnoticed is that every assertion on it was arithmetic: the numerators
    /// summed to `D`, and a constant field blended to the constant, both true
    /// of ANY four (or three) non-negative weights with the right total. A
    /// test that passes on both a triangle and a quad is not testing the mesh.
    ///
    /// This one is. For every room a level or two below the grid, take the
    /// room's centroid as a POSITION ON THE SPHERE, invert the projection with
    /// [`crate::cube::locate`] to recover where that position falls inside its
    /// ancestor quad, and require the largest weight to sit on the corner the
    /// position is nearest. The round trip is what makes it a mesh test rather
    /// than an identity: `centroid` -> `face_unit` -> `locate` -> ancestor
    /// parameters passes through the actual projection and back, so a mis-read
    /// path digit, a wrong ancestor, or triangle weights paired with quad
    /// corners all fail it.
    ///
    /// **IT DOES NOT CATCH A PERMUTED `corners()` WINDING, and an earlier draft
    /// of this line claimed it did.** The comparison is between weight INDICES
    /// and the hardcoded `unit_corners` below; it never reads `anc.corners()`
    /// and never reads the returned `Vertex`es, so re-winding `corners()` leaves
    /// it green. That axis is closed elsewhere, which is why the claim is
    /// deleted rather than the test strengthened:
    /// `hugging_a_corner_puts_the_weight_on_that_corner` reads `anc.corners()`
    /// positions directly, and `corner_weights_pin_vertex_weight_pairing`
    /// asserts entry `i` carries the vertex nearest corner `i`.
    ///
    /// It compares POSITIONS, never resolved vertices, which keeps it clear of
    /// the icosphere-Voronoi tie
    /// `corner_weights_pin_vertex_weight_pairing` measured around a cube
    /// corner.
    ///
    /// Why the dominant weight is well defined **for `d >= 1`, which is the
    /// only range this sweep visits**: the centroid parameter inside the
    /// ancestor is `((2x+1)/2s, (2y+1)/2s)` with `s = 2^d` even for `d >= 1`,
    /// so neither coordinate is ever exactly `1/2` and the bilinear argmax is
    /// unique — see [`Facet::corner_weights`]' own "Ties" section for the
    /// argument and for the `d == 0` four-way tie this sweep deliberately does
    /// not include. For each of the six corner pairs, "weight `i` beats weight
    /// `j`" reduces to exactly the same inequality as "corner `i` is nearer in
    /// the ancestor's parameter frame than corner `j`" — including the diagonal
    /// pair, where both reduce to `u + v < 1`.
    ///
    /// **TWO OF THE SIX PAIRS CAN BE EXACTLY EQUAL, and that is not a defect in
    /// the argmax claim.** `w0 == w2` whenever `u + v == 1` and `w1 == w3`
    /// whenever `u == v`; both happen constantly. Neither is ever the MAXIMUM
    /// (a tied pair forces the remaining off-diagonal corner strictly above
    /// both), which is why the argmax survives them. A review reading of this
    /// campaign proposed `s = 2, (x, y) = (0, 1)` as a counterexample; there
    /// `w = [3, 1, 3, 9]` — `w0 == w2 == 3` exactly, and the argmax is `w3 = 9`
    /// uniquely. The tie is real and the argmax is not tied.
    ///
    /// **WHAT THIS DELIBERATELY DOES NOT CLAIM, because it is false.** An
    /// earlier draft compared 3D CHORD distances from the centroid to the
    /// ancestor's four corner positions, and it fails: at globe level 2 an
    /// ancestor quad is a quarter of a cube face on a side, the tangent-warped
    /// projection is nowhere near affine over something that large, and the
    /// ordering genuinely inverts. Measured counterexample — `Facet { face: 0,
    /// path: [3, 3, 2, 1] }` at globe level 2 sits at parameter
    /// `(u, v) = (5/8, 3/8)` inside its ancestor, whose nearest corner is 1
    /// (parameter distance² 0.281 against corner 2's 0.531, not a near-tie),
    /// while its 3D-nearest ancestor corner is 2. Neither answer is wrong;
    /// they are answers to different questions, and the weights answer the
    /// parameter one by construction. The chord-distance claim IS true in the
    /// near-affine limit, and `hugging_a_corner_puts_the_weight_on_that_corner`
    /// asserts it there, where it is robust.
    #[test]
    fn the_dominant_corner_weight_is_the_nearest_corner_of_its_ancestor_quad() {
        use crate::NearestVertexIndex;
        let geo = Geosphere::new(2);
        let index = NearestVertexIndex::new(&geo);
        let gl = geo.depth();
        let mut checked = 0u32;
        for extra in 1..=2u32 {
            for addr in all_addrs(gl + extra) {
                let ws = addr.corner_weights(&geo, &index).expect("below the grid");
                let dominant = (0..4).max_by_key(|&i| ws[i].1).expect("four corners");
                // A strictly unique argmax, or this asserts nothing.
                for i in 0..4 {
                    if i != dominant {
                        assert!(
                            ws[dominant].1 > ws[i].1,
                            "weight tie at {addr:?}: {:?}",
                            ws.map(|(_, w)| w)
                        );
                    }
                }
                // The round trip: the centroid as a sphere POSITION, put back
                // into its ancestor quad's own unit square by inverting the
                // projection.
                let anc = addr.ancestor(gl).expect("ancestor at the globe level");
                let al = anc.face_lattice();
                let (face, a, b) = cube::locate(addr.centroid());
                assert_eq!(face, addr.face as usize, "{addr:?} left its own face");
                let span = 2.0 / al.scale as f64;
                let u = (a - face_param(al.x, al.scale)) / span;
                let v = (b - face_param(al.y, al.scale)) / span;
                assert!(
                    (0.0..=1.0).contains(&u) && (0.0..=1.0).contains(&v),
                    "{addr:?} recovered to ({u}, {v}), outside its own ancestor"
                );
                let unit_corners = [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)];
                let d2 = |k: usize| {
                    let (cu, cv) = unit_corners[k];
                    (u - cu) * (u - cu) + (v - cv) * (v - cv)
                };
                let nearest = (0..4)
                    .min_by(|&i, &j| d2(i).total_cmp(&d2(j)))
                    .expect("four corners");
                assert_eq!(
                    dominant, nearest,
                    "at {addr:?} the dominant weight is corner {dominant} but the \
                     centroid recovers to ({u}, {v}), nearest corner {nearest}"
                );
                checked += 1;
            }
        }
        assert!(checked > 0, "the sweep checked nothing");
    }

    /// The other half of the pairing, from the opposite direction, and the
    /// place the 3D claim IS made: walk the descent that HUGS each of the four
    /// quad corners in turn, and watch that corner's weight take over while
    /// the room physically closes on it.
    ///
    /// Three assertions, and the third is the one that looks at the sphere:
    /// the dominant weight is on the hugged corner; its share grows
    /// monotonically with depth; and the hugged corner is the 3D-nearest of
    /// the ancestor's four corner positions to the room's own centroid, by
    /// chord distance, with that distance strictly shrinking. This is the
    /// near-affine limit where a chord comparison is unambiguous (see
    /// `the_dominant_corner_weight_is_the_nearest_corner_of_its_ancestor_quad`
    /// for the measured case where it is not).
    ///
    /// This is the check the old three-corner function could not have passed
    /// at all: corner 3 (`(a_lo, b_hi)`, digit `0b01`) never received a weight
    /// from `bary_triple`, because a `[_; 4]` indexed at 0, 1 and 2 simply
    /// dropped it. So a run of digit `0b01` used to put its weight on some
    /// other corner entirely, silently.
    #[test]
    fn hugging_a_corner_puts_the_weight_on_that_corner() {
        use crate::NearestVertexIndex;
        let geo = Geosphere::new(3);
        let index = NearestVertexIndex::new(&geo);
        let gl = geo.depth();
        // digit -> the corner of the parent quad that child keeps, in
        // `corners`' winding (the map `a_parent_shares_its_own_corners_with_
        // its_children` pins).
        for (digit, corner) in [(0b00u8, 0usize), (0b10, 1), (0b11, 2), (0b01, 3)] {
            let mut previous_share = 0.0f64;
            let mut previous_chord = f64::INFINITY;
            for extra in 1..=6u32 {
                let addr = Facet {
                    face: 4,
                    path: [vec![2; gl as usize], vec![digit; extra as usize]].concat(),
                };
                let ws = addr.corner_weights(&geo, &index).expect("below the grid");
                let denom: f64 = ws.iter().map(|&(_, w)| w as f64).sum();
                let dominant = (0..4).max_by_key(|&i| ws[i].1).expect("four corners");
                assert_eq!(
                    dominant, corner,
                    "a run of digit {digit:#04b} must weight corner {corner} most, \
                     not corner {dominant}"
                );
                let share = ws[corner].1 as f64 / denom;
                assert!(
                    share > previous_share,
                    "corner {corner}'s share must grow as the descent closes on it: \
                     {share} did not exceed {previous_share}"
                );
                previous_share = share;
                // The 3D leg: the hugged corner is the physically nearest of
                // the ancestor's four corners, and getting nearer.
                let anc = addr.ancestor(gl).expect("ancestor at the globe level");
                let cs = anc.corners();
                let c = addr.centroid();
                let chord = |q: [f64; 3]| {
                    ((q[0] - c[0]) * (q[0] - c[0])
                        + (q[1] - c[1]) * (q[1] - c[1])
                        + (q[2] - c[2]) * (q[2] - c[2]))
                        .sqrt()
                };
                let nearest = (0..4)
                    .min_by(|&i, &j| chord(cs[i]).total_cmp(&chord(cs[j])))
                    .expect("four corners");
                assert_eq!(
                    nearest, corner,
                    "a run of digit {digit:#04b} must physically approach corner \
                     {corner}, not corner {nearest}"
                );
                let here = chord(cs[corner]);
                assert!(
                    here < previous_chord,
                    "the centroid must close on corner {corner}: {here} is not under \
                     {previous_chord}"
                );
                previous_chord = here;
            }
            // And in the limit it owns essentially all of the weight.
            assert!(
                previous_share > 0.9,
                "corner {corner}'s share reached only {previous_share} at depth {}",
                gl + 6
            );
        }
    }

    /// **THE ONE EXACT TIE, AND WHO DECIDES IT.** At `depth == globe_level` a
    /// room IS its own grid-level ancestor: its centroid is the quad's centre,
    /// equidistant from all four corners, and the bilinear numerators are
    /// `1, 1, 1, 1` over `D = 4`. No corner dominates, so the answer to "which
    /// vertex is this room's ground?" stops being a property of the geometry and
    /// becomes a property of the CONSUMER's tie-break rule.
    ///
    /// That case did not exist before The Pavement in the same way: the old
    /// three-corner barycentric stencil at `d = 0` gave `1, 1, 1`, also tied,
    /// but with three corners rather than four the divergence between the
    /// repository's two rules could not reach corner 3 at all. This test pins
    /// three things so no future reader has to re-derive them:
    ///
    /// 1. **The tie is exact**, not near — integer equality, no tolerance.
    /// 2. **`corner_weights` itself breaks nothing**: it returns all four in
    ///    `corners()` winding order, and entry `i` is still the vertex nearest
    ///    ancestor corner `i`. That ORDER is the documented contract every
    ///    consumer rule is defined against.
    /// 3. **Both live rules are deterministic, and they disagree here.**
    ///    `windows/locale`'s `dominant_corner` and `windows/vessel`'s
    ///    `containing_vertex` take max-weight-then-LOWEST-`Vertex`;
    ///    `windows/vessel/src/session.rs` uses
    ///    `corners.iter().max_by_key(|c| c.weight)`, which is Rust's
    ///    LAST-maximum, i.e. corner 3. Reproduced here rather than described,
    ///    because `expr_at_stratum` records the divergence as "not provably
    ///    identical on an exact corner-weight tie" and this is that tie.
    ///
    /// It asserts the divergence rather than resolving it: picking one rule for
    /// the whole repository moves behaviour in `windows/locale` or
    /// `windows/vessel`, which is not a kernel geometry decision.
    #[test]
    fn a_room_at_the_globe_level_is_an_exact_four_way_tie() {
        use crate::NearestVertexIndex;
        let geo = Geosphere::new(3);
        let index = NearestVertexIndex::new(&geo);
        let gl = geo.depth();
        for addr in all_addrs(gl) {
            let ws = addr
                .corner_weights(&geo, &index)
                .expect("at the grid level");
            // 1. Exact, integer, all four equal, summing to D.
            assert_eq!(
                ws.map(|(_, w)| w),
                [1, 1, 1, 1],
                "a room AT the globe level must weight its four corners equally: {addr:?}"
            );
            assert_eq!(
                ws.iter().map(|&(_, w)| w).sum::<u64>(),
                4 << (2 * (addr.depth() - gl)),
                "and still sum to D"
            );
            // 2. The order is the contract: entry i is corner i's vertex.
            let corner_pos = addr.corners();
            for (i, &(vertex, _)) in ws.iter().enumerate() {
                assert_eq!(
                    vertex,
                    index.nearest_to_position(&geo, corner_pos[i]),
                    "entry {i} left its corner even under a total tie"
                );
            }
            // 3. Both live rules, spelled out, on the same tied input.
            //    max-weight-then-lowest-Vertex (locale's `dominant_corner`,
            //    vessel's `containing_vertex`):
            let lowest = ws
                .iter()
                .max_by(|a, b| a.1.cmp(&b.1).then(b.0.0.cmp(&a.0.0)))
                .map(|&(v, _)| v)
                .expect("four corners");
            assert_eq!(
                lowest,
                ws.iter().map(|&(v, _)| v).min().expect("four corners"),
                "under a total tie the lowest-Vertex rule must pick the minimum id"
            );
            //    Rust's last-maximum (session.rs's `max_by_key`):
            let last = ws
                .iter()
                .max_by_key(|&&(_, w)| w)
                .map(|&(v, _)| v)
                .expect("four corners");
            assert_eq!(
                last, ws[3].0,
                "under a total tie `max_by_key` must pick the LAST entry, corner 3"
            );
            // Both are deterministic; whether they agree is a property of the
            // vertex ids at this address, not of the rule. Assert only that
            // each is what its own rule says — which is what makes the
            // divergence visible instead of accidental.
        }
    }

    /// The two exact NON-maximal equalities, pinned so the argmax claim in
    /// [`Facet::corner_weights`]' doc cannot rot into a false one.
    ///
    /// `w0 == w2` whenever `u + v == 1` and `w1 == w3` whenever `u == v`; both
    /// are reachable at every depth below the grid. Neither is ever the
    /// maximum. This walks every sub-position at four depths, counts how many
    /// carry each equality (so the test fails if a refactor makes them
    /// unreachable and the check goes vacuous), and asserts the argmax is
    /// strictly unique at all of them.
    #[test]
    fn the_two_exact_weight_equalities_are_never_the_maximum() {
        use crate::NearestVertexIndex;
        let geo = Geosphere::new(2);
        let index = NearestVertexIndex::new(&geo);
        let gl = geo.depth();
        let mut diagonal_ties = 0u32;
        let mut off_diagonal_ties = 0u32;
        for extra in 1..=4u32 {
            for addr in all_addrs(gl + extra) {
                let ws = addr.corner_weights(&geo, &index).expect("below the grid");
                let w = ws.map(|(_, q)| q);
                if w[0] == w[2] {
                    diagonal_ties += 1;
                }
                if w[1] == w[3] {
                    off_diagonal_ties += 1;
                }
                let max = *w.iter().max().expect("four corners");
                assert_eq!(
                    w.iter().filter(|&&q| q == max).count(),
                    1,
                    "the argmax must be strictly unique below the grid: {addr:?} has {w:?}"
                );
            }
        }
        assert!(
            diagonal_ties > 0,
            "no `w0 == w2` case was reached; this check has gone vacuous"
        );
        assert!(
            off_diagonal_ties > 0,
            "no `w1 == w3` case was reached; this check has gone vacuous"
        );
    }

    #[test]
    fn inheritance_at_a_cube_corner() {
        use crate::{NearestVertexIndex, Vertex};
        let geo = Geosphere::new(3);
        let index = NearestVertexIndex::new(&geo);
        // The all-0 path keeps a corner pinned at the base face's
        // `(a, b) = (-1, -1)` corner, which is one of the eight CUBE corners
        // where three faces meet. The test was named for the icosphere's
        // 5-valent pentagon vertex, which does not exist on this mesh; the
        // singular point it is probing does, and there are eight of them
        // instead of twelve (spec section 2.2).
        let addr = Facet {
            face: 0,
            path: vec![0, 0, 0, 0, 0],
        };
        let ws = addr.corner_weights(&geo, &index).expect("below the grid");
        let denom: u64 = 4 << (2 * (addr.path.len() as u32 - geo.depth()));
        assert_eq!(ws.iter().map(|&(_, w)| w).sum::<u64>(), denom);
        // The four corner vertices are distinct valid ids. FOUR, not three:
        // the ancestor is a quad. The count is not a formality — the whole
        // point of a corner-weight stencil is that it resolves four DIFFERENT
        // places on the grid, and a globe level coarse enough to collapse two
        // of them would silently halve the stencil.
        let ids: BTreeSet<Vertex> = ws.iter().map(|&(c, _)| c).collect();
        assert_eq!(ids.len(), 4, "four corners, four grid vertices");
        // And the weight is on the corner the all-0 descent hugs: corner 0,
        // the `(a_lo, b_lo)` corner, which here IS the cube corner.
        let dominant = (0..4).max_by_key(|&i| ws[i].1).expect("four corners");
        assert_eq!(dominant, 0, "an all-0 path hugs the (a_lo, b_lo) corner");
        assert!(
            addr.ancestor(geo.depth())
                .expect("ancestor at the globe level")
                .corners()[0]
                .iter()
                .all(|c| (c.abs() - 1.0 / 3.0f64.sqrt()).abs() < 1e-12),
            "corner 0 of this fixture's ancestor is a cube corner"
        );
    }

    #[test]
    fn containing_round_trips_room_centroids() {
        // Every interior room's own centroid must resolve back to that room.
        for depth in [1u32, 2, 3] {
            for face in 0..6u8 {
                // enumerate all rooms at `depth` on this face
                let mut stack = vec![Facet { face, path: vec![] }];
                for _ in 0..depth {
                    let mut next = Vec::new();
                    for a in stack {
                        for d in 0..4u8 {
                            next.push(a.child(d).unwrap());
                        }
                    }
                    stack = next;
                }
                for room in stack {
                    let got = Facet::containing(room.centroid(), depth);
                    assert_eq!(got, room, "centroid of {room:?} resolved to {got:?}");
                }
            }
        }
    }

    #[test]
    fn containing_at_depth_zero_is_the_base_face() {
        let room = Facet {
            face: 5,
            path: vec![1, 2],
        };
        let got = Facet::containing(room.centroid(), 0);
        assert_eq!(
            got,
            Facet {
                face: 5,
                path: vec![]
            }
        );
    }

    // `neighbor_order_matches_opposite_corner_contract` and
    // `edge_neighbours_are_lattice_adjacent` stood above. Both are DELETED,
    // and both were asserting the TRIANGLE rather than the address space:
    // the first pinned a three-neighbour contract ("neighbor[n] is across the
    // edge opposite corner n"), which has no meaning on a quad with four
    // corners and eight neighbours; the second pinned that an edge-neighbour
    // has the OPPOSITE barycentric orientation and steps one unit on exactly
    // one of three axes, which is the `up` flag and the barycentric lattice
    // that `FaceLattice` no longer has. The Pavement's Task 3 DELIVERED their
    // replacements, named in spec section 8 — `kernel/tests/suite/
    // cube_adjacency.rs` plus `the_derived_seam_table_is_twelve_edges_traversed
    // _both_ways` and `a_seam_step_and_its_return_are_inverses` here: neighbour
    // symmetry at every depth and across every seam, the 12 seam rotations
    // exhaustively, and the corner-quad arity.
    //
    // **THE ARITY CLAUSE ABOVE USED TO READ "all 8 corner quads reporting
    // exactly 7 neighbours", AND THAT COUNT IS WRONG** — the same slip the
    // task brief carried. The cube has 8 corners; each is a lattice vertex of
    // degree THREE, shared by one quad on each of the three faces meeting
    // there, so **24** quads report 7 at every depth (four corner quads per
    // face times six faces), grouping onto 8 distinct corners.
    // `arity_is_eight_except_at_the_cubes_eight_corners` asserts both halves.

    #[test]
    fn the_base_face_is_the_whole_quad() {
        let root = Facet {
            face: 3,
            path: vec![],
        };
        let l = root.face_lattice();
        assert_eq!(l.scale, 1);
        assert_eq!((l.x, l.y), (0, 0));
    }

    #[test]
    fn each_path_digit_is_the_pair_of_lattice_bits() {
        // The digit encoding is a contract between `face_lattice` (which
        // decodes it) and `child`/`containing` (which emit it): a digit is
        // `(hi_x << 1) | hi_y`. Assert it on all four digits at depth 1, where
        // the two halves are directly readable, and then on a deeper path
        // where the interleaving actually has to be undone bit by bit.
        for digit in 0..4u8 {
            let l = Facet {
                face: 0,
                path: vec![digit],
            }
            .face_lattice();
            assert_eq!(l.scale, 2);
            assert_eq!(
                (l.x, l.y),
                (i64::from(digit >> 1), i64::from(digit & 1)),
                "digit {digit} decodes to the wrong half-pair"
            );
        }
        // 0b10, 0b01, 0b11 -> x bits 1,0,1 = 0b101 = 5; y bits 0,1,1 = 0b011 = 3.
        let l = Facet {
            face: 0,
            path: vec![2, 1, 3],
        }
        .face_lattice();
        assert_eq!((l.x, l.y, l.scale), (5, 3, 8));
    }

    #[test]
    fn the_lattice_scale_matches_the_depth() {
        for depth in 0..8u32 {
            let r = Facet {
                face: 5,
                path: vec![2; depth as usize],
            };
            assert_eq!(r.face_lattice().scale, 1i64 << depth);
        }
    }

    #[test]
    fn neighbors_memo_bit_equals_recomputation() {
        // Every room's memoized neighbours must be EXACTLY (not just
        // equivalent-up-to-order) what a fresh `neighbors()` call returns —
        // integer geometry, so plain equality is already exact (no float ULP
        // concern the way `coord`'s bit-pin needs `to_bits()`). One memo
        // shared across every base face and both interior and edge-crossing
        // rooms (the-waymark, Task 3).
        let mut memo = RoomMeshMemo::new();
        for level in [0u32, 1, 2, 3] {
            for addr in all_addrs(level) {
                let expected = addr.neighbors();
                let got = addr.neighbors_memo(&mut memo);
                assert_eq!(got, expected, "neighbour mismatch at {addr:?}");
                // A second read (guaranteed cache hit) must agree too.
                let got_again = addr.neighbors_memo(&mut memo);
                assert_eq!(got_again, expected, "cached read mismatch at {addr:?}");
            }
        }
    }

    #[test]
    fn neighbors_memo_actually_memoizes() {
        // Not just correct — actually a cache: two reads of the SAME room
        // insert exactly one entry, and distinct rooms accumulate one entry
        // each (private-field check, same module).
        let mut memo = RoomMeshMemo::new();
        let a = Facet {
            face: 3,
            path: vec![1, 2],
        };
        let b = Facet {
            face: 5,
            path: vec![0, 3, 1],
        };
        let _ = a.neighbors_memo(&mut memo);
        assert_eq!(memo.neighbors.len(), 1);
        let _ = a.neighbors_memo(&mut memo);
        assert_eq!(
            memo.neighbors.len(),
            1,
            "a repeat read must not grow the memo"
        );
        let _ = b.neighbors_memo(&mut memo);
        assert_eq!(memo.neighbors.len(), 2, "a distinct room adds one entry");
    }

    #[test]
    fn corner_weights_memo_bit_equals_recomputation() {
        use crate::NearestVertexIndex;
        // Same exact-equality pin as `neighbors_memo`, over both grid-covered
        // and above-the-grid (`None`) rooms, at a couple of globe levels —
        // `corner_weights` is exact-integer too (weights are BILINEAR
        // numerators over `4 << (2 * (depth - globe_level))`), so no ULP
        // concern here either.
        for globe_level in [2u32, 3] {
            let geo = Geosphere::new(globe_level);
            let index = NearestVertexIndex::new(&geo);
            let mut memo = RoomMeshMemo::new();
            for level in [globe_level, globe_level + 1, globe_level + 2] {
                for addr in all_addrs(level) {
                    let expected = addr.corner_weights(&geo, &index);
                    let got = addr.corner_weights_memo(&geo, &index, &mut memo);
                    assert_eq!(got, expected, "corner_weights mismatch at {addr:?}");
                    let got_again = addr.corner_weights_memo(&geo, &index, &mut memo);
                    assert_eq!(got_again, expected, "cached read mismatch at {addr:?}");
                }
            }
            // Above-the-grid rooms (shallower than the globe level) must
            // memoize their `None` too, not just grid-covered `Some`s.
            for addr in all_addrs(globe_level.saturating_sub(1)) {
                let expected = addr.corner_weights(&geo, &index);
                assert!(expected.is_none(), "fixture must be above the grid");
                let got = addr.corner_weights_memo(&geo, &index, &mut memo);
                assert_eq!(got, expected, "None must memoize at {addr:?}");
            }
        }
    }

    #[test]
    fn corner_weights_memo_actually_memoizes() {
        use crate::NearestVertexIndex;
        let geo = Geosphere::new(3);
        let index = NearestVertexIndex::new(&geo);
        let mut memo = RoomMeshMemo::new();
        let a = Facet {
            face: 2,
            path: vec![0, 3, 1, 2, 3],
        };
        let b = Facet {
            face: 4,
            path: vec![1, 1, 1, 1, 1],
        };
        let _ = a.corner_weights_memo(&geo, &index, &mut memo);
        assert_eq!(memo.corner_weights.len(), 1);
        let _ = a.corner_weights_memo(&geo, &index, &mut memo);
        assert_eq!(
            memo.corner_weights.len(),
            1,
            "a repeat read must not grow the memo"
        );
        let _ = b.corner_weights_memo(&geo, &index, &mut memo);
        assert_eq!(
            memo.corner_weights.len(),
            2,
            "a distinct room adds one entry"
        );
    }

    #[test]
    fn corner_weights_lookup_distinguishes_miss_from_a_cached_none() {
        use crate::NearestVertexIndex;
        // Above-the-grid `None` and "never looked up" must read differently
        // through the read-only lookup — a `_cached` consumer (Task 3 fix
        // round, Finding 1) that only checks "is there a Some inner value"
        // would otherwise treat a cached above-grid `None` as a miss and
        // recompute forever, defeating the whole point of prefilling it.
        let geo = Geosphere::new(3);
        let index = NearestVertexIndex::new(&geo);
        let mut memo = RoomMeshMemo::new();
        let above_grid = Facet {
            face: 0,
            path: vec![1],
        };
        let below_grid = Facet {
            face: 0,
            path: vec![0, 3, 1, 2, 3],
        };
        assert_eq!(
            memo.corner_weights_lookup(&above_grid),
            None,
            "never looked up: a true miss"
        );
        let _ = above_grid.corner_weights_memo(&geo, &index, &mut memo);
        assert_eq!(
            memo.corner_weights_lookup(&above_grid),
            Some(None),
            "cached as above-grid: a HIT whose inner value is None"
        );
        assert_eq!(
            memo.corner_weights_lookup(&below_grid),
            None,
            "a different, never-looked-up room is still a miss"
        );
        let expected = below_grid.corner_weights(&geo, &index);
        let _ = below_grid.corner_weights_memo(&geo, &index, &mut memo);
        assert_eq!(
            memo.corner_weights_lookup(&below_grid),
            Some(expected),
            "cached as below-grid: a HIT whose inner value is the real weights"
        );
    }

    #[test]
    fn corner_weights_memo_serves_two_globe_levels_correctly() {
        use crate::NearestVertexIndex;
        // The-forebay Task 3, spec §2.1b: this test REPLACES
        // `corner_weights_memo_asserts_against_geo_level_aliasing`, which
        // required a `should_panic` on exactly this scenario (one memo fed
        // from two different `Geosphere` levels). That guard existed because
        // the OLD key (`Facet` alone) could not tell two levels apart —
        // completing the key with the level (spec §2.1: `Geosphere::new`
        // takes a level and nothing else, so the level determines the whole
        // `(geo, index)` pair) turns "two levels" into "two different keys",
        // which deletes the bug class rather than needing a stronger guard.
        // The property this test pins is the one that replaces the panic:
        // one memo answers correctly for EACH level, and neither insert
        // disturbs the other.
        let geo_a = Geosphere::new(2);
        let index_a = NearestVertexIndex::new(&geo_a);
        let geo_b = Geosphere::new(3);
        let index_b = NearestVertexIndex::new(&geo_b);
        let mut memo = RoomMeshMemo::new();
        let addr = Facet {
            face: 5,
            path: vec![0, 3, 1, 2, 3],
        };
        let other = Facet {
            face: 3,
            path: vec![1, 0, 3, 1, 2, 3],
        };
        let expected_a = addr.corner_weights(&geo_a, &index_a);
        let expected_other_b = other.corner_weights(&geo_b, &index_b);

        // First insert, at level 2...
        let got_a = addr.corner_weights_memo(&geo_a, &index_a, &mut memo);
        assert_eq!(got_a, expected_a, "level-2 answer through the memo");

        // ...a DIFFERENT room at level 3 through the SAME memo must be
        // answered correctly too, not panic (the old bug class, deleted).
        let got_other_b = other.corner_weights_memo(&geo_b, &index_b, &mut memo);
        assert_eq!(
            got_other_b, expected_other_b,
            "level-3 answer through the SAME memo"
        );

        // Re-reading the level-2 entry afterwards must still be correct —
        // the level-3 insert must not have clobbered or aliased it.
        let got_a_again = addr.corner_weights_memo(&geo_a, &index_a, &mut memo);
        assert_eq!(
            got_a_again, expected_a,
            "level-2 answer survives a level-3 insert into the same memo"
        );
    }

    #[test]
    fn memo_counts_hits_and_misses_separately_per_half() {
        let geo = Geosphere::new(3);
        let index = NearestVertexIndex::new(&geo);
        let addr = Facet {
            face: 0,
            path: vec![0, 0, 0],
        };
        let mut memo = RoomMeshMemo::new();

        // Cold: one miss on each half, no hits.
        let _ = addr.corner_weights_memo(&geo, &index, &mut memo);
        let _ = addr.neighbors_memo(&mut memo);
        assert_eq!(memo.corner_weights_misses(), 1);
        assert_eq!(memo.corner_weights_hits(), 0);
        assert_eq!(memo.neighbors_misses(), 1);
        assert_eq!(memo.neighbors_hits(), 0);

        // Warm: the same address hits both halves and adds no miss.
        let _ = addr.corner_weights_memo(&geo, &index, &mut memo);
        let _ = addr.neighbors_memo(&mut memo);
        assert_eq!(memo.corner_weights_hits(), 1);
        assert_eq!(memo.corner_weights_misses(), 1);
        assert_eq!(memo.neighbors_hits(), 1);
        assert_eq!(memo.neighbors_misses(), 1);
    }

    #[test]
    fn a_cached_none_counts_as_a_hit_not_a_miss() {
        // An above-the-grid room caches Some(None) -- a cached ABSENCE. Reading it
        // again must count a HIT: conflating a cached None with "not looked up
        // yet" is the exact distinction corner_weights_lookup's Option<Option<_>>
        // exists to draw, and a counter that got it wrong would report a
        // permanently cold cache for every above-the-grid room.
        let geo = Geosphere::new(5);
        let index = NearestVertexIndex::new(&geo);
        let shallow = Facet {
            face: 0,
            path: vec![0],
        };
        assert!(
            shallow.depth() < geo.depth(),
            "this address must be above the grid"
        );

        let mut memo = RoomMeshMemo::new();
        assert!(
            shallow
                .corner_weights_memo(&geo, &index, &mut memo)
                .is_none()
        );
        assert_eq!(memo.corner_weights_misses(), 1);
        assert!(
            shallow
                .corner_weights_memo(&geo, &index, &mut memo)
                .is_none()
        );
        assert_eq!(
            memo.corner_weights_hits(),
            1,
            "a cached absence must read as a hit"
        );
        assert_eq!(memo.corner_weights_misses(), 1, "and must not re-miss");
    }

    #[test]
    fn room_mesh_memo_public_surface_is_unchanged_by_the_forebay() {
        // The migration behind this type MUST NOT move its public API: every
        // caller is in windows/vessel, windows/locale or windows/lab, and
        // campaign/the-hand holds off two of those three. If this stops
        // compiling, the campaign's zero-vessel-edit constraint is broken and
        // the right move is to STOP, not to update the callers.
        let mut memo = RoomMeshMemo::new();
        let cloned = memo.clone();
        let _ = format!("{cloned:?}");
        let _ = RoomMeshMemo::default();

        let geo = Geosphere::new(3);
        let index = NearestVertexIndex::new(&geo);
        let addr = Facet {
            face: 0,
            path: vec![0, 0, 0],
        };

        let _: Option<[(Vertex, u64); 4]> = addr.corner_weights(&geo, &index);
        let _: Option<[(Vertex, u64); 4]> = addr.corner_weights_memo(&geo, &index, &mut memo);
        let _: Vec<Facet> = addr.neighbors();
        let _: Vec<Facet> = addr.neighbors_memo(&mut memo);
        let _: Option<Option<[(Vertex, u64); 4]>> = memo.corner_weights_lookup(&addr);
        let _: Option<u32> = memo.corner_weights_geo_level();
        let _: (u64, u64) = (memo.corner_weights_hits(), memo.corner_weights_misses());
        let _: (u64, u64) = (memo.neighbors_hits(), memo.neighbors_misses());
    }
    #[test]
    fn containing_round_trips_a_facets_own_centroid_at_every_depth() {
        for depth in [0u32, 1, 3, 6, 12, 13] {
            for face in 0..6u8 {
                let f = Facet {
                    face,
                    path: vec![0; depth as usize],
                };
                assert_eq!(Facet::containing(f.centroid(), depth), f);
            }
        }
    }

    #[test]
    fn a_facet_has_four_corners_and_no_orientation_flag() {
        let f = Facet {
            face: 0,
            path: vec![1, 2, 3],
        };
        assert_eq!(f.corners().len(), 4);
        let l = f.face_lattice();
        assert_eq!(l.scale, 1 << 3);
        assert!(l.x >= 0 && l.x < l.scale && l.y >= 0 && l.y < l.scale);
    }

    #[test]
    fn every_facet_round_trips_its_own_centroid_across_the_whole_lattice() {
        // The brief's round-trip test walks one quad per face (the all-0
        // path). This walks EVERY quad at depth 3 on every face, so a
        // bisection that only happens to work at the lattice origin — an
        // inverted comparison, a mis-ordered digit — cannot pass.
        for addr in all_addrs(3) {
            let got = Facet::containing(addr.centroid(), addr.depth());
            assert_eq!(got, addr, "centroid of {addr:?} resolved to {got:?}");
        }
    }

    #[test]
    fn corners_are_watertight_across_the_lattice() {
        // Two quads sharing an edge must share its two endpoints BIT for bit,
        // not to a tolerance: both reach them through `face_param` and
        // `cube::face_unit`, and `face_param` is exact on a power-of-two
        // scale. This is the property that replaces the old
        // byte-identical-to-`Geosphere` trust transfer.
        let scale = 1i64 << 3;
        for face in 0..6u8 {
            for x in 0..scale {
                for y in 0..scale {
                    let here = quad(face, x, y, 3).corners();
                    if x + 1 < scale {
                        let east = quad(face, x + 1, y, 3).corners();
                        // this quad's (a_hi, b_lo) / (a_hi, b_hi) edge is the
                        // eastern quad's (a_lo, b_lo) / (a_lo, b_hi) edge.
                        assert_eq!(here[1], east[0], "seam gap east of {x},{y}");
                        assert_eq!(here[2], east[3], "seam gap east of {x},{y}");
                    }
                    if y + 1 < scale {
                        let north = quad(face, x, y + 1, 3).corners();
                        assert_eq!(here[3], north[0], "seam gap north of {x},{y}");
                        assert_eq!(here[2], north[1], "seam gap north of {x},{y}");
                    }
                }
            }
        }
    }

    #[test]
    fn a_parent_shares_its_own_corners_with_its_children() {
        // Child `(hi_x << 1) | hi_y` takes the parent's corner diagonally
        // opposite the quad's centre on that side — bit for bit, again by
        // construction rather than by tolerance. Corner `k` of the parent is
        // corner `k` of exactly one child.
        for parent in all_addrs(2) {
            let pc = parent.corners();
            for digit in 0..4u8 {
                let child = parent.child(digit).expect("depth 3 is under the cap");
                let cc = child.corners();
                // digit -> which parent corner this child inherits, in the
                // `(a_lo,b_lo), (a_hi,b_lo), (a_hi,b_hi), (a_lo,b_hi)` winding.
                let k = match digit {
                    0b00 => 0,
                    0b10 => 1,
                    0b11 => 2,
                    _ => 3,
                };
                assert_eq!(
                    cc[k], pc[k],
                    "child {digit} of {parent:?} lost parent corner {k}"
                );
            }
        }
    }

    #[test]
    fn pack_rejects_a_face_past_the_cube() {
        // `pack_rejects_invalid`'s `face: 20` case no longer discriminates the
        // bound that actually moved: 20 was refused before this campaign and
        // is refused after it. 6 is the first face the cube does not have.
        for face in 6..20u8 {
            assert_eq!(
                Facet { face, path: vec![] }.pack(),
                Err(FacetError::Invalid),
                "face {face} is not a cube face"
            );
        }
        for face in 0..6u8 {
            assert!(Facet { face, path: vec![] }.pack().is_ok(), "face {face}");
        }
    }

    #[test]
    fn a_centroid_is_the_parameter_centre_of_its_own_corners_quad() {
        // `centroid` and `corners` must describe the SAME quad. They are
        // computed from different `face_param` arguments, so a scale or an
        // off-by-one in either would separate them: assert the centroid is
        // strictly inside the quad by checking `locate` puts it back between
        // the corner parameters.
        for addr in all_addrs(3) {
            let l = addr.face_lattice();
            let (face, a, b) = cube::locate(addr.centroid());
            assert_eq!(face, addr.face as usize, "centroid left its face");
            assert!(
                a > face_param(l.x, l.scale) && a < face_param(l.x + 1, l.scale),
                "centroid of {addr:?} is outside its own quad in a: {a}"
            );
            assert!(
                b > face_param(l.y, l.scale) && b < face_param(l.y + 1, l.scale),
                "centroid of {addr:?} is outside its own quad in b: {b}"
            );
        }
    }

    // Build the `Facet` at integer lattice `(x, y)` and `depth` on `face` by
    // interleaving the coordinates' bits — the forward direction of
    // `face_lattice`'s decode, which is what Task 3 will need publicly.
    fn quad(face: u8, x: i64, y: i64, depth: u32) -> Facet {
        let path = (0..depth)
            .rev()
            .map(|i| {
                let hi_x = ((x >> i) & 1) as u8;
                let hi_y = ((y >> i) & 1) as u8;
                (hi_x << 1) | hi_y
            })
            .collect();
        let f = Facet { face, path };
        let l = f.face_lattice();
        assert_eq!(
            (l.x, l.y, l.scale),
            (x, y, 1i64 << depth),
            "quad() is wrong"
        );
        f
    }
}
