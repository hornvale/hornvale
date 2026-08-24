//! The Geosphere: a deterministic icosphere region graph over the unit
//! sphere. Vertices are the vertices of a subdivided icosahedron; adjacency is
//! the triangulation's edges. It is seed-independent (fully determined by its
//! subdivision level) and never serialized — recomputed on demand, like a
//! `Field`. This is the spatial substrate the terrain and climate domains
//! compute over.

use crate::math;
use std::collections::{BTreeMap, BTreeSet};

/// Identifier for a vertex — an index into the mesh's vertices.
/// type-audit: bare-ok(index)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Vertex(pub u32);

/// A geographic coordinate in degrees.
/// type-audit: pending(wave-1)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GeoCoord {
    /// Latitude in degrees, `[-90, 90]` (north positive).
    pub latitude: f64,
    /// Longitude in degrees, `(-180, 180]` (east positive).
    pub longitude: f64,
}

/// A discretized planetary surface. Vertices are the vertices of an icosahedron
/// subdivided `level` times; every vertex sits on the unit sphere.
#[derive(Clone, Debug)]
pub struct Geosphere {
    /// Subdivision depth (0 = the bare icosahedron).
    depth: u32,
    /// Unit-sphere position per vertex, indexed by `Vertex`.
    positions: Vec<[f64; 3]>,
    /// Geographic coordinate per vertex, indexed by `Vertex`. Precomputed at
    /// construction (`asin(z)` / `atan2(y, x)` of `positions`) so `coord` is a
    /// lookup instead of a transcendental pair per call — bit-identical to
    /// recomputing it, since it is the same expression on the same position.
    coords: Vec<GeoCoord>,
    /// Adjacent vertices per vertex, ascending, indexed by `Vertex`.
    neighbors: Vec<Vec<Vertex>>,
}

/// A value per vertex, indexed by `Vertex`. Built from a `Geosphere`, so it has
/// exactly one entry per vertex. This is the representation domains use for
/// derived per-vertex data (elevation, temperature, biome).
#[derive(Clone, Debug, PartialEq)]
pub struct VertexMap<T> {
    values: Vec<T>,
}

impl<T> VertexMap<T> {
    /// Build a `VertexMap` by evaluating `f` at every vertex of `geo`, in
    /// ascending `Vertex` order.
    pub fn from_fn(geo: &Geosphere, mut f: impl FnMut(Vertex) -> T) -> VertexMap<T> {
        VertexMap {
            values: geo.vertices().map(&mut f).collect(),
        }
    }

    /// The value at a vertex.
    pub fn get(&self, id: Vertex) -> &T {
        &self.values[id.0 as usize]
    }

    /// Build a new `VertexMap` by mapping every `(Vertex, &value)` pair. Length-
    /// preserving by construction, so the result spans the same geosphere
    /// without needing one passed in — which is what lets a typed field
    /// (`ecology::CapacityMap`) combine with another without unwrapping.
    pub fn map_indexed<U>(&self, mut f: impl FnMut(Vertex, &T) -> U) -> VertexMap<U> {
        VertexMap {
            values: self
                .values
                .iter()
                .enumerate()
                .map(|(i, v)| f(Vertex(i as u32), v))
                .collect(),
        }
    }

    /// The number of vertices.
    /// type-audit: bare-ok(count)
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Whether the map is empty.
    /// type-audit: bare-ok(flag)
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Iterate `(Vertex, &value)` pairs in ascending `Vertex` order.
    pub fn iter(&self) -> impl Iterator<Item = (Vertex, &T)> {
        self.values
            .iter()
            .enumerate()
            .map(|(i, v)| (Vertex(i as u32), v))
    }
}

/// Normalize a 3-vector to unit length.
pub(crate) fn normalize(v: [f64; 3]) -> [f64; 3] {
    let [x, y, z] = v;
    let len = (x * x + y * y + z * z).sqrt();
    [x / len, y / len, z / len]
}

/// Edge midpoint projected onto the unit sphere — the subdivision step
/// (average, then normalize). The one place the sphere-midpoint op is defined.
pub(crate) fn slerp_mid(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    normalize([
        (a[0] + b[0]) / 2.0,
        (a[1] + b[1]) / 2.0,
        (a[2] + b[2]) / 2.0,
    ])
}

/// The twelve icosahedron vertices (golden-ratio rectangles), unnormalized,
/// and the twenty triangular faces as vertex-index triples. Fixed data — the
/// source of the Geosphere's determinism.
fn base_icosahedron() -> (Vec<[f64; 3]>, Vec<[u32; 3]>) {
    let p = (1.0 + 5.0_f64.sqrt()) / 2.0; // golden ratio
    let raw = [
        [-1.0, p, 0.0],
        [1.0, p, 0.0],
        [-1.0, -p, 0.0],
        [1.0, -p, 0.0],
        [0.0, -1.0, p],
        [0.0, 1.0, p],
        [0.0, -1.0, -p],
        [0.0, 1.0, -p],
        [p, 0.0, -1.0],
        [p, 0.0, 1.0],
        [-p, 0.0, -1.0],
        [-p, 0.0, 1.0],
    ];
    let vertices = raw.iter().map(|&v| normalize(v)).collect();
    let faces = vec![
        [0, 11, 5],
        [0, 5, 1],
        [0, 1, 7],
        [0, 7, 10],
        [0, 10, 11],
        [1, 5, 9],
        [5, 11, 4],
        [11, 10, 2],
        [10, 7, 6],
        [7, 1, 8],
        [3, 9, 4],
        [3, 4, 2],
        [3, 2, 6],
        [3, 6, 8],
        [3, 8, 9],
        [4, 9, 5],
        [2, 4, 11],
        [6, 2, 10],
        [8, 6, 7],
        [9, 8, 1],
    ];
    (vertices, faces)
}

/// The base icosahedron (12 vertices, 20 faces), computed once. `room` reads
/// this immutably; `Geosphere::new` keeps taking its own owned copy to mutate.
#[allow(clippy::type_complexity)]
pub(crate) fn base_data() -> &'static (Vec<[f64; 3]>, Vec<[u32; 3]>) {
    use std::sync::OnceLock;
    static BASE: OnceLock<(Vec<[f64; 3]>, Vec<[u32; 3]>)> = OnceLock::new();
    BASE.get_or_init(base_icosahedron)
}

/// Subdivide each triangular face into four, projecting new edge-midpoint
/// vertices onto the unit sphere. Shared midpoints are deduplicated via an
/// edge cache keyed by the ordered vertex-index pair, so vertex numbering is
/// deterministic (faces visited in order; a new midpoint takes the next
/// sequential index on first encounter).
fn subdivide(positions: Vec<[f64; 3]>, faces: Vec<[u32; 3]>) -> (Vec<[f64; 3]>, Vec<[u32; 3]>) {
    let mut positions = positions;
    let mut cache: BTreeMap<(u32, u32), u32> = BTreeMap::new();
    let mut midpoint = |a: u32, b: u32, positions: &mut Vec<[f64; 3]>| -> u32 {
        let key = (a.min(b), a.max(b));
        if let Some(&idx) = cache.get(&key) {
            return idx;
        }
        let mid = slerp_mid(positions[a as usize], positions[b as usize]);
        let idx = positions.len() as u32;
        positions.push(mid);
        cache.insert(key, idx);
        idx
    };
    let mut new_faces = Vec::with_capacity(faces.len() * 4);
    for [a, b, c] in faces {
        let ab = midpoint(a, b, &mut positions);
        let bc = midpoint(b, c, &mut positions);
        let ca = midpoint(c, a, &mut positions);
        new_faces.push([a, ab, ca]);
        new_faces.push([b, bc, ab]);
        new_faces.push([c, ca, bc]);
        new_faces.push([ab, bc, ca]);
    }
    (positions, new_faces)
}

/// Derive per-vertex adjacency from the triangular faces: two vertices are
/// adjacent iff they share a face edge. Neighbor lists are sorted ascending.
fn build_neighbors(vertex_count: usize, faces: &[[u32; 3]]) -> Vec<Vec<Vertex>> {
    let mut sets: Vec<BTreeSet<u32>> = vec![BTreeSet::new(); vertex_count];
    for &[a, b, c] in faces {
        for (u, v) in [(a, b), (b, c), (c, a)] {
            sets[u as usize].insert(v);
            sets[v as usize].insert(u);
        }
    }
    sets.into_iter()
        .map(|s| s.into_iter().map(Vertex).collect())
        .collect()
}

impl Geosphere {
    /// Build an icosphere subdivided `depth` times. (Task 1 handles the base;
    /// Task 2 adds subdivision for `depth > 0`.)
    /// type-audit: bare-ok(count)
    pub fn new(depth: u32) -> Geosphere {
        let (mut positions, mut faces) = base_icosahedron();
        for _ in 0..depth {
            (positions, faces) = subdivide(positions, faces);
        }
        let neighbors = build_neighbors(positions.len(), &faces);
        // Precompute the geographic coordinate once per vertex. This is the exact
        // expression `coord` used to evaluate per call, on the same stored
        // position, so every consumer receives a bit-identical GeoCoord.
        let coords = positions
            .iter()
            .map(|&[x, y, z]| GeoCoord {
                latitude: math::asin(z).to_degrees(),
                longitude: math::atan2(y, x).to_degrees(),
            })
            .collect();
        Geosphere {
            depth,
            positions,
            coords,
            neighbors,
        }
    }

    /// The subdivision depth — how many times the base icosahedron has been
    /// refined. Named `depth`, not `level`: "level" means three other things in
    /// this repository (see `book/src/reference/lexicon-of-place.md`), two of
    /// them on wire schemas that cannot move.
    /// type-audit: bare-ok(count)
    pub fn depth(&self) -> u32 {
        self.depth
    }

    /// The number of vertices.
    /// type-audit: bare-ok(count)
    pub fn vertex_count(&self) -> usize {
        self.positions.len()
    }

    /// Iterate every vertex id in ascending order.
    pub fn vertices(&self) -> impl Iterator<Item = Vertex> {
        (0..self.positions.len() as u32).map(Vertex)
    }

    /// The unit-sphere position of a vertex.
    /// type-audit: pending(wave-1)
    pub fn position(&self, id: Vertex) -> [f64; 3] {
        self.positions[id.0 as usize]
    }

    /// The geographic coordinate of a vertex.
    pub fn coord(&self, id: Vertex) -> GeoCoord {
        self.coords[id.0 as usize]
    }

    /// The vertices adjacent to `id`, in ascending `Vertex` order.
    pub fn neighbors(&self, id: Vertex) -> &[Vertex] {
        &self.neighbors[id.0 as usize]
    }

    /// Bounded breadth-first hop distance between two vertices over the neighbour
    /// graph. `Some(hops)` if `b` is within `max` hops of `a` (0 if `a == b`),
    /// else `None`. Integer-only, deterministic (no transcendentals).
    /// type-audit: bare-ok(count)
    pub fn hops_between(&self, a: Vertex, b: Vertex, max: u32) -> Option<u32> {
        if a == b {
            return Some(0);
        }
        let mut visited: std::collections::BTreeSet<Vertex> = std::collections::BTreeSet::new();
        visited.insert(a);
        let mut frontier: Vec<Vertex> = vec![a];
        for depth in 1..=max {
            let mut next: Vec<Vertex> = Vec::new();
            for &c in &frontier {
                for &n in self.neighbors(c) {
                    if n == b {
                        return Some(depth);
                    }
                    if visited.insert(n) {
                        next.push(n);
                    }
                }
            }
            if next.is_empty() {
                break;
            }
            frontier = next;
        }
        None
    }
}

/// Latitude bands in the nearest-vertex index.
const BAND_COUNT: usize = 30;
/// Height of one band, degrees.
const BAND_DEGREES: f64 = 180.0 / BAND_COUNT as f64;
/// Longitude buckets within each latitude band (A1: the 2-D grid). Combined
/// with the ±1 latitude bands, this windows the scan to a small neighborhood
/// instead of three full latitude rings.
const LON_BUCKETS: usize = 60;
/// Width of one longitude bucket, degrees.
const LON_DEGREES: f64 = 360.0 / LON_BUCKETS as f64;
/// Safety factor on the measured max edge length when sizing the longitude
/// window: icosphere triangles are near-equilateral so the covering radius is
/// below the longest edge, but the margin absorbs triangle-shape irregularity
/// and bucket-edge slack. Widening only costs a little scan; under-covering
/// would silently change a result, so err wide. Pinned by the equality test.
const COVER_MARGIN: f64 = 1.5;

/// Dot product of two unit vectors.
fn dot3(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Latitude band of a coordinate (degrees), north (0) to south.
fn lat_band(latitude: f64) -> usize {
    (((90.0 - latitude) / BAND_DEGREES) as usize).min(BAND_COUNT - 1)
}

/// Longitude bucket of a coordinate (degrees, `(-180, 180]`).
fn lon_bucket(longitude: f64) -> usize {
    (((longitude + 180.0) / LON_DEGREES) as usize).min(LON_BUCKETS - 1)
}

/// A latitude-banded index for pixel→vertex lookups, with a longitude window
/// that skips the dot product for far-away vertices. Vertices sit in 30 bands of 6°
/// (ascending `Vertex`); a query scans its band ± 1 but computes a dot only for
/// vertices whose longitude bucket is within a 1/cos(lat)-widened window of the
/// query — the true nearest (and any equal-distance tie-partner) always lies
/// inside it by the measured coverage bound; near the poles the window
/// saturates to the full ring, i.e. the earlier band-only scan. Returns the
/// bit-identical vertex the full band scan did (same max dot, same
/// first-in-scan-order tie-break) — pinned by an all-levels equality test.
#[derive(Debug, Clone)]
pub struct NearestVertexIndex {
    /// Vertices by `band * LON_BUCKETS + lon_bucket`, ascending `Vertex` within
    /// each bucket. A query visits only the buckets in its band ± 1 × longitude
    /// window (the speed), and a `(band, Vertex)` tie-break key reproduces the
    /// band scan's first-in-scan-order winner regardless of bucket visit order
    /// (the correctness).
    grid: Vec<Vec<Vertex>>,
    /// Angular coverage bound in degrees: `COVER_MARGIN ×` the mesh's longest
    /// vertex-to-neighbor edge. The nearest vertex to any query lies within the
    /// covering radius (below the longest edge), so a longitude window of
    /// `ceil(cover_deg / cos(lat) / LON_DEGREES) + 1` buckets provably contains
    /// it — and any equal-distance tie-partner too, so skipping out-of-window
    /// vertices never changes a result. Bigger at coarser levels → saturates to a
    /// full-ring scan.
    cover_deg: f64,
}

impl NearestVertexIndex {
    /// Bucket every vertex of `geo` by (latitude band, longitude bucket),
    /// ascending `Vertex`, and measure the coverage bound from the longest
    /// vertex-to-neighbor edge.
    pub fn new(geo: &Geosphere) -> NearestVertexIndex {
        let mut grid = vec![Vec::new(); BAND_COUNT * LON_BUCKETS];
        let mut max_edge = 0.0_f64;
        for vertex in geo.vertices() {
            let c = geo.coord(vertex);
            grid[lat_band(c.latitude) * LON_BUCKETS + lon_bucket(c.longitude)].push(vertex);
            let p = geo.position(vertex);
            for &n in geo.neighbors(vertex) {
                // Unit vectors; clamp guards acos's domain against fp drift.
                let ang = math::acos(dot3(p, geo.position(n)).clamp(-1.0, 1.0));
                if ang > max_edge {
                    max_edge = ang;
                }
            }
        }
        NearestVertexIndex {
            grid,
            cover_deg: max_edge.to_degrees() * COVER_MARGIN,
        }
    }

    /// The vertex nearest a coordinate (degrees), by maximum dot product.
    /// Inverts the coord convention: latitude = asin(z), longitude =
    /// atan2(y, x).
    /// type-audit: pending(wave-1)
    pub fn nearest(&self, geo: &Geosphere, latitude: f64, longitude: f64) -> Vertex {
        let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
        let cos_lat = math::cos(lat);
        let target = [
            cos_lat * math::cos(lon),
            cos_lat * math::sin(lon),
            math::sin(lat),
        ];
        self.scan_at(geo, target, latitude, longitude, cos_lat)
    }

    /// The vertex nearest a unit-sphere position, by maximum dot product. Because
    /// a room's ancestor-corner positions are byte-identical to mesh vertices,
    /// this returns that exact vertex (self-dot = 1.0 wins).
    /// type-audit: pending(wave-1)
    pub fn nearest_to_position(&self, geo: &Geosphere, pos: [f64; 3]) -> Vertex {
        let latitude = math::asin(pos[2]).to_degrees();
        let longitude = math::atan2(pos[1], pos[0]).to_degrees();
        // cos(lat) = sqrt(1 - z²) on the unit sphere — no transcendental.
        let cos_lat = (1.0 - pos[2] * pos[2]).max(0.0).sqrt();
        self.scan_at(geo, pos, latitude, longitude, cos_lat)
    }

    /// Shared windowed scan given the query's target vector, latitude,
    /// longitude (all degrees), and cos(lat).
    fn scan_at(
        &self,
        geo: &Geosphere,
        target: [f64; 3],
        latitude: f64,
        longitude: f64,
        cos_lat: f64,
    ) -> Vertex {
        let band = lat_band(latitude);
        let lo = band.saturating_sub(1);
        let hi = (band + 1).min(BAND_COUNT - 1);
        let cl = cos_lat.abs().max(1e-6);
        let cover_rad = self.cover_deg.to_radians();
        let k = (self.cover_deg / cl / LON_DEGREES).ceil() as usize + 1;
        // Near the poles the `cover / cos(lat)` linearization underestimates the
        // longitude reach (a vertex 180° away in longitude is only a few degrees
        // away angularly), so it would wrongly exclude the true nearest. Guard:
        // when the query is within ~2× the coverage radius of a pole, scan the
        // full ring — exactly the original band-only scan. Elsewhere the
        // linearization is accurate and the window is a safe superset.
        let full_ring = cl < 2.0 * cover_rad || 2 * k + 1 >= LON_BUCKETS;
        let ql = lon_bucket(longitude);
        // Visit only the in-window buckets (the speed). The winner is the max
        // dot; on an exact tie, the lexicographically smallest `(band, Vertex)`
        // — which reproduces the full band scan's first-in-scan-order winner
        // (bands lo→hi, ascending Vertex within band) regardless of the order
        // buckets are visited here. Every max/tie vertex is in-window by the
        // coverage bound, so restricting to the window cannot change the result.
        let mut best = Vertex(0);
        let mut best_dot = f64::NEG_INFINITY;
        let mut best_key = (usize::MAX, u32::MAX);
        macro_rules! scan_bucket {
            ($b:expr, $l:expr) => {
                for &vertex in &self.grid[$b * LON_BUCKETS + $l] {
                    let d = dot3(geo.position(vertex), target);
                    let key = ($b, vertex.0);
                    // Exact-equality tie detection is intentional: it selects
                    // the same vertex the band scan's strict-`>` first hit did.
                    #[allow(clippy::float_cmp)]
                    let tie = d == best_dot;
                    if d > best_dot || (tie && key < best_key) {
                        best_dot = d;
                        best = vertex;
                        best_key = key;
                    }
                }
            };
        }
        for b in lo..=hi {
            if full_ring {
                for l in 0..LON_BUCKETS {
                    scan_bucket!(b, l);
                }
            } else {
                for dl in 0..=2 * k {
                    scan_bucket!(b, (ql + dl + LON_BUCKETS - k) % LON_BUCKETS);
                }
            }
        }
        best
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The reference: the earlier full-band scan (band ± 1, ALL longitudes,
    /// ascending Vertex within each band, first-in-scan-order tie-break). The
    /// A1 grid must return this exact vertex for every query.
    fn band_scan(
        geo: &Geosphere,
        buckets: &[Vec<Vertex>],
        latitude: f64,
        longitude: f64,
    ) -> Vertex {
        let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
        let target = [
            crate::math::cos(lat) * crate::math::cos(lon),
            crate::math::cos(lat) * crate::math::sin(lon),
            crate::math::sin(lat),
        ];
        let band = lat_band(latitude);
        let lo = band.saturating_sub(1);
        let hi = (band + 1).min(BAND_COUNT - 1);
        let mut best = Vertex(0);
        let mut best_dot = f64::NEG_INFINITY;
        for bucket in &buckets[lo..=hi] {
            for &vertex in bucket {
                let d = dot3(geo.position(vertex), target);
                if d > best_dot {
                    best_dot = d;
                    best = vertex;
                }
            }
        }
        best
    }

    #[test]
    fn a1_grid_matches_the_full_band_scan_over_a_dense_sweep() {
        // Every level the mesh is built at (2–6 across renders, room, scene,
        // the climate provider, and the census). The equality assertion IS the
        // coverage proof: an under-covering window would return a different
        // vertex than the band scan and fail here. Level 2 is the coarse case
        // where the covering radius is largest and the window saturates.
        for level in [2u32, 3, 4, 5, 6] {
            let geo = Geosphere::new(level);
            let index = NearestVertexIndex::new(&geo);
            // Reference latitude buckets, built once (ascending Vertex).
            let mut buckets = vec![Vec::new(); BAND_COUNT];
            for c in geo.vertices() {
                buckets[lat_band(geo.coord(c).latitude)].push(c);
            }
            // A render-like equirectangular sweep of query points.
            let (nlat, nlon) = (90usize, 180usize);
            for iy in 0..nlat {
                let lat = 90.0 - (iy as f64 + 0.5) * 180.0 / nlat as f64;
                for ix in 0..nlon {
                    let lon = -180.0 + (ix as f64 + 0.5) * 360.0 / nlon as f64;
                    assert_eq!(
                        index.nearest(&geo, lat, lon),
                        band_scan(&geo, &buckets, lat, lon),
                        "A1 grid disagreed at level {level}, lat {lat:.3}, lon {lon:.3}"
                    );
                }
            }
            // Every vertex center must resolve to its own vertex (self-dot = 1).
            for c in geo.vertices() {
                assert_eq!(
                    index.nearest_to_position(&geo, geo.position(c)),
                    c,
                    "A1 grid: vertex {c:?} did not resolve to itself at level {level}"
                );
            }
        }
    }

    /// Position-target band scan — the reference for `nearest_to_position`
    /// (raw unit vector as the target, not lat/lon-derived).
    fn band_scan_pos(geo: &Geosphere, buckets: &[Vec<Vertex>], pos: [f64; 3]) -> Vertex {
        let band = lat_band(math::asin(pos[2].clamp(-1.0, 1.0)).to_degrees());
        let lo = band.saturating_sub(1);
        let hi = (band + 1).min(BAND_COUNT - 1);
        let mut best = Vertex(0);
        let mut best_dot = f64::NEG_INFINITY;
        for bucket in &buckets[lo..=hi] {
            for &vertex in bucket {
                let d = dot3(geo.position(vertex), pos);
                if d > best_dot {
                    best_dot = d;
                    best = vertex;
                }
            }
        }
        best
    }

    #[test]
    fn a1_grid_tie_break_matches_band_scan_at_edge_midpoints() {
        // Edge midpoints are exactly equidistant (equal dot) from their two
        // endpoint vertices — they force the dot-product tie the dense equirect
        // sweep never hits, exercising the (band, Vertex) tie-key. The A1 grid
        // must pick the same vertex the band scan's first-in-scan-order does.
        for level in [2u32, 3, 4, 5, 6] {
            let geo = Geosphere::new(level);
            let index = NearestVertexIndex::new(&geo);
            let mut buckets = vec![Vec::new(); BAND_COUNT];
            for c in geo.vertices() {
                buckets[lat_band(geo.coord(c).latitude)].push(c);
            }
            for c in geo.vertices() {
                for &n in geo.neighbors(c) {
                    let mid = slerp_mid(geo.position(c), geo.position(n));
                    assert_eq!(
                        index.nearest_to_position(&geo, mid),
                        band_scan_pos(&geo, &buckets, mid),
                        "tie-break mismatch at level {level}, edge {c:?}-{n:?}"
                    );
                }
            }
        }
    }

    #[test]
    fn coord_cache_bit_equals_recomputation_at_every_vertex() {
        // The cached `coord` must be bit-for-bit identical to recomputing
        // asin(z)/atan2(y, x) from the stored position — the byte-identity
        // contract that lets the cache replace the per-call transcendentals
        // (The Lookup). Compared on raw bits so a last-ULP drift fails loudly.
        for level in 0..=5 {
            let geo = Geosphere::new(level);
            for id in geo.vertices() {
                let [x, y, z] = geo.position(id);
                let expected = GeoCoord {
                    latitude: crate::math::asin(z).to_degrees(),
                    longitude: crate::math::atan2(y, x).to_degrees(),
                };
                let got = geo.coord(id);
                assert_eq!(
                    got.latitude.to_bits(),
                    expected.latitude.to_bits(),
                    "latitude drift at level {level}, vertex {id:?}"
                );
                assert_eq!(
                    got.longitude.to_bits(),
                    expected.longitude.to_bits(),
                    "longitude drift at level {level}, vertex {id:?}"
                );
            }
        }
    }

    #[test]
    fn base_icosahedron_has_twelve_unit_vertices() {
        let geo = Geosphere::new(0);
        assert_eq!(geo.depth(), 0);
        assert_eq!(geo.vertex_count(), 12);
        for id in geo.vertices() {
            let [x, y, z] = geo.position(id);
            let len = (x * x + y * y + z * z).sqrt();
            assert!(
                (len - 1.0).abs() < 1e-12,
                "vertex {id:?} not unit-length: {len}"
            );
        }
    }

    #[test]
    fn subdivision_yields_the_icosphere_vertex_counts() {
        // 10 * 4^L + 2
        assert_eq!(Geosphere::new(0).vertex_count(), 12);
        assert_eq!(Geosphere::new(1).vertex_count(), 42);
        assert_eq!(Geosphere::new(2).vertex_count(), 162);
        assert_eq!(Geosphere::new(3).vertex_count(), 642);
    }

    #[test]
    fn subdivided_vertices_are_all_unit_length() {
        let geo = Geosphere::new(3);
        for id in geo.vertices() {
            let [x, y, z] = geo.position(id);
            let len = (x * x + y * y + z * z).sqrt();
            assert!(
                (len - 1.0).abs() < 1e-12,
                "vertex {id:?} not unit-length: {len}"
            );
        }
    }

    #[test]
    fn hops_between_is_bounded_bfs() {
        let geo = Geosphere::new(5);
        let c = geo.vertices().next().unwrap();
        let n = geo.neighbors(c)[0];
        assert_eq!(geo.hops_between(c, c, 3), Some(0));
        assert_eq!(geo.hops_between(c, n, 3), Some(1));
        assert_eq!(
            geo.hops_between(c, n, 0),
            None,
            "neighbour is beyond a 0-hop bound"
        );
        let two = *geo
            .neighbors(n)
            .iter()
            .find(|&&x| x != c && !geo.neighbors(c).contains(&x))
            .expect("a 2-hop vertex exists");
        assert_eq!(geo.hops_between(c, two, 3), Some(2));
    }

    #[test]
    fn adjacency_is_mutual_and_has_the_right_valence() {
        let geo = Geosphere::new(3);
        let mut fives = 0usize;
        let mut sixes = 0usize;
        for id in geo.vertices() {
            let ns = geo.neighbors(id);
            match ns.len() {
                5 => fives += 1,
                6 => sixes += 1,
                other => panic!("vertex {id:?} has {other} neighbors (expected 5 or 6)"),
            }
            // sorted ascending, no self, no duplicates
            let mut sorted = ns.to_vec();
            sorted.sort();
            sorted.dedup();
            assert_eq!(
                sorted.as_slice(),
                ns,
                "neighbors of {id:?} not sorted/deduped"
            );
            assert!(
                !ns.contains(&id),
                "vertex {id:?} lists itself as a neighbor"
            );
            // mutual: each neighbor lists id back
            for &n in ns {
                assert!(
                    geo.neighbors(n).contains(&id),
                    "{n:?} not mutual with {id:?}"
                );
            }
        }
        assert_eq!(fives, 12, "exactly twelve pentagonal vertices expected");
        assert_eq!(sixes, geo.vertex_count() - 12);
    }

    #[test]
    fn coordinates_are_in_range_and_convert_correctly() {
        let geo = Geosphere::new(3);
        for id in geo.vertices() {
            let c = geo.coord(id);
            assert!(
                c.latitude >= -90.0 && c.latitude <= 90.0,
                "lat out of range: {}",
                c.latitude
            );
            assert!(
                c.longitude > -180.0 && c.longitude <= 180.0,
                "lon out of range: {}",
                c.longitude
            );
        }
    }

    #[test]
    fn coordinate_conversion_matches_known_directions() {
        // A vertex whose position is the +z pole would read latitude +90; test the
        // conversion directly through a constructed sphere is awkward, so assert
        // the mapping via the closest vertex to +z on a fine sphere instead.
        let geo = Geosphere::new(4);
        let north = geo
            .vertices()
            .max_by(|a, b| geo.position(*a)[2].total_cmp(&geo.position(*b)[2]))
            .unwrap();
        let c = geo.coord(north);
        // The most-northern vertex sits high but not exactly at the pole; assert it
        // is in the northern hemisphere and its longitude is well-defined.
        assert!(
            c.latitude > 60.0,
            "northernmost vertex latitude {} too low",
            c.latitude
        );
        assert!(c.longitude > -180.0 && c.longitude <= 180.0);
    }

    #[test]
    fn vertexmap_covers_every_vertex_and_indexes_by_id() {
        let geo = Geosphere::new(2);
        let doubled = VertexMap::from_fn(&geo, |id| id.0 * 2);
        assert_eq!(doubled.len(), geo.vertex_count());
        assert!(!doubled.is_empty());
        for id in geo.vertices() {
            assert_eq!(*doubled.get(id), id.0 * 2);
        }
        let total: u32 = doubled.iter().map(|(_, v)| *v).sum();
        let expected: u32 = geo.vertices().map(|id| id.0 * 2).sum();
        assert_eq!(total, expected);
    }

    #[test]
    fn geosphere_is_deterministic() {
        // Same level -> byte-identical mesh (positions, neighbors, coords).
        let a = Geosphere::new(4);
        let b = Geosphere::new(4);
        assert_eq!(a.vertex_count(), b.vertex_count());
        for id in a.vertices() {
            assert_eq!(a.position(id), b.position(id), "position drift at {id:?}");
            assert_eq!(a.neighbors(id), b.neighbors(id), "neighbor drift at {id:?}");
            assert_eq!(a.coord(id), b.coord(id), "coord drift at {id:?}");
        }
    }

    #[test]
    fn nearest_vertex_index_agrees_with_brute_force() {
        let geo = Geosphere::new(4);
        let index = NearestVertexIndex::new(&geo);
        for (latitude, longitude) in [(0.0, 0.0), (89.0, 10.0), (-89.0, -170.0), (45.5, 179.5)] {
            let banded = index.nearest(&geo, latitude, longitude);
            let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
            let target = [
                super::math::cos(lat) * super::math::cos(lon),
                super::math::cos(lat) * super::math::sin(lon),
                super::math::sin(lat),
            ];
            let mut best = Vertex(0);
            let mut best_dot = f64::NEG_INFINITY;
            for vertex in geo.vertices() {
                let d = super::dot3(geo.position(vertex), target);
                if d > best_dot {
                    best_dot = d;
                    best = vertex;
                }
            }
            assert_eq!(banded, best, "at ({latitude}, {longitude})");
        }
    }
}
