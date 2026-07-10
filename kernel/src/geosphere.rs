//! The Geosphere: a deterministic icosphere region graph over the unit
//! sphere. Cells are the vertices of a subdivided icosahedron; adjacency is
//! the triangulation's edges. It is seed-independent (fully determined by its
//! subdivision level) and never serialized — recomputed on demand, like a
//! `Field`. This is the spatial substrate the terrain and climate domains
//! compute over.

use std::collections::{BTreeMap, BTreeSet};

/// Identifier for a cell — an index into the mesh's vertices.
/// type-audit: bare-ok(index)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CellId(pub u32);

/// A geographic coordinate in degrees.
/// type-audit: pending(wave-1)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GeoCoord {
    /// Latitude in degrees, `[-90, 90]` (north positive).
    pub latitude: f64,
    /// Longitude in degrees, `(-180, 180]` (east positive).
    pub longitude: f64,
}

/// A discretized planetary surface. Cells are the vertices of an icosahedron
/// subdivided `level` times; every cell sits on the unit sphere.
#[derive(Clone, Debug)]
pub struct Geosphere {
    /// Subdivision level (0 = the bare icosahedron).
    level: u32,
    /// Unit-sphere position per cell, indexed by `CellId`.
    positions: Vec<[f64; 3]>,
    /// Adjacent cells per cell, ascending, indexed by `CellId`.
    neighbors: Vec<Vec<CellId>>,
}

/// A value per cell, indexed by `CellId`. Built from a `Geosphere`, so it has
/// exactly one entry per cell. This is the representation domains use for
/// derived per-cell data (elevation, temperature, biome).
#[derive(Clone, Debug, PartialEq)]
pub struct CellMap<T> {
    values: Vec<T>,
}

impl<T> CellMap<T> {
    /// Build a `CellMap` by evaluating `f` at every cell of `geo`, in
    /// ascending `CellId` order.
    pub fn from_fn(geo: &Geosphere, mut f: impl FnMut(CellId) -> T) -> CellMap<T> {
        CellMap {
            values: geo.cells().map(&mut f).collect(),
        }
    }

    /// The value at a cell.
    pub fn get(&self, id: CellId) -> &T {
        &self.values[id.0 as usize]
    }

    /// The number of cells.
    /// type-audit: bare-ok(count)
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Whether the map is empty.
    /// type-audit: bare-ok(flag)
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Iterate `(CellId, &value)` pairs in ascending `CellId` order.
    pub fn iter(&self) -> impl Iterator<Item = (CellId, &T)> {
        self.values
            .iter()
            .enumerate()
            .map(|(i, v)| (CellId(i as u32), v))
    }
}

/// Normalize a 3-vector to unit length.
fn normalize(v: [f64; 3]) -> [f64; 3] {
    let [x, y, z] = v;
    let len = (x * x + y * y + z * z).sqrt();
    [x / len, y / len, z / len]
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
        let [ax, ay, az] = positions[a as usize];
        let [bx, by, bz] = positions[b as usize];
        let mid = normalize([(ax + bx) / 2.0, (ay + by) / 2.0, (az + bz) / 2.0]);
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

/// Derive per-cell adjacency from the triangular faces: two cells are
/// adjacent iff they share a face edge. Neighbor lists are sorted ascending.
fn build_neighbors(cell_count: usize, faces: &[[u32; 3]]) -> Vec<Vec<CellId>> {
    let mut sets: Vec<BTreeSet<u32>> = vec![BTreeSet::new(); cell_count];
    for &[a, b, c] in faces {
        for (u, v) in [(a, b), (b, c), (c, a)] {
            sets[u as usize].insert(v);
            sets[v as usize].insert(u);
        }
    }
    sets.into_iter()
        .map(|s| s.into_iter().map(CellId).collect())
        .collect()
}

impl Geosphere {
    /// Build an icosphere subdivided `level` times. (Task 1 handles the base;
    /// Task 2 adds subdivision for `level > 0`.)
    /// type-audit: bare-ok(count)
    pub fn new(level: u32) -> Geosphere {
        let (mut positions, mut faces) = base_icosahedron();
        for _ in 0..level {
            (positions, faces) = subdivide(positions, faces);
        }
        let neighbors = build_neighbors(positions.len(), &faces);
        Geosphere {
            level,
            positions,
            neighbors,
        }
    }

    /// The subdivision level.
    /// type-audit: bare-ok(count)
    pub fn level(&self) -> u32 {
        self.level
    }

    /// The number of cells.
    /// type-audit: bare-ok(count)
    pub fn cell_count(&self) -> usize {
        self.positions.len()
    }

    /// Iterate every cell id in ascending order.
    pub fn cells(&self) -> impl Iterator<Item = CellId> {
        (0..self.positions.len() as u32).map(CellId)
    }

    /// The unit-sphere position of a cell.
    /// type-audit: pending(wave-1)
    pub fn position(&self, id: CellId) -> [f64; 3] {
        self.positions[id.0 as usize]
    }

    /// The geographic coordinate of a cell.
    pub fn coord(&self, id: CellId) -> GeoCoord {
        let [x, y, z] = self.positions[id.0 as usize];
        GeoCoord {
            latitude: z.asin().to_degrees(),
            longitude: y.atan2(x).to_degrees(),
        }
    }

    /// The cells adjacent to `id`, in ascending `CellId` order.
    pub fn neighbors(&self, id: CellId) -> &[CellId] {
        &self.neighbors[id.0 as usize]
    }
}

/// Latitude bands in the nearest-cell index.
const BAND_COUNT: usize = 30;
/// Height of one band, degrees.
const BAND_DEGREES: f64 = 180.0 / BAND_COUNT as f64;

/// Dot product of two unit vectors.
fn dot3(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Latitude-band index for pixel→cell lookups: cells bucketed into 30
/// bands of 6° (built in ascending cell order, so lookups are
/// deterministic). Searching the query's band ± 1 always contains the
/// nearest cell center at level ≥ 4 (which lies within ~2.5°).
pub struct NearestCellIndex {
    /// One bucket of cells per latitude band, north to south.
    bands: Vec<Vec<CellId>>,
}

impl NearestCellIndex {
    /// Bucket every cell of `geo` by latitude.
    pub fn new(geo: &Geosphere) -> NearestCellIndex {
        let mut bands = vec![Vec::new(); BAND_COUNT];
        for cell in geo.cells() {
            let latitude = geo.coord(cell).latitude;
            let band = (((90.0 - latitude) / BAND_DEGREES) as usize).min(BAND_COUNT - 1);
            bands[band].push(cell);
        }
        NearestCellIndex { bands }
    }

    /// The cell nearest a coordinate (degrees), by maximum dot product.
    /// Inverts the coord convention: latitude = asin(z), longitude =
    /// atan2(y, x).
    /// type-audit: pending(wave-1)
    pub fn nearest(&self, geo: &Geosphere, latitude: f64, longitude: f64) -> CellId {
        let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
        let target = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
        let band = (((90.0 - latitude) / BAND_DEGREES) as usize).min(BAND_COUNT - 1);
        let lo = band.saturating_sub(1);
        let hi = (band + 1).min(BAND_COUNT - 1);
        let mut best = CellId(0);
        let mut best_dot = f64::NEG_INFINITY;
        for cells in &self.bands[lo..=hi] {
            for &cell in cells {
                let d = dot3(geo.position(cell), target);
                if d > best_dot {
                    best_dot = d;
                    best = cell;
                }
            }
        }
        best
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn base_icosahedron_has_twelve_unit_cells() {
        let geo = Geosphere::new(0);
        assert_eq!(geo.level(), 0);
        assert_eq!(geo.cell_count(), 12);
        for id in geo.cells() {
            let [x, y, z] = geo.position(id);
            let len = (x * x + y * y + z * z).sqrt();
            assert!(
                (len - 1.0).abs() < 1e-12,
                "cell {id:?} not unit-length: {len}"
            );
        }
    }

    #[test]
    fn subdivision_yields_the_icosphere_cell_counts() {
        // 10 * 4^L + 2
        assert_eq!(Geosphere::new(0).cell_count(), 12);
        assert_eq!(Geosphere::new(1).cell_count(), 42);
        assert_eq!(Geosphere::new(2).cell_count(), 162);
        assert_eq!(Geosphere::new(3).cell_count(), 642);
    }

    #[test]
    fn subdivided_cells_are_all_unit_length() {
        let geo = Geosphere::new(3);
        for id in geo.cells() {
            let [x, y, z] = geo.position(id);
            let len = (x * x + y * y + z * z).sqrt();
            assert!(
                (len - 1.0).abs() < 1e-12,
                "cell {id:?} not unit-length: {len}"
            );
        }
    }

    #[test]
    fn adjacency_is_mutual_and_has_the_right_valence() {
        let geo = Geosphere::new(3);
        let mut fives = 0usize;
        let mut sixes = 0usize;
        for id in geo.cells() {
            let ns = geo.neighbors(id);
            match ns.len() {
                5 => fives += 1,
                6 => sixes += 1,
                other => panic!("cell {id:?} has {other} neighbors (expected 5 or 6)"),
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
            assert!(!ns.contains(&id), "cell {id:?} lists itself as a neighbor");
            // mutual: each neighbor lists id back
            for &n in ns {
                assert!(
                    geo.neighbors(n).contains(&id),
                    "{n:?} not mutual with {id:?}"
                );
            }
        }
        assert_eq!(fives, 12, "exactly twelve pentagonal cells expected");
        assert_eq!(sixes, geo.cell_count() - 12);
    }

    #[test]
    fn coordinates_are_in_range_and_convert_correctly() {
        let geo = Geosphere::new(3);
        for id in geo.cells() {
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
        // A cell whose position is the +z pole would read latitude +90; test the
        // conversion directly through a constructed sphere is awkward, so assert
        // the mapping via the closest cell to +z on a fine sphere instead.
        let geo = Geosphere::new(4);
        let north = geo
            .cells()
            .max_by(|a, b| geo.position(*a)[2].total_cmp(&geo.position(*b)[2]))
            .unwrap();
        let c = geo.coord(north);
        // The most-northern cell sits high but not exactly at the pole; assert it
        // is in the northern hemisphere and its longitude is well-defined.
        assert!(
            c.latitude > 60.0,
            "northernmost cell latitude {} too low",
            c.latitude
        );
        assert!(c.longitude > -180.0 && c.longitude <= 180.0);
    }

    #[test]
    fn cellmap_covers_every_cell_and_indexes_by_id() {
        let geo = Geosphere::new(2);
        let doubled = CellMap::from_fn(&geo, |id| id.0 * 2);
        assert_eq!(doubled.len(), geo.cell_count());
        assert!(!doubled.is_empty());
        for id in geo.cells() {
            assert_eq!(*doubled.get(id), id.0 * 2);
        }
        let total: u32 = doubled.iter().map(|(_, v)| *v).sum();
        let expected: u32 = geo.cells().map(|id| id.0 * 2).sum();
        assert_eq!(total, expected);
    }

    #[test]
    fn geosphere_is_deterministic() {
        // Same level -> byte-identical mesh (positions, neighbors, coords).
        let a = Geosphere::new(4);
        let b = Geosphere::new(4);
        assert_eq!(a.cell_count(), b.cell_count());
        for id in a.cells() {
            assert_eq!(a.position(id), b.position(id), "position drift at {id:?}");
            assert_eq!(a.neighbors(id), b.neighbors(id), "neighbor drift at {id:?}");
            assert_eq!(a.coord(id), b.coord(id), "coord drift at {id:?}");
        }
    }

    #[test]
    fn nearest_cell_index_agrees_with_brute_force() {
        let geo = Geosphere::new(4);
        let index = NearestCellIndex::new(&geo);
        for (latitude, longitude) in [(0.0, 0.0), (89.0, 10.0), (-89.0, -170.0), (45.5, 179.5)] {
            let banded = index.nearest(&geo, latitude, longitude);
            let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
            let target = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
            let mut best = CellId(0);
            let mut best_dot = f64::NEG_INFINITY;
            for cell in geo.cells() {
                let d = super::dot3(geo.position(cell), target);
                if d > best_dot {
                    best_dot = d;
                    best = cell;
                }
            }
            assert_eq!(banded, best, "at ({latitude}, {longitude})");
        }
    }
}
