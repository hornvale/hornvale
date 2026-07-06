//! The Geosphere: a deterministic icosphere region graph over the unit
//! sphere. Cells are the vertices of a subdivided icosahedron; adjacency is
//! the triangulation's edges. It is seed-independent (fully determined by its
//! subdivision level) and never serialized — recomputed on demand, like a
//! `Field`. This is the spatial substrate the terrain and climate domains
//! compute over.

use std::collections::{BTreeMap, BTreeSet};

/// Identifier for a cell — an index into the mesh's vertices.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CellId(pub u32);

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
    pub fn level(&self) -> u32 {
        self.level
    }

    /// The number of cells.
    pub fn cell_count(&self) -> usize {
        self.positions.len()
    }

    /// Iterate every cell id in ascending order.
    pub fn cells(&self) -> impl Iterator<Item = CellId> {
        (0..self.positions.len() as u32).map(CellId)
    }

    /// The unit-sphere position of a cell.
    pub fn position(&self, id: CellId) -> [f64; 3] {
        self.positions[id.0 as usize]
    }

    /// The cells adjacent to `id`, in ascending `CellId` order.
    pub fn neighbors(&self, id: CellId) -> &[CellId] {
        &self.neighbors[id.0 as usize]
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
}
