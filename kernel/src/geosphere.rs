//! The Geosphere: a deterministic icosphere region graph over the unit
//! sphere. Cells are the vertices of a subdivided icosahedron; adjacency is
//! the triangulation's edges. It is seed-independent (fully determined by its
//! subdivision level) and never serialized — recomputed on demand, like a
//! `Field`. This is the spatial substrate the terrain and climate domains
//! compute over.

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
    /// Triangular faces as cell-index triples (used to derive adjacency).
    #[allow(dead_code)]
    faces: Vec<[u32; 3]>,
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

impl Geosphere {
    /// Build an icosphere subdivided `level` times. (Task 1 handles the base;
    /// Task 2 adds subdivision for `level > 0`.)
    pub fn new(level: u32) -> Geosphere {
        let (positions, faces) = base_icosahedron();
        Geosphere {
            level,
            positions,
            faces,
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
}
