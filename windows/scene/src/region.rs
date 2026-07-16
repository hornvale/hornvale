//! `scene/tiles-region/v1`: one cube-sphere quadtree tile's footprint, sampled
//! at higher on-tile density than the global lattice. Continuous layers are
//! barycentrically interpolated between geosphere cells (a smooth surface, not
//! new physics — the ~110 km cell spacing is the resolution floor); discrete
//! layers stay nearest-cell. The projection here is the normative one, shared
//! byte-for-byte with the orrery's `cubeSphere.ts` and the reference page.

use crate::SceneError;
use hornvale_kernel::{CellId, Geosphere, NearestCellIndex};

/// Deepest addressable quadtree level (the client clamps its own to ~18; this
/// bound is a generous superset). Beyond the cell floor a tile is honest but
/// carries no detail the coarser tile lacked.
/// type-audit: bare-ok(count)
pub const MAX_REGION_LEVEL: u32 = 24;
/// Largest legal `samples` (quads per edge); the node grid is `(N+1)²`.
/// type-audit: bare-ok(count)
pub const MAX_REGION_SAMPLES: u32 = 256;

/// The six cube-face bases `(n, u, v)`: a face point is `normalize(n + a·u + b·v)`
/// for `(a, b) ∈ [-1, 1]²`. Identical to the orrery's `FACES`.
const FACES: [[[f64; 3]; 3]; 6] = [
    [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]],
    [[-1.0, 0.0, 0.0], [0.0, -1.0, 0.0], [0.0, 0.0, 1.0]],
    [[0.0, 1.0, 0.0], [-1.0, 0.0, 0.0], [0.0, 0.0, 1.0]],
    [[0.0, -1.0, 0.0], [1.0, 0.0, 0.0], [0.0, 0.0, 1.0]],
    [[0.0, 0.0, 1.0], [1.0, 0.0, 0.0], [0.0, 1.0, 0.0]],
    [[0.0, 0.0, -1.0], [-1.0, 0.0, 0.0], [0.0, 1.0, 0.0]],
];

/// A face parameter: `-1 + 2·(index + offset)/2^level`, a dyadic rational in
/// [-1, 1]. `offset ∈ [0, 1]` walks a tile's edge.
fn param(index: u32, offset: f64, level: u32) -> f64 {
    -1.0 + 2.0 * (f64::from(index) + offset) / (1u64 << level) as f64
}

/// The unit vector for face parameters `(a, b)`: `normalize(n + a·u + b·v)`.
fn face_unit(face: usize, a: f64, b: f64) -> [f64; 3] {
    let [n, u, v] = FACES[face];
    let x = n[0] + a * u[0] + b * v[0];
    let y = n[1] + a * u[1] + b * v[1];
    let z = n[2] + a * u[2] + b * v[2];
    let len = (x * x + y * y + z * z).sqrt();
    [x / len, y / len, z / len]
}

/// One regional tile address (scene-protocol: The Region §3.1).
/// type-audit: bare-ok(index: face), bare-ok(count: level), bare-ok(index: ix), bare-ok(index: iy), bare-ok(count: samples)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RegionAddr {
    /// One of the six cube faces (0..=5).
    pub face: u32,
    /// Quadtree depth; the face is `2^level × 2^level` tiles.
    pub level: u32,
    /// Tile column on the face (0..2^level).
    pub ix: u32,
    /// Tile row on the face (0..2^level).
    pub iy: u32,
    /// Quads per tile edge N; the node grid is `(N+1)²`.
    pub samples: u32,
}

impl RegionAddr {
    /// Validate the address against the fixed contract bounds, loudly.
    pub fn validate(&self) -> Result<(), SceneError> {
        if self.face > 5 {
            return Err(SceneError::RegionFaceOutOfRange(self.face));
        }
        if self.level > MAX_REGION_LEVEL {
            return Err(SceneError::RegionLevelOutOfRange(self.level));
        }
        let bound = 1u64 << self.level;
        if u64::from(self.ix) >= bound || u64::from(self.iy) >= bound {
            return Err(SceneError::RegionTileOutOfRange {
                ix: self.ix,
                iy: self.iy,
                level: self.level,
            });
        }
        if self.samples == 0 || self.samples > MAX_REGION_SAMPLES {
            return Err(SceneError::RegionSamplesOutOfRange(self.samples));
        }
        Ok(())
    }

    /// The `(N+1)²` node unit vectors, row-major (`i = row·(N+1) + col`, row is
    /// the `iy`/`b` direction). Assumes a validated address.
    /// type-audit: bare-ok(render-internal: return)
    pub fn node_units(&self) -> Vec<[f64; 3]> {
        let n = self.samples;
        let face = self.face as usize;
        let mut units = Vec::with_capacity(((n + 1) * (n + 1)) as usize);
        for row in 0..=n {
            let b = param(self.iy, f64::from(row) / f64::from(n), self.level);
            for col in 0..=n {
                let a = param(self.ix, f64::from(col) / f64::from(n), self.level);
                units.push(face_unit(face, a, b));
            }
        }
        units
    }
}

/// Planar barycentric of `p` in triangle `(a, b, c)` (Ericson, *Real-Time
/// Collision Detection*). Returns `(u, v, w)` with `u + v + w = 1`; `u` is the
/// weight of `a`. Exact `(1, 0, 0)` when `p == a`.
fn barycentric(p: [f64; 3], a: [f64; 3], b: [f64; 3], c: [f64; 3]) -> (f64, f64, f64) {
    let sub = |x: [f64; 3], y: [f64; 3]| [x[0] - y[0], x[1] - y[1], x[2] - y[2]];
    let dot = |x: [f64; 3], y: [f64; 3]| x[0] * y[0] + x[1] * y[1] + x[2] * y[2];
    let (v0, v1, v2) = (sub(b, a), sub(c, a), sub(p, a));
    let d00 = dot(v0, v0);
    let d01 = dot(v0, v1);
    let d11 = dot(v1, v1);
    let d20 = dot(v2, v0);
    let d21 = dot(v2, v1);
    let denom = d00 * d11 - d01 * d01;
    if denom == 0.0 {
        return (1.0, 0.0, 0.0); // degenerate triangle: fall back to `a`
    }
    let v = (d11 * d20 - d01 * d21) / denom;
    let w = (d00 * d21 - d01 * d20) / denom;
    (1.0 - v - w, v, w)
}

/// The three geosphere cells whose triangle contains `s`, with barycentric
/// weights in [0,1] summing to 1. The triangle is the nearest cell `c` plus a
/// pair of its neighbours that are themselves adjacent (a fan triangle around
/// `c`); the one containing `s` wins. Transcendental-free (dot products only),
/// so cross-platform byte-identical. Exact at a cell centre. Degrades to
/// nearest-cell `(c, 1.0)` if no fan triangle contains `s` (a measure-zero
/// safety net, not a normal path).
fn triangle_weights(geo: &Geosphere, index: &NearestCellIndex, s: [f64; 3]) -> [(CellId, f64); 3] {
    let c = index.nearest_to_position(geo, s);
    let pc = geo.position(c);
    let neigh = geo.neighbors(c);
    let mut best: Option<([(CellId, f64); 3], f64)> = None;
    for (i, &a) in neigh.iter().enumerate() {
        for &b in &neigh[i + 1..] {
            // Only adjacent neighbour pairs form a real fan triangle.
            if !geo.neighbors(a).contains(&b) {
                continue;
            }
            let (wc, wa, wb) = barycentric(s, pc, geo.position(a), geo.position(b));
            let min = wc.min(wa).min(wb);
            // Pick the triangle whose most-negative weight is closest to zero;
            // a containing triangle has all weights >= 0.
            if best.as_ref().map(|(_, m)| min > *m).unwrap_or(true) {
                best = Some(([(c, wc), (a, wa), (b, wb)], min));
            }
        }
    }
    match best {
        Some((mut w, min)) if min >= -1e-9 => {
            // Clamp tiny negatives and renormalise so weights are a clean
            // partition of unity.
            let mut sum = 0.0;
            for (_, x) in w.iter_mut() {
                if *x < 0.0 {
                    *x = 0.0;
                }
                sum += *x;
            }
            if sum > 0.0 {
                for (_, x) in w.iter_mut() {
                    *x /= sum;
                }
            }
            w
        }
        _ => [(c, 1.0), (c, 0.0), (c, 0.0)],
    }
}

/// Barycentrically interpolate a per-cell scalar at `s`.
// Unused outside tests until a later Region task wires continuous-layer
// sampling through it; the allow keeps `triangle_weights`/`barycentric`
// (which it calls) live for clippy too.
#[allow(dead_code)]
fn interp(
    geo: &Geosphere,
    index: &NearestCellIndex,
    s: [f64; 3],
    value: impl Fn(CellId) -> f64,
) -> f64 {
    triangle_weights(geo, index, s)
        .iter()
        .map(|(c, w)| w * value(*c))
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn addr(face: u32, level: u32, ix: u32, iy: u32, samples: u32) -> RegionAddr {
        RegionAddr {
            face,
            level,
            ix,
            iy,
            samples,
        }
    }

    #[test]
    fn node_grid_is_sized_and_deterministic() {
        let a = addr(0, 3, 4, 4, 16);
        a.validate().unwrap();
        let u1 = a.node_units();
        let u2 = a.node_units();
        assert_eq!(u1.len(), 17 * 17);
        assert_eq!(u1, u2);
        assert!(
            u1.iter()
                .all(|p| (p[0] * p[0] + p[1] * p[1] + p[2] * p[2] - 1.0).abs() < 1e-12)
        );
    }

    #[test]
    fn level_zero_center_node_points_along_face_normal() {
        // level 0, samples 2 → the center node (row=1,col=1) is (a,b)=(0,0) → the face normal.
        let a = addr(0, 0, 0, 0, 2);
        let units = a.node_units();
        // Row-major index row·(N+1)+col spelled out literally (N=2, so N+1=3);
        // the `1 *` is intentional documentation of the formula, not dead code.
        #[allow(clippy::identity_op)]
        let center = units[1 * 3 + 1];
        assert!(
            (center[0] - 1.0).abs() < 1e-12,
            "face 0 normal is +x: {center:?}"
        );
    }

    #[test]
    fn validation_is_loud() {
        assert!(matches!(
            addr(6, 0, 0, 0, 1).validate(),
            Err(SceneError::RegionFaceOutOfRange(6))
        ));
        assert!(matches!(
            addr(0, 25, 0, 0, 1).validate(),
            Err(SceneError::RegionLevelOutOfRange(25))
        ));
        assert!(matches!(
            addr(0, 2, 4, 0, 1).validate(),
            Err(SceneError::RegionTileOutOfRange { .. })
        ));
        assert!(matches!(
            addr(0, 0, 0, 0, 0).validate(),
            Err(SceneError::RegionSamplesOutOfRange(0))
        ));
        assert!(matches!(
            addr(0, 0, 0, 0, 300).validate(),
            Err(SceneError::RegionSamplesOutOfRange(300))
        ));
        assert!(addr(0, 0, 0, 0, 1).validate().is_ok());
    }

    use hornvale_kernel::Geosphere;

    #[test]
    fn weights_are_exact_at_a_cell_center() {
        let geo = Geosphere::new(4);
        let index = NearestCellIndex::new(&geo);
        for cell in geo.cells().take(50) {
            let s = geo.position(cell);
            let w = triangle_weights(&geo, &index, s);
            // The cell carries essentially all the weight; the sum is 1.
            let total: f64 = w.iter().map(|(_, x)| x).sum();
            assert!((total - 1.0).abs() < 1e-9, "weights sum to 1: {total}");
            let on_cell: f64 = w.iter().filter(|(c, _)| *c == cell).map(|(_, x)| x).sum();
            assert!(
                on_cell > 1.0 - 1e-9,
                "cell {cell:?} weight {on_cell} not ~1"
            );
        }
    }

    #[test]
    fn weights_are_a_partition_of_unity_off_center() {
        let geo = Geosphere::new(4);
        let index = NearestCellIndex::new(&geo);
        // A point between two cell centers.
        let a = geo.position(CellId(20));
        let b = geo.position(geo.neighbors(CellId(20))[0]);
        let mid = {
            let m = [
                (a[0] + b[0]) / 2.0,
                (a[1] + b[1]) / 2.0,
                (a[2] + b[2]) / 2.0,
            ];
            let len = (m[0] * m[0] + m[1] * m[1] + m[2] * m[2]).sqrt();
            [m[0] / len, m[1] / len, m[2] / len]
        };
        let w = triangle_weights(&geo, &index, mid);
        let total: f64 = w.iter().map(|(_, x)| x).sum();
        assert!((total - 1.0).abs() < 1e-9);
        assert!(
            w.iter().all(|(_, x)| *x >= -1e-9 && *x <= 1.0 + 1e-9),
            "weights in [0,1]: {w:?}"
        );
    }

    #[test]
    fn interp_at_a_cell_center_returns_that_cells_value() {
        let geo = Geosphere::new(4);
        let index = NearestCellIndex::new(&geo);
        let value = |c: CellId| c.0 as f64; // an arbitrary per-cell scalar
        for cell in geo.cells().take(50) {
            let got = interp(&geo, &index, geo.position(cell), value);
            assert!((got - cell.0 as f64).abs() < 1e-6, "cell {cell:?}: {got}");
        }
    }
}
