//! `scene/tiles-region/v1`: one cube-sphere quadtree tile's footprint, sampled
//! at higher on-tile density than the global lattice. Continuous layers are
//! barycentrically interpolated between geosphere cells (a smooth surface, not
//! new physics — the ~110 km cell spacing is the resolution floor); discrete
//! layers stay nearest-cell. The projection here is the normative one, shared
//! byte-for-byte with the orrery's `cubeSphere.ts` and the reference page.

use crate::{SceneError, WaterfallPoint};
use hornvale_kernel::{CellId, Geosphere, NearestCellIndex, World};
use serde::Serialize;

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

/// The inverse of `face_unit`: which face a unit sphere position belongs to
/// (the standard cube-map assignment — the face whose normal has the
/// largest dot product with `p`) and its `(a, b)` parameters on that face.
/// Exact for points not exactly on a face seam (measure zero); each `FACES`
/// row is an orthonormal `(n, u, v)` triple, so `p·n = 1/|q|` and
/// `p·u = a·(p·n)` where `q = n + a·u + b·v` is `face_unit`'s pre-normalized
/// vector — dividing recovers `a` (and `b` likewise) exactly.
/// Transcendental-free (dot products and one division), so cross-platform
/// byte-identical.
fn locate_on_cube(p: [f64; 3]) -> (usize, f64, f64) {
    let dot = |x: [f64; 3], y: [f64; 3]| x[0] * y[0] + x[1] * y[1] + x[2] * y[2];
    let (face, _) = FACES
        .iter()
        .enumerate()
        .map(|(f, [n, _, _])| (f, dot(p, *n)))
        .max_by(|a, b| a.1.total_cmp(&b.1))
        .expect("FACES is nonempty");
    let [n, u, v] = FACES[face];
    let pn = dot(p, n);
    (face, dot(p, u) / pn, dot(p, v) / pn)
}

/// Whether unit-sphere position `p` falls inside tile `addr`'s footprint —
/// exact cube-face-and-parameter-box containment (§ its face plus its
/// `(a, b)` range at `addr.level`/`addr.ix`/`addr.iy`), not a lat/lon
/// approximation (which would distort near the poles and the antimeridian).
fn tile_contains(addr: &RegionAddr, p: [f64; 3]) -> bool {
    let (face, a, b) = locate_on_cube(p);
    if face != addr.face as usize {
        return false;
    }
    let a_lo = param(addr.ix, 0.0, addr.level);
    let a_hi = param(addr.ix, 1.0, addr.level);
    let b_lo = param(addr.iy, 0.0, addr.level);
    let b_hi = param(addr.iy, 1.0, addr.level);
    (a_lo..=a_hi).contains(&a) && (b_lo..=b_hi).contains(&b)
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

/// One `scene/tiles-region/v1` document (The Region §3.3). Field order is the
/// JSON key order and is contract. Per-node layers are `(samples+1)²`,
/// row-major (`i = row·(samples+1) + col`). Continuous layers are barycentric;
/// discrete layers (`ocean`, `biome`, `plate`) are nearest-cell.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed), bare-ok(index: face), bare-ok(count: level), bare-ok(index: ix), bare-ok(index: iy), bare-ok(count: samples), pending(wave-3: sea_level_m), bare-ok(diagnostic-value: season_period_days), bare-ok(count: circulation_bands), bare-ok(identifier-text: biome_legend), waiver(elevation-convention: elevation_m), bare-ok(flag: ocean), bare-ok(index: biome), bare-ok(index: plate), bare-ok(ratio: unrest), bare-ok(diagnostic-value: t_mean_c), bare-ok(diagnostic-value: t_swing_c), bare-ok(ratio: moisture), bare-ok(index: water), bare-ok(identifier-text: water_legend), bare-ok(diagnostic-value: drainage)
#[derive(Debug, Serialize)]
pub struct RegionScene {
    /// Always `scene/tiles-region/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// Cube face (0..=5).
    pub face: u32,
    /// Quadtree level.
    pub level: u32,
    /// Tile column.
    pub ix: u32,
    /// Tile row.
    pub iy: u32,
    /// Quads per edge; the node grid is `(samples+1)²`.
    pub samples: u32,
    /// Sea level, meters (document-level).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub sea_level_m: f64,
    /// Seasonal sinusoid period, standard days (document-level; self-contained).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub season_period_days: f64,
    /// Circulation bands per hemisphere; omitted when tidally locked.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub circulation_bands: Option<u32>,
    /// The biome catalog, v1's stable append-only order.
    pub biome_legend: Vec<String>,
    /// Elevation per node, meters (barycentric).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub elevation_m: Vec<f64>,
    /// Ocean flag per node (nearest-cell; the categorical coastline truth).
    pub ocean: Vec<bool>,
    /// Biome index per node (nearest-cell).
    pub biome: Vec<u16>,
    /// Plate id per node (nearest-cell).
    pub plate: Vec<u32>,
    /// Unrest per node, [0,1] (barycentric).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub unrest: Vec<f64>,
    /// Annual-mean temperature per node, °C (barycentric).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub t_mean_c: Vec<f64>,
    /// Hemisphere-signed seasonal half-swing per node, °C (barycentric; 0 when locked).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub t_swing_c: Vec<f64>,
    /// Moisture index per node, [0,1] (barycentric).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub moisture: Vec<f64>,
    /// The water classification per node, as an index into `water_legend`
    /// (WaterKind: ocean / salt-basin / river / dry-land) (nearest-cell).
    /// Appended per the schema stability contract.
    pub water: Vec<u8>,
    /// The water-kind catalog in stable index order — `water`'s values index
    /// into this. Appended per the schema stability contract.
    pub water_legend: Vec<String>,
    /// Flow-accumulation drainage per node (0 on ocean/dry land); river
    /// magnitude (nearest-cell, coupled to `water`). Appended per the schema
    /// stability contract.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub drainage: Vec<f64>,
    /// Waterfall sites within this tile's footprint — high-drainage
    /// watercourses crossing a scarp. Appended per the schema stability
    /// contract.
    pub waterfalls: Vec<WaterfallPoint>,
}

/// Build the `scene/tiles-region/v1` scene for one tile address. Deterministic:
/// same world + same address → byte-identical once serialized.
/// type-audit: bare-ok(index: face), bare-ok(count: level), bare-ok(index: ix), bare-ok(index: iy), bare-ok(count: samples)
pub fn tiles_region_scene(
    world: &World,
    face: u32,
    level: u32,
    ix: u32,
    iy: u32,
    samples: u32,
) -> Result<RegionScene, SceneError> {
    let addr = RegionAddr {
        face,
        level,
        ix,
        iy,
        samples,
    };
    addr.validate()?;
    let terrain =
        hornvale_worldgen::terrain_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let climate =
        hornvale_worldgen::climate_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let t_index = NearestCellIndex::new(terrain.geosphere());
    let c_index = NearestCellIndex::new(climate.geosphere());
    let biomes = climate.biome_map();
    let catalog = hornvale_climate::Biome::catalog();
    let units = addr.node_units();

    let mut elevation_m = Vec::with_capacity(units.len());
    let mut ocean = Vec::with_capacity(units.len());
    let mut biome = Vec::with_capacity(units.len());
    let mut plate = Vec::with_capacity(units.len());
    let mut unrest = Vec::with_capacity(units.len());
    let mut t_mean_c = Vec::with_capacity(units.len());
    let mut t_swing_c = Vec::with_capacity(units.len());
    let mut moisture = Vec::with_capacity(units.len());
    let mut water = Vec::with_capacity(units.len());
    let mut drainage = Vec::with_capacity(units.len());
    for s in &units {
        let tg = terrain.geosphere();
        let cg = climate.geosphere();
        // Discrete: nearest-cell.
        let t_cell = t_index.nearest_to_position(tg, *s);
        let c_cell = c_index.nearest_to_position(cg, *s);
        ocean.push(terrain.is_ocean(t_cell));
        water.push(terrain.water_kind_at(t_cell).index());
        drainage.push(terrain.drainage_at(t_cell));
        let b = *biomes.get(c_cell);
        biome.push(
            catalog
                .iter()
                .position(|e| *e == b)
                .expect("biome in catalog") as u16,
        );
        plate.push(terrain.plate_of(t_cell));
        // Continuous: barycentric.
        elevation_m.push(interp(tg, &t_index, *s, |c| terrain.elevation_at(c).get()));
        unrest.push(interp(tg, &t_index, *s, |c| terrain.unrest_at(c)));
        t_mean_c.push(interp(cg, &c_index, *s, |c| {
            climate.mean_temperature_at(c).get()
        }));
        t_swing_c.push(interp(cg, &c_index, *s, |c| climate.seasonal_swing_at(c)));
        // Barycentric interpolation is a convex combination in theory, but
        // when several sampled cells sit exactly at the moisture domain's
        // boundary (`1.0` — common now that ocean cells clamp there),
        // floating-point summation can overshoot by an ULP or two; clamp
        // back into the declared `[0, 1]` domain.
        moisture.push(interp(cg, &c_index, *s, |c| climate.moisture_at(c)).clamp(0.0, 1.0));
    }
    let waterfalls = terrain
        .waterfalls()
        .iter()
        .filter(|&&cell| tile_contains(&addr, terrain.geosphere().position(cell)))
        .map(|&cell| {
            let c = terrain.geosphere().coord(cell);
            WaterfallPoint {
                latitude: c.latitude,
                longitude: c.longitude,
            }
        })
        .collect();
    Ok(RegionScene {
        schema: "scene/tiles-region/v1".to_string(),
        seed: world.seed.0,
        face,
        level,
        ix,
        iy,
        samples,
        sea_level_m: terrain.sea_level().get(),
        season_period_days: climate.year_length_std(),
        circulation_bands: climate.band_count(),
        biome_legend: catalog.iter().map(|b| b.name().to_string()).collect(),
        elevation_m,
        ocean,
        biome,
        plate,
        unrest,
        t_mean_c,
        t_swing_c,
        moisture,
        water,
        water_legend: hornvale_terrain::WaterKind::LEGEND
            .iter()
            .map(|s| s.to_string())
            .collect(),
        drainage,
        waterfalls,
    })
}

/// Serialize a `RegionScene` as compact JSON (mirrors `scene_json`).
/// type-audit: bare-ok(artifact: return)
pub fn region_json(scene: &RegionScene) -> String {
    serde_json::to_string(scene).expect("a RegionScene always serializes")
}

/// Per-node actual temperature at `day`, °C, on the same node grid as
/// [`tiles_region_scene`] — the barycentric interpolation of `temperature_at`.
/// This equals `t_mean_c[i] + t_swing_c[i]·sin(τ·frac(day/period))` plus the
/// interpolated diurnal term (spinning worlds only), because interpolation is
/// linear and so distributes over every additive term `temperature_at` sums
/// per cell — interpolation commutes with the evaluator (The Region §3.4).
/// Full precision (not quantized); the cross-repo contract test pins the
/// client's reconstruction against these values.
/// type-audit: bare-ok(index: face), bare-ok(count: level), bare-ok(index: ix), bare-ok(index: iy), bare-ok(count: samples), bare-ok(diagnostic-value: day), bare-ok(diagnostic-value: return)
pub fn temperature_grid_region(
    world: &World,
    face: u32,
    level: u32,
    ix: u32,
    iy: u32,
    samples: u32,
    day: f64,
) -> Result<Vec<f64>, SceneError> {
    let addr = RegionAddr {
        face,
        level,
        ix,
        iy,
        samples,
    };
    addr.validate()?;
    let climate =
        hornvale_worldgen::climate_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let c_index = NearestCellIndex::new(climate.geosphere());
    let cg = climate.geosphere();
    Ok(addr
        .node_units()
        .iter()
        .map(|s| interp(cg, &c_index, *s, |c| climate.temperature_at(c, day).get()))
        .collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_climate::RotationRegime;

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

    use hornvale_kernel::Seed;
    use hornvale_worldgen::{SkyChoice, build_world, climate_of};

    fn gen_seed(seed: u64) -> hornvale_kernel::World {
        build_world(
            Seed(seed),
            &Default::default(),
            SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .unwrap_or_else(|e| panic!("seed {seed} builds: {e}"))
    }

    fn gen42() -> hornvale_kernel::World {
        gen_seed(42)
    }

    #[test]
    fn water_fields_are_sized_legend_matches_and_ocean_has_no_drainage() {
        // Seed 44 (like the tiles_scene sibling test) reliably carries river
        // cells. Anchor the region tile on a known river cell's position so
        // the tile actually exercises a River node rather than hoping a
        // fixed address happens to land on one (rivers are sparse
        // per-region). The tile is chosen wide (level 3) and densely
        // sampled (64 quads/edge, ~19.6 km/sample — finer than the ~110 km
        // geosphere cell spacing) so nearest-cell sampling reliably finds
        // river cells inside the tile's footprint.
        let w = gen_seed(44);
        let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
        let river_cell = terrain
            .geosphere()
            .cells()
            .find(|&c| terrain.water_kind_at(c) == hornvale_terrain::WaterKind::River)
            .expect("seed 44 has river cells (see the tiles_scene sibling test)");
        let pos = terrain.geosphere().position(river_cell);
        let level = 3;
        let (face, a, b) = locate_on_cube(pos);
        let n = 1u64 << level;
        let ix = (((a + 1.0) * 0.5 * n as f64) as u64).min(n - 1) as u32;
        let iy = (((b + 1.0) * 0.5 * n as f64) as u64).min(n - 1) as u32;
        let samples = 64;
        let scene = tiles_region_scene(&w, face as u32, level, ix, iy, samples).unwrap();
        let nodes = (samples as usize + 1).pow(2);

        assert_eq!(scene.water.len(), nodes);
        assert_eq!(scene.drainage.len(), nodes);
        assert_eq!(
            scene.water_legend,
            vec!["ocean", "salt-basin", "river", "dry-land"]
        );
        for i in 0..nodes {
            if scene.ocean[i] {
                assert_eq!(scene.drainage[i], 0.0);
            }
        }
        assert!(
            scene
                .water
                .iter()
                .any(|&wtr| wtr == hornvale_terrain::WaterKind::River.index()),
            "expected at least one river node in the tile anchored on a known river cell"
        );
        let json = region_json(&scene);
        assert!(json.contains("water_legend"));
        assert!(json.contains("drainage"));
        assert!(json.contains("waterfalls"));
    }

    #[test]
    fn region_scene_is_sized_and_byte_deterministic() {
        let w = gen42();
        let a = region_json(&tiles_region_scene(&w, 0, 3, 4, 4, 16).unwrap());
        let b = region_json(&tiles_region_scene(&w, 0, 3, 4, 4, 16).unwrap());
        assert_eq!(a, b);
        let scene = tiles_region_scene(&w, 0, 3, 4, 4, 16).unwrap();
        let nodes = 17 * 17;
        for len in [
            scene.elevation_m.len(),
            scene.ocean.len(),
            scene.biome.len(),
            scene.plate.len(),
            scene.unrest.len(),
            scene.t_mean_c.len(),
            scene.t_swing_c.len(),
            scene.moisture.len(),
        ] {
            assert_eq!(len, nodes);
        }
        assert_eq!(scene.schema, "scene/tiles-region/v1");
        assert_eq!(scene.biome_legend.len(), 22);
        assert!(scene.moisture.iter().all(|&m| (0.0..=1.0).contains(&m)));
    }

    #[test]
    fn discrete_layers_match_nearest_cell() {
        let w = gen42();
        let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
        let index = NearestCellIndex::new(terrain.geosphere());
        let a = RegionAddr {
            face: 2,
            level: 2,
            ix: 1,
            iy: 1,
            samples: 8,
        };
        let scene = tiles_region_scene(&w, a.face, a.level, a.ix, a.iy, a.samples).unwrap();
        for (i, s) in a.node_units().iter().enumerate() {
            let cell = index.nearest_to_position(terrain.geosphere(), *s);
            assert_eq!(scene.ocean[i], terrain.is_ocean(cell), "ocean node {i}");
            assert_eq!(scene.plate[i], terrain.plate_of(cell), "plate node {i}");
        }
    }

    #[test]
    fn locked_world_zeroes_swing_and_omits_bands() {
        use hornvale_astronomy::{RotationPin, SkyPins};
        let sky = SkyPins {
            rotation: Some(RotationPin::Locked),
            ..Default::default()
        };
        let w = build_world(
            Seed(42),
            &sky,
            SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .unwrap();
        let scene = tiles_region_scene(&w, 0, 2, 1, 1, 8).unwrap();
        assert!(scene.t_swing_c.iter().all(|&s| s == 0.0));
        assert_eq!(scene.circulation_bands, None);
        assert!(!region_json(&scene).contains("circulation_bands"));
    }

    #[test]
    fn continuous_layers_are_interpolated_not_nearest_cell() {
        // At a fine enough tile, some node's interpolated elevation must differ
        // from the raw nearest-cell elevation — proving barycentric blending is
        // actually happening (guards against a silent fallback to nearest-cell).
        let w = gen42();
        let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
        let index = NearestCellIndex::new(terrain.geosphere());
        let addr = RegionAddr {
            face: 0,
            level: 4,
            ix: 8,
            iy: 8,
            samples: 16,
        };
        let scene =
            tiles_region_scene(&w, addr.face, addr.level, addr.ix, addr.iy, addr.samples).unwrap();
        let any_differs = addr.node_units().iter().enumerate().any(|(i, s)| {
            let cell = index.nearest_to_position(terrain.geosphere(), *s);
            (scene.elevation_m[i] - terrain.elevation_at(cell).get()).abs() > 1e-6
        });
        assert!(
            any_differs,
            "no node's elevation differs from nearest-cell — interpolation is not happening"
        );
    }

    /// The per-node diurnal contribution, reconstructed exactly the way
    /// `temperature_at` computes it per-cell (own latitude, own precomputed
    /// amplitude) and then barycentrically interpolated — the same order of
    /// operations `interp(temperature_at)` performs internally, so this is an
    /// exact algebraic identity (interpolation distributes over the sum),
    /// not an approximation. Exactly `0.0` when the world is locked (the
    /// diurnal term never applies there, however nonzero the precomputed
    /// amplitude field happens to be).
    fn interp_diurnal(
        climate: &hornvale_climate::GeneratedClimate,
        c_index: &NearestCellIndex,
        s: [f64; 3],
        obliquity_deg: f64,
        year_phase: f64,
        day_fraction: f64,
    ) -> f64 {
        let RotationRegime::Spinning { day_std } = climate.regime() else {
            return 0.0;
        };
        interp(climate.geosphere(), c_index, s, |c| {
            let coord = climate.geosphere().coord(c);
            let amp = climate.diurnal_amp_at(c);
            hornvale_climate::diurnal_anomaly(
                amp,
                coord.latitude,
                coord.longitude,
                obliquity_deg,
                year_phase,
                day_fraction,
                day_std,
            )
            .get()
        })
    }

    #[test]
    fn temperature_grid_region_commutes_with_the_evaluator() {
        let w = gen42();
        let (face, level, ix, iy, samples) = (0u32, 3u32, 4u32, 4u32, 16u32);
        // The provider values, at full precision (pre-quantization), from an
        // independent rebuild of the layers.
        let climate = climate_of(&w).unwrap();
        let c_index = NearestCellIndex::new(climate.geosphere());
        let addr = RegionAddr {
            face,
            level,
            ix,
            iy,
            samples,
        };
        let period = climate.year_length_std();
        let offset = climate.year_phase_offset();
        let obliquity_deg = climate.obliquity_deg();
        let tau = std::f64::consts::TAU;
        for day in [0.0_f64, 91.3, 200.0, 366.5] {
            let grid = temperature_grid_region(&w, face, level, ix, iy, samples, day).unwrap();
            let phase = (day / period + offset).rem_euclid(1.0);
            let theta = hornvale_kernel::math::sin(tau * phase);
            let day_fraction = day.rem_euclid(1.0);
            for (i, s) in addr.node_units().iter().enumerate() {
                // `grid[i]` is interp(temperature_at) (form A); `rhs` is
                // interp(mean) + interp(swing)·θ + interp(diurnal) (form B).
                // Commutation is exact in real arithmetic — interpolation is
                // linear and distributes over the sum of the three additive
                // terms `temperature_at` computes per-cell — but only to
                // float rounding here (weighted sums reduce in a different
                // order), so assert tight-approximate — NOT assert_eq!.
                let mean = interp(climate.geosphere(), &c_index, *s, |c| {
                    climate.mean_temperature_at(c).get()
                });
                let swing = interp(climate.geosphere(), &c_index, *s, |c| {
                    climate.seasonal_swing_at(c)
                });
                let diurnal =
                    interp_diurnal(&climate, &c_index, *s, obliquity_deg, phase, day_fraction);
                let rhs = mean + swing * theta + diurnal;
                assert!(
                    (grid[i] - rhs).abs() <= 1e-9 * grid[i].abs().max(1.0),
                    "commutation node {i} day {day}: {} vs {}",
                    grid[i],
                    rhs
                );
            }
        }
    }

    #[test]
    fn temperature_grid_region_at_zero_phase_equals_t_mean_plus_diurnal() {
        let w = gen42();
        let climate = climate_of(&w).unwrap();
        // The seasonal term vanishes at `frac(day/year + offset) = 0`, which is
        // day zero only when `year_phase_offset` is zero — so shift the probed
        // day by the true offset rather than assuming day zero. The diurnal
        // term does NOT vanish here (it depends on the day's fractional part,
        // not the year phase), so the expected value is `t_mean_c` plus the
        // interpolated diurnal contribution — not `t_mean_c` alone.
        let period = climate.year_length_std();
        let offset = climate.year_phase_offset();
        let obliquity_deg = climate.obliquity_deg();
        let zero_phase_day = (-offset).rem_euclid(1.0) * period;
        let c_index = NearestCellIndex::new(climate.geosphere());
        let addr = RegionAddr {
            face: 0,
            level: 3,
            ix: 4,
            iy: 4,
            samples: 16,
        };
        let grid = temperature_grid_region(&w, 0, 3, 4, 4, 16, zero_phase_day).unwrap();
        let scene = tiles_region_scene(&w, 0, 3, 4, 4, 16).unwrap();
        let day_fraction = zero_phase_day.rem_euclid(1.0);
        for (i, (g, m)) in grid.iter().zip(scene.t_mean_c.iter()).enumerate() {
            let s = addr.node_units()[i];
            let diurnal = interp_diurnal(&climate, &c_index, s, obliquity_deg, 0.0, day_fraction);
            let expected = m + diurnal;
            // grid is full precision; t_mean_c is quantized — agree to ~8 sig digits.
            assert!(
                (g - expected).abs() <= 1e-6 * g.abs().max(1.0),
                "zero-phase grid vs t_mean+diurnal: {g} vs {expected}"
            );
        }
    }
}
