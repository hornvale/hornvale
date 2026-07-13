//! Plates: Voronoi regions on the unit sphere, moving about a drawn Euler
//! pole, with a drawn orogenic maturity. Continental/oceanic character is
//! no longer a plate property — it lives in the crust field (`crust.rs`,
//! Crust epoch v2): a plate's cells are continental or oceanic cell by
//! cell, by crust thickness, independent of which plate they ride on.

use crate::pins::TerrainPins;
use crate::streams;
use hornvale_kernel::{CellMap, Geosphere, Seed, Stream, math};

/// A tectonic plate.
/// type-audit: bare-ok(index: id), bare-ok(ratio: seed_position), bare-ok(ratio: euler_axis), bare-ok(ratio: rate), bare-ok(ratio: maturity), bare-ok(ratio: weight)
#[derive(Debug, Clone, PartialEq)]
pub struct Plate {
    /// Index into the globe's plate list (equals its position in the Vec).
    pub id: u32,
    /// Voronoi seed position, a unit vector.
    pub seed_position: [f64; 3],
    /// Euler-pole rotation axis, a unit vector.
    pub euler_axis: [f64; 3],
    /// Angular rate about the Euler pole (dimensionless model units, 0.2–1.0).
    pub rate: f64,
    /// Orogenic maturity in [0, 1]: 0 = young (sharp, high, restless),
    /// 1 = old (worn, low, quiet).
    pub maturity: f64,
    /// Voronoi weight; larger = larger region.
    pub weight: f64,
}

/// Dot product.
pub(crate) fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Cross product a × b.
pub(crate) fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

/// a − b.
pub(crate) fn sub(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}

/// a scaled by s.
pub(crate) fn scale(a: [f64; 3], s: f64) -> [f64; 3] {
    [a[0] * s, a[1] * s, a[2] * s]
}

/// Euclidean length.
pub(crate) fn norm(a: [f64; 3]) -> f64 {
    dot(a, a).sqrt()
}

/// a normalized to unit length.
pub(crate) fn normalize(a: [f64; 3]) -> [f64; 3] {
    scale(a, 1.0 / norm(a))
}

/// Draw a uniformly distributed unit vector: z uniform in [-1, 1), azimuth
/// uniform in [0, 2π) — the area-preserving cylindrical construction.
/// Consumes exactly two `next_f64` draws.
pub(crate) fn unit_vector(stream: &mut Stream) -> [f64; 3] {
    let z = 2.0 * stream.next_f64() - 1.0;
    let azimuth = std::f64::consts::TAU * stream.next_f64();
    let r = (1.0 - z * z).sqrt();
    [r * math::cos(azimuth), r * math::sin(azimuth), z]
}

/// Drawn plate-count range (spec §4: ~8–40).
const PLATE_COUNT_MIN: u32 = 8;
/// Upper end of the drawn plate-count range.
const PLATE_COUNT_MAX: u32 = 40;

/// Generate the plate list: count, seed positions, Euler-pole motions,
/// weights, and maturities, each sub-step from its own labeled
/// stream. Pins override drawn values but never skip a draw (pin
/// isolation). Assumes `pins` already validated. `notes` receives a
/// metering entry when `plates` is pinned (Task 7's metering convention:
/// `pinned <name> <value> (seed draws <drawn>)`).
/// type-audit: bare-ok(prose: notes)
pub fn generate_plates(
    terrain_seed: Seed,
    pins: &TerrainPins,
    notes: &mut Vec<String>,
) -> Vec<Plate> {
    let drawn_count = terrain_seed
        .derive(streams::PLATE_COUNT)
        .stream()
        .range_u32(PLATE_COUNT_MIN, PLATE_COUNT_MAX);
    let count = pins.plates.unwrap_or(drawn_count);
    if let Some(n) = pins.plates {
        notes.push(format!("pinned plates {n} (seed draws {drawn_count})"));
    }

    let mut seed_stream = terrain_seed.derive(streams::PLATE_SEEDS).stream();
    let positions: Vec<[f64; 3]> = (0..count).map(|_| unit_vector(&mut seed_stream)).collect();

    let mut motion_stream = terrain_seed.derive(streams::PLATE_MOTION).stream();
    let motions: Vec<([f64; 3], f64)> = (0..count)
        .map(|_| {
            let axis = unit_vector(&mut motion_stream);
            let rate = 0.2 + 0.8 * motion_stream.next_f64();
            (axis, rate)
        })
        .collect();

    let mut maturity_stream = terrain_seed.derive(streams::MATURITY).stream();
    let maturities: Vec<f64> = (0..count).map(|_| maturity_stream.next_f64()).collect();

    /// Heavy-tail shape: weight = 1 / (1 - TAIL * u), u uniform — range
    /// [1, 12.5], median ~1.85, a few giants per world. Task 9 iteration 2
    /// softened this from 0.95 (range [1, 20]): the after-census's
    /// plate-size-gini median overshot its 0.45-0.75 band at 0.767, so the
    /// tail's giants are trimmed while the heavy-tail shape is kept.
    const WEIGHT_TAIL: f64 = 0.92;
    let mut weight_stream = terrain_seed.derive(streams::PLATE_WEIGHTS).stream();
    let weights: Vec<f64> = (0..count)
        .map(|_| 1.0 / (1.0 - WEIGHT_TAIL * weight_stream.next_f64()))
        .collect();

    (0..count)
        .map(|i| {
            let i = i as usize;
            Plate {
                id: i as u32,
                seed_position: positions[i],
                euler_axis: motions[i].0,
                rate: motions[i].1,
                maturity: maturities[i],
                weight: weights[i],
            }
        })
        .collect()
}

/// Edge-noise amplitude, radians (~3.4 degrees: boundaries wander a cell
/// or two without tearing plates apart).
const EDGE_AMP: f64 = 0.06;

/// Assign each cell to a plate by weighted, edge-noised angular distance:
/// score = (angle + noise) / weight, smallest wins, ties to the lower
/// plate id. Noise is stateless per-plate hash-noise (`plate-edge`) — no
/// draws, so any grid level samples the same boundaries.
/// type-audit: bare-ok(index)
pub fn assign_plates(geo: &Geosphere, terrain_seed: Seed, plates: &[Plate]) -> CellMap<u32> {
    let edge_root = terrain_seed.derive(crate::streams::PLATE_EDGE);
    CellMap::from_fn(geo, |cell| {
        let position = geo.position(cell);
        let mut best = 0u32;
        let mut best_score = f64::INFINITY;
        for plate in plates {
            let angle = math::acos(dot(position, plate.seed_position).clamp(-1.0, 1.0));
            let noise_seed = edge_root.derive(&format!("plate-{}", plate.id));
            let noise =
                EDGE_AMP * (2.0 * crate::crust::sphere_fbm01(noise_seed, position, 8.0, 4) - 1.0);
            let score = (angle + noise).max(0.0) / plate.weight;
            if score < best_score {
                best_score = score;
                best = plate.id;
            }
        }
        best
    })
}

/// A plate's surface velocity at a unit-sphere position: ω × r, always
/// tangent to the sphere.
/// type-audit: bare-ok(ratio)
pub fn velocity_at(plate: &Plate, position: [f64; 3]) -> [f64; 3] {
    cross(scale(plate.euler_axis, plate.rate), position)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::TerrainPins;
    use crate::streams;
    use hornvale_kernel::{Geosphere, Seed};

    #[test]
    fn plate_count_stays_in_the_drawn_range_and_pins_override() {
        for seed in 0..32u64 {
            let plates = generate_plates(
                Seed(seed).derive(streams::ROOT),
                &TerrainPins::default(),
                &mut Vec::new(),
            );
            assert!((8..=40).contains(&(plates.len() as u32)), "seed {seed}");
            for (i, p) in plates.iter().enumerate() {
                assert_eq!(p.id, i as u32);
                assert!((norm(p.seed_position) - 1.0).abs() < 1e-12);
                assert!((norm(p.euler_axis) - 1.0).abs() < 1e-12);
                assert!((0.2..=1.0).contains(&p.rate));
                assert!((0.0..=1.0).contains(&p.maturity));
            }
        }
        let pins = TerrainPins {
            plates: Some(12),
            ..TerrainPins::default()
        };
        assert_eq!(
            generate_plates(Seed(42).derive(streams::ROOT), &pins, &mut Vec::new()).len(),
            12
        );
    }

    #[test]
    fn every_cell_joins_exactly_one_plate() {
        let geo = Geosphere::new(2);
        let terrain_seed = Seed(42).derive(streams::ROOT);
        let plates = generate_plates(terrain_seed, &TerrainPins::default(), &mut Vec::new());
        let assignment = assign_plates(&geo, terrain_seed, &plates);
        assert_eq!(assignment.len(), geo.cell_count());
        for (_, plate) in assignment.iter() {
            assert!((*plate as usize) < plates.len());
        }
    }

    #[test]
    fn velocities_are_tangent_to_the_sphere() {
        let geo = Geosphere::new(2);
        let plates = generate_plates(
            Seed(7).derive(streams::ROOT),
            &TerrainPins::default(),
            &mut Vec::new(),
        );
        for cell in geo.cells() {
            let p = geo.position(cell);
            for plate in &plates {
                assert!(dot(velocity_at(plate, p), p).abs() < 1e-12);
            }
        }
    }

    #[test]
    fn plate_weights_are_heavy_tailed_and_regions_concentrate() {
        let geo = Geosphere::new(4);
        let mut ginis = Vec::new();
        for seed in 0..12u64 {
            let plates = generate_plates(
                Seed(seed).derive(streams::ROOT),
                &TerrainPins::default(),
                &mut Vec::new(),
            );
            for p in &plates {
                // True range of 1 / (1 - 0.92 u), u in [0, 1): [1, 12.5).
                assert!((1.0..12.5).contains(&p.weight), "weight {}", p.weight);
            }
            let assignment = assign_plates(&geo, Seed(seed).derive(streams::ROOT), &plates);
            let mut counts = vec![0usize; plates.len()];
            for (_, plate) in assignment.iter() {
                counts[*plate as usize] += 1;
            }
            ginis.push(crate::shape::gini(&counts).expect("nonzero cells"));
        }
        let median = {
            let mut g = ginis.clone();
            g.sort_by(|a, b| a.total_cmp(b));
            g[g.len() / 2]
        };
        assert!(
            median > 0.35,
            "plate sizes not heavy-tailed: median gini {median}"
        );
    }

    /// Two equal-weight antipodal plates, weight 1.0 each, at the poles.
    fn two_test_plates() -> Vec<Plate> {
        vec![
            Plate {
                id: 0,
                seed_position: [0.0, 0.0, 1.0],
                euler_axis: [1.0, 0.0, 0.0],
                rate: 1.0,
                maturity: 0.5,
                weight: 1.0,
            },
            Plate {
                id: 1,
                seed_position: [0.0, 0.0, -1.0],
                euler_axis: [1.0, 0.0, 0.0],
                rate: 1.0,
                maturity: 0.5,
                weight: 1.0,
            },
        ]
    }

    #[test]
    fn plate_edges_wander_off_the_great_circle() {
        // Two equal-weight antipodal plates: the unweighted boundary is the
        // equator; with edge noise, boundary-adjacent cells must appear at
        // varied latitudes.
        let geo = Geosphere::new(4);
        let plates = two_test_plates();
        let assignment = assign_plates(&geo, Seed(42).derive(streams::ROOT), &plates);
        let mut boundary_lats = Vec::new();
        for cell in geo.cells() {
            let mine = *assignment.get(cell);
            if geo
                .neighbors(cell)
                .iter()
                .any(|n| *assignment.get(*n) != mine)
            {
                boundary_lats.push(geo.coord(cell).latitude);
            }
        }
        let (lo, hi) = boundary_lats
            .iter()
            .fold((f64::INFINITY, f64::NEG_INFINITY), |(a, b), v| {
                (a.min(*v), b.max(*v))
            });
        assert!(
            hi - lo > 5.0,
            "boundary hugs the equator: spread {} deg",
            hi - lo
        );
    }
}
