//! Plates: Voronoi regions on the unit sphere, each flagged continental or
//! oceanic, moving about a drawn Euler pole, with a drawn orogenic maturity.

use crate::pins::TerrainPins;
use crate::streams;
use hornvale_kernel::{CellMap, Geosphere, Seed, Stream};

/// A tectonic plate.
/// type-audit: bare-ok(index: id), bare-ok(ratio: seed_position), bare-ok(flag: continental), bare-ok(ratio: euler_axis), bare-ok(ratio: rate), bare-ok(ratio: maturity), bare-ok(ratio: weight)
#[derive(Debug, Clone, PartialEq)]
pub struct Plate {
    /// Index into the globe's plate list (equals its position in the Vec).
    pub id: u32,
    /// Voronoi seed position, a unit vector.
    pub seed_position: [f64; 3],
    /// Continental (buoyant, high base) or oceanic (dense, low base).
    pub continental: bool,
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
    [r * azimuth.cos(), r * azimuth.sin(), z]
}

/// Drawn plate-count range (spec §4: ~8–40).
const PLATE_COUNT_MIN: u32 = 8;
/// Upper end of the drawn plate-count range.
const PLATE_COUNT_MAX: u32 = 40;

/// Generate the plate list: count, seed positions, continental flags,
/// Euler-pole motions, and maturities, each sub-step from its own labeled
/// stream. Pins override drawn values but never skip a draw (pin
/// isolation). Assumes `pins` already validated.
pub fn generate_plates(terrain_seed: Seed, pins: &TerrainPins) -> Vec<Plate> {
    let drawn_count = terrain_seed
        .derive(streams::PLATE_COUNT)
        .stream()
        .range_u32(PLATE_COUNT_MIN, PLATE_COUNT_MAX);
    let count = pins.plates.unwrap_or(drawn_count);

    let mut seed_stream = terrain_seed.derive(streams::PLATE_SEEDS).stream();
    let positions: Vec<[f64; 3]> = (0..count).map(|_| unit_vector(&mut seed_stream)).collect();

    let mut kind_stream = terrain_seed.derive(streams::PLATE_KIND).stream();
    let continental_fraction = 0.25 + 0.25 * kind_stream.next_f64();
    let rolls: Vec<f64> = (0..count).map(|_| kind_stream.next_f64()).collect();
    let continental = continental_flags(
        &positions,
        &rolls,
        continental_fraction,
        pins.supercontinent,
    );

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
    /// [1, 20], median ~1.9, a few giants per world.
    const WEIGHT_TAIL: f64 = 0.95;
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
                continental: continental[i],
                euler_axis: motions[i].0,
                rate: motions[i].1,
                maturity: maturities[i],
                weight: weights[i],
            }
        })
        .collect()
}

/// Continental flags. Scattered (the default and `Some(false)`): each plate
/// is continental when its roll clears the fraction. Supercontinent
/// (`Some(true)`): the same expected number of continental plates, but
/// clustered around the plate with the lowest roll — nearest seed positions
/// win. Both paths consume identical draws, so the pin never perturbs the
/// stream (pin isolation).
fn continental_flags(
    positions: &[[f64; 3]],
    rolls: &[f64],
    fraction: f64,
    supercontinent: Option<bool>,
) -> Vec<bool> {
    if supercontinent != Some(true) {
        return rolls.iter().map(|r| *r < fraction).collect();
    }
    let count = positions.len();
    let continental_count = ((fraction * count as f64).round() as usize).max(1);
    let mut center = 0usize;
    for i in 1..count {
        if rolls[i].total_cmp(&rolls[center]) == std::cmp::Ordering::Less {
            center = i;
        }
    }
    let mut by_closeness: Vec<usize> = (0..count).collect();
    by_closeness.sort_by(|a, b| {
        dot(positions[*b], positions[center])
            .total_cmp(&dot(positions[*a], positions[center]))
            .then(a.cmp(b))
    });
    let mut flags = vec![false; count];
    for i in by_closeness.into_iter().take(continental_count) {
        flags[i] = true;
    }
    flags
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
            let angle = dot(position, plate.seed_position).clamp(-1.0, 1.0).acos();
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
            let plates = generate_plates(Seed(seed).derive(streams::ROOT), &TerrainPins::default());
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
            generate_plates(Seed(42).derive(streams::ROOT), &pins).len(),
            12
        );
    }

    #[test]
    fn every_cell_joins_exactly_one_plate() {
        let geo = Geosphere::new(2);
        let terrain_seed = Seed(42).derive(streams::ROOT);
        let plates = generate_plates(terrain_seed, &TerrainPins::default());
        let assignment = assign_plates(&geo, terrain_seed, &plates);
        assert_eq!(assignment.len(), geo.cell_count());
        for (_, plate) in assignment.iter() {
            assert!((*plate as usize) < plates.len());
        }
    }

    #[test]
    fn velocities_are_tangent_to_the_sphere() {
        let geo = Geosphere::new(2);
        let plates = generate_plates(Seed(7).derive(streams::ROOT), &TerrainPins::default());
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
            let plates = generate_plates(Seed(seed).derive(streams::ROOT), &TerrainPins::default());
            for p in &plates {
                assert!((1.0..=20.0).contains(&p.weight), "weight {}", p.weight);
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
                continental: true,
                euler_axis: [1.0, 0.0, 0.0],
                rate: 1.0,
                maturity: 0.5,
                weight: 1.0,
            },
            Plate {
                id: 1,
                seed_position: [0.0, 0.0, -1.0],
                continental: true,
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

    #[test]
    fn supercontinent_clusters_without_perturbing_the_draws() {
        let seed = Seed(42).derive(streams::ROOT);
        let scattered = generate_plates(seed, &TerrainPins::default());
        let clustered = generate_plates(
            seed,
            &TerrainPins {
                supercontinent: Some(true),
                ..TerrainPins::default()
            },
        );
        assert_eq!(scattered.len(), clustered.len());
        for (a, b) in scattered.iter().zip(&clustered) {
            assert_eq!(a.seed_position, b.seed_position);
            assert_eq!(a.euler_axis, b.euler_axis);
            assert_eq!(a.rate, b.rate);
            assert_eq!(a.maturity, b.maturity);
        }
        assert!(clustered.iter().filter(|p| p.continental).count() >= 1);
        // Explicitly re-affirming the scattered layout is byte-identical.
        let reaffirmed = generate_plates(
            seed,
            &TerrainPins {
                supercontinent: Some(false),
                ..TerrainPins::default()
            },
        );
        assert_eq!(scattered, reaffirmed);
    }
}
