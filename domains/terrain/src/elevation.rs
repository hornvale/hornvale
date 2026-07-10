//! Elevation (meters, bare f64 by documented convention), sea level, and
//! the unrest field. Elevation = continental/oceanic base + the nearest
//! same-plate boundary's contribution decayed by graph distance and shaped
//! by the plate's maturity + drawn hotspots + a per-cell micro-epsilon that
//! guarantees a strict ordering (so the sea-level percentile is exact).

use crate::boundaries::{BoundaryKind, CellBoundary};
use crate::pins::TerrainPins;
use crate::plates::{Plate, dot, unit_vector};
use crate::streams;
use hornvale_kernel::{CellId, CellMap, Geosphere, Seed};

/// Continental platform base elevation, meters.
const CONTINENT_BASE_M: f64 = 400.0;
/// Abyssal oceanic base elevation, meters.
const OCEAN_BASE_M: f64 = -4000.0;
/// Per-cell-id micro-relief, meters. Breaks every elevation tie so the
/// sea-level percentile is exact; at 40,962 cells (the canonical level-6
/// grid) the total spread is ~0.04 m — physically invisible, a declared
/// approximation.
const CELL_EPSILON_M: f64 = 1e-6;
/// Maximum possible closing speed (two rate-1.0 plates head-on); boundary
/// magnitudes are normalized against it.
const MAX_CLOSING_SPEED: f64 = 2.0;

/// Signed peak amplitude (meters, at full closing speed, before the
/// maturity factor) a boundary kind contributes on a cell whose own plate
/// is `continental`. The mixed kinds are side-dependent: a coastal range
/// rises on the continent while the trench deepens offshore; an ocean–ocean
/// arc rises on the overriding side (`arc_side`, deterministically the
/// higher plate id) while the other side takes the trench.
fn boundary_amplitude_m(kind: BoundaryKind, continental: bool, arc_side: bool) -> f64 {
    match kind {
        BoundaryKind::ContinentalCollision => 5000.0,
        BoundaryKind::CoastalRange => {
            if continental {
                3000.0
            } else {
                -3000.0
            }
        }
        BoundaryKind::IslandArc => {
            if arc_side {
                1500.0
            } else {
                -2500.0
            }
        }
        BoundaryKind::ContinentalRift => -500.0,
        BoundaryKind::OceanicRidge => 1500.0,
        BoundaryKind::Transform => 0.0,
    }
}

/// A mantle hotspot: a fixed Gaussian dome of uplift.
struct Hotspot {
    /// Dome center, a unit vector.
    position: [f64; 3],
    /// Peak uplift, meters.
    strength_m: f64,
}

/// Angular half-width of a hotspot dome, radians (~3°: one to two cells at
/// level 5).
const HOTSPOT_SIGMA_RAD: f64 = 0.05;

impl Hotspot {
    /// Gaussian dome contribution at a unit-sphere position, meters.
    fn contribution_m(&self, position: [f64; 3]) -> f64 {
        let angle = dot(self.position, position).clamp(-1.0, 1.0).acos();
        self.strength_m * (-(angle * angle) / (2.0 * HOTSPOT_SIGMA_RAD * HOTSPOT_SIGMA_RAD)).exp()
    }
}

/// Draw 3–8 hotspots from the hotspots stream: count first, then position
/// (two draws) and strength (1000–3000 m) per hotspot, sequentially.
fn draw_hotspots(terrain_seed: Seed) -> Vec<Hotspot> {
    let mut stream = terrain_seed.derive(streams::HOTSPOTS).stream();
    let count = stream.range_u32(3, 8);
    (0..count)
        .map(|_| {
            let position = unit_vector(&mut stream);
            let strength_m = 1000.0 + 2000.0 * stream.next_f64();
            Hotspot {
                position,
                strength_m,
            }
        })
        .collect()
}

/// Pure elevation assembly over explicit inputs (hotspots included), so
/// tests can pin the hotspot list. See the module doc for the formula.
fn assemble_elevation(
    geo: &Geosphere,
    plates: &[Plate],
    plate_of: &CellMap<u32>,
    boundaries: &CellMap<Option<CellBoundary>>,
    distances: &CellMap<Option<(u32, CellId)>>,
    hotspots: &[Hotspot],
) -> CellMap<f64> {
    CellMap::from_fn(geo, |cell| {
        let plate = &plates[*plate_of.get(cell) as usize];
        let base = if plate.continental {
            CONTINENT_BASE_M
        } else {
            OCEAN_BASE_M
        };
        let boundary_term = match *distances.get(cell) {
            None => 0.0,
            Some((distance, source)) => {
                let contact = (*boundaries.get(source)).expect("BFS sources are boundary cells");
                let arc_side = plate.id > contact.other_plate;
                let amplitude = boundary_amplitude_m(contact.kind, plate.continental, arc_side);
                // Young plates (maturity 0): 1.5x amplitude, sharp 1.5-cell
                // falloff. Old plates (maturity 1): 0.5x amplitude, worn
                // 4.5-cell falloff.
                let factor = 1.5 - plate.maturity;
                let decay_cells = 1.5 + 3.0 * plate.maturity;
                amplitude
                    * (contact.magnitude / MAX_CLOSING_SPEED)
                    * factor
                    * (-f64::from(distance) / decay_cells).exp()
            }
        };
        let position = geo.position(cell);
        let hotspot_term: f64 = hotspots.iter().map(|h| h.contribution_m(position)).sum();
        base + boundary_term + hotspot_term + CELL_EPSILON_M * f64::from(cell.0)
    })
}

/// Per-cell elevation in meters: continental/oceanic base, the nearest
/// same-plate boundary's contribution decayed by graph distance and shaped
/// by maturity, drawn hotspots, and a strict-ordering micro-epsilon.
/// type-audit: bare-ok(index: plate_of), bare-ok(count: distances), waiver(elevation-convention: return)
pub fn generate_elevation(
    terrain_seed: Seed,
    geo: &Geosphere,
    plates: &[Plate],
    plate_of: &CellMap<u32>,
    boundaries: &CellMap<Option<CellBoundary>>,
    distances: &CellMap<Option<(u32, CellId)>>,
) -> CellMap<f64> {
    let hotspots = draw_hotspots(terrain_seed);
    assemble_elevation(geo, plates, plate_of, boundaries, distances, &hotspots)
}

/// Derive sea level: draw (or pin) a target ocean fraction, then place sea
/// level at the elevation percentile that puts exactly that fraction of
/// cells strictly below it. The draw is consumed whether pinned or not
/// (pin isolation). Sort uses `total_cmp` — elevations are finite and
/// strictly ordered by construction.
/// type-audit: waiver(elevation-convention: elevation), pending(wave-2: return)
pub fn derive_sea_level(terrain_seed: Seed, pins: &TerrainPins, elevation: &CellMap<f64>) -> f64 {
    let drawn = 0.5
        + 0.25
            * terrain_seed
                .derive(streams::OCEAN_FRACTION)
                .stream()
                .next_f64();
    let target = pins.ocean_fraction.unwrap_or(drawn);
    let mut sorted: Vec<f64> = elevation.iter().map(|(_, e)| *e).collect();
    sorted.sort_by(|a, b| a.total_cmp(b));
    let index = ((target * sorted.len() as f64) as usize).min(sorted.len() - 1);
    sorted[index]
}

/// Relative restlessness of each boundary kind.
fn intensity(kind: BoundaryKind) -> f64 {
    match kind {
        BoundaryKind::ContinentalCollision => 0.8,
        BoundaryKind::CoastalRange => 0.9,
        BoundaryKind::IslandArc => 0.9,
        BoundaryKind::ContinentalRift => 0.7,
        BoundaryKind::OceanicRidge => 0.6,
        BoundaryKind::Transform => 0.5,
    }
}

/// Distance decay length for unrest, in cells.
const UNREST_DECAY_CELLS: f64 = 2.0;

/// The unrest field, per cell in [0, 1]: boundary intensity × normalized
/// closing speed × youth (inverse maturity), decayed by distance to the
/// nearest same-plate boundary; clamped. Old quiet interiors approach zero.
/// Nothing consumes it in C3 — it is banked (spec §15).
/// type-audit: bare-ok(index: plate_of), bare-ok(count: distances), bare-ok(ratio: return)
pub fn generate_unrest(
    geo: &Geosphere,
    plates: &[Plate],
    plate_of: &CellMap<u32>,
    boundaries: &CellMap<Option<CellBoundary>>,
    distances: &CellMap<Option<(u32, CellId)>>,
) -> CellMap<f64> {
    CellMap::from_fn(geo, |cell| {
        let Some((distance, source)) = *distances.get(cell) else {
            return 0.0;
        };
        let contact = (*boundaries.get(source)).expect("BFS sources are boundary cells");
        let plate = &plates[*plate_of.get(cell) as usize];
        let youth = 1.5 - plate.maturity;
        let raw = intensity(contact.kind)
            * (contact.magnitude / MAX_CLOSING_SPEED)
            * youth
            * (-f64::from(distance) / UNREST_DECAY_CELLS).exp();
        raw.clamp(0.0, 1.0)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::boundaries::{BoundaryKind, boundary_distance, boundary_field};
    use crate::pins::TerrainPins;
    use crate::plates::{Plate, assign_plates, generate_plates};
    use crate::streams;
    use hornvale_kernel::{Geosphere, Seed};

    /// Two continental hemisphere plates spinning against each other:
    /// convergent where y < 0, divergent where y > 0.
    fn hemisphere_plates(maturity: f64) -> Vec<Plate> {
        vec![
            Plate {
                id: 0,
                seed_position: [0.0, 0.0, 1.0],
                continental: true,
                euler_axis: [1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
            },
            Plate {
                id: 1,
                seed_position: [0.0, 0.0, -1.0],
                continental: true,
                euler_axis: [-1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
            },
        ]
    }

    #[test]
    fn collision_uplift_towers_over_the_interior_and_decays_inland() {
        let geo = Geosphere::new(3);
        let plates = hemisphere_plates(0.0);
        let plate_of = assign_plates(&geo, &plates);
        let boundaries = boundary_field(&geo, &plate_of, &plates);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        let elevation = assemble_elevation(&geo, &plates, &plate_of, &boundaries, &distances, &[]);
        let mut peak = f64::NEG_INFINITY;
        for (cell, contact) in boundaries.iter() {
            if let Some(c) = contact
                && c.kind == BoundaryKind::ContinentalCollision
            {
                peak = peak.max(*elevation.get(cell));
            }
        }
        assert!(peak > 3000.0, "collision peak {peak} too low");
        for (cell, entry) in distances.iter() {
            if let Some((distance, _)) = entry
                && *distance >= 8
            {
                let e = *elevation.get(cell);
                assert!(
                    (e - 400.0).abs() < 100.0,
                    "cell {} interior elevation {e} strays from base",
                    cell.0
                );
            }
        }
    }

    #[test]
    fn sea_level_hits_a_pinned_ocean_fraction() {
        let geo = Geosphere::new(3);
        let pins = TerrainPins {
            ocean_fraction: Some(0.65),
            ..TerrainPins::default()
        };
        for seed in [1u64, 7, 42] {
            let terrain_seed = Seed(seed).derive(streams::ROOT);
            let plates = generate_plates(terrain_seed, &pins);
            let plate_of = assign_plates(&geo, &plates);
            let boundaries = boundary_field(&geo, &plate_of, &plates);
            let distances = boundary_distance(&geo, &plate_of, &boundaries);
            let elevation = generate_elevation(
                terrain_seed,
                &geo,
                &plates,
                &plate_of,
                &boundaries,
                &distances,
            );
            let sea = derive_sea_level(terrain_seed, &pins, &elevation);
            let below = elevation.iter().filter(|(_, e)| **e < sea).count();
            let achieved = below as f64 / elevation.len() as f64;
            assert!(
                (achieved - 0.65).abs() <= 0.01,
                "seed {seed}: achieved {achieved}"
            );
        }
    }

    #[test]
    fn unrest_is_high_on_young_convergent_boundaries_and_dies_inland() {
        let geo = Geosphere::new(3);
        let plates = hemisphere_plates(0.0);
        let plate_of = assign_plates(&geo, &plates);
        let boundaries = boundary_field(&geo, &plate_of, &plates);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        let unrest = generate_unrest(&geo, &plates, &plate_of, &boundaries, &distances);
        let mut boundary_max = 0.0f64;
        for (cell, contact) in boundaries.iter() {
            if contact.is_some() {
                boundary_max = boundary_max.max(*unrest.get(cell));
            }
        }
        assert!(boundary_max > 0.5, "young boundary max {boundary_max}");
        for (cell, entry) in distances.iter() {
            if let Some((distance, _)) = entry
                && *distance >= 8
            {
                assert!(
                    *unrest.get(cell) < 0.05,
                    "cell {} interior unrest {}",
                    cell.0,
                    unrest.get(cell)
                );
            }
        }
        for (_, u) in unrest.iter() {
            assert!((0.0..=1.0).contains(u));
        }
    }

    #[test]
    fn old_plates_are_quieter_than_young_ones() {
        let geo = Geosphere::new(3);
        let young = hemisphere_plates(0.0);
        let old = hemisphere_plates(1.0);
        let plate_of = assign_plates(&geo, &young);
        let boundaries = boundary_field(&geo, &plate_of, &young);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        let unrest_young = generate_unrest(&geo, &young, &plate_of, &boundaries, &distances);
        let unrest_old = generate_unrest(&geo, &old, &plate_of, &boundaries, &distances);
        let max_young = unrest_young.iter().map(|(_, u)| *u).fold(0.0, f64::max);
        let max_old = unrest_old.iter().map(|(_, u)| *u).fold(0.0, f64::max);
        assert!(
            max_old < max_young,
            "old {max_old} not quieter than young {max_young}"
        );
    }
}
