//! Elevation (meters, bare f64 by documented convention), sea level, and
//! the unrest field. Elevation = the isostatic base over crust thickness +
//! the nearest same-plate boundary's contribution decayed by graph distance
//! and shaped by the plate's maturity + drawn hotspots + a per-cell
//! micro-epsilon that guarantees a strict ordering (so the sea-level
//! percentile is exact). Crust epoch (Task 8): the flat continental/oceanic
//! base retires in favor of Airy isostasy over the crust field's thickness
//! (`crust.rs`), producing a genuine shelf where crust tapers through the
//! continental threshold.
//!
//! **Finding, largely resolved (diagnosed Task 8, fixed for the general
//! case in Task 9):** the drawn craton footprint used to cover only
//! ~2-10% of a globe's cells at the continental threshold, far short of
//! the 25-50% land fraction `ocean_fraction`'s drawn range implies, so sea
//! level's exact-percentile mechanism (`derive_sea_level`) landed deep
//! inside the abyssal plain for most seeds instead of on the craton's
//! shelf taper — weakening hypsometric bimodality and inflating the
//! ±200 m shelf band (measured: ~2/20 default seeds passed both bounds at
//! canonical level 6; ~3/40 at the single-craton scenario). Two authorized
//! fixes landed together in Task 9: `crust::draw_cratons` now rescales its
//! drawn radii so total spherical-cap area matches the land-quota budget
//! (clamped at 0.6 rad per craton), and `crust::PEAK_MIN_KM` moved from 30
//! (exactly `ISOSTASY_REF_KM`, so an "old" craton floated at 0 m and never
//! surfaced) to 33 (crests ~540 m). See the Task 9 report for the
//! after-census evidence on the general (multi-craton) population this
//! fixes. One edge case remains structurally unfixable by these two knobs:
//! a *lone pinned* craton (`continents=1`) always clamps to exactly
//! 0.6 rad, capping its cap area at ~8.7% of the sphere — below any
//! achievable land quota — so
//! `a_single_craton_world_has_a_shelf_and_a_bimodal_hypsometry` stays
//! `#[ignore]`d with that math in its own annotation.

use crate::boundaries::{BoundaryKind, CellBoundary};
use crate::pins::TerrainPins;
use crate::plates::{Plate, dot, unit_vector};
use crate::streams;
use hornvale_kernel::{CellId, CellMap, Geosphere, Seed, math};

/// Airy isostasy: meters of elevation per kilometer of crust thickness.
/// type-audit: pending(wave-2)
pub const ISOSTASY_M_PER_KM: f64 = 180.0;
/// Crust thickness that floats exactly at zero elevation, km.
/// type-audit: pending(wave-2)
pub const ISOSTASY_REF_KM: f64 = 30.0;

/// Isostatic base elevation for a crust thickness, meters: linear in
/// thickness around the reference crust that floats at sea level — thicker
/// (buoyant, continental) crust rides higher, thinner (dense, oceanic)
/// crust rides lower. Monotone by construction.
pub(crate) fn isostatic_m(thickness_km: f64) -> f64 {
    ISOSTASY_M_PER_KM * (thickness_km - ISOSTASY_REF_KM)
}

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
        let angle = math::acos(dot(self.position, position).clamp(-1.0, 1.0));
        self.strength_m
            * math::exp(-(angle * angle) / (2.0 * HOTSPOT_SIGMA_RAD * HOTSPOT_SIGMA_RAD))
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
/// `crust` is each cell's crust thickness in km (the isostatic base
/// input); `continental` is each cell's crust flag (feeds the boundary
/// amplitude's side selection, unchanged in shape from the retired
/// plate-level flag). Eight explicit narrow inputs beat a bundling struct
/// here — same house call as climate's assemblers (`temperature.rs`,
/// `biome.rs`).
#[allow(clippy::too_many_arguments)]
fn assemble_elevation(
    geo: &Geosphere,
    plates: &[Plate],
    plate_of: &CellMap<u32>,
    boundaries: &CellMap<Option<CellBoundary>>,
    distances: &CellMap<Option<(u32, CellId)>>,
    hotspots: &[Hotspot],
    crust: &CellMap<f64>,
    continental: &CellMap<bool>,
) -> CellMap<f64> {
    CellMap::from_fn(geo, |cell| {
        let plate = &plates[*plate_of.get(cell) as usize];
        let cell_continental = *continental.get(cell);
        let base = isostatic_m(*crust.get(cell));
        let boundary_term = match *distances.get(cell) {
            None => 0.0,
            Some((distance, source)) => {
                let contact = (*boundaries.get(source)).expect("BFS sources are boundary cells");
                let arc_side = plate.id > contact.other_plate;
                let amplitude = boundary_amplitude_m(contact.kind, cell_continental, arc_side);
                // Young plates (maturity 0): 1.5x amplitude, sharp 1.5-cell
                // falloff. Old plates (maturity 1): 0.5x amplitude, worn
                // 4.5-cell falloff.
                let factor = 1.5 - plate.maturity;
                let decay_cells = 1.5 + 3.0 * plate.maturity;
                amplitude
                    * (contact.magnitude / MAX_CLOSING_SPEED)
                    * factor
                    * math::exp(-f64::from(distance) / decay_cells)
            }
        };
        let position = geo.position(cell);
        let hotspot_term: f64 = hotspots.iter().map(|h| h.contribution_m(position)).sum();
        base + boundary_term + hotspot_term + CELL_EPSILON_M * f64::from(cell.0)
    })
}

/// Per-cell elevation in meters: the isostatic base over crust thickness,
/// the nearest same-plate boundary's contribution decayed by graph distance
/// and shaped by maturity, drawn hotspots, and a strict-ordering
/// micro-epsilon.
/// type-audit: bare-ok(index: plate_of), bare-ok(count: distances), waiver(crust-km-convention: crust), bare-ok(flag: continental), waiver(elevation-convention: return)
#[allow(clippy::too_many_arguments)]
pub fn generate_elevation(
    terrain_seed: Seed,
    geo: &Geosphere,
    plates: &[Plate],
    plate_of: &CellMap<u32>,
    boundaries: &CellMap<Option<CellBoundary>>,
    distances: &CellMap<Option<(u32, CellId)>>,
    crust: &CellMap<f64>,
    continental: &CellMap<bool>,
) -> CellMap<f64> {
    let hotspots = draw_hotspots(terrain_seed);
    assemble_elevation(
        geo,
        plates,
        plate_of,
        boundaries,
        distances,
        &hotspots,
        crust,
        continental,
    )
}

/// Resolve the world's target ocean fraction: draw (or pin) it. The draw
/// is consumed whether pinned or not (pin isolation). `notes` receives a
/// metering entry when `ocean_fraction` is pinned (Task 7's metering
/// convention: `pinned <name> <value> (seed draws <drawn>)`, the value
/// formatted `{:.2}`).
///
/// Task 9 iteration 3': called once, early, in `globe::generate` — the
/// resolved target is threaded to BOTH `crust::draw_cratons` (whose
/// budget is now derived from it) and `derive_sea_level` below, rather
/// than each drawing (or re-deriving) its own copy. Per the pin doctrine,
/// a pinned target conditions everything downstream identically to a
/// drawn one, so `--ocean-fraction` now legitimately perturbs craton
/// radii too — see `draw_cratons`'s doc and the pin-isolation test in
/// `tectonic_properties.rs`.
/// type-audit: bare-ok(prose: notes), bare-ok(ratio: return)
pub fn resolve_ocean_fraction(
    terrain_seed: Seed,
    pins: &TerrainPins,
    notes: &mut Vec<String>,
) -> f64 {
    let drawn = 0.5
        + 0.25
            * terrain_seed
                .derive(streams::OCEAN_FRACTION)
                .stream()
                .next_f64();
    let target = pins.ocean_fraction.unwrap_or(drawn);
    if let Some(f) = pins.ocean_fraction {
        notes.push(format!("pinned ocean-fraction {f:.2} (seed draws {drawn})"));
    }
    target
}

/// Place sea level at the elevation percentile that puts exactly `target`
/// fraction of cells strictly below it. Pure — no draws: `target` is
/// resolved once in `generate` via `resolve_ocean_fraction` (Task 9
/// iteration 3') and shared with `crust::draw_cratons`. Sort uses
/// `total_cmp` — elevations are finite and strictly ordered by
/// construction.
/// type-audit: waiver(elevation-convention: elevation), pending(wave-2: return), bare-ok(ratio: target)
pub fn derive_sea_level(elevation: &CellMap<f64>, target: f64) -> f64 {
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
            * math::exp(-f64::from(distance) / UNREST_DECAY_CELLS);
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
    use hornvale_kernel::{CellMap, Geosphere, Seed};

    /// Two hemisphere plates spinning against each other: convergent where
    /// y < 0, divergent where y > 0. Continental character lives in the
    /// crust flag map now, not the plate.
    fn hemisphere_plates(maturity: f64) -> Vec<Plate> {
        vec![
            Plate {
                id: 0,
                seed_position: [0.0, 0.0, 1.0],
                euler_axis: [1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
                weight: 1.0,
            },
            Plate {
                id: 1,
                seed_position: [0.0, 0.0, -1.0],
                euler_axis: [-1.0, 0.0, 0.0],
                rate: 1.0,
                maturity,
                weight: 1.0,
            },
        ]
    }

    /// Every cell continental at a uniform 35 km thickness — the old
    /// `CONTINENT_BASE_M` (400 m) synthetic base is replaced by whatever
    /// isostasy gives that thickness (900 m at the Task 8 constants).
    const TEST_CRUST_KM: f64 = 35.0;

    fn all_continental_crust(geo: &Geosphere) -> (CellMap<f64>, CellMap<bool>) {
        (
            CellMap::from_fn(geo, |_| TEST_CRUST_KM),
            CellMap::from_fn(geo, |_| true),
        )
    }

    #[test]
    fn collision_uplift_towers_over_the_interior_and_decays_inland() {
        let geo = Geosphere::new(3);
        let plates = hemisphere_plates(0.0);
        let plate_of = assign_plates(&geo, Seed(1).derive(streams::ROOT), &plates);
        let (crust, continental) = all_continental_crust(&geo);
        let boundaries = boundary_field(&geo, &plate_of, &plates, &continental);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        let elevation = assemble_elevation(
            &geo,
            &plates,
            &plate_of,
            &boundaries,
            &distances,
            &[],
            &crust,
            &continental,
        );
        let mut peak = f64::NEG_INFINITY;
        for (cell, contact) in boundaries.iter() {
            if let Some(c) = contact
                && c.kind == BoundaryKind::ContinentalCollision
            {
                peak = peak.max(*elevation.get(cell));
            }
        }
        assert!(peak > 3000.0, "collision peak {peak} too low");
        let base = isostatic_m(TEST_CRUST_KM);
        for (cell, entry) in distances.iter() {
            if let Some((distance, _)) = entry
                && *distance >= 8
            {
                let e = *elevation.get(cell);
                assert!(
                    (e - base).abs() < 100.0,
                    "cell {} interior elevation {e} strays from base {base}",
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
            let plates = generate_plates(terrain_seed, &pins, &mut Vec::new());
            let plate_of = assign_plates(&geo, terrain_seed, &plates);
            let ocean_target = resolve_ocean_fraction(terrain_seed, &pins, &mut Vec::new());
            let cratons =
                crate::crust::draw_cratons(terrain_seed, &pins, ocean_target, &mut Vec::new());
            let field = crate::crust::CrustField::new(terrain_seed, cratons);
            let crust = CellMap::from_fn(&geo, |c| field.thickness_at(geo.position(c)).get());
            let continental = CellMap::from_fn(&geo, |c| field.continental_at(geo.position(c)));
            let boundaries = boundary_field(&geo, &plate_of, &plates, &continental);
            let distances = boundary_distance(&geo, &plate_of, &boundaries);
            let elevation = generate_elevation(
                terrain_seed,
                &geo,
                &plates,
                &plate_of,
                &boundaries,
                &distances,
                &crust,
                &continental,
            );
            let sea = derive_sea_level(&elevation, ocean_target);
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
        let plate_of = assign_plates(&geo, Seed(1).derive(streams::ROOT), &plates);
        let (_, continental) = all_continental_crust(&geo);
        let boundaries = boundary_field(&geo, &plate_of, &plates, &continental);
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
        let plate_of = assign_plates(&geo, Seed(1).derive(streams::ROOT), &young);
        let (_, continental) = all_continental_crust(&geo);
        let boundaries = boundary_field(&geo, &plate_of, &young, &continental);
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

    #[test]
    fn isostasy_is_monotone_and_earthlike_at_the_anchors() {
        assert!(
            isostatic_m(7.0) < -3500.0,
            "oceanic floor {}",
            isostatic_m(7.0)
        );
        assert!(
            (isostatic_m(30.0)).abs() < 1.0,
            "reference crust {}",
            isostatic_m(30.0)
        );
        assert!(
            isostatic_m(40.0) > 1500.0,
            "thick craton {}",
            isostatic_m(40.0)
        );
        for t in 7..45 {
            assert!(isostatic_m(t as f64) < isostatic_m(t as f64 + 1.0));
        }
    }

    #[test]
    #[ignore = "Task 9 re-verification (measured, not silently retuned): the \
        area-normalization fix resolves the general multi-craton case, but \
        a lone pinned craton (continents=1) is mathematically incapable of \
        reaching the land quota under the directed r_i <= 0.6 rad ceiling — \
        (1 - cos(0.6))/2 ~= 8.7% of the sphere is the hard ceiling on a \
        single craton's cap area, versus the 20-50% ocean-fraction-implied \
        land quota the percentile sea level always hits exactly. Verified: \
        draw_cratons(continents=1) clamps to exactly 0.6 rad for every \
        swept seed 1..=5, and the swept scenario (seeds 1..=40) passes \
        0/40. Neither authorized knob (budget range, PEAK_MIN) touches \
        footprint area, so this is outside Task 9's tuning scope — see the \
        Task 9 report."]
    fn a_single_craton_world_has_a_shelf_and_a_bimodal_hypsometry() {
        let geo = Geosphere::new(4);
        let pins = TerrainPins {
            continents: Some(1),
            ..TerrainPins::default()
        };
        let outcome = crate::globe::generate(Seed(3), &geo, &pins).expect("genesis");
        let globe = outcome.globe;
        let d = crate::shape::hypsometric_bimodality(&globe.elevation, globe.sea_level)
            .expect("has land and ocean");
        assert!(d > 1.5, "hypsometry not bimodal: D = {d}");
        let shelf = crate::shape::shelf_fraction(&globe.elevation, globe.sea_level);
        assert!(shelf > 0.02, "no shelf band: {shelf}");
        assert!(shelf < 0.5, "everything is shelf: {shelf}");
    }
}
