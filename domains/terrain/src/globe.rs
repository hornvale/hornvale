//! The tectonic globe: the assembled outcome of terrain genesis. A world is
//! a seed plus a ledger — the globe is never serialized, always re-derived.

use crate::boundaries::{self, CellBoundary};
use crate::crust::{Craton, Terrane};
use crate::pins::{self, GenesisError, TerrainPins};
use crate::plates::Plate;
use crate::streams;
use crate::{crust, elevation, plates};
use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation, Seed};

/// A generated tectonic globe over the shared Geosphere. Recomputed from
/// the seed on demand; never serialized.
/// type-audit: bare-ok(index: plate_of), bare-ok(ratio: unrest), bare-ok(count: drainage), bare-ok(flag: endorheic), waiver(crust-km-convention: crust), bare-ok(ratio: crust_age), bare-ok(count: boundary_distance)
#[derive(Debug, Clone, PartialEq)]
pub struct TectonicGlobe {
    /// Plate index per cell (an index into `plates`).
    pub plate_of: CellMap<u32>,
    /// Crust thickness per cell, in kilometers (bare f64 under the
    /// `crust-km-convention` type-audit waiver, its own family's future
    /// wave — see decision 0044's roadmap). The field's own `CrustKm`
    /// newtype validates at construction; the per-cell sample here is a
    /// plain `f64` because it feeds bulk numeric assembly, not a single
    /// validated boundary crossing.
    pub crust: CellMap<f64>,
    /// Winning-craton age per cell, in `[0, 1]` (0 on oceanic floor). Sampled
    /// from the same `CrustField` `crust` was, at genesis; never serialized.
    pub crust_age: CellMap<f64>,
    /// Elevation per cell, relative to the isostatic reference datum (see
    /// `hornvale_kernel::ReferenceElevation`).
    pub elevation: CellMap<ReferenceElevation>,
    /// Unrest per cell, in [0, 1]. Banked for future consumers (spec §15).
    pub unrest: CellMap<f64>,
    /// Sea level: cells strictly below it are ocean.
    pub sea_level: ReferenceElevation,
    /// The plates, indexed by `plate_of`'s values.
    pub plates: Vec<Plate>,
    /// The strongest cross-plate boundary contact per cell (`None` for plate
    /// interiors). Recomputed at genesis, never serialized; consumed by
    /// marine biomes via the composition root (spec §6).
    pub boundary: CellMap<Option<CellBoundary>>,
    /// Flow-accumulation drainage per cell (upstream land-cell count; 0 on
    /// ocean). Recomputed at genesis, never serialized.
    pub drainage: CellMap<f64>,
    /// Endorheic mask: land cells whose downhill path never reaches the sea.
    pub endorheic: CellMap<bool>,
    /// The drawn craton set this globe's crust field was built from
    /// (Crust epoch, Task 8). Majors only — microcontinents live in
    /// `microcontinents` instead, so `continental_supply`, `--continents`
    /// metering, and craton-census metrics keep counting majors alone.
    /// Recomputed at genesis, never serialized.
    pub cratons: Vec<Craton>,
    /// The drawn terrane set — accreted exotic slivers welded to
    /// continental margins (Sculpting, spec §3) — this globe's crust
    /// field was built from. Recomputed at genesis, never serialized.
    pub terranes: Vec<Terrane>,
    /// The drawn microcontinent set (Sculpting, spec §3): tiny
    /// ocean-basin cratons sized below the continent-count metric's
    /// floor. Folded into the crust field's craton list alongside
    /// `cratons`, but kept in a separate list here so `cratons` keeps
    /// meaning "majors only". Recomputed at genesis, never serialized.
    pub microcontinents: Vec<Craton>,
    /// Graph distance from each cell to the nearest same-plate boundary
    /// cell, with that boundary attributed. Recomputed at genesis, never
    /// serialized. `None` = no reachable same-plate boundary.
    pub boundary_distance: CellMap<Option<(u32, CellId)>>,
    /// The material buffer per cell (The Ground, spec §2). Recomputed at
    /// genesis, never serialized.
    pub lithology: CellMap<crate::lithology::MaterialBuffer>,
    /// Seed for lithology sub-cell patchiness hash-noise. Hash-noise only —
    /// never consumed as a `Stream`, so it carries no draw-order/save-format
    /// contract (see `streams::LITHOLOGY`).
    pub lithology_seed: Seed,
}

impl TectonicGlobe {
    /// Seed for lithology sub-cell hash-noise (no stream draws — hash-noise
    /// only, like `coast-render`/`plate-edge`).
    pub fn lithology_noise_seed(&self) -> Seed {
        self.lithology_seed
    }
}

/// What tectonic genesis produced: the globe plus degradation notes.
/// type-audit: bare-ok(prose: notes)
#[derive(Debug, Clone, PartialEq)]
pub struct GenesisOutcome {
    /// The generated globe.
    pub globe: TectonicGlobe,
    /// Degradation notes (empty when genesis was untroubled).
    pub notes: Vec<String>,
}

/// Generate a tectonic globe. Derives the terrain root stream from the
/// world seed exactly once; every sub-step consumes its own labeled child
/// stream in a fixed order whether its value is pinned or drawn (pin
/// isolation). Pins fail loudly; generation never retries across seeds —
/// the seed is a world's identity.
pub fn generate(
    world_seed: Seed,
    geosphere: &Geosphere,
    pins: &TerrainPins,
) -> Result<GenesisOutcome, GenesisError> {
    pins::validate(pins)?;
    let terrain_seed = world_seed.derive(streams::ROOT);
    let mut notes = Vec::new();
    let plate_list = plates::generate_plates(terrain_seed, pins, &mut notes);
    // Task 9 iteration 3': the ocean-fraction target is resolved once,
    // here, and threaded to both the craton budget and sea level below —
    // a pinned target conditions both identically to a drawn one.
    let ocean_target = elevation::resolve_ocean_fraction(terrain_seed, pins, &mut notes);
    let cratons = crust::draw_cratons(terrain_seed, pins, ocean_target, &mut notes);
    // Microcontinents (Sculpting, spec §3): a second new drawn set of tiny
    // ocean-basin cratons, sized below the continent-count metric's floor.
    // A new stream label independent of `cratons`'/`terranes`' own, so
    // this consumes no draws the pre-Sculpting draw order relied on. Kept
    // OUT of `cratons` (and so out of `continental_supply`, `--continents`
    // metering, and craton-census metrics — those keep counting majors
    // only) but folded into the crust field's craton list below.
    let micro = crust::draw_microcontinents(terrain_seed, &cratons);
    // Terranes (Sculpting, spec §3): drawn after the craton set, since
    // placement rides on the drawn cratons' rims. A new stream label, so
    // this consumes no draws the pre-Sculpting draw order relied on.
    let terranes = crust::draw_terranes(terrain_seed, &cratons, &plate_list);
    // Single-craton hypsometry: soften an unreachable land quota to the
    // shelf break instead of drowning the percentile into the abyss.
    let supply = crust::continental_supply(&cratons);
    let effective_ocean = elevation::effective_ocean_target(ocean_target, supply, &mut notes);
    let field = crust::CrustField::new_with_terranes(
        terrain_seed,
        [cratons.clone(), micro.clone()].concat(),
        terranes.clone(),
    );
    let crust_map = CellMap::from_fn(geosphere, |c| {
        field.thickness_at(geosphere.position(c)).get()
    });
    let crust_age_map = CellMap::from_fn(geosphere, |c| field.age_at(geosphere.position(c)));
    let continental = CellMap::from_fn(geosphere, |c| field.continental_at(geosphere.position(c)));
    let plate_of = plates::assign_plates(geosphere, terrain_seed, &plate_list);
    let boundary_map = boundaries::boundary_field(geosphere, &plate_of, &plate_list, &continental);
    let distances = boundaries::boundary_distance(geosphere, &plate_of, &boundary_map);
    let elevation_map = elevation::generate_elevation(
        terrain_seed,
        geosphere,
        &plate_list,
        &plate_of,
        &boundary_map,
        &distances,
        &crust_map,
        &continental,
    );
    let sea_level = elevation::derive_sea_level(&elevation_map, effective_ocean);
    let unrest =
        elevation::generate_unrest(geosphere, &plate_list, &plate_of, &boundary_map, &distances);
    let (drainage, endorheic) =
        crate::drainage::drainage_field(geosphere, &elevation_map, sea_level);

    let mut populated = vec![false; plate_list.len()];
    for (_, plate) in plate_of.iter() {
        populated[*plate as usize] = true;
    }
    let empty = populated.iter().filter(|p| !**p).count();
    if empty > 0 {
        notes.push(format!("{empty} plate(s) hold no cells at this resolution"));
    }
    if boundary_map.iter().all(|(_, b)| b.is_none()) {
        notes.push("no plate boundaries at this resolution".to_string());
    }

    // Lithology (The Ground, spec §2) is a pure function of the assembled
    // globe's other fields, so the globe is built first with a placeholder
    // buffer, then the real buffer is assembled and swapped in. The noise
    // seed here is hash-noise only (`streams::LITHOLOGY`) — never consumed
    // as a `Stream`, so this is not a new draw-order contract.
    let lithology_seed = terrain_seed.derive(streams::LITHOLOGY);
    let placeholder_lithology = CellMap::from_fn(geosphere, |_| crate::lithology::MaterialBuffer {
        silica: 0.0,
        grain: 0.0,
        induration: 0.0,
        carbonate: 0.0,
        metamorphic_grade: 0.0,
        porosity: 0.0,
        margin: crate::lithology::MarginPolarity::Interior,
        soil_depth: crate::lithology::SoilDepth::new(0.0),
        basement: crate::lithology::Basement::Oceanic,
        thaumic: 0.0,
    });

    let mut globe = TectonicGlobe {
        plate_of,
        crust: crust_map,
        crust_age: crust_age_map,
        elevation: elevation_map,
        unrest,
        sea_level,
        plates: plate_list,
        boundary: boundary_map,
        drainage,
        endorheic,
        cratons,
        terranes,
        microcontinents: micro,
        boundary_distance: distances,
        lithology: placeholder_lithology,
        lithology_seed,
    };
    globe.lithology = crate::lithology::assemble_material(geosphere, &globe);

    Ok(GenesisOutcome { globe, notes })
}

/// Headline numbers of a globe, for facts and the almanac.
/// type-audit: bare-ok(count: plate_count), bare-ok(ratio: ocean_fraction), pending(wave-2: sea_level_m), pending(wave-2: highest_elevation_m)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GlobeSummary {
    /// How many plates the globe drew (or was pinned to).
    pub plate_count: u32,
    /// Achieved ocean fraction: cells strictly below sea level, over all cells.
    pub ocean_fraction: f64,
    /// Sea level, meters.
    pub sea_level_m: f64,
    /// Highest cell elevation, meters.
    pub highest_elevation_m: f64,
}

/// Summarize a globe's headline numbers. Deterministic: iteration is in
/// ascending cell order and elevations are finite.
pub fn summarize(globe: &TectonicGlobe) -> GlobeSummary {
    let ocean_cells = globe
        .elevation
        .iter()
        .filter(|(_, e)| **e < globe.sea_level)
        .count();
    let highest = globe
        .elevation
        .iter()
        .map(|(_, e)| e.get())
        .fold(f64::NEG_INFINITY, f64::max);
    GlobeSummary {
        plate_count: globe.plates.len() as u32,
        ocean_fraction: ocean_cells as f64 / globe.elevation.len() as f64,
        sea_level_m: globe.sea_level.get(),
        highest_elevation_m: highest,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::{GenesisError, TerrainPins};
    use hornvale_kernel::{Geosphere, Seed};

    #[test]
    fn genesis_is_deterministic() {
        let geo = Geosphere::new(3);
        let a = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let b = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        assert_eq!(a, b);
    }

    #[test]
    fn different_seeds_produce_different_globes() {
        let geo = Geosphere::new(3);
        let a = generate(Seed(1), &geo, &TerrainPins::default()).unwrap();
        let b = generate(Seed(2), &geo, &TerrainPins::default()).unwrap();
        assert_ne!(a.globe.elevation, b.globe.elevation);
    }

    #[test]
    fn summary_reports_the_globe() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let s = summarize(&outcome.globe);
        assert_eq!(s.plate_count as usize, outcome.globe.plates.len());
        assert!((0.4..=0.8).contains(&s.ocean_fraction));
        assert_eq!(s.sea_level_m, outcome.globe.sea_level.get());
        assert!(s.highest_elevation_m > s.sea_level_m);
    }

    #[test]
    fn invalid_pins_fail_loudly_and_never_retry() {
        let geo = Geosphere::new(2);
        let err = generate(
            Seed(1),
            &geo,
            &TerrainPins {
                plates: Some(1),
                ..TerrainPins::default()
            },
        )
        .unwrap_err();
        assert!(matches!(err, GenesisError::InvalidPin { .. }));
        let err = generate(
            Seed(1),
            &geo,
            &TerrainPins {
                ocean_fraction: Some(1.5),
                ..TerrainPins::default()
            },
        )
        .unwrap_err();
        assert!(matches!(err, GenesisError::InvalidPin { .. }));
    }

    #[test]
    fn pinned_values_are_metered_in_the_notes() {
        let geo = Geosphere::new(3);
        let pins = TerrainPins {
            plates: Some(12),
            ocean_fraction: Some(0.80),
            continents: Some(5),
            ..TerrainPins::default()
        };
        let outcome = generate(Seed(42), &geo, &pins).expect("genesis");
        let metered: Vec<&String> = outcome
            .notes
            .iter()
            .filter(|n| n.starts_with("pinned "))
            .collect();
        assert!(
            metered
                .iter()
                .any(|n| n.starts_with("pinned plates 12 (seed draws "))
        );
        assert!(
            metered
                .iter()
                .any(|n| n.starts_with("pinned ocean-fraction 0.80 (seed draws "))
        );
        assert!(
            metered
                .iter()
                .any(|n| n.starts_with("pinned continents 5 (seed draws "))
        );
        let unpinned = generate(Seed(42), &geo, &TerrainPins::default()).expect("genesis");
        assert!(!unpinned.notes.iter().any(|n| n.starts_with("pinned ")));
    }

    #[test]
    fn a_supply_limited_world_meters_the_shelf_break_fallback() {
        let geo = Geosphere::new(3);
        let pins = TerrainPins {
            continents: Some(1),
            ..TerrainPins::default()
        };
        let outcome = generate(Seed(3), &geo, &pins).expect("genesis");
        assert!(
            outcome.notes.iter().any(|n| n.contains("shelf break")),
            "fallback never engaged: {:?}",
            outcome.notes
        );
        // Default worlds never trip it — the genesis half of the
        // byte-identity guard (the craton-level half sweeps 64 seeds in
        // tectonic_properties.rs).
        let default = generate(Seed(3), &geo, &TerrainPins::default()).expect("genesis");
        assert!(!default.notes.iter().any(|n| n.contains("shelf break")));
    }
}
