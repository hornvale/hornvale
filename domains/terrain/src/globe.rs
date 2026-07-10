//! The tectonic globe: the assembled outcome of terrain genesis. A world is
//! a seed plus a ledger — the globe is never serialized, always re-derived.

use crate::boundaries::{self, CellBoundary};
use crate::pins::{self, GenesisError, TerrainPins};
use crate::plates::Plate;
use crate::streams;
use crate::{elevation, plates};
use hornvale_kernel::{CellMap, Geosphere, Seed};

/// A generated tectonic globe over the shared Geosphere. Recomputed from
/// the seed on demand; never serialized.
/// type-audit: bare-ok(index: plate_of), waiver(elevation-convention: elevation), bare-ok(ratio: unrest), pending(wave-2: sea_level), bare-ok(count: drainage), bare-ok(flag: endorheic)
#[derive(Debug, Clone, PartialEq)]
pub struct TectonicGlobe {
    /// Plate index per cell (an index into `plates`).
    pub plate_of: CellMap<u32>,
    /// Elevation per cell, in meters (bare f64 by documented convention;
    /// see the plan's Global Constraints for the typed-quantities waiver).
    pub elevation: CellMap<f64>,
    /// Unrest per cell, in [0, 1]. Banked for future consumers (spec §15).
    pub unrest: CellMap<f64>,
    /// Sea level, in meters: cells strictly below it are ocean.
    pub sea_level: f64,
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
    let plate_list = plates::generate_plates(terrain_seed, pins);
    let plate_of = plates::assign_plates(geosphere, &plate_list);
    let boundary_map = boundaries::boundary_field(geosphere, &plate_of, &plate_list);
    let distances = boundaries::boundary_distance(geosphere, &plate_of, &boundary_map);
    let elevation_map = elevation::generate_elevation(
        terrain_seed,
        geosphere,
        &plate_list,
        &plate_of,
        &boundary_map,
        &distances,
    );
    let sea_level = elevation::derive_sea_level(terrain_seed, pins, &elevation_map);
    let unrest =
        elevation::generate_unrest(geosphere, &plate_list, &plate_of, &boundary_map, &distances);
    let (drainage, endorheic) =
        crate::drainage::drainage_field(geosphere, &elevation_map, sea_level);

    let mut notes = Vec::new();
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

    Ok(GenesisOutcome {
        globe: TectonicGlobe {
            plate_of,
            elevation: elevation_map,
            unrest,
            sea_level,
            plates: plate_list,
            boundary: boundary_map,
            drainage,
            endorheic,
        },
        notes,
    })
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
        .map(|(_, e)| *e)
        .fold(f64::NEG_INFINITY, f64::max);
    GlobeSummary {
        plate_count: globe.plates.len() as u32,
        ocean_fraction: ocean_cells as f64 / globe.elevation.len() as f64,
        sea_level_m: globe.sea_level,
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
        assert_eq!(s.sea_level_m, outcome.globe.sea_level);
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
}
