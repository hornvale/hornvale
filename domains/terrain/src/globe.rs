//! The tectonic globe: the assembled outcome of terrain genesis. A world is
//! a seed plus a ledger — the globe is never serialized, always re-derived.

use crate::boundaries::{self, CellBoundary};
use crate::crust::{Craton, Terrane};
use crate::elevation::TrailSeamount;
use crate::pins::{self, GenesisError, TerrainPins};
use crate::plates::Plate;
use crate::streams;
use crate::{crust, elevation, plates};
use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation, Seed, math};

/// A generated tectonic globe over the shared Geosphere. Recomputed from
/// the seed on demand; never serialized.
/// type-audit: bare-ok(index: plate_of), bare-ok(ratio: unrest), bare-ok(count: drainage), bare-ok(flag: endorheic), waiver(crust-km-convention: crust), bare-ok(ratio: crust_age), bare-ok(count: boundary_distance), bare-ok(ratio: induration), pending(wave-2: sediment_thickness), pending(wave-2: carve_delta_m), bare-ok(ratio: carve_reroute_fraction), pending(wave-2: trim_ocean_loss_m3)
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
    /// Induration/hardness per cell, `[0,1]` (the Sculpting/Ground seam,
    /// spec §4). Computed before elevation from crust age,
    /// continental-vs-oceanic, and boundary proximity — a pure function, no
    /// new draws — so a later elevation carve can read hardness before it
    /// runs. Agrees with `lithology`'s `induration` axis everywhere (see
    /// `induration_field_matches_the_assembled_buffer`); recomputed at
    /// genesis, never serialized.
    pub induration: CellMap<f64>,
    /// Hotspot trail seamounts (Sculpting Task 6): each drawn hotspot
    /// smeared into an age-progressive chain along its plate's local
    /// velocity, upstream. `age_index == 0` entries are the live hotspot
    /// domes themselves. Retained for Task 9's atolls. Recomputed at
    /// genesis (from the existing hotspot draws — no new draws), never
    /// serialized.
    pub trail_seamounts: Vec<TrailSeamount>,
    /// Deposited sediment thickness per cell, metres (≥ 0; the carve,
    /// Sculpting spec §5/§2 stage 8): repose's receiver-side gains,
    /// routing's floodplain/playa deposit, the marine wedge/delta fill, and
    /// atoll cap material, all summed — NET of the sea-trim (ruling #5c: a
    /// trimmed cell's sediment shrinks by the trimmed meters, floored at
    /// 0). Feeds `lithology`'s `soil_depth` and the `Alluvium` gate (see
    /// `crate::lithology::soil_depth_at` / `classify_rock`). Recomputed at
    /// genesis, never serialized.
    pub sediment_thickness: CellMap<f64>,
    /// The generate-level net elevation delta per cell, metres (± —
    /// incision subtracts, repose/deposition/wedge/delta/atoll all add,
    /// the sea-trim of ruling #5c subtracts again): the full carve + trim
    /// composition, so `elevation == elevation_pre + carve_delta_m` is an
    /// identity. Retained so consumers can see how much of a cell's relief
    /// the carve moved. Recomputed at genesis, never serialized.
    pub carve_delta_m: CellMap<f64>,
    /// Cells a river-mouth delta lobe raised above sea level (the carve,
    /// Sculpting Task 9). Recomputed at genesis, never serialized.
    pub delta_cells: Vec<CellId>,
    /// Cells an atoll rim capped over a drowned seamount (the carve,
    /// Sculpting Task 9); `assemble_material` overrides these cells'
    /// carbonate to a reef-building high value regardless of the ordinary
    /// shallow-shelf test. Recomputed at genesis, never serialized.
    pub atoll_cells: Vec<CellId>,
    /// Waterfall (knickpoint) sites the carve found (Sculpting Task 11, spec
    /// §5): land cells where a high-drainage watercourse crosses a sharp
    /// PRE-carve induration step. Sorted ascending `CellId`. Recomputed at
    /// genesis, never serialized.
    pub waterfall_sites: Vec<CellId>,
    /// The A→B→C escalation diagnostic (Sculpting Task 12, spec §8,
    /// preregistered — a permanent census column): the flux-weighted
    /// fraction of this world's [`crate::carve::REROUTE_TOP_RIVERS`]
    /// largest pre-carve rivers' mainstem cells whose downhill target
    /// changed across the carve (see
    /// [`crate::carve::rerouted_flow_fraction`]). Diagnostic-only —
    /// recomputed at genesis, never serialized. Preregistered thresholds:
    /// **`< 0.10`** engine A is self-consistent, ship it; **`0.10..=0.30`**
    /// flag, Nathan decides whether engine B enters evaluation; **`> 0.30`**
    /// the one-shot lied to itself, A is rejected as sole engine and B
    /// enters.
    pub carve_reroute_fraction: f64,
    /// The sea-trim's booked oceanic loss (ruling #5c): the total volume
    /// proxy `trim_to_sea` removed re-capping marine fill against `sea_1`
    /// (Σ of the per-cell trims' magnitudes, one unit area per cell — the
    /// same volume convention every other carve book uses). Generate-level
    /// composition is carve + trim, so the composed books are the carve's
    /// own totals plus this loss (asserted by
    /// `generate_level_books_account_for_every_eroded_unit` in
    /// tests/carve_properties.rs). Recomputed at genesis, never
    /// serialized.
    pub trim_ocean_loss_m3: f64,
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
    // Induration (the Sculpting/Ground seam, spec §4): a pure function of
    // crust age, continental-vs-oceanic, and boundary proximity, computed
    // here — before elevation — so a later carve pass can read hardness.
    // `assemble_material` (below) calls the same `induration_at` function
    // over the fully-assembled globe; the two must always agree (see
    // `induration_field_matches_the_assembled_buffer`).
    let induration_map = CellMap::from_fn(geosphere, |c| {
        crate::lithology::induration_at(
            *crust_age_map.get(c),
            *continental.get(c),
            boundary_map.get(c).map(|b| b.kind),
            distances.get(c).map(|(hops, _)| hops),
        )
    });
    // Hotspot trails (Sculpting Task 6): smear each drawn hotspot into an
    // age-progressive seamount chain along its plate's local velocity, from
    // the SAME hotspot draws `generate_elevation` used to make internally —
    // computed here (not inside `generate_elevation`) because it needs
    // `plate_of`, which this function already holds.
    let trail_seamounts_list =
        elevation::trail_seamounts(terrain_seed, &plate_list, &plate_of, geosphere);
    // v3 pipeline (spec §2): induration precedes elevation (Task 4);
    // elevation carries decorations (Tasks 1–6); then the carve.
    let elevation_pre = elevation::generate_elevation(
        terrain_seed,
        geosphere,
        &plate_list,
        &plate_of,
        &boundary_map,
        &distances,
        &trail_seamounts_list,
        &crust_map,
        &continental,
        &induration_map,
    );
    let sea_pre = elevation::derive_sea_level(&elevation_pre, effective_ocean);
    let (drainage_pre, endorheic_pre) =
        crate::drainage::drainage_field(geosphere, &elevation_pre, sea_pre);
    let downhill = crate::drainage::downhill_targets(geosphere, &elevation_pre, sea_pre);
    // Carbonate and margin polarity, pre-elevation (mirroring induration's
    // extraction, Task 4): pointwise functions of fields already in hand
    // (continental flag, crust thickness, latitude, plate/position), so the
    // carve can read them before its own elevation output exists.
    // `assemble_material` (below, over the carved globe) calls the same
    // two functions, so the pre-carve fields and the assembled buffer's
    // `carbonate`/`margin` axes can never diverge (barring the atoll
    // carbonate override, which only applies post-carve).
    let carbonate_pre = CellMap::from_fn(geosphere, |c| {
        let lat = math::asin(geosphere.position(c)[2].clamp(-1.0, 1.0)).abs();
        crate::lithology::carbonate_at(*continental.get(c), *crust_map.get(c), lat)
    });
    let margins = CellMap::from_fn(geosphere, |c| {
        let plate = &plate_list[*plate_of.get(c) as usize];
        crate::lithology::margin_polarity(plate, geosphere.position(c), *continental.get(c))
    });
    let carve_params = crate::carve::CarveParams::default();
    let cd = crate::carve::carve(
        geosphere,
        &elevation_pre,
        sea_pre,
        &drainage_pre,
        &endorheic_pre,
        &downhill,
        &induration_map,
        &carbonate_pre,
        &margins,
        &boundary_map,
        &plate_of,
        &trail_seamounts_list,
        &carve_params,
    );
    let elevation_carved = CellMap::from_fn(geosphere, |c| {
        ReferenceElevation::new(elevation_pre.get(c).get() + cd.delta_m.get(c))
            .expect("carved elevation finite")
    });
    // The bounded solve→trim→solve sequence (ruling #5c; decision 0053's
    // percentile-exact discipline unchanged — both solves use the SAME
    // `effective_ocean` target the pre-carve sea level used above).
    // Exactly one trim, exactly two solves, then STOP — a fixed sequence,
    // never iterated to convergence (the one-shot doctrine, like repose's
    // fixed sweep budget).
    //
    // Solve 1: sea level on the carved surface.
    let sea_1 = elevation::derive_sea_level(&elevation_carved, effective_ocean);
    // Trim: re-cap the carve's marine fill against sea_1 (cells the wedge
    // deposited on — ocean by sea_pre, the classification the carve ran
    // with — to sea_1 - wedge_freeboard_m; atoll cells to
    // sea_1 - atoll_freeboard_m; delta lobes exempt). Generate-level
    // composition = carve + trim: the trim applies to elevation AND
    // sediment together (sediment floored at 0), and the trimmed volume is
    // booked as OCEANIC LOSS at this level — retained on the globe as
    // `trim_ocean_loss_m3` — while `cd`'s own books stay internally exact
    // for the carve alone (the mass-balance battery in
    // tests/carve_properties.rs asserts them, and
    // `generate_level_books_account_for_every_eroded_unit` asserts the
    // composed generate-level identity over the retained state).
    let (trim_delta, trim_ocean_loss_m3) = crate::carve::trim_to_sea(
        geosphere,
        &elevation_carved,
        &elevation_pre,
        &cd.sediment_thickness_m,
        &cd.delta_cells,
        &cd.atoll_cells,
        sea_pre,
        sea_1,
        &carve_params,
    );
    let elevation_map = CellMap::from_fn(geosphere, |c| {
        ReferenceElevation::new(elevation_carved.get(c).get() + trim_delta.get(c))
            .expect("trimmed elevation finite")
    });
    let sediment_final = CellMap::from_fn(geosphere, |c| {
        (cd.sediment_thickness_m.get(c) + trim_delta.get(c)).max(0.0)
    });
    // Solve 2 (final): sea level on the trimmed surface. The second solve
    // typically lands a little BELOW sea_1 (the trimmed shelf block that
    // straddled sea_1's rank slides out from under it). Only `shift >= 0`
    // is structural (the trim only lowers cells); the residual staying
    // `<= wedge_freeboard_m` is EMPIRICAL — max observed 39.957 m across a
    // 120-world review sweep (L4/L5/L6 × seeds 1..=40, worst margin
    // 0.043 m at L5 seed 18) — so tuning that changes the freeboards or
    // shelf density must re-verify it. The residual is ACCEPTED — the
    // sequence stops here by ruling; see
    // `trim_recaps_hold_after_the_final_solve` (tests/carve_properties.rs)
    // for the tolerance this bounds.
    let sea_level = elevation::derive_sea_level(&elevation_map, effective_ocean);
    let unrest =
        elevation::generate_unrest(geosphere, &plate_list, &plate_of, &boundary_map, &distances);
    // Final drainage/endorheic, re-derived on the trimmed surface with the
    // final sea level — what the globe retains for every consumer.
    let (drainage, endorheic) =
        crate::drainage::drainage_field(geosphere, &elevation_map, sea_level);
    // The A→B→C escalation diagnostic (Sculpting Task 12, spec §8): both
    // drainage trees are already in scope here, so the fraction is folded
    // in now and only the number retained (`carve_reroute_fraction`), not a
    // second drainage `CellMap` — cheaper, and the globe never needs the
    // pre-carve tree again once this line runs. `downhill_targets` is cheap
    // relative to `drainage_field`, which the line above already paid for
    // to get `drainage`/`endorheic`; calling it once more on the final
    // surface is simpler than plumbing the internal downhill vector out of
    // `drainage_field` itself.
    let post_downhill = crate::drainage::downhill_targets(geosphere, &elevation_map, sea_level);
    let carve_reroute_fraction = crate::carve::rerouted_flow_fraction(
        geosphere,
        &drainage_pre,
        &downhill,
        &drainage,
        &post_downhill,
        sea_pre,
        sea_level,
        crate::carve::REROUTE_TOP_RIVERS,
    );

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
        induration: induration_map,
        trail_seamounts: trail_seamounts_list,
        sediment_thickness: sediment_final,
        // The retained delta is the FULL generate-level composition
        // (carve + trim), so `elevation == elevation_pre + carve_delta_m`
        // stays an identity for consumers.
        carve_delta_m: CellMap::from_fn(geosphere, |c| *cd.delta_m.get(c) + *trim_delta.get(c)),
        delta_cells: cd.delta_cells,
        atoll_cells: cd.atoll_cells,
        waterfall_sites: cd.waterfall_sites,
        carve_reroute_fraction,
        trim_ocean_loss_m3,
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
    fn induration_field_matches_the_assembled_buffer() {
        let geo = Geosphere::new(4);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        for cell in geo.cells() {
            assert_eq!(
                *outcome.globe.induration.get(cell),
                outcome.globe.lithology.get(cell).induration,
                "seam and buffer disagree at {cell:?}"
            );
        }
    }

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
    fn the_carved_globe_has_a_shelf_mode_and_consistent_drainage() {
        let geo = Geosphere::new(5);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        // Shelf: some ocean cells sit in the near-sea band the wedge builds.
        let band = g
            .elevation
            .iter()
            .filter(|(_, e)| {
                let d = g.sea_level.get() - e.get();
                (0.0..=200.0).contains(&d)
            })
            .count();
        assert!(band > 0, "no near-sea-level marine band");
        // Final drainage was computed on the final surface: every land cell's
        // downhill neighbor relationship is consistent with final elevations.
        // (drainage_field already guarantees this; assert its determinism:)
        let (d2, _) = crate::drainage::drainage_field(&geo, &g.elevation, g.sea_level);
        assert_eq!(g.drainage, d2);
        // Sediment field is retained and non-negative.
        assert!(g.sediment_thickness.iter().all(|(_, s)| *s >= 0.0));
        // Soil depth reads real sediment: on the max-sediment land cell,
        // soil depth is positive.
        let (c, _) = g
            .sediment_thickness
            .iter()
            .filter(|(c, _)| *g.elevation.get(*c) >= g.sea_level)
            .max_by(|a, b| a.1.total_cmp(b.1).then(b.0.cmp(&a.0)))
            .unwrap();
        assert!(g.lithology.get(c).soil_depth.get() > 0.0);
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
