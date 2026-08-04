//! The Mire's preregistered measurement (spec §6, frozen 2026-08-03, amended
//! 2026-08-04 for delivery mechanism only — H1/H2/H3 themselves are
//! unchanged): does weather-gated conductance (Task 6) actually move world
//! topology, or does the mud/snow/frozen-mire machinery sit latent and
//! unobserved?
//!
//! ## Why this ships as a heavy-tier calibration test, not lab metrics
//!
//! The original plan called for three registered lab metrics plus a
//! `studies/the-mire.study.json`. That turned out not to be viable:
//! `studies/the-census.study.json` and eight other studies declare
//! `"metrics": "all"`, and `windows/lab/src/study.rs:225` resolves
//! `MetricSelection::All(_) => Ok(reg)` — the whole registry, unfiltered, no
//! opt-in. Registering a metric that gates the connection graph would run
//! Task 6's measured ~3.5s-per-world gating cost on every census world too —
//! roughly two hours added to `the-census` alone — and would drift nine
//! studies' committed `rows.csv` and per-metric SVGs. So this measurement
//! ships as the established pattern for an expensive one-off battery: a
//! heavy-tier `#[ignore]`d test, policed by
//! `windows/lab/tests/preregistration_guard.rs`.
//!
//! ## Population (frozen, spec §6)
//!
//! 200 seeds (`1..=200`), default pins (`SkyPins`/`TerrainPins`/
//! `SettlementPins::default()`, `SkyChoice::Generated`), the standard
//! icosphere mesh (`hornvale_terrain::GLOBE_LEVEL`, unconditionally 6 —
//! there is no smaller "study mesh" in this codebase), **land cells only**
//! (`!Biome::is_marine()`), evaluated at 12 days evenly spaced across one
//! converged annual trajectory (`GeneratedClimate::year_length_std() / 12`
//! per step).
//!
//! ## Caching (Task 6's review flagged the trap this guards against)
//!
//! Per world, in order: build once to `BuildDepth::Settlements`
//! (`build_world_to_with_artifacts`, which hands back the terrain and
//! climate the build already produced rather than making the caller
//! re-sculpt via `terrain_of`/`climate_from` — a second sculpt this test
//! does not need to pay); build the **ungated** graph once directly via
//! `hornvale_worldgen::connection_graph` (bypassing `connection_graph_of`,
//! which would re-derive terrain and climate from the world a second time —
//! wasted work when we already hold both); compute the two `SubstrateField`s
//! once each. Only the per-day `ConnectionGraph::scale_conductance` pass and
//! `reachable_regions` walk repeat per sample day — both O(edges), not
//! O(cells × years).
//!
//! ## `DEFAULT_MIN_CONDUCTANCE` — a threshold this test defines, not a
//! shipped default
//!
//! Neither `domains/topology` nor `windows/worldgen` ships a canonical
//! "default" conductance threshold for judging an edge passable — the one
//! existing consumer, `windows/almanac/src/connections.rs`'s
//! `ISOLATION_THRESHOLD = 1e-6`, answers a different question ("is this
//! edge severed outright, e.g. by ocean, at all") and is calibrated to sit
//! *below* the smallest positive baseline conductance so it only ever
//! catches genuinely zero-conductance edges. At that threshold weather
//! gating (which scales an existing positive conductance by a `[0,1]`
//! factor, only reaching exactly zero at full mud+snow saturation) would
//! almost never flip an edge's passability, making the swing this study
//! looks for structurally invisible regardless of whether the mechanism
//! works.
//!
//! So this test defines its own threshold. **A first attempt, and why it was
//! wrong, disclosed here because it changed after a readout:** the first
//! version of this file set `DEFAULT_MIN_CONDUCTANCE` from the flat-terrain
//! baseline established in Task 3 (`hornvale_worldgen::BASE_COST = 10`, so a
//! perfectly flat `Adjacency` edge's unweathered conductance is `1 /
//! BASE_COST = 0.1`) at half that, `0.05`. An ad hoc diagnostic run against
//! the 5-seed pilot (not shipped in this file — a throwaway measurement, not
//! a test) showed this was not a calibration choice about weather at all: the
//! REAL pooled distribution of `Adjacency` edge conductances over seeds
//! 1..=5 (447,475 directed entries) has **p100 = 0.0417** — its maximum,
//! full stop, is already below the assumed threshold. Real sculpted terrain
//! essentially never presents a perfectly flat neighbor pair at this mesh
//! resolution (`SLOPE_SCALE` dominates `BASE_COST` almost everywhere), so
//! `0.05` classified **100% of land-land edges as already-impassable before
//! any weather scaling ran** — the largest region was a constant 40 cells on
//! every single sample day, for every pilot seed. That is a broken
//! instrument (it cannot possibly respond to weather, since scaling can only
//! shrink an already-below-threshold conductance further), not a measurement
//! of H1. This is disclosed here per the brief's instruction to say so
//! explicitly and prominently when a constant changes after a readout —
//! `min_conductance` is not on the off-limits list
//! (`MUD_PENALTY`/`SNOW_PENALTY`/`SNOW_IMPEDING_MM`/either substrate
//! default), and this is fixing a broken instrument, not retuning a
//! prediction: the model was never given a chance to be measured at
//! `0.05`.
//!
//! **The value actually shipped:** `DEFAULT_MIN_CONDUCTANCE = 0.002`,
//! measured as the pooled median `Adjacency`-edge conductance over the same
//! 5-seed pilot (pooled over 447,475 directed entries:
//! p10/p25/p50/p75/p90 = 0.00098/0.00134/0.00194/0.00368/0.00635, rounded to
//! one significant figure). Sitting near the real median puts roughly half
//! of a world's `Adjacency` edges within reach of crossing the threshold as
//! weather's `[0,1]` multiplier moves their conductance, which is the
//! condition under which a seasonal swing is even possible to observe. This
//! was fixed **before** re-running H1/H2/H3 on the pilot a second time, and
//! frozen in the spec (commit `6e56e22a`) **before the full 200-seed run
//! executed at all** — the pilot's own wall-clock extrapolation to 200 seeds
//! came in over the ~60-minute budget on first measurement, so the initial
//! attempt at this task stopped there and reported it rather than running or
//! shrinking the population. Two output-preserving performance fixes
//! (`8ea57283`, `ebf95039` — hoisting a per-call `Fbm` construction, and
//! batching a per-pair hop-radius BFS to per-settlement) landed afterward and
//! brought the full run to ~49 minutes projected, then a real ~17.3-minute
//! `--release` execution (see `task-7-report.md`) completed the actual
//! 200-seed population this test's `SAMPLE` constant declares.
//! `min_conductance` was never adjusted after seeing any H1/H2/H3 numbers at
//! any scale, pilot or full.
//!
//! ## Latitude bands (H2)
//!
//! Three coarse bands of `|latitude|`, degrees: equatorial `[0,30)`,
//! temperate `[30,60)`, polar `[60,90]`. A world with zero land cells in a
//! band contributes no reading for that band from that seed (rather than a
//! spurious `0/0`).
//!
//! ## Anti-vacuity
//!
//! H1 and H2 are NOT a self-comparison: the numerator (the largest
//! conductance-thresholded connected region, `ConnectionGraph::
//! reachable_regions`) is computed independently for each of the 12 sample
//! days from the SAME ungated graph, scaled by that day's *actual* substrate
//! reads (`SubstrateField::at`, `GeneratedClimate::is_frozen_at`) — a day
//! that produced no seasonal change in wetness/snowpack would legitimately
//! produce zero swing, and a day that did produces a nonzero one. Nothing
//! here can pass by construction; see the mutation-testing note below H1/H2
//! for the checked failure mode.
use hornvale_astronomy::SkyPins;
use hornvale_climate::GeneratedClimate;
use hornvale_climate::snowpack::DEFAULT_SNOWPACK;
use hornvale_climate::substrate::SubstrateField;
use hornvale_climate::wetness::{DEFAULT_WETNESS, receptivity};
use hornvale_kernel::{CellId, CellMap, Seed, Value};
use hornvale_terrain::TerrainPins;
use hornvale_topology::{ConnectionGraph, EdgeKind};
use hornvale_worldgen::graph_derive::weather_conductance_factor;
use hornvale_worldgen::{
    BuildDepth, GraphConfig, SettlementPins, SkyChoice, WorldComponents,
    build_world_to_with_artifacts, connection_graph,
};
use std::collections::BTreeSet;

/// The population size the spec froze (§6): seeds `1..=SAMPLE`.
const SAMPLE: u64 = 200;

/// How many days, evenly spaced across one converged annual trajectory, H1
/// and H2 sample per world (spec §6).
const N_DAYS: usize = 12;

/// `|latitude|` band edges in degrees: equatorial, temperate, polar.
const BANDS: [(f64, f64); 3] = [(0.0, 30.0), (30.0, 60.0), (60.0, 90.0)];

/// See the module doc's "`DEFAULT_MIN_CONDUCTANCE`" section for the full
/// story, including the first (broken) attempt this superseded: this is the
/// pooled MEDIAN real `Adjacency`-edge conductance measured over the 5-seed
/// pilot, rounded to one significant figure -- not a physical constant, a
/// measured calibration fixed before the full 200-seed study ran.
fn default_min_conductance() -> f64 {
    0.002
}

/// How many land cells H3 samples per seed — a stride across the full land
/// roster rather than every cell, since `year_of_day_contexts` is O(year
/// days) per cell and the two `SubstrateField`s already pay that cost once
/// over EVERY cell; re-paying it over every land cell a second time (for H3
/// alone) would double a cost this test otherwise avoids. Striding still
/// checks every one of the 200 seeds, which is what H3 asks for
/// ("for every seed in the population") — it does not ask for every cell.
const H3_SAMPLE_STRIDE_TARGET: usize = 24;

/// One built world's cached readout surface: everything the per-day sampling
/// loop needs, computed exactly once per world.
struct WorldSample {
    /// Every land cell (`!Biome::is_marine()`), in ascending `CellId` order.
    land_cells: Vec<CellId>,
    /// `land_cells`, partitioned by `BANDS` (same order).
    land_by_band: [Vec<CellId>; 3],
    /// The converged annual period, standard days.
    year_length: f64,
    /// The unweathered connection graph — `GraphConfig::default()`, `day:
    /// None` — built once.
    ungated: ConnectionGraph,
    /// Surface wetness's converged annual trajectory, every cell, computed
    /// once.
    wetness: SubstrateField,
    /// Snowpack's converged annual trajectory, every cell, computed once.
    snow: SubstrateField,
    /// The reconstructed climate — kept for `is_frozen_at`/`precip_at`/
    /// `year_of_day_contexts` reads, which are all O(1) or O(year-days)
    /// lookups, never a re-sculpt.
    climate: GeneratedClimate,
}

/// Build one world to `BuildDepth::Settlements`, then assemble everything
/// [`WorldSample`] caches. Panics loudly (this is a measurement battery, not
/// production code) if a sampled seed fails to build — a silent skip would
/// quietly shrink the preregistered population.
fn build_sample(seed: u64, wc: &WorldComponents) -> WorldSample {
    let artifacts = build_world_to_with_artifacts(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        BuildDepth::Settlements,
    )
    .unwrap_or_else(|e| panic!("seed {seed} failed to build to BuildDepth::Settlements: {e:?}"));

    let world = artifacts.world;
    let terrain = artifacts
        .terrain
        .expect("BuildDepth::Settlements builds terrain");
    let climate = artifacts
        .climate
        .expect("BuildDepth::Settlements builds climate");

    let geo = terrain.geosphere();
    let elevation = &terrain.globe().elevation;
    let biome = climate.biome_map();
    let current = CellMap::from_fn(geo, |c| climate.current_at(c));

    let settlements: Vec<CellId> = hornvale_settlement::all_settlements(&world)
        .iter()
        .map(
            |s| match world.ledger.value_of(s.id, hornvale_settlement::CELL_ID) {
                Some(Value::Number(n)) => CellId(*n as u32),
                _ => panic!("settlement {} has no cell-id fact", s.id.0),
            },
        )
        .collect();

    let cfg = GraphConfig::default();
    let ungated = connection_graph(geo, elevation, &biome, &current, &settlements, &cfg);

    // Computed ONCE each -- the whole point of this cache. See the module
    // doc's "Caching" section: everything below this point is a lookup, not
    // a recompute.
    let wetness = SubstrateField::compute(&climate, &DEFAULT_WETNESS);
    let snow = SubstrateField::compute(&climate, &DEFAULT_SNOWPACK);

    let land_cells: Vec<CellId> = geo.cells().filter(|&c| !biome.get(c).is_marine()).collect();
    let mut land_by_band: [Vec<CellId>; 3] = Default::default();
    for &c in &land_cells {
        let lat = geo.coord(c).latitude.abs();
        for (band_idx, &(lo, hi)) in BANDS.iter().enumerate() {
            let is_last_band = band_idx == BANDS.len() - 1;
            let in_band = lat >= lo && (lat < hi || is_last_band);
            if in_band {
                land_by_band[band_idx].push(c);
            }
        }
    }

    let year_length = climate.year_length_std();

    WorldSample {
        land_cells,
        land_by_band,
        year_length,
        ungated,
        wetness,
        snow,
        climate,
    }
}

/// The weather-gated graph on `day`: a clone of the cached ungated graph
/// (built once per world), with every `Adjacency`/`LandRoute` edge's
/// conductance scaled by the mean of its endpoints'
/// `weather_conductance_factor` -- reading the two cached `SubstrateField`s
/// by lookup, never recomputing them. Mirrors `connection_graph_of`'s own
/// gating pass exactly (Task 6), so this test measures the shipped
/// mechanism rather than a parallel reimplementation of it.
fn gated_graph(sample: &WorldSample, day: f64) -> ConnectionGraph {
    let mut graph = sample.ungated.clone();
    let factor_at = |cell: CellId| -> f64 {
        let wetness_mm = sample.wetness.at(cell, day);
        let snow_mm = sample.snow.at(cell, day);
        let frozen = sample.climate.is_frozen_at(cell, day);
        weather_conductance_factor(
            receptivity(wetness_mm, DEFAULT_WETNESS.field_capacity_mm),
            snow_mm,
            frozen,
        )
    };
    graph.scale_conductance(|from, edge| match edge.kind {
        EdgeKind::WaterRoute => 1.0,
        EdgeKind::Adjacency | EdgeKind::LandRoute => (factor_at(from) + factor_at(edge.to)) / 2.0,
    });
    graph
}

/// The largest connected region at `min_conductance` -- the "mainland" a
/// day's weather leaves standing. Deterministic: `reachable_regions` already
/// orders its output by each region's minimum `CellId`, so ties in `len()`
/// resolve the same way every run.
fn largest_region(graph: &ConnectionGraph, min_conductance: f64) -> BTreeSet<CellId> {
    graph
        .reachable_regions(min_conductance)
        .into_iter()
        .max_by_key(|r| r.len())
        .unwrap_or_default()
}

/// What share of `cells` sit inside `region`.
fn fraction_in(cells: &[CellId], region: &BTreeSet<CellId>) -> f64 {
    if cells.is_empty() {
        return 0.0;
    }
    let hit = cells.iter().filter(|c| region.contains(c)).count();
    hit as f64 / cells.len() as f64
}

/// `max - min` over a day-indexed series -- the "swing" both H1 and H2 read.
fn swing(values: &[f64]) -> f64 {
    let hi = values.iter().copied().fold(f64::MIN, f64::max);
    let lo = values.iter().copied().fold(f64::MAX, f64::min);
    hi - lo
}

/// The median of a value set, sorted with `total_cmp` (the workspace's
/// deterministic float-sort rule -- no `HashMap`/`HashSet`, no raw `<`
/// comparator that could panic or silently misorder a NaN).
fn median(values: &[f64]) -> f64 {
    assert!(!values.is_empty(), "median of an empty population");
    let mut sorted = values.to_vec();
    sorted.sort_by(f64::total_cmp);
    let n = sorted.len();
    if n % 2 == 1 {
        sorted[n / 2]
    } else {
        (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0
    }
}

/// One seed's H1/H2 readout: the all-land swing, and each band's swing
/// (`None` if that seed carries no land in that band).
struct SeedReadout {
    all_land_swing: f64,
    band_swings: [Option<f64>; 3],
}

fn readout_for(sample: &WorldSample, min_conductance: f64) -> SeedReadout {
    let days: Vec<f64> = (0..N_DAYS)
        .map(|i| i as f64 * sample.year_length / N_DAYS as f64)
        .collect();

    let mut all_land = Vec::with_capacity(N_DAYS);
    let mut band_series: [Vec<f64>; 3] = Default::default();

    for &day in &days {
        let graph = gated_graph(sample, day);
        let region = largest_region(&graph, min_conductance);
        all_land.push(fraction_in(&sample.land_cells, &region));
        for (band, series) in sample.land_by_band.iter().zip(band_series.iter_mut()) {
            if !band.is_empty() {
                series.push(fraction_in(band, &region));
            }
        }
    }

    let band_swings = std::array::from_fn(|b| {
        if band_series[b].len() == N_DAYS {
            Some(swing(&band_series[b]))
        } else {
            None
        }
    });

    SeedReadout {
        all_land_swing: swing(&all_land),
        band_swings,
    }
}

/// The stride [`h3_violations_for`] walks `land_cells` at: at most every
/// cell, but no finer than needed to land roughly
/// [`H3_SAMPLE_STRIDE_TARGET`] samples across the whole land roster.
fn h3_stride(land_cell_count: usize) -> usize {
    (land_cell_count / H3_SAMPLE_STRIDE_TARGET).max(1)
}

/// H3: annual sum of daily precipitation equals `precip_at(cell)`, per cell,
/// within tolerance -- already unit-tested in `domains/climate` at cell
/// scale (`substrate.rs`'s
/// `a_cells_year_of_contexts_reproduces_its_annual_climatology`); this
/// re-confirms it at study scale (every one of the 200 seeds), over a
/// stride of land cells (see [`H3_SAMPLE_STRIDE_TARGET`]'s doc comment for
/// why a stride rather than every cell). Returns `(checked_count,
/// violations)`.
fn h3_violations_for(sample: &WorldSample) -> (usize, Vec<(CellId, f64, f64)>) {
    let stride = h3_stride(sample.land_cells.len());
    let mut checked = 0usize;
    let mut violations = Vec::new();
    for &cell in sample.land_cells.iter().step_by(stride) {
        checked += 1;
        let year = sample.climate.year_of_day_contexts(cell);
        let summed: f64 = year.iter().map(|c| c.precip_mm).sum();
        let annual = sample.climate.precip_at(cell).get();
        let tolerance = annual.abs() * 1e-6 + 1e-6;
        if (summed - annual).abs() > tolerance {
            violations.push((cell, summed, annual));
        }
    }
    (checked, violations)
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn the_mires_preregistered_readout() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let min_conductance = default_min_conductance();

    let mut readouts = Vec::with_capacity(SAMPLE as usize);
    let mut h3_total_checked = 0usize;
    let mut h3_total_violations = Vec::new();

    for seed in 1..=SAMPLE {
        let sample = build_sample(seed, &wc);

        let readout = readout_for(&sample, min_conductance);
        readouts.push(readout);

        let (checked, violations) = h3_violations_for(&sample);
        h3_total_checked += checked;
        for (cell, summed, annual) in violations {
            h3_total_violations.push((seed, cell, summed, annual));
        }
    }

    // --- H1: the swing exists, and is not absurd. ---
    let all_land_swings: Vec<f64> = readouts.iter().map(|r| r.all_land_swing).collect();
    let h1_median = median(&all_land_swings);

    // --- H2 (load-bearing): the swing differentiates |latitude|. ---
    let band_medians: [Option<f64>; 3] = std::array::from_fn(|b| {
        let vals: Vec<f64> = readouts.iter().filter_map(|r| r.band_swings[b]).collect();
        if vals.is_empty() {
            None
        } else {
            Some(median(&vals))
        }
    });

    // --- H3: the invariant. ---
    let h3_ok = h3_total_violations.is_empty();

    eprintln!("=== The Mire: preregistered readout ({SAMPLE} seeds) ===");
    eprintln!(
        "min_conductance = {min_conductance} (measured pooled-median Adjacency-edge conductance, \
         5-seed pilot)"
    );
    eprintln!(
        "H1: all-land swing median = {h1_median:.4} (bounds: [0.05, 0.60]); \
         min = {:.4}, max = {:.4}",
        all_land_swings.iter().copied().fold(f64::MAX, f64::min),
        all_land_swings.iter().copied().fold(f64::MIN, f64::max),
    );
    for (b, (lo, hi)) in BANDS.iter().enumerate() {
        match band_medians[b] {
            Some(m) => eprintln!(
                "H2: band [{lo},{hi}) |latitude| swing median = {m:.4} (n={})",
                readouts
                    .iter()
                    .filter(|r| r.band_swings[b].is_some())
                    .count()
            ),
            None => eprintln!("H2: band [{lo},{hi}) |latitude|: no seed carried land here"),
        }
    }
    eprintln!(
        "H3: {h3_total_checked} cell-seed samples checked, {} violations",
        h3_total_violations.len()
    );
    if !h3_total_violations.is_empty() {
        for (seed, cell, summed, annual) in h3_total_violations.iter().take(5) {
            eprintln!("  seed {seed} cell {cell:?}: daily-summed {summed} vs annual {annual}");
        }
    }

    assert!(
        h1_median >= 0.05,
        "H1 floor: weather-gated conductance did not move world topology (median swing \
         {h1_median:.4} < 0.05) -- a real finding, not a test bug: the drama of weather may \
         be local rather than systemic at min_conductance={min_conductance}"
    );
    assert!(
        h1_median <= 0.60,
        "H1 ceiling: a world whose graph swings {h1_median:.4} of its land connectivity across \
         12 sample days looks like a bug, not a season -- the ceiling is deliberate"
    );

    let present_band_medians: Vec<f64> = band_medians.iter().filter_map(|m| *m).collect();
    assert!(
        present_band_medians.len() >= 2,
        "H2 needs at least two |latitude| bands with land to say anything about how the swing \
         varies across them; only {} band(s) carried land in this population",
        present_band_medians.len()
    );
    for w in present_band_medians.windows(2) {
        assert!(
            w[0] <= w[1],
            "H2 (load-bearing) falsified: swing did not increase monotonically with \
             |latitude| band -- band medians were {present_band_medians:?}. This is the \
             headline finding if it fails, not a bug to fix by retuning MUD_PENALTY, \
             SNOW_PENALTY, SNOW_IMPEDING_MM, or either substrate default."
        );
    }

    assert!(
        h3_ok,
        "H3 (the invariant) failed: {} of {h3_total_checked} cell-seed samples' daily \
         precipitation did not sum to the annual climatology within tolerance -- see the \
         eprintln'd samples above",
        h3_total_violations.len()
    );
}
