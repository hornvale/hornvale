//! The Connection Graph's **cost gate** (Task 5): the size-risk guard.
//! `connection_graph`'s land-route derivation (Task 4) is bounded by
//! construction -- `add_land_routes` only runs `least_cost` on settlement
//! pairs within `GraphConfig::land_route_radius` hops, checked BEFORE the
//! costlier search (see `windows/worldgen/src/graph_derive.rs`'s doc
//! comment) -- but "bounded by construction" is a claim, not a measurement.
//! This battery builds a real seed-42 world to `BuildDepth::Settlements`,
//! derives the graph, and MEASURES (a) wall-time and (b) the total number of
//! land-route `least_cost` attempts (settlement pairs that passed the
//! radius bound), asserting both stay under a budget set comfortably above
//! the measured value (a falsification ceiling, not a target -- see the
//! module doc's "measured" numbers below for what was actually observed).
//!
//! `#[ignore]`d: a live-worldgen build takes minutes, so this is deferred
//! from the commit gate (`make gate`) to `make gate-full`. The light half of
//! this gate -- the same attempt-count measurement, on a small pinned
//! fixture -- runs in `make gate` as
//! `windows/worldgen/tests/graph_derive.rs`'s
//! `land_route_attempts_are_bounded_on_the_fixture`.
//!
//! `tumult_predation_bake_stays_within_budget` is a second, independent
//! battery in this file (The Tumult campaign C3 slice 1, Task 4): it bounds
//! the wall-time of the whole seed-42 `BuildDepth::Settlements` build --
//! which since The Tumult includes the predation bake (raid/flee/roll-
//! downhill), not just the moving-sea per-era graphs it originally measured
//! -- and separately confirms, from the bake's own cascade-size histogram,
//! that relaxation cascades dissipate on their own and are not being
//! silently truncated by `CASCADE_DEPTH_CAP`.
//!
//! ## Measured (seed 42, `BuildDepth::Settlements`, default `GraphConfig`)
//!
//! Recorded once, on this machine (`cargo test --test graph_cost -- --ignored
//! --nocapture`), before the budgets below were chosen:
//!
//! ```text
//! 129 settlements, 1684 land-route attempts (of 8256 possible pairs), 2.6251s wall-time
//! ```
//!
//! The radius bound (`GraphConfig::default().land_route_radius = 12`) is
//! doing real filtering work -- 1684 of 8256 possible settlement pairs
//! (≈20%) -- not merely a no-op, and the whole derivation (adjacency + water
//! routes + all 1684 land-route searches) finishes in ~2.6s. See the
//! constants below for the budgets chosen against these numbers.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, CASCADE_DEPTH_CAP, GraphConfig, SettlementPins, SkyChoice, WorldComponents,
    build_world_to, cascade_sizes, connection_graph_of, history_for, land_route_attempt_count,
    terrain_of,
};
// The measurement harness times ONE derivation call for a diagnostic
// (never sim logic, never a fact, never seeded from wall-clock) -- exempt
// from the wall-clock ban (clippy.toml / decision 0001), same pattern as
// `windows/chronicle/src/measure.rs`'s bake-timing helpers.
#[allow(clippy::disallowed_types)]
// benchmark harness: measuring the derivation, not sim logic
use std::time::Instant;

/// Wall-time budget for one `connection_graph_of` call on a seed-42 world at
/// `BuildDepth::Settlements`. Measured: **2.6251s** (module doc). Budgeted
/// at roughly 5.7x that -- a falsification ceiling for a real regression
/// (e.g. an accidentally-unbounded search or a much slower machine), not a
/// target to approach.
const WALL_TIME_BUDGET_SECS: f64 = 15.0;

/// Land-route `least_cost` attempt-count budget (settlement pairs within
/// `GraphConfig::default().land_route_radius`). Measured: **1684 attempts**
/// against 129 settlements (module doc; `C(129,2) = 8256` possible pairs
/// total, so the radius bound cut the search space by ≈80%). Budgeted at
/// roughly 3x that measured value.
const ATTEMPT_BUDGET: usize = 5000;

/// The cost gate: build seed-42 to `BuildDepth::Settlements`, derive the
/// connection graph, and assert both the measured wall-time and the
/// measured land-route attempt count stay under their budgets. Prints both
/// numbers (`--nocapture`) so a future re-measurement doesn't need to
/// re-derive the harness.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn connection_graph_cost_is_bounded_on_seed_42() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let world = build_world_to(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .expect("seed 42 builds to BuildDepth::Settlements");

    let cfg = GraphConfig::default();

    #[allow(clippy::disallowed_types)] // benchmark harness: measuring the derivation, not sim logic
    let start = Instant::now();
    let _graph = connection_graph_of(&world, &cfg);
    #[allow(clippy::disallowed_types)] // benchmark harness
    let elapsed_secs = start.elapsed().as_secs_f64();

    // Land-route attempts, measured independently of the graph derivation
    // itself (the same settlement-pair/radius bound `add_land_routes` runs,
    // exposed as `land_route_attempt_count` for exactly this purpose).
    let settlements = hornvale_settlement::all_settlements(&world);
    let terrain = terrain_of(&world).expect("world was built with terrain");
    let geo = terrain.geosphere();
    let cells: Vec<hornvale_kernel::CellId> = settlements
        .iter()
        .map(
            |s| match world.ledger.value_of(s.id, hornvale_settlement::CELL_ID) {
                Some(hornvale_kernel::Value::Number(n)) => hornvale_kernel::CellId(*n as u32),
                _ => panic!("settlement {} has no cell-id fact", s.id.0),
            },
        )
        .collect();
    let attempts = land_route_attempt_count(geo, &cells, &cfg);
    let possible_pairs = settlements.len() * settlements.len().saturating_sub(1) / 2;

    eprintln!(
        "connection_graph_cost_is_bounded_on_seed_42: {} settlements, {attempts} land-route \
         attempts (of {possible_pairs} possible pairs), {elapsed_secs:.4}s wall-time",
        settlements.len(),
    );

    assert!(
        elapsed_secs < WALL_TIME_BUDGET_SECS,
        "connection_graph_of took {elapsed_secs:.4}s on seed 42, budget is {WALL_TIME_BUDGET_SECS}s"
    );
    assert!(
        attempts < ATTEMPT_BUDGET,
        "connection_graph_of attempted {attempts} land routes on seed 42, budget is {ATTEMPT_BUDGET}"
    );
}

/// Wall-time budget for the whole `build_world_to(.., BuildDepth::Settlements)`
/// call on a seed-42 world. This build derives 25 per-era connection graphs
/// (the moving-sea bake, `connection_graph_at`, one per `CLIMATE_ERAS` era)
/// AND runs the deep-history bake across them -- which, since The Tumult,
/// resolves conflict as opportunistic predation (raid/flee/roll-downhill)
/// rather than crowding-only migration, alongside everything the existing
/// `connection_graph_cost_is_bounded_on_seed_42` measures separately.
///
/// **Measured** (this machine, `cargo test --test graph_cost -- --ignored
/// tumult --nocapture`, before this budget was chosen): **6.11s**. Budgeted
/// at roughly 4.9x that -- a falsification ceiling for a real regression
/// (predation making the bake itself much costlier, or a much slower/loaded
/// machine), not a target to approach.
const PREDATION_BAKE_BUDGET_SECS: u64 = 30;

/// Upper bound on a single relaxation cascade's SIZE (displacement count),
/// derived from the highest occupied bin of [`cascade_sizes`]'s log2
/// histogram (bin `i` covers sizes `[2^i, 2^(i+1))`, so the highest size a
/// nonempty bin `i` can contain is `2^(i+1) - 1`).
///
/// **Measured**: seed 42 alone fires one cascade of size 1 (this machine,
/// same run as [`PREDATION_BAKE_BUDGET_SECS`]'s measurement) -- a thin
/// single-seed sample, corroborated by Task 3's pooled measurement over
/// seeds 1..=100 (`windows/worldgen/tests/history_tumult.rs`'s module
/// docs): **nothing above size 3** in 2974 conquests. Budgeted at 1/4 of
/// [`CASCADE_DEPTH_CAP`] (256) rather than a multiple of the measured value,
/// because the question this asserts is not "how big do cascades get" (that
/// is Task 3's headline, adjudicated there) but "are they anywhere near
/// being silently truncated by the cap" -- 64 is >20x the largest cascade
/// ever observed, while still catching a real regression long before it
/// would reach 256.
const CASCADE_SIZE_BUDGET: u32 = CASCADE_DEPTH_CAP / 4;

/// The Tumult's predation-bake cost gate (renamed from the moving-sea
/// campaign's `moving_sea_bake_stays_within_budget`, which this build now
/// also exercises): build seed-42 to `BuildDepth::Settlements` and assert
/// the WHOLE build -- 25 per-era connection graphs plus the predation bake
/// that resolves raid/flee/roll-downhill across them -- stays under a
/// wall-time budget. Separately, re-derive the SAME bake via [`history_for`]
/// (documented byte-identical to the settlement stage's own bake -- see its
/// doc comment) to read back [`cascade_sizes`] and confirm no relaxation
/// cascade is anywhere near [`CASCADE_DEPTH_CAP`]: real avalanches on this
/// model dissipate against `VIABLE_MIN` within a hop or two (Task 3's
/// pooled measurement: max size 3 in 2974 conquests), so this asserts they
/// are not secretly being truncated by the depth cap instead. Prints both
/// measurements (`--nocapture`) so a future re-measurement doesn't need to
/// re-derive the harness.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn tumult_predation_bake_stays_within_budget() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

    #[allow(clippy::disallowed_types)] // benchmark harness: measuring the derivation, not sim logic
    let start = Instant::now();
    let _world = build_world_to(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .expect("seed 42 builds to BuildDepth::Settlements");
    #[allow(clippy::disallowed_types)] // benchmark harness
    let elapsed = start.elapsed();

    eprintln!(
        "tumult_predation_bake_stays_within_budget: {elapsed:?} to build seed-42 to \
         BuildDepth::Settlements (25 per-era graphs + predation bake), budget \
         {PREDATION_BAKE_BUDGET_SECS}s"
    );

    assert!(
        elapsed.as_secs() < PREDATION_BAKE_BUDGET_SECS,
        "the predation-bake build regressed: {elapsed:?} to build seed-42 settlements \
         (budget {PREDATION_BAKE_BUDGET_SECS}s)"
    );

    // Re-derive the same bake (byte-identical, per `history_for`'s doc
    // comment) to read back its cascade-size histogram and confirm the
    // depth cap is not the reason chains stop.
    let h = history_for(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
    )
    .expect("seed 42 bakes for the diagnostic history");
    let hist = cascade_sizes(&h);
    let highest_occupied_bin = hist.iter().rposition(|&count| count > 0);
    let max_size_upper_bound = match highest_occupied_bin {
        Some(bin) => (1u32 << (bin + 1)) - 1,
        None => 0,
    };

    eprintln!(
        "tumult_predation_bake_stays_within_budget: cascade_hist {hist:?}, highest occupied bin \
         {highest_occupied_bin:?}, max cascade size <= {max_size_upper_bound} (CASCADE_DEPTH_CAP \
         is {CASCADE_DEPTH_CAP}, budget {CASCADE_SIZE_BUDGET})"
    );

    assert!(
        max_size_upper_bound < CASCADE_SIZE_BUDGET,
        "a relaxation cascade on seed 42 grew to within reach of CASCADE_DEPTH_CAP \
         ({CASCADE_DEPTH_CAP}): histogram {hist:?} implies a cascade of size up to \
         {max_size_upper_bound}, budget {CASCADE_SIZE_BUDGET} -- avalanches are supposed to \
         dissipate against VIABLE_MIN well short of the depth cap, not be silently truncated by it"
    );
}
