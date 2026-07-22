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
    BuildDepth, GraphConfig, SettlementPins, SkyChoice, WorldComponents, build_world_to,
    connection_graph_of, land_route_attempt_count, terrain_of,
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
