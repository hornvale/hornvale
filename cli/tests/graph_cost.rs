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
//! Recorded on this machine (`cargo test --test graph_cost -- --ignored
//! --nocapture`), before the budgets below were chosen. The first line is the
//! original Connection Graph measurement; the second is The Tithe's
//! re-measurement, on a world its tribute mechanism made materially larger:
//!
//! ```text
//! 129 settlements, 1684 land-route attempts (of 8256 possible pairs), 2.6251s wall-time
//! 344 settlements, 10663 land-route attempts (of 58996 possible pairs), 30.5-31.4s wall-time
//! ```
//!
//! The radius bound (`GraphConfig::default().land_route_radius = 12`) is
//! doing real filtering work at both sizes -- 1684 of 8256 possible pairs
//! (≈20%) then, 10 663 of 58 996 (≈18%) now -- not merely a no-op. **The
//! settlement count nearly trebled between the two readings and the budgets
//! were re-baselined for it** (see [`WALL_TIME_BUDGET_SECS`] and
//! [`ATTEMPT_BUDGET`], which carry the counterfactual measurement showing this
//! is a bigger living world rather than a slower derivation).
//!
//! `tithe_tribute_bake_stays_within_budget` is a third battery (The Tithe,
//! living-community C3 slice 2, Task 7 -- spec §8.5): it bounds the
//! **tribute bake's wall-time** and the **relation table's size**, the
//! latter of which nothing in the tree bounded before. See that test's
//! constants for its own measured numbers and for what each ceiling catches.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, CASCADE_DEPTH_CAP, GraphConfig, SettlementPins, SkyChoice, WorldComponents,
    build_world_to, cascade_sizes, census, connection_graph_of, history_for,
    land_route_attempt_count, terrain_of,
};
// The measurement harness times ONE derivation call for a diagnostic
// (never sim logic, never a fact, never seeded from wall-clock) -- exempt
// from the wall-clock ban (clippy.toml / decision 0001), same pattern as
// `windows/chronicle/src/measure.rs`'s bake-timing helpers.
#[allow(clippy::disallowed_types)]
// benchmark harness: measuring the derivation, not sim logic
use std::time::Instant;

/// Wall-time budget for one `connection_graph_of` call on a seed-42 world at
/// `BuildDepth::Settlements`.
///
/// **Re-baselined by The Tithe (Task 7), and the reason is the campaign's own
/// headline.** The original budget was 15.0s against a measured 2.6251s on a
/// world of 129 settlements (module doc). Tribute keeps communities alive that
/// predation alone destroyed, so seed 42 now stands at **344** settlements at
/// `now` where The Tumult's build stood at 203 — and the land-route derivation
/// is superlinear in that count. Re-measured on this machine, three solo runs:
/// **31.397, 30.775, 30.539s**. The counterfactual was taken rather than
/// assumed: with subordination disabled (≈ the pre-Tithe world) the same call
/// on the same code measures **7.972s over 203 settlements** and passes the old
/// budget, so this is a bigger living world and not a slower derivation.
///
/// Budgeted at **90.0s**, ≈2.9× the re-measured value. Kept nearer the
/// measurement than the old 5.7× because 5.7× of 31s would be a three-minute
/// ceiling no plausible regression could reach.
const WALL_TIME_BUDGET_SECS: f64 = 90.0;

/// Land-route `least_cost` attempt-count budget (settlement pairs within
/// `GraphConfig::default().land_route_radius`).
///
/// **Re-baselined by The Tithe (Task 7)**, same cause as
/// [`WALL_TIME_BUDGET_SECS`] above: 129 → 344 settlements. Originally 5000
/// against a measured 1684 attempts; the pre-Tithe world had already drifted to
/// **4209** (measured with subordination disabled) — 84% of that budget — and
/// The Tithe's larger world takes it to **10 663 attempts of `C(344,2)` =
/// 58 996 possible pairs**.
///
/// Budgeted at **30 000**, and chosen against the radius bound's own job rather
/// than as a multiple of the measurement: 30 000 is ≈51% of the possible pairs
/// on today's world, so this ceiling fires exactly when
/// `GraphConfig::land_route_radius` has stopped filtering most of the search
/// space (it filters ≈82% today) — which is the regression the bound exists to
/// catch, and it is stated in pairs rather than in a ratio so it does not
/// silently loosen as the world grows again.
const ATTEMPT_BUDGET: usize = 30_000;

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
    let raided = census(&h).raided;
    let highest_occupied_bin = hist.iter().rposition(|&count| count > 0);
    let max_size_upper_bound = match highest_occupied_bin {
        Some(bin) => (1u32 << (bin + 1)) - 1,
        None => 0,
    };

    eprintln!(
        "tumult_predation_bake_stays_within_budget: cascade_hist {hist:?}, highest occupied bin \
         {highest_occupied_bin:?}, max cascade size <= {max_size_upper_bound} (CASCADE_DEPTH_CAP \
         is {CASCADE_DEPTH_CAP}, budget {CASCADE_SIZE_BUDGET}), over {raided} conquests"
    );

    // Non-vacuity FIRST (The Tumult, final review F-5). The budget assertion
    // below reads "max cascade size <= 0 < budget" on an ALL-ZERO histogram and
    // passes without having looked at anything, so on its own it cannot support
    // the claim its own message makes. What makes the reading real is that the
    // relaxation path ran at all: every conquest calls `Bake::relocate` exactly
    // once, so `raided > 0` means the histogram is a measurement of this
    // world's cascade sizes rather than the absence of a measurement.
    //
    // With that established, an empty histogram is a legitimate outcome and not
    // a hole: it says every displaced people found a home in one hop, i.e. no
    // cascade came anywhere near the depth cap — which is exactly what the
    // budget assertion is here to establish. A floor on the histogram itself is
    // deliberately NOT asserted: seed 42 pools a single cascade, and pinning
    // that would freeze the campaign's own falsification (see
    // `windows/worldgen/tests/history_tumult.rs`).
    assert!(
        raided > 0,
        "predation never fired on seed 42, so the cascade histogram {hist:?} measures \
         nothing and this budget check would pass vacuously"
    );
    assert!(
        max_size_upper_bound < CASCADE_SIZE_BUDGET,
        "a relaxation cascade on seed 42 grew to within reach of CASCADE_DEPTH_CAP \
         ({CASCADE_DEPTH_CAP}): histogram {hist:?} over {raided} conquests implies a cascade \
         of size up to {max_size_upper_bound}, budget {CASCADE_SIZE_BUDGET} -- avalanches are \
         supposed to dissipate against VIABLE_MIN well short of the depth cap, not be silently \
         truncated by it"
    );
}

/// Wall-time budget for one [`history_for`] call on seed 42 — spec §8.5's
/// **bake wall-time**. That call builds to `BuildDepth::Terrain`, reconstructs
/// terrain/climate, derives the per-era graphs and runs the whole deep-history
/// bake, of which The Tithe's tribute path (subordination, collection, flight,
/// revolt, dissolution) is the part this campaign added.
///
/// **Measured** (this machine, debug profile — the profile `make gate-full`
/// runs the heavy tier in — five consecutive solo runs before this budget was
/// chosen): **2.103, 2.388, 3.778, 2.798, 2.106 s**, median **2.39 s**. Under
/// three-way parallel load in the same binary the same call measured **10.002,
/// 1.591, 1.506 s**.
///
/// **State what this catches, because the spread already says what it cannot.**
/// The loaded spread on this box is **6.6×** (1.51 s to 10.00 s) for a bake
/// whose *work* did not change by one instruction, so no ceiling read off a
/// wall-clock here can catch a 2× regression without firing spuriously on an
/// unlucky run. Budgeted at **30 s** — 12.6× the solo median and **3.0× the
/// worst loaded run observed** — which catches an order-of-magnitude cost
/// regression and nothing finer. The concrete shape it is here for: the raid
/// classification in `maybe_raid` runs `self.tribute.values().any(..)` and
/// `.filter(..).count()` **per candidate, per raider, per epoch**, so bake cost
/// carries a `communities × neighbours × relation-table` term. A change that
/// let the relation table grow with the world instead of staying near it turns
/// that term quadratic, and quadratic is what 30 s finds. The same 30 s as
/// [`PREDATION_BAKE_BUDGET_SECS`], deliberately: the two bound overlapping
/// builds and a reader should not have to reconcile two different ceilings.
const TRIBUTE_BAKE_BUDGET_SECS: u64 = 30;

/// Ceiling on the **relation table's size** — spec §8.5's second half, and the
/// quantity nothing in this tree bounded before this test.
///
/// **Measured**: seed 42 stands at **164** tribute relations at `now`, against
/// **344** communities alive to key them (`tribute` is a `BTreeMap` keyed by
/// the *subordinate*, so one patron per community is structural and 344 is the
/// coherent maximum). Budgeted at **400**, ≈2.4× the measured value.
///
/// **A tighter multiple than the wall-time budgets in this file, and
/// deliberately so.** This is a count on a pinned seed: it is deterministic and
/// carries no hardware or load variance at all, so the headroom it needs is
/// against *physics* drift, not noise. 2.4× absorbs a slice that grows the
/// living world by half again — The Tithe itself moved seed 42 from 203 alive
/// to 344 — while still binding.
///
/// **What it was verified to catch** (mutation-verified, T6's ladder): removing
/// the two dissolution lines from `Bake::close`, so relations outlive their
/// parties, takes seed 42 to **482** entries — more entries than the **286**
/// communities then alive to key them — and this ceiling fires on it, with the
/// message naming both numbers.
///
/// **That verification was taken in the release profile, and the reason
/// matters.** In the debug profile — the one `make gate-full` runs the heavy
/// tier in — the same mutation is caught one step earlier, by the bake's own
/// `debug_assert!` that every standing relation names two alive communities.
/// So on that arm this ceiling is the *second* line of defence, not the first.
/// Its own line of defence is the growth shape neither that `debug_assert!` nor
/// `every_standing_relation_names_two_living_communities` can see: a table
/// whose entries are all **live and coherent** and simply too many — chained
/// tribute, or a subordinate acquiring a second patron. Nothing else in the
/// tree would redden on that.
///
/// **What it was verified NOT to be tripped by**, which is why it is a size
/// gate and not a re-statement of a coherence gate: deleting spec §4.3b's
/// `min_vassal` guard, so a patron accepts *any* target however small, moves
/// the table only 164 → **169**. The one-level-star topology (§4.4) pins the
/// table near the live-community count on its own, so this ceiling fires on
/// unbounded growth — a future slice's chained tribute (§9's depth lever), or a
/// keying change admitting more than one patron per subordinate — rather than
/// on ordinary calibration. `every_standing_relation_names_two_living_
/// communities` in `windows/worldgen/tests/history_tithe.rs` owns the coherence
/// reading; this one owns the size.
const RELATION_TABLE_BUDGET: usize = 400;

/// The Tithe's cost gate (spec §8.5): run the seed-42 tribute bake through
/// [`history_for`] — documented byte-identical to the settlement stage's own
/// bake — and assert both its wall-time and the size of the relation table it
/// leaves standing stay under their budgets. Prints those two alongside the
/// collection-event count, the live-community count and the widest star
/// (`--nocapture`) so a future re-measurement doesn't need to re-derive the
/// harness.
///
/// **`tribute_collection_events` and `max_subordinates` are reported and NOT
/// bounded**, each for its own reason. `max_subordinates` is cardinality, which
/// §4.4 leaves deliberately unbounded — a runaway hub is a finding, not a
/// failure. `tribute_collection_events` looks like the cost *integral* (the
/// collection pass is linear in the table, so summing it over epochs is the
/// work the table caused) but it is not one: the pass `continue`s on a dead
/// party **before** incrementing the counter, so under the no-dissolution
/// mutation above the count *falls* (4555 → 3882) while the real iteration
/// count rises. A ceiling on it would have been a number no mutation tried
/// could move, which is decoration. It is printed because it is the right
/// diagnostic for a human re-measuring, not because it is a gate.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn tithe_tribute_bake_stays_within_budget() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

    #[allow(clippy::disallowed_types)] // benchmark harness: measuring the bake, not sim logic
    let start = Instant::now();
    let h = history_for(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
    )
    .expect("seed 42 bakes for the tribute cost gate");
    #[allow(clippy::disallowed_types)] // benchmark harness
    let elapsed = start.elapsed();

    let c = census(&h);
    let relations = h.tribute.len();

    eprintln!(
        "tithe_tribute_bake_stays_within_budget: {elapsed:?} to bake seed-42's deep history \
         (budget {TRIBUTE_BAKE_BUDGET_SECS}s); {relations} standing tribute relations (budget \
         {RELATION_TABLE_BUDGET}) over {} communities alive at now; reported and unbounded: \
         {} collection events, widest star {} subordinates",
        c.alive_at_now, c.tribute_collection_events, c.max_subordinates
    );

    // Non-vacuity FIRST, the same discipline `tumult_predation_bake_stays_
    // within_budget` above carries. A build where subordination never fired
    // leaves an EMPTY relation table, and "0 < 400" passes without having
    // measured anything — the green-and-unreddenable shape this campaign has
    // been bitten by. `subordinations_formed` counts first-time relations over
    // the whole span, so a positive count means the table below is a reading of
    // this world's tribute structure rather than the absence of one.
    assert!(
        c.subordinations_formed > 0,
        "subordination never fired on seed 42, so the relation table is trivially empty and \
         this size budget would pass vacuously"
    );

    assert!(
        elapsed.as_secs() < TRIBUTE_BAKE_BUDGET_SECS,
        "the tribute bake regressed: {elapsed:?} to bake seed-42's deep history (budget \
         {TRIBUTE_BAKE_BUDGET_SECS}s) over {relations} standing relations — see this budget's \
         doc comment for what a ceiling on a wall-clock can and cannot establish"
    );
    assert!(
        relations < RELATION_TABLE_BUDGET,
        "the relation table grew past its size budget (spec §8.5): {relations} standing \
         relations at now (budget {RELATION_TABLE_BUDGET}) over {} communities alive to key \
         them, {} collection events, widest star {} — the table is keyed by the SUBORDINATE and \
         the star topology is one level deep, so it should sit near the live-community count; \
         well above it means relations are outliving their parties or a subordinate has \
         acquired a second patron",
        c.alive_at_now,
        c.tribute_collection_events,
        c.max_subordinates
    );
}
