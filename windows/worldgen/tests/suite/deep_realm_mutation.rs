//! The Deep Realm, Task 7: the mutation proof.
//!
//! The program's shared acceptance criterion: a green test proves the code
//! ran; only the mutation proves the axis is visible (spec §7, H3).
//!
//! **Two halves, both required (spec §7.1).** Over 30 seeds, Task 0 measured
//! `Horizon::Regolith` at 0 of 55,947 live caves — the generator never
//! produces the shallow cave the naive translation of H3 fabricates. A
//! mutation that only swaps a hand-built `Cave`'s `deepest_horizon` between two
//! literals proves `chamber_exists`/`chamber_at` read their `cave` argument
//! (the **derivation** half); it says nothing about whether the **pipeline**
//! that builds a world ever hands them the value terrain actually authored,
//! rather than some default or constant. This campaign's sibling (The
//! Tolerance) shipped exactly that gap: a mutation proof passed while every
//! world under test carried a fabricated parameter no author had written.
//! Both halves are here for that reason.

#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Band, CellId, Seed};
use hornvale_terrain::{Cave, CaveKind, GeothermalGradient, Horizon, TerrainPins, rung_at_depth};
use hornvale_worldgen::chamber::{BRANCHES_PER_SYSTEM, ChamberAddr, chamber_exists};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

/// The permanent lattice ladder, top to bottom — **the DELVE ladder since
/// `chamber/v2`** (The Underworld, spec §4.1), which is what
/// `ChamberAddr.band` indexes. `chamber.rs`'s own `rung_rank`/`rung_of_rank`
/// bijection is private to that module, so this file restates the same
/// five-variant order rather than reaching for it; the order itself is pinned
/// independently by `hornvale_terrain::rungs`.
///
/// It used to be `Horizon`'s five variants, because the gate used to be
/// `band_rank(cave.deepest_horizon)`. Restating a ladder is only safe while the
/// restatement names the ladder the gate actually uses — this file is the
/// reason that caveat is worth writing down.
const BAND_LADDER: [Band; 5] = [
    Band::Undercroft,
    Band::Shallows,
    Band::Deeps,
    Band::Underdeep,
    Band::Nadir,
];

/// The geothermal gradient every hand-built fixture below is placed under,
/// K/km — 24.0, the measured median band across the three preregistered seeds
/// (gradient p50 = 24.419 / 25.004 / 23.082, `underworld_ladder_probe.rs`).
/// `chamber_exists` gates on the delve ladder, so a budget in metres only
/// decides a cave's reach once a cell's gradient turns it into a ΔT.
fn fixture_gradient() -> GeothermalGradient {
    GeothermalGradient::new(24.0)
}

/// The column the hand-built caves below are built against: 401 m of cover on
/// 35 km of continental crust, an ordinary land column the generator produces
/// in quantity. Band tops `[0, 1, 401, 17700.5, 35000]` m.
fn fixture_column() -> hornvale_terrain::StratigraphicColumn {
    hornvale_terrain::column(
        35.0,
        0.3,
        true,
        400.0,
        1.0,
        hornvale_terrain::RockClass::Sandstone,
        hornvale_terrain::Basement::Continental,
    )
}

/// A cave with no formation opinion — `Cave::kind` is not read by
/// `chamber_exists`/`chamber_at` (see `chamber.rs`'s module doc) — carrying a
/// depth budget of `reach_m`, with its `deepest_horizon` **derived from that
/// budget** by `Cave::from_reach`.
///
/// The derivation is the point (The Underworld, spec §4.0). This used to be
/// `cave_reaching(band)`, a struct literal pairing a chosen band with a fixed
/// 1 km budget — which for `Roots` names a world no generator can produce, and
/// which would let this test go green over a broken model the moment anything
/// downstream reads the budget instead of the band.
fn cave_reaching_m(reach_m: f64) -> Cave {
    Cave::from_reach(CaveKind::Fracture, reach_m, &fixture_column())
}

/// Every chamber address that exists over all five bands **of floor 0** at
/// `(seed, cell)`, under `cave`'s budget. Walks all five bands regardless of
/// `cave.deepest_horizon` — `chamber_exists` itself gates on the budget, so a
/// full walk measures exactly what the budget lets through rather than
/// baking the ladder's shape into this helper too.
///
/// **Floor 0, not the whole lattice** (The Stope, `chamber/v3`): the lattice
/// admits `FLOORS_PER_RUN_CEILING` floors per run, so this is 1/20 of the
/// address space — and since Task 2's per-run floor draw, floor 0 is also the
/// only floor EVERY run admits (every band's frozen range has a minimum of at
/// least 1), which is what keeps the two arms below sampling the same
/// population rather than two differently-truncated ones.
/// Every caller here uses this helper COMPARATIVELY — a deep
/// cave's count against a shallow one's, an authored budget's against a
/// fabricated one's — and both arms sample the identical slice under the
/// identical density, so the comparison is sound and the slice is not a
/// confound. What this number is NOT is the count of chambers under a cell;
/// do not read it as one.
fn chamber_count(seed: Seed, cave: &Cave, gradient: GeothermalGradient, cell: CellId) -> usize {
    let mut count = 0usize;
    for band in 0..BAND_LADDER.len() as u8 {
        for branch in 0..BRANCHES_PER_SYSTEM {
            let addr = ChamberAddr {
                cell,
                entrance: 0,
                band,
                branch,
                floor: 0,
            };
            if chamber_exists(seed, cave, gradient, addr) {
                count += 1;
            }
        }
    }
    count
}

/// The deepest band with at least one existing chamber at `(seed, cell)`
/// under `cave`'s budget, searching **floor 0** for the same reason
/// [`chamber_count`] does — its callers compare two arms over the identical
/// slice. `None` if no chamber exists at all. Existence is
/// sparse (a coin-flip density per address), so an arbitrary probe cell can
/// legitimately come back empty; callers that need a guaranteed nonempty
/// result pick a `(seed, cell)` this is known to return `Some` for.
fn deepest_reached(
    seed: Seed,
    cave: &Cave,
    gradient: GeothermalGradient,
    cell: CellId,
) -> Option<Band> {
    (0..BAND_LADDER.len() as u8).rev().find_map(|band| {
        let reached = (0..BRANCHES_PER_SYSTEM).any(|branch| {
            chamber_exists(
                seed,
                cave,
                gradient,
                ChamberAddr {
                    cell,
                    entrance: 0,
                    band,
                    branch,
                    floor: 0,
                },
            )
        });
        reached.then_some(BAND_LADDER[band as usize])
    })
}

/// A budget stopping inside the cover — derives `Horizon::Cover`, and the
/// `Shallows` rung at [`fixture_gradient`].
const SHALLOW_REACH_M: f64 = 200.0;

/// The fabricated budget the pipeline half downgrades a real cave to: 1 m.
///
/// Chosen to land on the top rung under ANY gradient in the physical band —
/// 1 m is ΔT = 0.015 K at 15 K/km and 0.030 K at 30 K/km, both inside
/// `Undercroft`'s `[0, 2)` K — so the mutation is a genuine downgrade for
/// every cell the probe could pick, not only for the one it happens to. The
/// real measured minimum reach is 200 m (`underworld_ladder_probe.rs`,
/// reach-p10 = 200.0 / 215.3 / 201.7), so no generated cave is anywhere near
/// it.
const FABRICATED_REACH_M: f64 = 1.0;
/// A budget cutting past the 401 m basement contact — derives
/// `Horizon::Basement` (the deepest band a metre budget can reach), and the
/// `Underdeep` rung at [`fixture_gradient`], two rungs below
/// [`SHALLOW_REACH_M`].
const DEEP_REACH_M: f64 = 2000.0;

/// **The derivation half.** Two hand-built `Cave`s differing only in their
/// depth budget prove `chamber_exists`/`chamber_at` read their `cave`
/// argument: the deeper budget grows a strictly larger chamber graph, and the
/// shallow cave never reaches past its own in-budget bands.
///
/// **Re-derived by The Underworld (spec §4.0), and it got stronger.** The pair
/// used to be `Roots` against `Regolith`, and its own doc conceded that
/// `Regolith` "never occurs in a live-generated cave (0 of 55,947)" — a
/// mutation over a value no real world holds. `Roots` is now unreachable too
/// (17.7 km against a 3 km budget). Both ends of the comparison are now bands
/// the generator actually produces in quantity: over 30 worlds, `Cover` 1,830
/// and `Basement` 46,486. That closes half of the caveat this test used to
/// carry.
///
/// **What it still does NOT prove**, and why the pipeline half below exists:
/// a hand-built pair says nothing about whether the pipeline hands
/// `chamber_exists` the budget terrain actually authored.
/// claim: behavior(seed-pooled) — a deeper depth budget admits strictly more
/// chamber addresses than a shallower one, pooled over a small fixed seed set
/// because one seed separates adjacent bands by only a chamber or two
/// (seedless sweep: builds no world)
#[test]
fn a_shallow_cave_has_a_shallow_graph() {
    let seed = Seed(90210);
    let cell = CellId(9);

    let deep_cave = cave_reaching_m(DEEP_REACH_M);
    let shallow_cave = cave_reaching_m(SHALLOW_REACH_M);
    // The fixtures must actually differ on the axis the GATE reads, or the
    // comparison is vacuous. Both are asserted: the band pair is the archive
    // fact these fixtures have always carried, and the rung pair is what
    // `chamber_exists` now gates on.
    assert_eq!(deep_cave.deepest_horizon, Horizon::Basement);
    assert_eq!(shallow_cave.deepest_horizon, Horizon::Cover);
    assert_eq!(
        rung_at_depth(deep_cave.depth_reach_m, fixture_gradient()),
        Band::Underdeep
    );
    assert_eq!(
        rung_at_depth(shallow_cave.depth_reach_m, fixture_gradient()),
        Band::Shallows
    );

    let deep = chamber_count(seed, &deep_cave, fixture_gradient(), cell);
    let shallow = chamber_count(seed, &shallow_cave, fixture_gradient(), cell);
    println!(
        "derivation half (seed {}, cell {}): {DEEP_REACH_M} m budget (Basement) \
         = {deep} chambers, {SHALLOW_REACH_M} m budget (Cover) = {shallow} chambers",
        seed.0, cell.0
    );
    assert!(
        deep > shallow,
        "the {DEEP_REACH_M} m budget gave {deep}, the {SHALLOW_REACH_M} m budget gave {shallow}"
    );

    // Existence is a coin flip per address, so one (seed, cell) separates the
    // two budgets by only a chamber or two — 6 against 5 here. Pooling several
    // seeds makes the claim about the BUDGET rather than about one draw. The
    // retired Roots/Regolith pair differed by three whole bands and did not
    // need this; Basement/Cover differ by one, and does.
    let (mut pooled_deep, mut pooled_shallow) = (0usize, 0usize);
    for raw in [90210u64, 1, 2, 3, 4, 5, 6, 7] {
        pooled_deep += chamber_count(Seed(raw), &deep_cave, fixture_gradient(), cell);
        pooled_shallow += chamber_count(Seed(raw), &shallow_cave, fixture_gradient(), cell);
    }
    println!("  pooled over 8 seeds: deep = {pooled_deep}, shallow = {pooled_shallow}");
    assert!(
        pooled_deep > pooled_shallow,
        "pooled over 8 seeds the deeper budget gave {pooled_deep} and the \
         shallower {pooled_shallow} — the budget is not being read"
    );

    // …and the shallow cave never reaches past its own in-budget rungs.
    let reached = deepest_reached(seed, &shallow_cave, fixture_gradient(), cell);
    assert!(
        matches!(reached, Some(Band::Undercroft) | Some(Band::Shallows)),
        "a Shallows-budget cave reached {reached:?} — either the budget is not \
         being read, or no chamber exists at all and this arm is vacuous"
    );
}

/// **The pipeline half.** Builds one real world to `BuildDepth::Terrain`
/// and reads a real cell's real `Cave` back out through
/// `GeneratedTerrain::cave_at` — the exact accessor the shipped consumer
/// (`windows/vessel`'s `chamber_column_here`) calls before handing the
/// result to `chamber_at`. This is deliberately NOT a hand-built `Cave`
/// literal: it is whatever the live generator actually authored for this
/// cell, at whatever band that happens to be (never `Regolith`, per Task 0).
///
/// The mutation: a **fabricated** copy of that same real cave, with
/// `depth_reach_m` forced down to [`FABRICATED_REACH_M`] — a budget the
/// generator did not author for this cell. If chamber_exists (or anything
/// upstream of it) silently substituted a default/constant budget instead of
/// the one terrain measured, feeding it the *real* cave and the *fabricated*
/// one would be indistinguishable. It is not: the real, terrain-authored
/// budget grows a strictly larger graph and reaches strictly deeper than the
/// fabricated downgrade of the identical cave.
///
/// **The mutated FIELD moved in The Underworld, and it had to.** This used to
/// force `deepest_horizon` down to `Regolith`, because the gate was
/// `band_rank(cave.deepest_horizon)`. Since `chamber/v2` the gate reads
/// `depth_reach_m` and classifies it on the delve ladder, so a `deepest_horizon`
/// mutation now perturbs nothing the gate consults — it would have left this
/// test green while proving nothing at all, which is the precise failure mode
/// the whole file exists to prevent (a mutation proves only what it
/// perturbs). The violation is the same shape as before: an invariant-breaking
/// `(band, reach)` pair, built the one sanctioned way, only now broken from
/// the other side.
///
/// **Scope note, stated rather than silently assumed:** this crate cannot
/// reach `windows/vessel`'s `delve_at` directly — `vessel` depends on
/// `worldgen`, so the reverse dependency this test would need is a layering
/// cycle `cli/tests/architecture.rs` forbids. The strongest check available
/// from inside `hornvale-worldgen` is therefore to drive `chamber_exists`
/// with a REAL, terrain-authored `Cave` (via the same `cave_at` accessor
/// vessel calls) rather than a hand-built one, and confirm the mutation
/// still reddens. Read directly (`windows/vessel/src/session.rs`,
/// `chamber_column_here` and `delve_at`), the actual call site passes
/// `terrain.cave_at(cell)`'s result straight through with no intermediate
/// reconstruction, so there is no assembly seam left unexercised there —
/// but that is a code-reading argument, not something this test can assert.
#[test]
fn the_pipeline_hands_chamber_exists_the_budget_terrain_actually_authored() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let artifacts = build_world_to_with_artifacts(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Terrain,
    )
    .expect("seed 42 builds to BuildDepth::Terrain");
    let terrain = artifacts
        .terrain
        .expect("BuildDepth::Terrain sculpts terrain");
    let seed = artifacts.world.seed;
    let geo = terrain.geosphere();

    let (cell, real_cave) = geo
        .cells()
        .filter(|&c| !terrain.is_ocean(c))
        .find_map(|c| terrain.cave_at(c).map(|cave| (c, cave)))
        .expect("seed 42 has at least one land cave cell at BuildDepth::Terrain");

    let real_gradient = terrain.geothermal_gradient_at(cell);
    assert!(
        rung_at_depth(real_cave.depth_reach_m, real_gradient) > Band::Undercroft,
        "cell {cell:?}'s real cave already sits on the top rung ({} m at {} K/km), \
         so a downgrade to {FABRICATED_REACH_M} m is not a genuine mutation; pick \
         a different cell/seed",
        real_cave.depth_reach_m,
        real_gradient.get()
    );

    // ┌──────────────────────────────────────────────────────────────────────┐
    // │ THE ONE DELIBERATE VIOLATION of `Cave`'s derived-field invariant in   │
    // │ the whole workspace. `Cave` is `#[non_exhaustive]`, so a struct       │
    // │ literal here does not compile at all (E0639) — reaching for           │
    // │ `from_parts_unchecked` is the only way, and that is the point: the    │
    // │ violation is named at the call site instead of looking like ordinary  │
    // │ construction. If this is ever the SECOND caller of that function,     │
    // │ something has gone wrong.                                             │
    // └──────────────────────────────────────────────────────────────────────┘
    //
    // It is the content of the mutation: force `depth_reach_m` away from the
    // budget terrain authored while leaving `deepest_horizon` where it was,
    // producing a pair the generator cannot author. If anything downstream
    // substituted a default budget for terrain's, the real and fabricated
    // caves would be indistinguishable.
    let fabricated_cave = Cave::from_parts_unchecked(
        real_cave.kind,
        real_cave.deepest_horizon,
        FABRICATED_REACH_M,
    );
    assert!(
        !fabricated_cave.band_agrees_with_reach(&terrain.column_at(cell)),
        "the fabrication must actually violate the invariant, or it is not a \
         mutation — real band {:?}, real budget {} m, fabricated budget \
         {FABRICATED_REACH_M} m",
        real_cave.deepest_horizon,
        real_cave.depth_reach_m
    );

    let authored_count = chamber_count(seed, &real_cave, real_gradient, cell);
    let fabricated_count = chamber_count(seed, &fabricated_cave, real_gradient, cell);
    let authored_deepest = deepest_reached(seed, &real_cave, real_gradient, cell);
    let fabricated_deepest = deepest_reached(seed, &fabricated_cave, real_gradient, cell);

    println!(
        "pipeline half (seed {}, cell {}): terrain authored depth_reach_m = \
         {} m at {} K/km, rung {:?} ({authored_count} chambers, deepest reached \
         {authored_deepest:?}); fabricated {FABRICATED_REACH_M} m downgrade of \
         the SAME cave = {fabricated_count} chambers, deepest reached \
         {fabricated_deepest:?}",
        seed.0,
        cell.0,
        real_cave.depth_reach_m,
        real_gradient.get(),
        rung_at_depth(real_cave.depth_reach_m, real_gradient)
    );

    assert!(
        authored_count > fabricated_count,
        "the real, terrain-authored cave ({authored_count} chambers, \
         {} m) did not out-grow a fabricated {FABRICATED_REACH_M} m downgrade \
         of the SAME cave ({fabricated_count} chambers) — the budget reaching \
         chamber_exists is not the one terrain authored",
        real_cave.depth_reach_m
    );
    assert_ne!(
        authored_deepest, fabricated_deepest,
        "the authored and fabricated budgets reached the same depth at \
         cell {cell:?} — the mutation did not change anything downstream"
    );
}
