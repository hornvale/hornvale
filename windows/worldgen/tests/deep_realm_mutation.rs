//! The Deep Realm, Task 7: the mutation proof.
//!
//! The program's shared acceptance criterion: a green test proves the code
//! ran; only the mutation proves the axis is visible (spec §7, H3).
//!
//! **Two halves, both required (spec §7.1).** Over 30 seeds, Task 0 measured
//! `BandKind::Regolith` at 0 of 55,947 live caves — the generator never
//! produces the shallow cave the naive translation of H3 fabricates. A
//! mutation that only swaps a hand-built `Cave`'s `deepest_band` between two
//! literals proves `chamber_exists`/`chamber_at` read their `cave` argument
//! (the **derivation** half); it says nothing about whether the **pipeline**
//! that builds a world ever hands them the value terrain actually authored,
//! rather than some default or constant. This campaign's sibling (The
//! Tolerance) shipped exactly that gap: a mutation proof passed while every
//! world under test carried a fabricated parameter no author had written.
//! Both halves are here for that reason.

#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{CellId, Seed};
use hornvale_terrain::{BandKind, Cave, CaveKind, TerrainPins};
use hornvale_worldgen::chamber::{ChamberAddr, SLOTS_PER_BAND, chamber_exists};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

/// The permanent band ladder, top to bottom. `chamber.rs`'s own
/// `band_rank`/`band_of_rank` bijection is private to that module (the
/// practical notes above this task confirm it), so this file restates the
/// same five-variant order rather than reaching for it — the ladder itself
/// is `hornvale_climate::Realm::UNDERDARK.strata()`'s order, pinned
/// independently by `domains/climate/tests/facets.rs`.
const BAND_LADDER: [BandKind; 5] = [
    BandKind::Regolith,
    BandKind::Cover,
    BandKind::Basement,
    BandKind::Roots,
    BandKind::Underneath,
];

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
/// depth budget of `reach_m`, with its `deepest_band` **derived from that
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

/// Every chamber address that exists over the whole five-band lattice at
/// `(seed, cell)`, under `cave`'s budget. Walks all five bands regardless of
/// `cave.deepest_band` — `chamber_exists` itself gates on the budget, so a
/// full walk measures exactly what the budget lets through rather than
/// baking the ladder's shape into this helper too.
fn chamber_count(seed: Seed, cave: &Cave, cell: CellId) -> usize {
    let mut count = 0usize;
    for band in 0..BAND_LADDER.len() as u8 {
        for slot in 0..SLOTS_PER_BAND {
            let addr = ChamberAddr {
                cell,
                entrance: 0,
                band,
                slot,
            };
            if chamber_exists(seed, cave, addr) {
                count += 1;
            }
        }
    }
    count
}

/// The deepest band with at least one existing chamber at `(seed, cell)`
/// under `cave`'s budget — `None` if no chamber exists at all. Existence is
/// sparse (a coin-flip density per address), so an arbitrary probe cell can
/// legitimately come back empty; callers that need a guaranteed nonempty
/// result pick a `(seed, cell)` this is known to return `Some` for.
fn deepest_reached(seed: Seed, cave: &Cave, cell: CellId) -> Option<BandKind> {
    (0..BAND_LADDER.len() as u8).rev().find_map(|band| {
        let reached = (0..SLOTS_PER_BAND).any(|slot| {
            chamber_exists(
                seed,
                cave,
                ChamberAddr {
                    cell,
                    entrance: 0,
                    band,
                    slot,
                },
            )
        });
        reached.then_some(BAND_LADDER[band as usize])
    })
}

/// A budget stopping inside the cover — derives `BandKind::Cover`, rank 1.
const SHALLOW_REACH_M: f64 = 200.0;
/// A budget cutting past the 401 m basement contact — derives
/// `BandKind::Basement`, rank 2, the deepest band a metre budget can reach.
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
    // The fixtures must actually differ in band, or the comparison is vacuous.
    assert_eq!(deep_cave.deepest_band, BandKind::Basement);
    assert_eq!(shallow_cave.deepest_band, BandKind::Cover);

    let deep = chamber_count(seed, &deep_cave, cell);
    let shallow = chamber_count(seed, &shallow_cave, cell);
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
        pooled_deep += chamber_count(Seed(raw), &deep_cave, cell);
        pooled_shallow += chamber_count(Seed(raw), &shallow_cave, cell);
    }
    println!("  pooled over 8 seeds: deep = {pooled_deep}, shallow = {pooled_shallow}");
    assert!(
        pooled_deep > pooled_shallow,
        "pooled over 8 seeds the deeper budget gave {pooled_deep} and the \
         shallower {pooled_shallow} — the budget is not being read"
    );

    // …and the shallow cave never reaches past its own in-budget bands.
    let reached = deepest_reached(seed, &shallow_cave, cell);
    assert!(
        matches!(reached, Some(BandKind::Regolith) | Some(BandKind::Cover)),
        "a Cover-budget cave reached {reached:?} — either the budget is not \
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
/// `deepest_band` forced down to `Regolith` — a budget the generator did
/// not author for this cell. If chamber_exists (or anything upstream of it)
/// silently substituted a default/constant budget instead of the one
/// terrain measured, feeding it the *real* cave and the *fabricated* one
/// would be indistinguishable. It is not: the real, terrain-authored budget
/// grows a strictly larger graph and reaches strictly deeper than the
/// fabricated downgrade of the identical cave.
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

    assert_ne!(
        real_cave.deepest_band,
        BandKind::Regolith,
        "cell {cell:?}'s real cave was already Regolith — Task 0 measured this \
         at 0 of 55,947 caves, so this probe cell no longer demonstrates a \
         genuine downgrade; pick a different cell/seed"
    );

    // THE ONE DELIBERATE VIOLATION of `Cave`'s derived-field invariant in the
    // workspace, and it is the content of the mutation: a struct literal that
    // forces `deepest_band` away from the band the cave's own budget reaches,
    // producing a pair no constructor would ever emit. That is precisely the
    // fabrication being detected — if anything downstream substituted a
    // default budget for terrain's, the real and fabricated caves would be
    // indistinguishable. Every other construction site goes through
    // `Cave::new`/`Cave::from_reach`.
    let fabricated_cave = Cave {
        deepest_band: BandKind::Regolith,
        ..real_cave
    };
    assert!(
        !fabricated_cave.band_agrees_with_reach(&terrain.column_at(cell)),
        "the fabrication must actually violate the invariant, or it is not a \
         mutation — real band {:?}, budget {} m",
        real_cave.deepest_band,
        real_cave.depth_reach_m
    );

    let authored_count = chamber_count(seed, &real_cave, cell);
    let fabricated_count = chamber_count(seed, &fabricated_cave, cell);
    let authored_deepest = deepest_reached(seed, &real_cave, cell);
    let fabricated_deepest = deepest_reached(seed, &fabricated_cave, cell);

    println!(
        "pipeline half (seed {}, cell {}): terrain authored deepest_band = \
         {:?} ({authored_count} chambers, deepest reached {authored_deepest:?}); \
         fabricated Regolith downgrade of the SAME cave = {fabricated_count} \
         chambers, deepest reached {fabricated_deepest:?}",
        seed.0, cell.0, real_cave.deepest_band
    );

    assert!(
        authored_count > fabricated_count,
        "the real, terrain-authored cave ({authored_count} chambers) did not \
         out-grow a fabricated Regolith downgrade of the SAME cave \
         ({fabricated_count} chambers) — the budget reaching chamber_exists \
         is not the one terrain authored"
    );
    assert_ne!(
        authored_deepest, fabricated_deepest,
        "the authored and fabricated budgets reached the same depth at \
         cell {cell:?} — the mutation did not change anything downstream"
    );
}
