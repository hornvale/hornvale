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

/// A cave with no formation opinion — `Cave::kind` is not read by
/// `chamber_exists`/`chamber_at` (see `chamber.rs`'s module doc) — reaching
/// exactly `band`.
fn cave_reaching(band: BandKind) -> Cave {
    Cave {
        kind: CaveKind::Fracture,
        deepest_band: band,
    }
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

/// **The derivation half.** Two hand-built `Cave`s differing only in
/// `deepest_band` prove `chamber_exists`/`chamber_at` read their `cave`
/// argument: a `Roots`-reaching budget grows a strictly larger chamber
/// graph than a `Regolith`-reaching one, and the shallow cave never reaches
/// past its own single in-budget band.
///
/// **What this does NOT prove**, and why the pipeline half below exists:
/// `BandKind::Regolith` never occurs in a live-generated cave (0 of 55,947,
/// Task 0), so the "shallow" case here perturbs a value no real world holds.
/// A green result here is necessary but not sufficient — see
/// `the_pipeline_hands_chamber_exists_the_budget_terrain_actually_authored`.
#[test]
fn a_shallow_cave_has_a_shallow_graph() {
    let seed = Seed(90210);
    let cell = CellId(9);

    let deep = chamber_count(seed, &cave_reaching(BandKind::Roots), cell);
    let shallow = chamber_count(seed, &cave_reaching(BandKind::Regolith), cell);
    println!(
        "derivation half (seed {}, cell {}): Roots budget = {deep} chambers, \
         Regolith budget = {shallow} chambers",
        seed.0, cell.0
    );
    assert!(deep > shallow, "Roots gave {deep}, Regolith gave {shallow}");

    assert_eq!(
        deepest_reached(seed, &cave_reaching(BandKind::Regolith), cell),
        Some(BandKind::Regolith),
        "a Regolith cave reached deeper — the budget is not being read"
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

    let fabricated_cave = Cave {
        deepest_band: BandKind::Regolith,
        ..real_cave
    };

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
