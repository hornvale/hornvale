//! The Deep Realm, Task 0: is there an underworld worth building?
//!
//! `cave_at` has shipped since The Lode and has never had a consumer, so its
//! distribution has never been checked against anything. This battery is the
//! campaign's gate: if caves are vanishingly rare, or almost none reach past
//! band 1 (the shallowest `depth_reach_bands` value — "cover only", per
//! `Cave::depth_reach_bands`'s own doc comment), the underworld is a
//! scattering of shallow pockets and the campaign reports that and stops.
//!
//! **Land** is `!terrain.is_ocean(cell)` — the identical predicate
//! `windows/worldgen/tests/{confluence,demesne,watershed_measure}.rs` use
//! throughout, and the one `cave_at` itself already gates on internally (it
//! returns `None` on every `is_ocean` cell — see `GeneratedTerrain::cave_at`),
//! so this harness introduces no second, independently-chosen land test.
//!
//! Built to `BuildDepth::Terrain`, the shallowest rung that produces terrain
//! at all — caves are a terrain-only feature (`domains/terrain/src/features.rs`),
//! and nothing here reads climate or settlements, so a `Full` build would
//! only pay for sculpting this harness never looks at (per `windows/worldgen`'s
//! own build-depth-ladder rule).
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out, reused
//! verbatim from `windows/worldgen/tests/artifacts.rs`'s
//! `build_world_to_with_artifacts` idiom.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{CellId, Seed};
use hornvale_terrain::{CaveKind, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};
use std::collections::BTreeSet;

const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

/// One seed's measured cave substrate: land/cave cell counts, the
/// `depth_reach_bands` histogram (indexed `[band - 1]`, bands run `1..=4`
/// per `cave_at`'s doc comment), the `CaveKind` breakdown, and the
/// clustering split (cave cells with >=1 neighbouring cave, vs. none).
struct SeedReport {
    seed: u64,
    land_cells: usize,
    cave_cells: usize,
    depth_histogram: [usize; 4],
    kind_karst: usize,
    kind_lava_tube: usize,
    kind_fracture: usize,
    clustered: usize,
    solitary: usize,
}

/// Build `seed` to `BuildDepth::Terrain` and measure its cave substrate over
/// every land cell (`!terrain.is_ocean`).
fn measure_one(seed: Seed) -> SeedReport {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let artifacts = build_world_to_with_artifacts(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Terrain,
    )
    .unwrap_or_else(|e| panic!("{seed:?} failed to build: {e:?}"));
    let terrain = artifacts
        .terrain
        .unwrap_or_else(|| panic!("{seed:?} at BuildDepth::Terrain produced no terrain"));
    let geo = terrain.geosphere();

    let mut land_cells = 0usize;
    let mut cave_cells = 0usize;
    let mut depth_histogram = [0usize; 4];
    let mut kind_karst = 0usize;
    let mut kind_lava_tube = 0usize;
    let mut kind_fracture = 0usize;
    let mut cave_set: BTreeSet<CellId> = BTreeSet::new();

    for cell in geo.cells() {
        if terrain.is_ocean(cell) {
            continue;
        }
        land_cells += 1;
        if let Some(cave) = terrain.cave_at(cell) {
            cave_cells += 1;
            let idx = (cave.depth_reach_bands.clamp(1, 4) - 1) as usize;
            depth_histogram[idx] += 1;
            match cave.kind {
                CaveKind::Karst => kind_karst += 1,
                CaveKind::LavaTube => kind_lava_tube += 1,
                CaveKind::Fracture => kind_fracture += 1,
            }
            cave_set.insert(cell);
        }
    }

    // Clustering: a cave cell "clusters" if any of its mesh neighbours also
    // carries a cave; "stands alone" otherwise.
    let mut clustered = 0usize;
    let mut solitary = 0usize;
    for &cell in &cave_set {
        let has_cave_neighbor = geo.neighbors(cell).iter().any(|nb| cave_set.contains(nb));
        if has_cave_neighbor {
            clustered += 1;
        } else {
            solitary += 1;
        }
    }

    SeedReport {
        seed: seed.0,
        land_cells,
        cave_cells,
        depth_histogram,
        kind_karst,
        kind_lava_tube,
        kind_fracture,
        clustered,
        solitary,
    }
}

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn report_cave_substrate() {
    let mut per_seed: Vec<SeedReport> = Vec::new();
    for seed in SEEDS {
        per_seed.push(measure_one(Seed(seed)));
    }

    let mut total_land = 0usize;
    let mut total_caves = 0usize;
    let mut total_hist = [0usize; 4];
    let mut total_karst = 0usize;
    let mut total_lava_tube = 0usize;
    let mut total_fracture = 0usize;
    let mut total_clustered = 0usize;
    let mut total_solitary = 0usize;

    for r in &per_seed {
        println!(
            "seed {}: land={} caves={} depth_hist(1,2,3,4)={:?} karst={} lava_tube={} fracture={} clustered={} solitary={}",
            r.seed,
            r.land_cells,
            r.cave_cells,
            r.depth_histogram,
            r.kind_karst,
            r.kind_lava_tube,
            r.kind_fracture,
            r.clustered,
            r.solitary
        );
        total_land += r.land_cells;
        total_caves += r.cave_cells;
        for (total, count) in total_hist.iter_mut().zip(r.depth_histogram.iter()) {
            *total += count;
        }
        total_karst += r.kind_karst;
        total_lava_tube += r.kind_lava_tube;
        total_fracture += r.kind_fracture;
        total_clustered += r.clustered;
        total_solitary += r.solitary;
    }

    println!("== aggregate over seeds {:?} ==", SEEDS);
    println!("TOTAL land_cells = {total_land}");
    println!("TOTAL cave_cells = {total_caves}");
    println!(
        "1. cave fraction of land = {:.6} ({total_caves}/{total_land})",
        total_caves as f64 / total_land as f64
    );
    for band in 1..=4u32 {
        let count = total_hist[(band - 1) as usize];
        println!(
            "2. depth_reach_bands={band}: {count} caves ({:.6} of caves)",
            count as f64 / total_caves as f64
        );
    }
    println!(
        "3. fraction of caves reaching band 4 = {:.6} ({}/{total_caves})",
        total_hist[3] as f64 / total_caves as f64,
        total_hist[3]
    );
    println!(
        "4. clustering: clustered={total_clustered} ({:.6}), solitary={total_solitary} ({:.6})",
        total_clustered as f64 / total_caves as f64,
        total_solitary as f64 / total_caves as f64
    );
    println!(
        "CaveKind breakdown: karst={total_karst} ({:.6}) lava_tube={total_lava_tube} ({:.6}) fracture={total_fracture} ({:.6})",
        total_karst as f64 / total_caves as f64,
        total_lava_tube as f64 / total_caves as f64,
        total_fracture as f64 / total_caves as f64
    );

    // Guard assertions (task brief Step 2) - a harness that measures nothing
    // looks identical to one that works.
    assert!(!per_seed.is_empty(), "no seeds sampled");
    assert!(
        per_seed.iter().all(|r| r.land_cells > 0),
        "a seed had no land"
    );
    assert!(
        per_seed.iter().all(|r| r.cave_cells <= r.land_cells),
        "more caves than land cells — the land mask and cave_at disagree"
    );
}
