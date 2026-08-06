//! The Deep Realm, Task 0: is there an underworld worth building?
//!
//! `cave_at` had never had a consumer when this battery was first written, so
//! its distribution had never been checked against anything. This battery is
//! the campaign's gate: if caves are vanishingly rare, or almost none reach
//! past [`BandKind::Regolith`], the underworld is a scattering of shallow
//! pockets and the campaign reports that and stops.
//!
//! ## The substrate moved under this instrument, and what that changed
//!
//! The first run of this battery (2026-08-05) measured a broken model and
//! stopped the campaign: 0.26% of land, one `CaveKind`, one depth band, 3 of
//! 30 worlds caveless. The Hollow then repaired that model, and in doing so
//! **changed the type this battery reads**: `Cave::depth_reach_bands` (a
//! `u32` count, nominally `1..=4`) became [`Cave::deepest_band`] (a
//! [`BandKind`], five named variants `Regolith..Underneath`). That is a type
//! change, not a rename — a band derived from a column cannot reproduce a
//! count derived from a ratio.
//!
//! **The spec's criterion is therefore restated, never re-thresholded.** Spec
//! §7's wording — "vanishingly rare or almost none reach past `Regolith`" —
//! is kept verbatim, and only the instrument is re-expressed. No new numeric
//! threshold is authored here, because The Hollow published the numbers this
//! battery measures before it could be re-run: a threshold written now would
//! be a prediction of a known result rather than a gate. The prior unblinding
//! is disclosed in the campaign's chronicle.
//!
//! **What is reported is band VARIETY, not reach-the-deepest.** The frozen
//! wording asked "how many cells have reach 4", and the naive translation is
//! "what fraction reach `Roots`". That translation is unfaithful in the
//! direction that matters: a world where *every* cave is `Roots` scores 100%
//! on it and is exactly the falsification §7 names — a uniform column with
//! extra steps. What C2a's chamber graph consumes is a depth budget that
//! **differs by place**, so the distinct-band count is a first-class number
//! here and the deepest-band fraction is reported beside it as a ceiling
//! check.
//!
//! [`BandKind::Underneath`] is reported explicitly and separately: it is a
//! fifth band that the plan's Task 1 `UNDERDARK` ladder
//! (`Regolith/Cover/Basement/Roots`) does not include. A nonzero count is a
//! finding for Task 1, not a gate result.
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
use hornvale_terrain::{BandKind, CaveKind, TerrainPins};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};
use std::collections::BTreeSet;

const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

/// The five [`BandKind`] variants in declaration order, top to bottom. The
/// band histogram is indexed by position in this array, so a new variant
/// added to the enum reddens [`band_index`]'s exhaustive match rather than
/// silently landing in a neighbour's bucket.
const BAND_NAMES: [&str; 5] = ["Regolith", "Cover", "Basement", "Roots", "Underneath"];

/// Position of `band` in [`BAND_NAMES`]. Exhaustive by construction.
fn band_index(band: BandKind) -> usize {
    match band {
        BandKind::Regolith => 0,
        BandKind::Cover => 1,
        BandKind::Basement => 2,
        BandKind::Roots => 3,
        BandKind::Underneath => 4,
    }
}

/// One seed's measured cave substrate: land/cave cell counts, the
/// [`Cave::deepest_band`] histogram (indexed by [`band_index`]), the
/// `CaveKind` breakdown, and the clustering split (cave cells with >=1
/// neighbouring cave, vs. none).
struct SeedReport {
    seed: u64,
    land_cells: usize,
    cave_cells: usize,
    band_histogram: [usize; 5],
    kind_karst: usize,
    kind_lava_tube: usize,
    kind_fracture: usize,
    clustered: usize,
    solitary: usize,
}

impl SeedReport {
    /// How many distinct bands this seed's caves actually reach. This is the
    /// gate's load-bearing number: a chamber graph reads a depth budget, and a
    /// budget that takes one value everywhere is a uniform column with extra
    /// steps (spec §7's falsification).
    fn distinct_bands(&self) -> usize {
        self.band_histogram.iter().filter(|&&n| n > 0).count()
    }
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
    let mut band_histogram = [0usize; 5];
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
            band_histogram[band_index(cave.deepest_band)] += 1;
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
        band_histogram,
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
    let mut total_hist = [0usize; 5];
    let mut total_karst = 0usize;
    let mut total_lava_tube = 0usize;
    let mut total_fracture = 0usize;
    let mut total_clustered = 0usize;
    let mut total_solitary = 0usize;

    for r in &per_seed {
        println!(
            "seed {}: land={} caves={} bands(Reg,Cov,Bas,Roo,Und)={:?} distinct_bands={} karst={} lava_tube={} fracture={} clustered={} solitary={}",
            r.seed,
            r.land_cells,
            r.cave_cells,
            r.band_histogram,
            r.distinct_bands(),
            r.kind_karst,
            r.kind_lava_tube,
            r.kind_fracture,
            r.clustered,
            r.solitary
        );
        total_land += r.land_cells;
        total_caves += r.cave_cells;
        for (total, count) in total_hist.iter_mut().zip(r.band_histogram.iter()) {
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
    for (idx, name) in BAND_NAMES.iter().enumerate() {
        let count = total_hist[idx];
        println!(
            "2. deepest_band={name}: {count} caves ({:.6} of caves)",
            count as f64 / total_caves as f64
        );
    }

    // 3. The gate's load-bearing number. The frozen wording asked "how many
    //    cells have reach 4"; the ceiling reading is reported for continuity,
    //    but the number C2a's chamber graph actually consumes is whether the
    //    depth budget DIFFERS BY PLACE. A substrate where every cave sits at
    //    one band scores 100% on a reach-the-deepest criterion and is spec
    //    §7's falsification, not its pass.
    let past_regolith: usize = total_hist[1..].iter().sum();
    let distinct_overall = total_hist.iter().filter(|&&n| n > 0).count();
    let min_distinct = per_seed
        .iter()
        .map(SeedReport::distinct_bands)
        .min()
        .unwrap_or(0);
    let max_distinct = per_seed
        .iter()
        .map(SeedReport::distinct_bands)
        .max()
        .unwrap_or(0);
    let single_band_seeds = per_seed.iter().filter(|r| r.distinct_bands() <= 1).count();
    println!(
        "3a. caves past Regolith = {:.6} ({past_regolith}/{total_caves})  <- spec §7's stop-condition reading",
        past_regolith as f64 / total_caves as f64
    );
    println!(
        "3b. band VARIETY: {distinct_overall}/5 bands occur overall; per-seed distinct bands min={min_distinct} max={max_distinct}; seeds with <=1 band = {single_band_seeds}/{}",
        per_seed.len()
    );
    println!(
        "3c. ceiling check — deepest band reached: Roots = {:.6} ({}), Underneath = {:.6} ({})",
        total_hist[3] as f64 / total_caves as f64,
        total_hist[3],
        total_hist[4] as f64 / total_caves as f64,
        total_hist[4]
    );
    println!(
        "3d. NOTE: `Underneath` is not in the plan's Task 1 UNDERDARK ladder \
         (Regolith/Cover/Basement/Roots). Count above nonzero => a Task 1 finding."
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

    // Exhaustiveness. The Hollow's operational finding: checking that a
    // bucketed table sums to its population is a five-second arithmetic step
    // that turns a table into a statement about the WHOLE population — and it
    // is what revealed that campaign's own gate input was gapped rather than
    // merely non-uniform. This asserts the HARNESS counted every cave, not
    // that the world has any particular property, so it is a guard and not a
    // criterion.
    let hist_total: usize = total_hist.iter().sum();
    assert_eq!(
        hist_total, total_caves,
        "band histogram sums to {hist_total} but {total_caves} caves were counted — \
         a cave landed in no band"
    );
}
