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
use hornvale_kernel::{CellId, Geosphere, Seed, Value};
use hornvale_settlement::CELL_ID;
use hornvale_terrain::{BandKind, Cave, CaveKind, GeneratedTerrain, TerrainPins};
use hornvale_worldgen::chamber::{ChamberAddr, SLOTS_PER_BAND, chamber_exists};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};
use std::collections::{BTreeSet, VecDeque};

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

/// claim: readout(off-gate, heavy:) — prints cave-substrate composition
/// per seed (audit note: coincidental duplicate name with worldgen's
/// hollow_readout.rs::report_cave_substrate, a different test)
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

// =============================================================================
// Task 8: the preregistered readout.
//
// Three results, all REPORTED and never asserted (plan Task 8 Step 1) except
// for data-integrity guards in the same spirit as Task 0's — a harness that
// measures nothing must not look identical to one that works:
//
// 1. H2, reported on TWO different populations. The spec's own wording
//    ("chambers per cell is heavily zero-weighted") is true for a trivial
//    reason: ~88% of land cells hold no cave at all (Task 0), so a per-CELL
//    histogram is zero-weighted by cave RARITY, not by chamber SPARSITY.
//    Reporting chambers-per-CAVE (the population that actually has a lattice
//    to be sparse or dense in) alongside chambers-per-CELL is what keeps H2
//    from passing for the wrong reason.
// 2. The coin-flip prediction. `EXISTENCE_DENSITY = 0.5` (private to
//    `chamber.rs`; restated here as [`PREDICTED_EXISTENCE_DENSITY`], the
//    same restatement posture `deep_realm_mutation.rs` already takes with the
//    band ladder) predicts a Binomial chamber count per band-budget, which
//    has a narrow relative spread by construction — structurally close to
//    spec §7's falsification ("nothing about it differs by place"). This is
//    tested, not assumed: measured mean/sd/CV are printed beside the
//    theoretical Binomial values for every band that occurs.
// 3. The depth-weld breakdown (ledger #16, `MAP-cave-depth-weld`). The same
//    per-band grouping used for (2) is C2a's first real evidence for or
//    against splitting the weld: uniform WITHIN a band but differing BETWEEN
//    bands is a measured case for the split; freely varying within a band
//    says the weld is not this campaign's constraint.
//
// A fourth result, reachability (ledger #26), follows in its own section
// below — it shares this section's helpers but answers a different question
// (can a player get there, not what the graph looks like once they do).
//
// -----------------------------------------------------------------------------
// WHAT IT MEASURED (2026-08-06, seeds 1..=30, 469,122 land cells, 55,947 caves).
// Recorded here because a readout whose results live only in stdout is one
// somebody has to re-run to learn anything.
//
//   chambers per CAVE   median 5, p25 4, p75 7, max 15, mean 5.6799, cv 0.4044
//   chambers per CELL   median 0 (88.07% of land is cave-free), mean 0.6774
//
//   by band, measured vs theory:
//     Cover     ( 8 addr, 22395)  mean 4.0096 sd 1.4148 cv 0.3529
//               Binomial( 8, .5)  mean 4.0000 sd 1.4142 cv 0.3536
//     Basement  (12 addr, 20206)  mean 5.9955 sd 1.7352 cv 0.2894
//               Binomial(12, .5)  mean 6.0000 sd 1.7321 cv 0.2887
//     Roots     (16 addr, 13346)  mean 8.0047 sd 1.9920 cv 0.2489
//               Binomial(16, .5)  mean 8.0000 sd 2.0000 cv 0.2500
//
// Mean, sd AND cv match theory to 3-4 decimals in every band. The chamber
// count is not merely close to binomial — it IS `Binomial(4(rank+1), 0.5)`,
// with nothing else in it.
//
// **So spec §7's falsification is PARTIALLY TRIGGERED, and this is the
// campaign's headline rather than a footnote.** Scored clause by clause:
//
//   "same depth"     NOT falsified — depth differs by place, three values,
//                    from terrain's own measured budget.
//   "same shape"     FALSIFIED — given the band, shape is a fixed
//                    distribution with no place dependence at all. Two caves
//                    in the same band have statistically identical graphs
//                    whatever their rock, climate, elevation or kind;
//                    `chamber.rs` does not read `Cave::kind` at all.
//   "same contents"  FALSIFIED on the same terms — `stratum` is a pure
//                    function of band and `origin` is always `Found`, so
//                    contents are a function of depth alone.
//
// Two of three clauses fail, so "not triggered" is not a defensible summary.
// This is a good finding and it is precisely what the campaign was built to be
// able to detect: the fixed-lattice design bought edge symmetry and address
// stability (§3.2's hard problem, dissolved) at the cost of contributing no
// character of its own. **Everything the underworld has, terrain gave it.**
//
// It also makes the `MAP-cave-depth-weld` split the single highest-leverage
// next move, on evidence rather than aesthetics: the only place-character the
// underworld has comes from a 3-valued budget welded to the existence gate.
// C2a consumed that budget as-is (ledger #16) specifically so this measurement
// could exist.
// -----------------------------------------------------------------------------
// =============================================================================

/// Land-only graph-distance radii (terrain-cell hops — see [`land_distances`])
/// the coverage readout reports land-cell fractions at. These are NOT
/// simulated walk steps: Task 5's implementer's 8000-step locale-mesh walk
/// (room-scale) covered only 64 of these much coarser (~110 km) terrain
/// cells, so even a handful of hops here already spans a large real
/// distance.
const REPORT_RADII: [u32; N_RADII] = [1, 2, 3, 5, 10];

/// The length of [`REPORT_RADII`], named so [`T8SeedReport::coverage_by_radius`]
/// doesn't repeat a bare literal.
const N_RADII: usize = 5;

/// The coin-flip existence density Task 8 is testing — 0.5, restating
/// `chamber.rs`'s private `EXISTENCE_DENSITY` (invisible to this test crate
/// by design; `deep_realm_mutation.rs` already restates the band ladder for
/// the identical reason). This is the FROZEN prediction under measurement,
/// not a tunable: if the measured spread disagrees with the Binomial this
/// constant predicts, that is a finding about the model, not a cue to edit
/// this line.
/// type-audit: bare-ok(ratio)
const PREDICTED_EXISTENCE_DENSITY: f64 = 0.5;

/// How many lattice addresses are in budget for a cave whose `deepest_band`
/// sits at ladder position `band_idx` ([`band_index`]'s own numbering,
/// `0..=4`): `chamber_exists`'s gate is `addr.band <= band_rank(cave.
/// deepest_band)`, and `addr.band` ranges over `0..=band_idx` at
/// [`SLOTS_PER_BAND`] slots each.
fn addresses_in_budget(band_idx: usize) -> usize {
    (band_idx + 1) * usize::from(SLOTS_PER_BAND)
}

/// Every chamber address that exists over the FULL five-band lattice at
/// `(seed, cell)`, gated by `cave`'s own measured depth budget. Mirrors
/// `deep_realm_mutation.rs`'s private `chamber_count` helper, restated here
/// rather than shared — a test helper is not part of any crate's public
/// surface, and that file already restates the band ladder for the same
/// reason. Always probes `entrance: 0`: today's terrain model reports one
/// aperture per cave cell (see `ChamberAddr::entrance`'s own doc).
fn chamber_count_at(seed: Seed, cave: &Cave, cell: CellId) -> usize {
    let mut count = 0usize;
    for band in 0..BAND_NAMES.len() as u8 {
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

/// Whether `cave`'s canonical entrance address (`band = 0, slot = 0` — the
/// cave mouth `deep_realm_chamber.rs`'s `a_cave_mouth_reaches_at_least_one_
/// chamber` measures) holds no chamber: spec §3.4 rung 0, `Sealed` — "the
/// void exists and is unreachable." Task 5's `delve` refuses such a cave by
/// naming it sealed rather than claiming there is nothing there.
fn is_sealed(seed: Seed, cave: &Cave, cell: CellId) -> bool {
    !chamber_exists(
        seed,
        cave,
        ChamberAddr {
            cell,
            entrance: 0,
            band: 0,
            slot: 0,
        },
    )
}

/// Multi-source, LAND-ONLY (`!terrain.is_ocean`) graph distance in
/// terrain-cell hops from every cell in `sources` simultaneously, over
/// `geo.neighbors` — the "cheap graph/mesh distance" Task 8's brief asks for
/// explicitly INSTEAD OF a simulated walk. Mirrors `domains/terrain/src/
/// boundaries.rs`'s `boundary_distance`: a dense `Vec` indexed by `cell.0`
/// (kernel convention: dense-index storage is `Vec`, never a map), a
/// `VecDeque` FIFO queue, ascending source/neighbor enqueue order — fully
/// deterministic regardless of `sources`' own iteration order (a
/// `BTreeSet`). `None` for a cell no source can reach without crossing ocean
/// (a different landmass).
fn land_distances(
    geo: &Geosphere,
    terrain: &GeneratedTerrain,
    sources: &BTreeSet<CellId>,
) -> Vec<Option<u32>> {
    let mut dist: Vec<Option<u32>> = vec![None; geo.cell_count()];
    let mut queue: VecDeque<CellId> = VecDeque::new();
    for cell in geo.cells() {
        if sources.contains(&cell) {
            dist[cell.0 as usize] = Some(0);
            queue.push_back(cell);
        }
    }
    while let Some(cell) = queue.pop_front() {
        let d = dist[cell.0 as usize].expect("queued cells are always labeled before dequeue");
        for &neighbor in geo.neighbors(cell) {
            if terrain.is_ocean(neighbor) {
                continue;
            }
            if dist[neighbor.0 as usize].is_none() {
                dist[neighbor.0 as usize] = Some(d + 1);
                queue.push_back(neighbor);
            }
        }
    }
    dist
}

/// A distribution's shape, reported rather than collapsed to a mean — a
/// standing project lesson is that a median floor cannot see a heavy tail,
/// so this carries both tails and the middle. Percentiles are nearest-rank
/// over the sorted input (no interpolation): simple, and deterministic
/// regardless of platform.
#[derive(Debug)]
struct Summary {
    /// Population size.
    n: usize,
    /// Minimum value.
    min: usize,
    /// 25th percentile.
    p25: usize,
    /// 50th percentile (median).
    median: usize,
    /// 75th percentile.
    p75: usize,
    /// 90th percentile — the heavy-tail check a bare median cannot make.
    p90: usize,
    /// Maximum value.
    max: usize,
    /// Arithmetic mean.
    mean: f64,
    /// Population standard deviation.
    sd: f64,
}

impl std::fmt::Display for Summary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cv = if self.mean > 0.0 {
            self.sd / self.mean
        } else {
            0.0
        };
        write!(
            f,
            "n={} min={} p25={} median={} p75={} p90={} max={} mean={:.4} sd={:.4} cv={cv:.4}",
            self.n,
            self.min,
            self.p25,
            self.median,
            self.p75,
            self.p90,
            self.max,
            self.mean,
            self.sd
        )
    }
}

/// Summarizes `values` — an empty slice reports an all-zero `Summary` rather
/// than panicking, because a band bucket that never occurs (`Regolith`,
/// `Underneath` — Task 0) is a first-class, reportable finding, not an
/// error.
fn summarize(values: &[usize]) -> Summary {
    let mut sorted = values.to_vec();
    sorted.sort_unstable();
    let n = sorted.len();
    let percentile = |q: f64| -> usize {
        if n == 0 {
            return 0;
        }
        let idx = ((q * n as f64).ceil() as usize)
            .saturating_sub(1)
            .min(n - 1);
        sorted[idx]
    };
    let mean = if n == 0 {
        0.0
    } else {
        sorted.iter().sum::<usize>() as f64 / n as f64
    };
    let variance = if n == 0 {
        0.0
    } else {
        sorted
            .iter()
            .map(|&v| {
                let d = v as f64 - mean;
                d * d
            })
            .sum::<f64>()
            / n as f64
    };
    Summary {
        n,
        min: *sorted.first().unwrap_or(&0),
        p25: percentile(0.25),
        median: percentile(0.5),
        p75: percentile(0.75),
        p90: percentile(0.9),
        max: *sorted.last().unwrap_or(&0),
        mean,
        sd: variance.sqrt(),
    }
}

/// One seed's Task 8 measurement.
struct T8SeedReport {
    /// The seed measured.
    seed: u64,
    /// Land cells this seed's terrain sculpted (`!terrain.is_ocean`).
    land_cells: usize,
    /// `(chamber_count, deepest_band, sealed)` — one entry per land cell
    /// that carries a cave.
    cave_cells: Vec<(usize, BandKind, bool)>,
    /// Chamber count for EVERY land cell, cave or not (0 where there is no
    /// cave) — H2's own literal wording, reported so its zero-weighting can
    /// be attributed correctly rather than assumed.
    per_cell_counts: Vec<usize>,
    /// Land-only graph-hop distance ([`land_distances`]) from the flagship
    /// settlement's cell to the nearest land cell holding a NON-SEALED cave.
    /// `None` if this seed placed no settlement, or the flagship's landmass
    /// holds no reachable non-sealed cave.
    flagship_to_open_cave: Option<u32>,
    /// Fraction of land cells within each [`REPORT_RADII`] hop-radius of ANY
    /// non-sealed cave cell, aligned index-for-index with `REPORT_RADII`.
    coverage_by_radius: [f64; N_RADII],
}

/// Builds `seed` to `BuildDepth::Settlements` — the shallowest rung that
/// places a flagship (`BuildDepth`'s own doc: "…plus settlement placement,
/// naming, and glosses") — and measures Task 8's whole readout in one build:
/// H2's chamber distribution (per cave AND per cell), the depth-weld's
/// per-band breakdown (via each cave's own `deepest_band`), and
/// reachability from the flagship start. Terrain facts are a byte-identical
/// prefix of the `BuildDepth::Terrain` build Task 0 uses (`BuildDepth`'s own
/// doc), so this changes nothing about what Task 0 measured — it only adds
/// the settlement layer Task 8 additionally needs.
fn measure_t8(seed: Seed) -> T8SeedReport {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let artifacts = build_world_to_with_artifacts(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .unwrap_or_else(|e| panic!("{seed:?} failed to build to BuildDepth::Settlements: {e:?}"));
    let terrain = artifacts
        .terrain
        .unwrap_or_else(|| panic!("{seed:?} at BuildDepth::Settlements produced no terrain"));
    let world = artifacts.world;
    let geo = terrain.geosphere();

    // The flagship's cell, via the same `CELL_ID` fact `confluence.rs`
    // already reads a settlement's location through — no separate,
    // independently-chosen lookup.
    let flagship_cell: Option<CellId> = hornvale_settlement::all_settlements(&world)
        .into_iter()
        .next()
        .and_then(|s| match world.ledger.value_of(s.id, CELL_ID) {
            Some(Value::Number(n)) => Some(CellId(*n as u32)),
            _ => None,
        });

    let mut land_cells = 0usize;
    let mut land_cell_ids: Vec<CellId> = Vec::new();
    let mut cave_cells: Vec<(usize, BandKind, bool)> = Vec::new();
    let mut per_cell_counts: Vec<usize> = Vec::new();
    let mut non_sealed: BTreeSet<CellId> = BTreeSet::new();

    for cell in geo.cells() {
        if terrain.is_ocean(cell) {
            continue;
        }
        land_cells += 1;
        land_cell_ids.push(cell);
        match terrain.cave_at(cell) {
            Some(cave) => {
                let count = chamber_count_at(seed, &cave, cell);
                let sealed = is_sealed(seed, &cave, cell);
                if !sealed {
                    non_sealed.insert(cell);
                }
                cave_cells.push((count, cave.deepest_band, sealed));
                per_cell_counts.push(count);
            }
            None => per_cell_counts.push(0),
        }
    }

    let dist = land_distances(geo, &terrain, &non_sealed);
    let flagship_to_open_cave = flagship_cell.and_then(|c| dist[c.0 as usize]);

    let mut coverage_by_radius = [0.0f64; N_RADII];
    if land_cells > 0 {
        for (i, &radius) in REPORT_RADII.iter().enumerate() {
            let within = land_cell_ids
                .iter()
                .filter(|&&c| matches!(dist[c.0 as usize], Some(d) if d <= radius))
                .count();
            coverage_by_radius[i] = within as f64 / land_cells as f64;
        }
    }

    T8SeedReport {
        seed: seed.0,
        land_cells,
        cave_cells,
        per_cell_counts,
        flagship_to_open_cave,
        coverage_by_radius,
    }
}

/// claim: readout(off-gate, heavy:) — prints H2 depth-weld measurements,
/// with harness-sanity guard assertions per the module's own "a harness
/// that measures nothing" doc
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn report_h2_depth_weld_and_reachability() {
    let mut per_seed: Vec<T8SeedReport> = Vec::new();
    for seed in SEEDS {
        per_seed.push(measure_t8(Seed(seed)));
    }

    // Guard assertions, in Task 0's spirit — a harness that measures nothing
    // looks identical to one that works.
    assert!(!per_seed.is_empty(), "no seeds sampled");
    assert!(
        per_seed.iter().all(|r| r.land_cells > 0),
        "a seed had no land"
    );
    assert!(
        per_seed
            .iter()
            .all(|r| r.per_cell_counts.len() == r.land_cells),
        "per-cell chamber counts did not cover every land cell — the harness \
         is not measuring the whole population"
    );
    assert!(
        per_seed.iter().all(|r| r.cave_cells.len() <= r.land_cells),
        "more cave cells than land cells — the land mask and cave_at disagree"
    );
    for r in &per_seed {
        for (cov, rad) in r.coverage_by_radius.windows(2).zip(REPORT_RADII.windows(2)) {
            assert!(
                cov[1] + 1e-9 >= cov[0],
                "seed {}: coverage at R={} ({}) is LESS than at R={} ({}) — \
                 radius coverage must be monotonic non-decreasing",
                r.seed,
                rad[1],
                cov[1],
                rad[0],
                cov[0]
            );
        }
    }

    // ---- 1. H2: chambers per CAVE vs chambers per CELL --------------------
    println!(
        "== H2: chambers per CAVE vs chambers per CELL (seeds {:?}) ==",
        SEEDS
    );
    let all_cell_counts: Vec<usize> = per_seed
        .iter()
        .flat_map(|r| r.per_cell_counts.iter().copied())
        .collect();
    let all_cave_counts: Vec<usize> = per_seed
        .iter()
        .flat_map(|r| r.cave_cells.iter().map(|&(c, _, _)| c))
        .collect();
    let total_land: usize = per_seed.iter().map(|r| r.land_cells).sum();
    let total_caves = all_cave_counts.len();
    println!(
        "chambers-per-CELL, ALL {total_land} land cells ({:.4} of them cave-free): {}",
        1.0 - total_caves as f64 / total_land as f64,
        summarize(&all_cell_counts)
    );
    println!(
        "chambers-per-CAVE, the {total_caves} cave cells only: {}",
        summarize(&all_cave_counts)
    );
    let zero_chamber_caves = all_cave_counts.iter().filter(|&&c| c == 0).count();
    println!(
        "  of those {total_caves} caves, {zero_chamber_caves} ({:.4}) have ZERO chambers \
         ANYWHERE in their lattice — a stronger seal than just the entrance address",
        zero_chamber_caves as f64 / total_caves.max(1) as f64
    );

    // ---- 2 & 3. The density prediction AND the depth-weld breakdown, ------
    // ---- together — both read off the same per-band grouping. -------------
    println!();
    println!(
        "== EXISTENCE_DENSITY = {PREDICTED_EXISTENCE_DENSITY} prediction (narrow relative \
         spread, a coin flip per address) vs. the measured depth-weld breakdown, by band =="
    );
    let mut band_bucket_total = 0usize;
    for (idx, name) in BAND_NAMES.iter().enumerate() {
        let counts: Vec<usize> = per_seed
            .iter()
            .flat_map(|r| r.cave_cells.iter().copied())
            .filter(|&(_, band, _)| band_index(band) == idx)
            .map(|(c, _, _)| c)
            .collect();
        band_bucket_total += counts.len();
        let n_addr = addresses_in_budget(idx) as f64;
        let theoretical_mean = n_addr * PREDICTED_EXISTENCE_DENSITY;
        let theoretical_sd =
            (n_addr * PREDICTED_EXISTENCE_DENSITY * (1.0 - PREDICTED_EXISTENCE_DENSITY)).sqrt();
        let theoretical_cv = if theoretical_mean > 0.0 {
            theoretical_sd / theoretical_mean
        } else {
            0.0
        };
        if counts.is_empty() {
            println!(
                "  band={name}: 0 caves reach this band (Task 0: only Cover/Basement/Roots occur)"
            );
            continue;
        }
        println!(
            "  band={name} (in-budget addresses={n_addr:.0}, {} caves): measured {} | \
             predicted Binomial({n_addr:.0}, {PREDICTED_EXISTENCE_DENSITY}) mean={theoretical_mean:.4} \
             sd={theoretical_sd:.4} cv={theoretical_cv:.4}",
            counts.len(),
            summarize(&counts)
        );
    }
    assert_eq!(
        band_bucket_total, total_caves,
        "band buckets sum to {band_bucket_total} but {total_caves} caves were counted — \
         a cave landed in no band bucket"
    );

    // ---- 4. Reachability from a flagship start -----------------------------
    println!();
    println!("== Reachability (ledger #26): flagship start -> nearest NON-SEALED cave ==");
    let mut unreachable_or_settlementless = 0usize;
    let mut distances: Vec<usize> = Vec::new();
    for r in &per_seed {
        match r.flagship_to_open_cave {
            Some(d) => {
                distances.push(d as usize);
                println!(
                    "  seed {}: flagship -> nearest non-sealed cave = {d} land-graph hops",
                    r.seed
                );
            }
            None => {
                println!(
                    "  seed {}: NO reachable non-sealed cave from the flagship (no settlement, \
                     or a different landmass)",
                    r.seed
                );
                unreachable_or_settlementless += 1;
            }
        }
    }
    if !distances.is_empty() {
        println!(
            "  aggregate over {}/{} seeds with a computable distance: {}",
            distances.len(),
            per_seed.len(),
            summarize(&distances)
        );
    }
    println!(
        "  {unreachable_or_settlementless}/{} seeds had NO reachable non-sealed cave from the \
         flagship",
        per_seed.len()
    );

    println!();
    println!("== Land-cell coverage: fraction of land within R hops of ANY non-sealed cave ==");
    for (i, &radius) in REPORT_RADII.iter().enumerate() {
        let fractions: Vec<f64> = per_seed.iter().map(|r| r.coverage_by_radius[i]).collect();
        let mean = fractions.iter().sum::<f64>() / fractions.len() as f64;
        let mut sorted = fractions.clone();
        sorted.sort_by(f64::total_cmp);
        let median = sorted[sorted.len() / 2];
        println!(
            "  R={radius} hops: mean fraction of land within reach = {mean:.4}, median = {median:.4}"
        );
    }

    let total_sealed = per_seed
        .iter()
        .flat_map(|r| r.cave_cells.iter())
        .filter(|&&(_, _, sealed)| sealed)
        .count();
    println!();
    println!(
        "== Sealed fraction (ledger #23): {total_sealed}/{total_caves} = {:.4} (predicted ~0.485) ==",
        total_sealed as f64 / total_caves.max(1) as f64
    );

    assert!(total_caves > 0, "no caves measured across any seed");
}
