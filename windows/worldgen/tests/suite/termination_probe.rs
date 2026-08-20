//! THE STOPE, Task 0: where does a cave system's delve TERMINATE, and how
//! rare is [`DelveRung::Sunless`]?
//!
//! **A measurement that can end the campaign.** Spec §3.3 plans characters
//! for the deepest rung; if no cave system's delve reaches `Sunless`, that
//! rung has no tenants and the design must be reconsidered before any of it
//! is built. Nothing here changes production code — the terminating rung is
//! already a pure read over shipped fields.
//!
//! ## What is measured, and against what denominator
//!
//! A **cave system** is a cave-bearing LAND cell. Today's terrain model
//! reports at most one [`hornvale_terrain::Cave`] per cell
//! (`GeneratedTerrain::cave_at(id) -> Option<Cave>`), so one such cell is one
//! system and the denominator is the count of them. Ocean cells are excluded
//! and counted separately below so the choice is visible rather than implied;
//! a share here is always `systems terminating at rung r / cave-bearing land
//! cells`, never a share of all cells.
//!
//! The **terminating rung** is
//! `rung_at_depth(cave.depth_reach_m, terrain.geothermal_gradient_at(cell))`
//! — the DELVE ladder (`domains/terrain/src/delve.rs`), which is spaced by
//! temperature offset above the cell's surface datum. It is NOT
//! `Cave::deepest_band`, which is the STRATIGRAPHIC ladder (Regolith / Cover
//! / Basement / Roots / Underneath). The two are independent and neither
//! derives the other (`windows/worldgen/src/chamber.rs`'s module doc says so
//! explicitly), so the same `depth_reach_m` is two different rungs in two
//! cells whose gradients differ. A probe that read `deepest_band` would be
//! answering a different question than §4.1's branch table asks.
//!
//! ## The branch table (the campaign brief's Step 3), as a decision rule
//!
//! ```text
//! Sunless > 25%     -> the ladder's ΔT boundaries decide rarity, not this
//!                      campaign. STOP: an ordinary eldritch band is a
//!                      finding about The Underworld's calibration.
//! Sunless 1% - 15%  -> proceed; report the exact rate.
//! Sunless < 1%      -> the deepest characters have no tenants. STOP.
//! (15%, 25%]        -> THE BRIEF NAMES NO ARM HERE. See below.
//! ```
//!
//! The table as written has a **hole between 15% and 25%**, and it is not
//! academic: seed 42 lands in it (24.49%). That is recorded as
//! [`Branch::UnspecifiedByTheBrief`] rather than silently rounded into a
//! neighbour, because a classifier that quietly widens its own bands is how a
//! stopping rule stops meaning anything.
//!
//! The rate that decides the campaign is the **pooled** one across the seed
//! panel — one share over the union of the three seeds' systems — with each
//! seed's own rate printed beside it. Pooling weights by systems, which is
//! the population the design's characters would actually inhabit.
//!
//! ## Measured, 2026-08-20, seeds 42 / 7 / 1234
//!
//! Wall time for the whole probe (three `BuildDepth::Terrain` worlds and the
//! scan over them): **1.059 s**, warm tree.
//!
//! ```text
//! seed 42     land cells 11283   cave systems  874
//!   rung        systems     share    reach p50  reach p90  reach max
//!   Undercroft       77     8.81%         29.1       29.1       29.1
//!   Shallows        131    14.99%        234.9      252.1      266.1
//!   Deeps           399    45.65%        483.5      483.5     1003.4
//!   Underdeep        53     6.06%       1433.5     1502.0     1513.6
//!   Sunless         214    24.49%       2145.7     2398.9     2723.6
//!   depth_reach_m all systems: p10 200.0 p25 399.5 p50 483.5 p75 1502.0 p90 2271.9 max 2723.6
//!
//! seed 7      land cells 19332   cave systems 1681
//!   Undercroft       84     5.00%         28.0       44.4       44.4
//!   Shallows        599    35.63%        227.9      250.8      250.8
//!   Deeps           121     7.20%        518.0      985.1      986.0
//!   Underdeep       150     8.92%       1551.4     1929.1     1940.4
//!   Sunless         727    43.25%       2260.4     2474.3     2913.4
//!   depth_reach_m all systems: p10 215.3 p25 227.9 p50 1408.6 p75 2246.6 p90 2474.3 max 2913.4
//!
//! seed 1234   land cells 11684   cave systems 1266
//!   Undercroft       91     7.19%         29.3       46.5       73.6
//!   Shallows        144    11.37%        207.6      227.4      234.3
//!   Deeps           366    28.91%        478.2      686.9      932.9
//!   Underdeep       129    10.19%       1458.6     1903.8     2039.1
//!   Sunless         536    42.34%       2441.5     2702.6     3000.0
//!   depth_reach_m all systems: p10 201.7 p25 461.1 p50 1201.2 p75 2311.7 p90 2694.0 max 3000.0
//!
//! pooled      systems 3821   hist [252, 874, 886, 332, 1477]
//!   Sunless 1477/3821 = 38.65%
//!
//! per-seed branch:  42 -> UnspecifiedByTheBrief (24.49%)
//!                    7 -> BoundariesDecideRarity (43.25%)
//!                 1234 -> BoundariesDecideRarity (42.34%)
//! pooled branch:         BoundariesDecideRarity (38.65%)
//! ```
//!
//! **Branch: `Sunless > 25%` — STOP.** 38.65% of cave systems pooled (24.49 /
//! 43.25 / 42.34% per seed) terminate at `Sunless`. The rung is not rare; it
//! is the largest class on two of three seeds and the second largest on the
//! third. §3.3's deepest characters have abundant tenants, and how rare the
//! band *is* is decided by `HABITABLE_CEILING_K = 50.0` and the reach clamps
//! upstream of it — `LAVATUBE_CEILING_M`, `CAVE_REACH_CEILING_M`, and the
//! 15–30 K/km gradient clamp — not by anything this campaign builds.
//!
//! **Seed 42 lands in the hole in the brief's table**, at 24.49%, which is
//! why the pooled rate is the one the rule is applied to and why the fourth
//! `Branch` variant exists. No seed is anywhere near the `< 1%` arm; the
//! campaign-ending reading did not occur.
//!
//! **The reach ceiling is visible in the data.** Seed 1234's deepest system
//! reads exactly 3000.0 m — `CAVE_REACH_CEILING_M` — and every seed's
//! `Undercroft` p50 sits at 28–29 m against a p90 no higher than 46.5 m. The
//! distribution behind the rung shares is a row of atoms, not a spread, which
//! is the same structure `DEEPS_TOP_K`'s doc records; a ΔT boundary moved a
//! kelvin or two can therefore move a large share of systems, and that is
//! precisely why the branch above says the boundaries decide the rarity.
//!
//! **This reproduces figures already in the tree, which is the point.**
//! `domains/terrain/src/delve.rs`'s module doc records 24.5 / 43.2 / 42.3%
//! from the fit that PLACED the ladder's boundaries, and
//! `windows/worldgen/tests/suite/underworld_chamber_reach.rs` prints the same
//! five-bucket histogram as a byproduct of a different question
//! (`[77, 131, 399, 53, 214]` / `[84, 599, 121, 150, 727]` /
//! `[91, 144, 366, 129, 536]`). This probe takes the reading independently,
//! through the public `rung_at_depth` on a fresh build, and — unlike either of
//! those — **asserts the branch**, so the number stops being prose in a doc
//! comment that nothing re-checks.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_terrain::{DelveRung, TerrainPins, rung_at_depth, rungs};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

/// Seeds the campaign preregisters on (spec §5), matching every other
/// live-worldgen probe in this suite.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// The share of systems at or above which the brief calls the rung ordinary
/// and stops the campaign as a calibration finding.
const ABUNDANT_ABOVE: f64 = 0.25;

/// The top of the brief's "proceed" band.
const PROCEED_CEILING: f64 = 0.15;

/// The floor of the brief's "proceed" band — below it, the rung is empty
/// enough that the deepest characters have no tenants.
const NO_TENANTS_BELOW: f64 = 0.01;

/// Which arm of the brief's Step 3 branch table a Sunless share falls in.
///
/// Four arms for a three-arm table on purpose: the table leaves `(15%, 25%]`
/// unnamed, and a share landing there is a defect in the table, not a value
/// to be rounded into whichever neighbour is closer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Branch {
    /// `> 25%` — the ΔT boundaries decide rarity. STOP and report.
    BoundariesDecideRarity,
    /// `[1%, 15%]` — proceed, reporting the exact rate.
    Proceed,
    /// `< 1%` — the deepest characters have no tenants. STOP and report.
    NoTenants,
    /// `(15%, 25%]` — the brief's table names no arm here.
    UnspecifiedByTheBrief,
}

/// Apply the branch table to one Sunless share.
fn classify(share: f64) -> Branch {
    if share < NO_TENANTS_BELOW {
        Branch::NoTenants
    } else if share <= PROCEED_CEILING {
        Branch::Proceed
    } else if share > ABUNDANT_ABOVE {
        Branch::BoundariesDecideRarity
    } else {
        Branch::UnspecifiedByTheBrief
    }
}

/// The ladder's rank over its habitation rungs, shallowest first. `Surface`
/// is not a habitation rung and `rung_at_depth` never returns it, so it has
/// no bucket. Exhaustive: a sixth `DelveRung` fails this to compile.
fn rung_rank(rung: DelveRung) -> Option<usize> {
    match rung {
        DelveRung::Surface => None,
        DelveRung::Undercroft => Some(0),
        DelveRung::Shallows => Some(1),
        DelveRung::Deeps => Some(2),
        DelveRung::Underdeep => Some(3),
        DelveRung::Sunless => Some(4),
    }
}

/// Percentile of an ascending slice.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = ((sorted.len() - 1) as f64 * q).round() as usize;
    sorted[i]
}

/// claim: rate(Sunless share of cave systems; seeds 42 / 7 / 1234) — over the
/// cave-bearing land cells of each seed, the share of cave systems whose
/// delve terminates at each rung of the delve ladder, and the branch of the
/// campaign brief's Step 3 table that the pooled `Sunless` share selects.
///
/// It prints the full distribution, but it is not a bare readout: the branch
/// is a **decision rule**, encoded and asserted, so a later change that moves
/// the rate out of the band this campaign was authorised under reddens here
/// instead of printing a different number into a log nobody reads.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn where_does_a_delve_terminate() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

    // Pooled over the whole panel — the rate the branch table is applied to.
    let mut pooled_systems = 0usize;
    let mut pooled_hist = [0usize; 5];
    // Per seed, kept for the assertions after the table is printed.
    let mut per_seed: Vec<(u64, usize, [usize; 5])> = Vec::new();

    for seed_value in SEEDS {
        let seed = hornvale_kernel::Seed(seed_value);
        let artifacts = build_world_to_with_artifacts(
            seed,
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            // Terrain is the deepest rung this probe reads. `cave_at` and
            // `geothermal_gradient_at` are both `GeneratedTerrain`, so
            // building settlements or deep time would be paying for nothing.
            BuildDepth::Terrain,
        )
        .expect("probe seed builds");
        let terrain = artifacts
            .terrain
            .expect("terrain is Some at BuildDepth::Terrain");
        let geo = terrain.geosphere();

        let mut land = 0usize;
        let mut systems = 0usize;
        let mut ocean_caves = 0usize;
        let mut hist = [0usize; 5];
        let mut reach: Vec<f64> = Vec::new();
        let mut reach_by_rung: [Vec<f64>; 5] = Default::default();

        for cell in geo.cells() {
            if terrain.is_ocean(cell) {
                if terrain.cave_at(cell).is_some() {
                    ocean_caves += 1;
                }
                continue;
            }
            land += 1;
            let Some(cave) = terrain.cave_at(cell) else {
                continue;
            };
            systems += 1;
            let gradient = terrain.geothermal_gradient_at(cell);
            let rung = rung_at_depth(cave.depth_reach_m, gradient);
            let rank = rung_rank(rung).expect("rung_at_depth never returns Surface");
            hist[rank] += 1;
            reach.push(cave.depth_reach_m);
            reach_by_rung[rank].push(cave.depth_reach_m);
        }

        reach.sort_by(f64::total_cmp);
        let sunless_share = hist[4] as f64 / systems.max(1) as f64;

        println!(
            "\n== seed {seed_value} ==  land cells {land}  cave systems {systems}  \
             (ocean cells carrying a cave: {ocean_caves}, excluded)"
        );
        println!(
            "  {:<11} {:>7} {:>9}   {:>10} {:>10} {:>10}",
            "rung", "systems", "share", "reach p50", "reach p90", "reach max"
        );
        for rung in rungs().iter().copied() {
            let Some(rank) = rung_rank(rung) else {
                continue;
            };
            let mut d = reach_by_rung[rank].clone();
            d.sort_by(f64::total_cmp);
            println!(
                "  {:<11} {:>7} {:>8.2}%   {:>10.1} {:>10.1} {:>10.1}",
                format!("{rung:?}"),
                hist[rank],
                hist[rank] as f64 / systems.max(1) as f64 * 100.0,
                pct(&d, 0.50),
                pct(&d, 0.90),
                d.last().copied().unwrap_or(f64::NAN),
            );
        }
        println!(
            "  depth_reach_m over all systems: p10 {:.1}  p25 {:.1}  p50 {:.1}  \
             p75 {:.1}  p90 {:.1}  max {:.1}",
            pct(&reach, 0.10),
            pct(&reach, 0.25),
            pct(&reach, 0.50),
            pct(&reach, 0.75),
            pct(&reach, 0.90),
            reach.last().copied().unwrap_or(f64::NAN),
        );
        println!(
            "  Sunless share {:.2}%  ->  branch {:?}",
            sunless_share * 100.0,
            classify(sunless_share)
        );

        pooled_systems += systems;
        for rank in 0..5 {
            pooled_hist[rank] += hist[rank];
        }
        per_seed.push((seed_value, systems, hist));
    }

    let pooled_share = pooled_hist[4] as f64 / pooled_systems.max(1) as f64;
    let pooled_branch = classify(pooled_share);
    println!(
        "\n== pooled ==  systems {pooled_systems}  hist {pooled_hist:?}  \
         Sunless {}/{} = {:.2}%  ->  branch {pooled_branch:?}",
        pooled_hist[4],
        pooled_systems,
        pooled_share * 100.0
    );

    // --- Vacuity guards -----------------------------------------------------
    // A probe over zero systems would satisfy nothing below by having nothing
    // to satisfy, and the shares would all be 0/1 = 0 — indistinguishable
    // from a genuine "no tenants" reading.
    for (seed_value, systems, _) in &per_seed {
        assert!(
            *systems > 0,
            "seed {seed_value} has no cave systems — the probe is vacuous, \
             not reporting a real zero"
        );
    }
    assert_eq!(
        per_seed.len(),
        SEEDS.len(),
        "every seed in the panel must contribute a reading"
    );

    // --- Step 3, per seed: the campaign-ending arm ---------------------------
    // The campaign-ending arm, tested on every seed individually and not only
    // on the pool, because a pooled rate can hide an empty seed.
    for (seed_value, systems, hist) in &per_seed {
        let share = hist[4] as f64 / *systems as f64;
        assert_ne!(
            classify(share),
            Branch::NoTenants,
            "seed {seed_value}: Sunless holds {}/{systems} systems ({:.2}%) — \
             the deepest characters have no tenants and the design must be \
             reconsidered (brief Step 3, the `~0%` arm)",
            hist[4],
            share * 100.0
        );
        assert!(
            share > PROCEED_CEILING,
            "seed {seed_value}: Sunless share {:.2}% is at or below the brief's \
             proceed ceiling of {:.0}% — the panel no longer agrees on the \
             branch this campaign was authorised under; re-read Step 3 rather \
             than adjusting this bound",
            share * 100.0,
            PROCEED_CEILING * 100.0
        );
    }

    // --- The termination spread has not collapsed ---------------------------
    // `domains/terrain/src/delve.rs` claims every rung is occupied on every
    // seed. A ladder that collapsed onto one class would still print a
    // plausible table and would make every share below meaningless, so it is
    // asserted rather than trusted.
    for (seed_value, _, hist) in &per_seed {
        for rung in rungs().iter().copied() {
            let Some(rank) = rung_rank(rung) else {
                continue;
            };
            assert!(
                hist[rank] > 0,
                "seed {seed_value}: no cave system terminates at {rung:?} — the \
                 termination spread has collapsed (histogram {hist:?})"
            );
        }
    }

    // --- Step 3, pooled: the branch this campaign proceeds under ------------
    assert_eq!(
        pooled_branch,
        Branch::BoundariesDecideRarity,
        "pooled Sunless share is {:.2}% ({}/{pooled_systems}), which selects \
         {pooled_branch:?}; this campaign's Task 0 landed on \
         {:?} and everything downstream assumes it",
        pooled_share * 100.0,
        pooled_hist[4],
        Branch::BoundariesDecideRarity
    );
    assert!(
        pooled_share > ABUNDANT_ABOVE,
        "pooled Sunless share {:.2}% must exceed {:.0}% for the \
         BoundariesDecideRarity branch to hold",
        pooled_share * 100.0,
        ABUNDANT_ABOVE * 100.0
    );
}

mod branch_table {
    use super::*;

    /// claim: structural(no seeds; a pure read over the branch table) — the
    /// classifier implements the brief's Step 3 table, including the arm the
    /// table does not name. Cheap, so it is NOT `#[ignore]`d: it is the half
    /// of the probe that can be checked without building a world, and it is
    /// what makes the expensive half's verdict legible.
    #[test]
    fn the_branch_table_is_implemented_as_written() {
        assert_eq!(classify(0.0), Branch::NoTenants);
        assert_eq!(classify(0.009), Branch::NoTenants);
        assert_eq!(classify(0.01), Branch::Proceed);
        assert_eq!(classify(0.15), Branch::Proceed);
        // The hole in the brief's table, named rather than absorbed.
        assert_eq!(classify(0.1501), Branch::UnspecifiedByTheBrief);
        assert_eq!(classify(0.25), Branch::UnspecifiedByTheBrief);
        assert_eq!(classify(0.2449), Branch::UnspecifiedByTheBrief);
        assert_eq!(classify(0.2501), Branch::BoundariesDecideRarity);
        assert_eq!(classify(1.0), Branch::BoundariesDecideRarity);
    }

    /// claim: structural(no seeds) — `rung_rank` is a bijection onto
    /// `0..=4` over the habitation rungs, so the histogram's five buckets
    /// are the five rungs and nothing is silently dropped.
    #[test]
    fn every_habitation_rung_has_exactly_one_bucket() {
        let mut seen = vec![false; 5];
        for rung in rungs().iter().copied() {
            match rung_rank(rung) {
                None => assert_eq!(rung, DelveRung::Surface, "only Surface lacks a bucket"),
                Some(rank) => {
                    assert!(!seen[rank], "{rung:?} reuses bucket {rank}");
                    seen[rank] = true;
                }
            }
        }
        assert!(seen.iter().all(|s| *s), "some bucket has no rung: {seen:?}");
    }
}
