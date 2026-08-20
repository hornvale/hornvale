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
//! ## Follow-up, 2026-08-20: could `Sunless` be split into a sixth rung?
//!
//! Nathan asked what a sixth rung below `Sunless` would admit, targeting
//! `P(reach it | reached Underdeep) ~ 0-10%`.
//! [`could_sunless_be_split_into_a_sixth_rung`] takes the reading.
//!
//! **THE ANSWER IS NO, AND THE PRIOR THAT PREDICTED IT WAS WRONG ABOUT WHY.**
//! The hypothesis on the table was that the super-50 K population is a clamp
//! atom — everything pinned at `CAVE_REACH_CEILING_M`, so the class is
//! `gradient x 3.0` and has no interior. **Measured, 3 of 3821 systems (0.08%)
//! sit at that ceiling**; on seeds 42 and 7 the count is zero. The 200 m
//! lava-tube ceiling binds on 0.77-2.53%, all of it shallow. The gradient
//! clamps bind on nothing at all: the realized band is 20.694-29.226 K/km
//! inside a `[15, 30]` clamp. **No clamp is binding on the population in
//! question.**
//!
//! What is true is the compression, by a different route. ΔT above 50 K is one
//! mode with a thin whisker: 50-60 K holds 212 / 722 / 530 of the 214 / 727 /
//! 536 systems above the floor — **98.6% / 99.3% / 98.9%** — and p99 is only
//! 58.5 / 59.9 / 61.0 K. The observed max ΔT is **68.086 K against a possible
//! 90.0 K**, so a quarter of the theoretical headroom is never used: reach and
//! gradient do not attain their maxima together (a joint fact this readout
//! bounds but does not decompose).
//!
//! ```text
//! seed   systems   at 3000 m   at 200 m   ΔT>=50K   p50    p75    p90    p99    max
//!   42       874    0 (0.00%)  14 (1.60%)  24.49%  54.598 55.963 57.508 58.525 61.298
//!    7      1681    0 (0.00%)  13 (0.77%)  43.25%  55.183 56.620 57.697 59.901 67.603
//! 1234      1266    3 (0.24%)  32 (2.53%)  42.34%  56.710 58.817 59.733 61.028 68.086
//!
//! gradient K/km realized:  42: 21.670 / 24.419 / 29.226   (min / p50 / max)
//!                           7: 21.317 / 25.004 / 29.123
//!                        1234: 20.694 / 23.082 / 29.049
//! ```
//!
//! **Every candidate X fails on one of two axes, and the two failures are the
//! same fact seen twice.** Inside the mode (X <= 59) the class is far too big
//! and the edge sits in traffic; outside it (X >= 61) the edge is quiet only
//! because the region is empty, and a "stability" figure taken over 2 systems
//! measures emptiness, not stability.
//!
//! ```text
//! X (K)  systems 42/7/1234   P(>=X | Underdeep) 42/7/1234   within +-0.5 K 42/7/1234
//!   55      88 / 386 / 333     32.96% / 44.01% / 50.08%       4.69% / 7.67% / 0.87%
//!   58       9 /  65 / 186      3.37% /  7.41% / 27.97%       2.17% / 2.44% / 3.48%
//!   59       2 /  33 / 113      0.75% /  3.76% / 16.99%       0.11% / 1.31% / 7.50%
//!   60       2 /   5 /  36      0.75% /  0.57% /  5.41%       0.00% / 1.01% / 4.11%
//!   61       2 /   2 /   6      0.75% /  0.23% /  0.90%       0.23% / 0.12% / 0.95%
//!   62       0 /   2 /   5      0.00% /  0.23% /  0.75%       0.00% / 0.00% / 0.00%
//!   65+      0 /   2 /   5      0.00% /  0.23% /  0.75%       0.00% / 0.06% / 0.00%
//!   70+      0 /   0 /   0      0.00% /  0.00% /  0.00%       0.00% / 0.00% / 0.00%
//! ```
//!
//! Against the ladder's own edges (2 K and 8 K at 0.0%, 25 K at 0.2-1.1%,
//! 50 K at 0.0-1.5% and documented as the least well-placed):
//!
//! - **X = 60 is the only candidate in the target band on all three seeds**
//!   (0.57-5.41%), and its stability is **4.11% on seed 1234 — 2.7x the worst
//!   edge in the ladder**, worse than the 10 K edge that was condemned and
//!   moved. Its rate also swings 9.5x across the panel.
//! - **X = 61 has stability comparable to the 25 K edge** (0.12-0.95%) and
//!   admits **2 / 2 / 6 systems per world**. Seed 42's entire population above
//!   it is two caves.
//! - **X >= 62 gives seed 42 zero tenants** — the reading Task 0 exists to
//!   refuse, reappearing one rung down.
//!
//! So there is no X that is simultaneously occupied on every preregistered
//! seed, inside the target band, and stable. The useful answer is that
//! `Sunless` is not a tail that can be subdivided; it is a mode with a
//! two-cave whisker.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_terrain::{CAVE_REACH_CEILING_M, DelveRung, TerrainPins, rung_at_depth, rungs};
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

// ---------------------------------------------------------------------------
// THE SIXTH RUNG — could `Sunless` be split, and is there a stable X?
// ---------------------------------------------------------------------------

/// The ΔT at which `Sunless` begins — `hornvale_terrain::HABITABLE_CEILING_K`,
/// re-stated here as the floor of the population this readout examines rather
/// than imported, so that a change to the ladder does not silently redefine
/// what "above Sunless's floor" means in a table already printed.
const SUNLESS_FLOOR_K: f64 = 50.0;

/// The ΔT at which `Underdeep` begins — the conditioning event for the target
/// Nathan named, `P(reach the new rung | reached Underdeep)`.
const UNDERDEEP_FLOOR_K: f64 = 25.0;

/// The largest ΔT any cave can carry: the gradient's upper clamp
/// (`OCEANIC_GRADIENT_K_PER_KM = 30.0`, private to
/// `domains/terrain/src/strata.rs`) times [`CAVE_REACH_CEILING_M`] in km.
/// `delve.rs`'s own module doc states the same 90 K bound in prose; this is
/// the checkable form of it.
const MAX_POSSIBLE_DELTA_T_K: f64 = 30.0 * (CAVE_REACH_CEILING_M / 1000.0);

/// Candidate floors for a hypothetical sixth rung, in K above the datum.
///
/// The panel Nathan's question named was `{55, 60, 65, 70, 75, 80, 90, 100}`.
/// It is **extended downward and finely**, because the first run showed the
/// entire occupied range above `Sunless`'s floor ends by 68.1 K on the hottest
/// seed: every candidate at or above 70 K scores an unbroken column of zeros
/// and the three points below it are too coarse to see where the population
/// actually stops. The extra rows are 57-63 K, and they are added to *look*,
/// not to find a value that scores well — see this test's verdict in the
/// module doc, which is that none does.
const CANDIDATE_SPLITS_K: [f64; 14] = [
    55.0, 57.0, 58.0, 59.0, 60.0, 61.0, 62.0, 63.0, 65.0, 70.0, 75.0, 80.0, 90.0, 100.0,
];

/// How close to a clamp counts as sitting *on* it. The reach values are
/// arithmetic products of quantities that are themselves clamped, so an exact
/// `==` would undercount by a few ULP without changing the finding.
const CLAMP_EPSILON_M: f64 = 1e-6;

/// claim: rate(share of cave systems above candidate sixth-rung floors; seeds
/// 42 / 7 / 1234) — the shape of the ΔT distribution above `Sunless`'s floor,
/// how much of it sits on a reach clamp, and what a sixth rung placed at each
/// candidate X would admit.
///
/// **A design readout, not a gate on the ladder.** It changes no production
/// code and asserts nothing about where a sixth rung should go — placing one
/// is a decision, and a test that pinned a preferred X would be this
/// campaign's opinion masquerading as a measurement. What it *does* assert is
/// the physical ceiling the question runs into: no cave's reach exceeds
/// [`CAVE_REACH_CEILING_M`], so no cave's ΔT can exceed
/// [`MAX_POSSIBLE_DELTA_T_K`]. That bound is the answer's mechanism, so it is
/// checked rather than narrated.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn could_sunless_be_split_into_a_sixth_rung() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mut pooled_super: usize = 0;
    let mut pooled_systems: usize = 0;
    let mut pooled_at_reach_ceiling: usize = 0;
    let mut observed_max_delta_t = f64::NEG_INFINITY;

    for seed_value in SEEDS {
        let seed = hornvale_kernel::Seed(seed_value);
        let artifacts = build_world_to_with_artifacts(
            seed,
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Terrain,
        )
        .expect("probe seed builds");
        let terrain = artifacts
            .terrain
            .expect("terrain is Some at BuildDepth::Terrain");
        let geo = terrain.geosphere();

        let mut land = 0usize;
        let mut delta_t: Vec<f64> = Vec::new();
        let mut gradients: Vec<f64> = Vec::new();
        let mut at_reach_ceiling = 0usize;
        // `LAVATUBE_CEILING_M` is private to `domains/terrain/src/cave_depth.rs`
        // (line 128); its value is restated here because a test outside that
        // module cannot import it.
        let lavatube_ceiling_m = 200.0_f64;
        let mut at_lavatube_ceiling = 0usize;

        for cell in geo.cells() {
            if terrain.is_ocean(cell) {
                continue;
            }
            land += 1;
            let Some(cave) = terrain.cave_at(cell) else {
                continue;
            };
            let gradient = terrain.geothermal_gradient_at(cell);
            delta_t.push(gradient.get() * (cave.depth_reach_m / 1000.0));
            gradients.push(gradient.get());
            if (cave.depth_reach_m - CAVE_REACH_CEILING_M).abs() <= CLAMP_EPSILON_M {
                at_reach_ceiling += 1;
            }
            if (cave.depth_reach_m - lavatube_ceiling_m).abs() <= CLAMP_EPSILON_M {
                at_lavatube_ceiling += 1;
            }
            assert!(
                cave.depth_reach_m <= CAVE_REACH_CEILING_M + CLAMP_EPSILON_M,
                "seed {seed_value}: a cave reaches {} m, past CAVE_REACH_CEILING_M \
                 ({CAVE_REACH_CEILING_M}) — the ΔT ceiling this readout reasons \
                 from does not hold",
                cave.depth_reach_m
            );
        }

        let systems = delta_t.len();
        delta_t.sort_by(f64::total_cmp);
        gradients.sort_by(f64::total_cmp);
        let super_sunless: Vec<f64> = delta_t
            .iter()
            .copied()
            .filter(|d| *d >= SUNLESS_FLOOR_K)
            .collect();
        let reached_underdeep = delta_t.iter().filter(|d| **d >= UNDERDEEP_FLOOR_K).count();
        let seed_max = delta_t.last().copied().unwrap_or(f64::NAN);
        observed_max_delta_t = observed_max_delta_t.max(seed_max);

        println!("\n===== seed {seed_value} =====  land cells {land}  cave systems {systems}");

        // (4) The gradient band actually realized.
        println!(
            "  gradient K/km realized: min {:.3}  p50 {:.3}  max {:.3}  \
             -> ceiling ΔT = max x {:.1} km = {:.3} K",
            gradients.first().copied().unwrap_or(f64::NAN),
            pct(&gradients, 0.50),
            gradients.last().copied().unwrap_or(f64::NAN),
            CAVE_REACH_CEILING_M / 1000.0,
            gradients.last().copied().unwrap_or(f64::NAN) * (CAVE_REACH_CEILING_M / 1000.0),
        );

        // (2) How much of the population sits ON a clamp.
        println!(
            "  at CAVE_REACH_CEILING_M ({CAVE_REACH_CEILING_M} m): {at_reach_ceiling}/{systems} \
             ({:.2}%)   at LAVATUBE_CEILING_M ({lavatube_ceiling_m} m): \
             {at_lavatube_ceiling}/{systems} ({:.2}%)",
            at_reach_ceiling as f64 / systems.max(1) as f64 * 100.0,
            at_lavatube_ceiling as f64 / systems.max(1) as f64 * 100.0,
        );

        // (1) The shape of the super-50 K population.
        println!(
            "  ΔT >= {SUNLESS_FLOOR_K} K: {}/{systems} ({:.2}%)   \
             p50 {:.3}  p75 {:.3}  p90 {:.3}  p99 {:.3}  max {:.3}",
            super_sunless.len(),
            super_sunless.len() as f64 / systems.max(1) as f64 * 100.0,
            pct(&super_sunless, 0.50),
            pct(&super_sunless, 0.75),
            pct(&super_sunless, 0.90),
            pct(&super_sunless, 0.99),
            super_sunless.last().copied().unwrap_or(f64::NAN),
        );

        // 1 K bin occupancy from the Sunless floor to the max, zeros included
        // so a valley is visible as a valley rather than as an absent row.
        let top = seed_max.ceil().max(SUNLESS_FLOOR_K + 1.0) as i64;
        let lo = SUNLESS_FLOOR_K as i64;
        println!("  1 K bins over [{lo}, {top}) K (count per bin):");
        let mut line = String::new();
        for edge in lo..top {
            let count = super_sunless
                .iter()
                .filter(|d| **d >= edge as f64 && **d < (edge + 1) as f64)
                .count();
            line.push_str(&format!("{edge:>4}:{count:<6}"));
            if (edge - lo + 1) % 8 == 0 {
                println!("   {line}");
                line.clear();
            }
        }
        if !line.is_empty() {
            println!("   {line}");
        }

        // (3) The candidate split table.
        println!(
            "  {:>6} {:>8} {:>13} {:>16} {:>20} {:>14}",
            "X (K)",
            "systems",
            "share of ALL",
            "P(>=X | >=50K)",
            "P(>=X | Underdeep)",
            "within +-0.5 K"
        );
        for x in CANDIDATE_SPLITS_K {
            let at_or_above = delta_t.iter().filter(|d| **d >= x).count();
            let near = delta_t.iter().filter(|d| (**d - x).abs() <= 0.5).count();
            println!(
                "  {x:>6.0} {at_or_above:>8} {:>12.2}% {:>15.2}% {:>19.2}% {:>13.2}%",
                at_or_above as f64 / systems.max(1) as f64 * 100.0,
                at_or_above as f64 / super_sunless.len().max(1) as f64 * 100.0,
                at_or_above as f64 / reached_underdeep.max(1) as f64 * 100.0,
                near as f64 / systems.max(1) as f64 * 100.0,
            );
        }
        println!(
            "  (denominators: all systems {systems}; >= {SUNLESS_FLOOR_K} K              {}; >= {UNDERDEEP_FLOOR_K} K {reached_underdeep})",
            super_sunless.len()
        );

        assert!(
            systems > 0,
            "seed {seed_value} has no cave systems — this readout is vacuous"
        );
        assert!(
            !super_sunless.is_empty(),
            "seed {seed_value} has no system above {SUNLESS_FLOOR_K} K — there is \
             nothing to split and the readout below is vacuous"
        );

        pooled_systems += systems;
        pooled_super += super_sunless.len();
        pooled_at_reach_ceiling += at_reach_ceiling;
    }

    println!(
        "\n===== pooled =====  systems {pooled_systems}  ΔT >= {SUNLESS_FLOOR_K} K: \
         {pooled_super} ({:.2}%)  at the reach ceiling: {pooled_at_reach_ceiling} ({:.2}%)  \
         observed max ΔT {observed_max_delta_t:.3} K of a possible \
         {MAX_POSSIBLE_DELTA_T_K:.1} K",
        pooled_super as f64 / pooled_systems.max(1) as f64 * 100.0,
        pooled_at_reach_ceiling as f64 / pooled_systems.max(1) as f64 * 100.0,
    );

    // The mechanism the whole answer rests on, checked rather than narrated:
    // reach is clamped, the gradient is clamped, so ΔT has a hard ceiling.
    assert!(
        observed_max_delta_t <= MAX_POSSIBLE_DELTA_T_K + CLAMP_EPSILON_M,
        "observed max ΔT {observed_max_delta_t} K exceeds the ceiling the clamps \
         imply ({MAX_POSSIBLE_DELTA_T_K} K); the reasoning about a sixth rung's \
         headroom is built on that ceiling"
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
