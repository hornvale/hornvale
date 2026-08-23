//! THE DRIFT, Task 0: **how much of the underworld can a player reach today?**
//!
//! The campaign's baseline, taken *before* any production change, with the
//! instrument that will later judge the change. Nothing here touches
//! production code — every quantity is a pure read over shipped entry points
//! ([`chamber_exists`], [`passages_from`], [`entrance_count`],
//! [`entrance_mouth`]).
//!
//! # WHAT IS MEASURED, AND WHY THE DENOMINATORS ARE WHAT THEY ARE
//!
//! Spec §6 preregisters two quantities. Both are stated here in the units this
//! file computes them in, because a share is only as meaningful as its
//! denominator:
//!
//! ```text
//! SHARE OF A SYSTEM'S LEVELS REACHABLE FROM ITS OWN ENTRANCES
//!     Per system, over systems with at least one OPEN mouth, reported as a
//!     distribution (p10 / median / p90). >= 95% median is the intent;
//!     50-95% reports; < 50% means the deletion did not do what §4 claims.
//!
//! SHARE OF ALL EXISTING LEVELS REACHABLE
//!     The whole-world figure. Levels in systems with NO open entrance stay
//!     inside the denominator on purpose: an unreachable system is a real
//!     outcome and must stay visible rather than be defined away.
//! ```
//!
//! **ONE WALK PER SYSTEM, FROM THE UNION OF ITS OPEN MOUTHS — never a sum of
//! per-entrance walks.** Two mouths into one lattice reach overlapping sets,
//! and adding them counts a level twice. Before The Drift (Task 4, spec
//! amendment A.3) the union was written for a hazard this file called
//! INERT — `passages_from` propagated `addr.entrance` unchanged, so a walk
//! seeded at entrance *e* could never leave entrance *e*'s own sublattice,
//! and the union agreed with a naive sum by construction. **That hazard is
//! now LIVE.** `entrance` left `ChamberAddr` entirely, so every mouth of a
//! system addresses INTO the same shared lattice, and two mouths' walks
//! routinely overlap — summing them would double-count. The union is
//! written once, in [`reachable_union`], and both shares read off it.
//!
//! **A mouth is OPEN when the level it names exists.** That is exactly what
//! `docs/audits/underworld-lattice-seed-panel.md` counts as an "open
//! entrance": `underworld_readout` tests the mouth address with `chamber_at`,
//! which is [`chamber_exists`] plus content resolution and returns `None`
//! precisely when existence is refused. So this probe's openness test and the
//! committed artifact's are the same predicate, and the two are comparable
//! line for line.
//!
//! # THE DENOMINATOR IS THE SYSTEM'S ONE SHARED LATTICE, NOT A SUM OVER ENTRANCES
//!
//! **This section used to argue the opposite, and the reversal is the whole
//! story of Task 4.** Before The Drift, `chamber_exists` gated on
//! `branch_count_of(seed, cell, addr.entrance)` and every per-address stream
//! carried `entrance`, so each entrance realized its OWN private sublattice —
//! the committed witness summed existence over `0..drawn_entrances`, which is
//! how seed 42 reached 21,328 (pre-Task-1) or 42,820 (post-Task-1, pre-Task-4)
//! levels from 1,229 drawn entrances. Task 0's baseline measured this and
//! Nathan's ruling (spec amendment A) found it could not express the
//! campaign's own worked example — two doors into ONE Spider Cave.
//!
//! So `entrance` left `ChamberAddr` and `RunAddr` entirely (amendment A.3).
//! `levels` below is now the size of the system's ONE shared lattice, walked
//! ONCE regardless of how many entrances open into it — see [`read_system`].
//! Entrances survive only as which aperture a mouth resolves through
//! ([`entrance_count`], [`entrance_mouth`]); they no longer size the
//! population at all. This is why `levels` FELL from Task 1's 42,820 to
//! 30,272 on seed 42 even though nothing about the terrain or the existence
//! gate changed — a sum over per-entrance sublattices became a single
//! lattice's own size, which is smaller by construction whenever a system
//! draws more than one entrance. **Report this as a structural consequence
//! of amendment A.5, never as a regression**: the spec named this move in
//! advance for exactly this reason.
//!
//! **No more "head-lattice only" secondary reading.** It used to report the
//! entrance-0 sublattice alone, which was a distinct, smaller population than
//! the whole system's summed reading. With one shared lattice per system that
//! secondary reading is now definitionally identical to `levels`/`reachable`
//! themselves, so keeping it would print the same two numbers twice under a
//! different label — see [`SystemReach`]'s own doc.
//!
//! # MEASURED VALUES — BEFORE, 2026-08-23, tree at `campaign/the-drift` prior to Task 1
//!
//! ```text
//! seed 42
//!   systems 874    levels 21328   reachable 1496   entrances 1229 drawn / 511 open
//!   systems with an open mouth 450
//!   per-system reachable share   p10 3.70%   median 12.50%   p90 57.14%
//!   whole-world reachable share  7.01%   (1496 of 21328)
//!   levels per system            mean 24.40   p10 4   median 17   p90 54
//! seed 7
//!   systems 1681   levels 42131   reachable 3277   entrances 2382 drawn / 1070 open
//!   systems with an open mouth 940
//!   per-system reachable share   p10 3.74%   median 13.04%   p90 50.00%
//!   whole-world reachable share  7.78%   (3277 of 42131)
//!   levels per system            mean 25.06   p10 4   median 17   p90 57
//! seed 1234
//!   systems 1266   levels 36393   reachable 2493   entrances 1813 drawn / 831 open
//!   systems with an open mouth 715
//!   per-system reachable share   p10 3.23%   median 11.11%   p90 40.00%
//!   whole-world reachable share  6.85%   (2493 of 36393)
//!   levels per system            mean 28.75   p10 6   median 20   p90 64
//! ```
//!
//! Seed 42's three pre-change headline integers reproduce spec §1 and the
//! pre-Task-1 committed witness exactly: **1,496 reachable of 21,328 existing
//! (7.01%) from 511 open entrances**, across 874 systems.
//!
//! **The whole-world share was stable across the panel at 6.85-7.78%**, and
//! every seed's per-system median sat an order of magnitude under §6's 95%
//! intent. The pre-change world was not marginal on either quantity.
//!
//! # MEASURED VALUES — AFTER TASK 1, 2026-08-23 (`chamber_exists`'s existence
//! # coin deleted, spec §4.1) — SUPERSEDED BELOW, kept for the movement record
//!
//! ```text
//! seed 42
//!   systems 874    levels 42820   reachable 39140   entrances 1229 drawn / 1101 open
//!   systems with an open mouth 874
//!   per-system reachable share   p10 83.87%   median 100.00%   p90 100.00%
//!   whole-world reachable share  91.41%   (39140 of 42820)
//!   levels per system            mean 48.99   p10 9   median 34   p90 105
//! seed 7
//!   systems 1681   levels 84424   reachable 78677   entrances 2382 drawn / 2155 open
//!   systems with an open mouth 1681
//!   per-system reachable share   p10 85.85%   median 100.00%   p90 100.00%
//!   whole-world reachable share  93.19%   (78677 of 84424)
//!   levels per system            mean 50.22   p10 9   median 33   p90 112
//! seed 1234
//!   systems 1266   levels 72304   reachable 66986   entrances 1813 drawn / 1636 open
//!   systems with an open mouth 1266
//!   per-system reachable share   p10 84.27%   median 100.00%   p90 100.00%
//!   whole-world reachable share  92.64%   (66986 of 72304)
//!   levels per system            mean 57.11   p10 12   median 39   p90 128
//! ```
//!
//! Seed 42 moved from 1,496 reachable of 21,328 (7.01%) to 39,140 of 42,820
//! (91.41%) after Task 1 — levels itself roughly doubled (the coin used to
//! also suppress about half of every drawn floor from EXISTING at all, not
//! only from being reached) and the reachable count grew 26.2x. Every seed
//! cleared both of spec §6's preregistered intents there for the first time.
//!
//! # MEASURED VALUES — AFTER TASK 4, 2026-08-23 (`entrance` dropped from the
//! # address, spec amendment A.3) — THE CURRENT TREE
//!
//! ```text
//! seed 42
//!   systems 874    levels 30272   reachable 30272   entrances 1229 drawn / 1154 open
//!   systems with an open mouth 874
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  100.00%   (30272 of 30272)
//!   levels per system            mean 34.64   p10 8   median 28   p90 68
//! seed 7
//!   systems 1681   levels 60119   reachable 60119   entrances 2382 drawn / 2250 open
//!   systems with an open mouth 1681
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  100.00%   (60119 of 60119)
//!   levels per system            mean 35.76   p10 8   median 29   p90 75
//! seed 1234
//!   systems 1266   levels 49002   reachable 49002   entrances 1813 drawn / 1728 open
//!   systems with an open mouth 1266
//!   per-system reachable share   p10 100.00%   median 100.00%   p90 100.00%
//!   whole-world reachable share  100.00%   (49002 of 49002)
//!   levels per system            mean 38.71   p10 9   median 31   p90 76
//! ```
//!
//! **Seed 42: `levels` FELL from 42,820 (Task 1) to 30,272, and `reachable`
//! ROSE to meet it exactly — 30,272 of 30,272, 100.00%.** This is the shape
//! spec amendment A.5 predicted in advance: "the count falls for a reason
//! that has nothing to do with the deleted coin," because 21,328/42,820 were
//! sums over private per-entrance sublattices and 30,272 is one shared
//! lattice's own size. **All three panel seeds now read EXACTLY 100.00% on
//! both arms** — per-system median and whole-world share both at the
//! ceiling, not merely past spec §6's 95%/90% intents. `entrances` (the
//! drawn aperture count) is unchanged from Task 1 on every seed, because
//! `entrance_count` is untouched by this task; `open entrances` rose
//! slightly (1101->1154 on seed 42) because a mouth now resolves against the
//! system's ONE shared lattice rather than its own entrance's private one,
//! which is strictly more permissive.
//!
//! Wall time for the whole probe (three `BuildDepth::Terrain` worlds and
//! ~1.6M `chamber_exists` calls) is ~1.5 s in the optimized test profile.
//! It is `#[ignore]`d anyway, with the same reason every live-worldgen battery
//! in this suite carries: the cost that matters is the world build, and the
//! commit gate does not pay for those (decision 0132).
//!
//! # THE BAND CEILING IS DERIVED, NEVER RESTATED
//!
//! [`habitation_ranks`] walks the delve ladder through the shipped
//! [`rung_rank`], the same route `junctions.rs` uses, because
//! `chamber::rung_of_rank` is private. No literal `0..5` appears anywhere in
//! this file; a sixth habitation rung widens the scan without an edit here.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use std::collections::BTreeSet;

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Band, CellId, Seed};
use hornvale_terrain::{Cave, GeothermalGradient, TerrainPins, rungs};
use hornvale_worldgen::chamber::{
    BRANCHES_PER_SYSTEM, ChamberAddr, LEVELS_PER_BRANCH_CEILING, chamber_exists, entrance_count,
    entrance_mouth, passages_from, rung_rank,
};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

/// The seeds the campaign preregisters on (spec §5), matching every other
/// live-worldgen probe in this suite.
const SEEDS: [u64; 3] = [42, 7, 1234];

/// Seed 42's baseline, as `(levels, reachable, open_entrances)` — the exact
/// triple the committed witness `docs/audits/underworld-lattice-seed-panel.md`
/// renders, re-baselined once already.
///
/// **Pinned as an equality on purpose, and it has now broken twice, exactly
/// as designed.** Before Task 1 landed this held `(21328, 1496, 511)` —
/// spec §1's opening figure. Task 1 (spec §4.1) deleted `chamber_exists`'s
/// existence coin and moved it to `(42820, 39140, 1101)`. **Task 4 (spec
/// amendment A.3) moved it a second time, to `(30272, 30272, 1154)`**:
/// dropping `entrance` from `ChamberAddr` collapses each system's
/// per-entrance sublattices into ONE shared lattice, so `levels` fell (a
/// structural consequence of amendment A.5, not a regression — see this
/// module's header) while `reachable` rose to EXACTLY equal it: every one of
/// the three panel seeds now reads 100.00% whole-world reachable, not merely
/// past spec §6's 90% intent but at its ceiling. The module's own "MEASURED
/// VALUES" block records the movement in full. A move from any OTHER cause
/// from here on — a terrain change, a stream relabelling, a lattice constant
/// — is a determinism finding, and this equality still catches that.
///
/// A band was considered and rejected: this is not a noisy statistic but a
/// deterministic count over a fixed seed, and a band around a deterministic
/// count only buys room for an undetected change.
const SEED_42_BASELINE: (usize, usize, usize) = (30272, 30272, 1154);

/// The habitation band ranks, ascending — **derived from the delve ladder**
/// through the shipped [`rung_rank`], never restated as a literal range.
///
/// `chamber::rung_of_rank` is private to that module, so filtering [`rungs`]
/// through [`rung_rank`] is the sanctioned route from a test crate; it is
/// `junctions.rs`'s `habitation_bands` with the rung dropped, since this file
/// needs only the rank.
fn habitation_ranks() -> Vec<u8> {
    let mut ranks: Vec<u8> = rungs().iter().filter_map(|&rung| rung_rank(rung)).collect();
    ranks.sort_unstable();
    ranks
}

/// Percentile of an ascending slice of shares, by the round-half-away index
/// convention (`round((n-1) q)`) — the convention every other probe in this
/// suite uses, so the numbers are comparable across files.
fn pct(sorted: &[f64], q: f64) -> f64 {
    if sorted.is_empty() {
        return f64::NAN;
    }
    let i = ((sorted.len() - 1) as f64 * q).round() as usize;
    sorted[i]
}

/// The same percentile over an ascending slice of counts, so the two
/// distributions this file prints share one index convention.
fn pct_usize(sorted: &[usize], q: f64) -> usize {
    if sorted.is_empty() {
        return 0;
    }
    let i = ((sorted.len() - 1) as f64 * q).round() as usize;
    sorted[i]
}

/// A share, with an explicit zero for an empty denominator so a vacuous
/// reading is never rendered as a plausible number.
fn share(numerator: usize, denominator: usize) -> f64 {
    if denominator == 0 {
        0.0
    } else {
        numerator as f64 / denominator as f64
    }
}

/// The levels of one cave system reachable from ANY of `mouths`, by
/// [`passages_from`], with every mouth seeded into ONE shared set.
///
/// The union, not a sum: see this module's header. The shape mirrors
/// `underworld_readout::reachable_union`, which is what the committed witness
/// counts with.
fn reachable_union(
    seed: Seed,
    cave: &Cave,
    gradient: GeothermalGradient,
    mouths: &[ChamberAddr],
) -> BTreeSet<ChamberAddr> {
    let mut seen: BTreeSet<ChamberAddr> = BTreeSet::new();
    let mut frontier: Vec<ChamberAddr> = Vec::new();
    for &mouth in mouths {
        if seen.insert(mouth) {
            frontier.push(mouth);
        }
    }
    while let Some(addr) = frontier.pop() {
        for next in passages_from(seed, cave, gradient, addr) {
            if seen.insert(next) {
                frontier.push(next);
            }
        }
    }
    seen
}

/// One cave system's reading.
///
/// **No more "head-lattice only" secondary reading** (The Drift, amendment
/// A.3). Before this campaign each entrance realized its own private
/// sublattice, so "the entrance-0 sublattice alone" was a distinct, smaller
/// population than the whole system — the pair this struct used to carry as
/// `levels_head`/`reachable_head`. With `entrance` gone from `ChamberAddr`
/// there is exactly ONE lattice per system, so that secondary reading is now
/// definitionally identical to `levels`/`reachable` and carries no
/// information a reader could not already see; keeping it would print the
/// same two numbers twice under different labels.
#[derive(Clone, Copy, Debug, Default)]
struct SystemReach {
    /// Apertures this system draws, whether or not they open onto anything.
    drawn_mouths: usize,
    /// Apertures whose named level exists — the witness's "open entrances".
    open_mouths: usize,
    /// Levels that exist in this system's ONE shared lattice.
    levels: usize,
    /// Levels reached by one walk from the union of the open mouths.
    reachable: usize,
}

/// Read one cave system through the shipped entry points only.
fn read_system(
    seed: Seed,
    cell: CellId,
    cave: &Cave,
    gradient: GeothermalGradient,
    ranks: &[u8],
) -> SystemReach {
    let entrances = entrance_count(seed, cell);

    // EXISTENCE, over the system's ONE shared lattice — walked once, not once
    // per drawn entrance (The Drift, amendment A.3: every entrance now
    // addresses INTO the same lattice, rather than realizing its own).
    // Bounded by the LATTICE's own ceilings, never by a run's drawn length:
    // bounding by the draw would make `chamber_exists`'s level gate
    // unfalsifiable here, the same trap `underworld_readout`'s module doc
    // records.
    let mut levels = 0usize;
    for &rank in ranks {
        let band = Band::from_rank(rank).expect("ranks come from habitation_ranks()");
        for branch in 0..BRANCHES_PER_SYSTEM {
            for level in 0..LEVELS_PER_BRANCH_CEILING {
                let addr = ChamberAddr {
                    cell,
                    branch,
                    band,
                    level,
                };
                if chamber_exists(seed, cave, gradient, addr) {
                    levels += 1;
                }
            }
        }
    }

    // THE MOUTHS. `EntranceMouth` is a plain struct of `branch`/`band`/`floor`
    // with no accessors, so the address is assembled from its fields; the
    // entrance index comes from the loop, because the mouth type does not
    // carry the aperture it belongs to. Every mouth now resolves into the
    // SAME shared lattice `levels` walked above.
    let mouths: Vec<ChamberAddr> = (0..entrances)
        .map(|entrance| {
            let mouth = entrance_mouth(seed, cell, entrance);
            let band =
                Band::from_rank(mouth.band).expect("entrance_mouth only names a habitation rank");
            ChamberAddr {
                cell,
                branch: mouth.branch,
                band,
                level: mouth.floor,
            }
        })
        .filter(|&addr| chamber_exists(seed, cave, gradient, addr))
        .collect();

    let reached = reachable_union(seed, cave, gradient, &mouths);

    SystemReach {
        drawn_mouths: usize::from(entrances),
        open_mouths: mouths.len(),
        levels,
        reachable: reached.len(),
    }
}

/// One seed's whole-world reading — the object Task 1's re-run reused unchanged.
#[derive(Clone, Debug)]
struct ReachSummary {
    /// The seed this reading is of.
    seed: u64,
    /// Cave-bearing LAND cells. Ocean caves are excluded and counted
    /// separately, exactly as the committed witness does.
    systems: usize,
    /// Cave-bearing ocean cells, counted rather than silently dropped.
    ocean_systems: usize,
    /// Levels that exist, over every system and every drawn entrance.
    levels: usize,
    /// Levels reachable, one union walk per system.
    reachable: usize,
    /// Apertures drawn across the world.
    drawn_entrances: usize,
    /// Apertures whose named level exists.
    open_entrances: usize,
    /// Systems with at least one open mouth — §6's per-system denominator.
    systems_with_open_mouth: usize,
    /// Per-system reachable share, over those systems only, ascending.
    per_system_share: Vec<f64>,
    /// Existing levels per system, ascending — §6's REPORTED, never gated,
    /// quantity.
    levels_per_system: Vec<usize>,
}

impl ReachSummary {
    /// §6's whole-world quantity: the share of all existing levels reachable.
    fn world_share(&self) -> f64 {
        share(self.reachable, self.levels)
    }

    /// Print the reading. Unconditional: a block read only on failure is a
    /// block nobody reads.
    fn report(&self) {
        let shares = &self.per_system_share;
        let levels_each = &self.levels_per_system;
        let mean_levels = if self.systems == 0 {
            0.0
        } else {
            self.levels as f64 / self.systems as f64
        };
        println!("seed {}", self.seed);
        println!(
            "  systems {}   ocean caves {}   levels {}   reachable {}",
            self.systems, self.ocean_systems, self.levels, self.reachable
        );
        println!(
            "  entrances {} drawn, {} open   systems with an open mouth {}",
            self.drawn_entrances, self.open_entrances, self.systems_with_open_mouth
        );
        println!(
            "  per-system reachable share   p10 {:.2}%   median {:.2}%   p90 {:.2}%",
            100.0 * pct(shares, 0.10),
            100.0 * pct(shares, 0.50),
            100.0 * pct(shares, 0.90)
        );
        println!(
            "  whole-world reachable share  {:.2}%   ({} of {})",
            100.0 * self.world_share(),
            self.reachable,
            self.levels
        );
        println!(
            "  levels per system            mean {:.2}   p10 {}   median {}   p90 {}",
            mean_levels,
            pct_usize(levels_each, 0.10),
            pct_usize(levels_each, 0.50),
            pct_usize(levels_each, 0.90),
        );
        println!(
            "  §6 arms today: per-system median -> {}, whole-world -> {}",
            per_system_arm(pct(shares, 0.50)),
            world_arm(self.world_share())
        );
    }
}

/// §6's per-system arm, as a word. Printed, never asserted: this is the
/// PRE-change reading and the design predicts it lands in the bottom arm, so
/// asserting the intent here would make the baseline fail by construction.
fn per_system_arm(median: f64) -> &'static str {
    if median >= 0.95 {
        "the intent (>= 95%)"
    } else if median >= 0.50 {
        "report (50-95%)"
    } else {
        "below 50% — the pre-change world, as the spec expects"
    }
}

/// §6's whole-world arm, as a word. Printed, never asserted, for the same
/// reason as [`per_system_arm`].
fn world_arm(world: f64) -> &'static str {
    if world >= 0.90 {
        "the intent (>= 90%)"
    } else {
        "below 90% — the pre-change world, as the spec expects"
    }
}

/// Measure one seed, end to end. **Task 1's re-run reused this unchanged.**
fn reach_summary(seed: Seed, wc: &WorldComponents) -> ReachSummary {
    let artifacts = build_world_to_with_artifacts(
        seed,
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        wc,
        // Terrain is the deepest rung this probe reads: `cave_at` and
        // `geothermal_gradient_at` are both `GeneratedTerrain`, and the
        // chamber lattice is a function of the seed and those two facts.
        BuildDepth::Terrain,
    )
    .expect("probe seed builds");
    let terrain = artifacts
        .terrain
        .expect("terrain is Some at BuildDepth::Terrain");
    let geo = terrain.geosphere();
    let ranks = habitation_ranks();

    let mut summary = ReachSummary {
        seed: seed.0,
        systems: 0,
        ocean_systems: 0,
        levels: 0,
        reachable: 0,
        drawn_entrances: 0,
        open_entrances: 0,
        systems_with_open_mouth: 0,
        per_system_share: Vec::new(),
        levels_per_system: Vec::new(),
    };

    for cell in geo.cells() {
        // `cave_at` refuses an ocean cell as its first act, so the ocean test
        // is a COUNT of a case that never carries a cave today, not a guard
        // the walk depends on — and the day it does carry one, the artifact
        // and this probe both say so instead of silently including it.
        let Some(cave) = terrain.cave_at(cell) else {
            continue;
        };
        if terrain.is_ocean(cell) {
            summary.ocean_systems += 1;
            continue;
        }
        summary.systems += 1;
        let gradient = terrain.geothermal_gradient_at(cell);
        let sys = read_system(seed, cell, &cave, gradient, &ranks);

        summary.drawn_entrances += sys.drawn_mouths;
        summary.open_entrances += sys.open_mouths;
        summary.levels += sys.levels;
        summary.reachable += sys.reachable;
        summary.levels_per_system.push(sys.levels);
        if sys.open_mouths > 0 {
            summary.systems_with_open_mouth += 1;
            summary
                .per_system_share
                .push(share(sys.reachable, sys.levels));
        }
    }

    summary.per_system_share.sort_by(|a, b| a.total_cmp(b));
    summary.levels_per_system.sort_unstable();
    summary
}

/// THE BASELINE. Measures §6's two shares across the panel, prints them, and
/// pins seed 42's three headline integers against spec §1 and the committed
/// witness.
///
/// **This test changes no production code and asserts no design intent.** §6's
/// arms are printed, not gated, because they describe the world AFTER the
/// campaign's deletion; gating them here would make the baseline red by
/// construction and teach a reader to ignore it.
///
/// What IS asserted:
///
/// 1. **Non-vacuity, before any share is read.** Every ratio below is `0/0`
///    over an empty corpus and renders as a plausible `0.00%`, which is
///    indistinguishable from a genuine zero. Each denominator is asserted
///    non-empty first.
/// 2. **Seed 42's triple**, exactly — see [`SEED_42_BASELINE`].
/// 3. **The union is not a sum.** `reachable <= levels` per seed: a sum of
///    per-entrance walks can exceed the level count, a union cannot, so this
///    is the cheapest standing check that the double-count hazard the spec
///    names has not reappeared.
///
/// claim: readout(seed: the fixed 3-seed panel 42/7/1234) — a BASELINE, not a
/// quantified claim over seeds. Nothing here is asserted "for all seeds": the
/// panel's job is to say what the world is today so Task 1's deletion can be
/// judged by what it became — see the module header's "MEASURED VALUES —
/// AFTER" block, and the only equality is against seed 42's committed witness
/// (decision 0093).
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn the_drift_reachability_baseline() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

    let summaries: Vec<ReachSummary> = SEEDS.iter().map(|&s| reach_summary(Seed(s), &wc)).collect();

    for summary in &summaries {
        summary.report();
    }

    assert_eq!(
        summaries.len(),
        SEEDS.len(),
        "every seed in the panel must contribute a reading"
    );
    assert!(
        !habitation_ranks().is_empty(),
        "the delve ladder yielded no habitation rank — the whole band scan is \
         vacuous and every count below would be 0"
    );

    for summary in &summaries {
        let seed = summary.seed;
        assert!(
            summary.systems > 0,
            "seed {seed}: no cave systems — the probe is vacuous"
        );
        assert!(
            summary.levels > 0,
            "seed {seed}: no level exists anywhere — every share is 0/0 and \
             reads as a finding"
        );
        assert!(
            summary.systems_with_open_mouth > 0,
            "seed {seed}: no system has an open mouth — §6's per-system \
             distribution has an empty corpus"
        );
        assert_eq!(
            summary.per_system_share.len(),
            summary.systems_with_open_mouth,
            "seed {seed}: the per-system distribution must carry exactly one \
             entry per system with an open mouth"
        );
        assert!(
            summary.reachable <= summary.levels,
            "seed {seed}: {} levels reached but only {} exist — a walk was \
             summed rather than unioned",
            summary.reachable,
            summary.levels
        );
        assert!(
            summary.open_entrances <= summary.drawn_entrances,
            "seed {seed}: more open mouths ({}) than drawn ({})",
            summary.open_entrances,
            summary.drawn_entrances
        );
    }

    let s42 = summaries
        .iter()
        .find(|s| s.seed == 42)
        .expect("seed 42 is in the panel");
    assert_eq!(
        (s42.levels, s42.reachable, s42.open_entrances),
        SEED_42_BASELINE,
        "seed 42's baseline moved. Spec §1 and \
         docs/audits/underworld-lattice-seed-panel.md both record \
         (levels, reachable, open entrances) = {SEED_42_BASELINE:?}. If The \
         Drift's deletion has landed, this is the expected break and the new \
         triple belongs in SEED_42_BASELINE and in this module's header; if it \
         has not, something moved the world and that is the finding."
    );
}
