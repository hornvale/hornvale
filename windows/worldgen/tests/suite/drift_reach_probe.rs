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
//! and adding them counts a level twice. The Stope's committed witness was
//! double-count-safe only incidentally: `passages_from` propagates
//! `addr.entrance` unchanged (`..addr` in every candidate), so a walk seeded
//! at entrance *e* never leaves entrance *e*'s sublattice, and The Drift makes
//! entrances vary. The union is written once, in [`reachable_union`], and both
//! shares read off it.
//!
//! **The hazard is INERT in today's tree, and saying so is the point.**
//! `entrance_mouth` yields exactly one mouth per entrance index, and a walk
//! cannot leave its entrance's sublattice, so today the union and a naive sum
//! agree exactly — the union code is written for the world The Drift is about
//! to build, not for the one it measures. A reader must not take this probe's
//! green as evidence that the union is currently doing work; it is not, and
//! the check that would notice a regression (`reachable <= levels`) is
//! correspondingly weak until §4's entrances address one shared lattice.
//!
//! **A mouth is OPEN when the level it names exists.** That is exactly what
//! `docs/audits/underworld-lattice-seed-panel.md` counts as an "open
//! entrance": `underworld_readout` tests the mouth address with `chamber_at`,
//! which is [`chamber_exists`] plus content resolution and returns `None`
//! precisely when existence is refused. So this probe's openness test and the
//! committed artifact's are the same predicate, and the two are comparable
//! line for line.
//!
//! # THE DENOMINATOR IS THE WHOLE DRAWN ENTRANCE POPULATION, NOT ENTRANCE 0
//!
//! This is the one definitional choice in the file that could have gone the
//! other way, so it is stated rather than implied. A system draws
//! `entrance_count` apertures, and `chamber_exists` gates on
//! `branch_count_of(seed, cell, addr.entrance)`, `floors_in_run` over an
//! address carrying `entrance`, and a per-address existence stream that also
//! carries it. **Each entrance therefore realizes its own sublattice**, and
//! the committed witness sums existence over `0..drawn_entrances` — which is
//! how seed 42 reaches 21,328 levels across 874 systems (24.4 per system,
//! exactly the figure spec §6 reports) from 1,229 drawn entrances.
//!
//! Counting only entrance 0 would give a denominator about 874/1229 of that
//! and would NOT be comparable to the number that occasioned this campaign.
//! So this probe walks every drawn entrance. The entrance-0 sublattice is
//! *also* reported on its own line (`head-lattice only`), because a walk never
//! crosses the entrance axis, which makes that pair a self-consistent
//! secondary reading rather than a ratio of mismatched populations.
//!
//! # MEASURED VALUES — BEFORE, 2026-08-23, tree at `campaign/the-drift` prior to Task 1
//!
//! ```text
//! seed 42
//!   systems 874    levels 21328   reachable 1496   entrances 1229 drawn / 511 open
//!   systems with an open mouth 450
//!   per-system reachable share   p10 3.70%   median 12.50%   p90 57.14%
//!   whole-world reachable share  7.01%   (1496 of 21328)
//!   head-lattice only            7.73%   (1158 of 14976)
//!   levels per system            mean 24.40   p10 4   median 17   p90 54
//! seed 7
//!   systems 1681   levels 42131   reachable 3277   entrances 2382 drawn / 1070 open
//!   systems with an open mouth 940
//!   per-system reachable share   p10 3.74%   median 13.04%   p90 50.00%
//!   whole-world reachable share  7.78%   (3277 of 42131)
//!   head-lattice only            8.33%   (2462 of 29559)
//!   levels per system            mean 25.06   p10 4   median 17   p90 57
//! seed 1234
//!   systems 1266   levels 36393   reachable 2493   entrances 1813 drawn / 831 open
//!   systems with an open mouth 715
//!   per-system reachable share   p10 3.23%   median 11.11%   p90 40.00%
//!   whole-world reachable share  6.85%   (2493 of 36393)
//!   head-lattice only            7.57%   (1905 of 25165)
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
//! # MEASURED VALUES — AFTER, 2026-08-23, Task 1 (`chamber_exists`'s existence
//! # coin deleted, spec §4.1)
//!
//! ```text
//! seed 42
//!   systems 874    levels 42820   reachable 39140   entrances 1229 drawn / 1101 open
//!   systems with an open mouth 874
//!   per-system reachable share   p10 83.87%   median 100.00%   p90 100.00%
//!   whole-world reachable share  91.41%   (39140 of 42820)
//!   head-lattice only            100.00%   (30100 of 30100)
//!   levels per system            mean 48.99   p10 9   median 34   p90 105
//! seed 7
//!   systems 1681   levels 84424   reachable 78677   entrances 2382 drawn / 2155 open
//!   systems with an open mouth 1681
//!   per-system reachable share   p10 85.85%   median 100.00%   p90 100.00%
//!   whole-world reachable share  93.19%   (78677 of 84424)
//!   head-lattice only            100.00%   (59056 of 59056)
//!   levels per system            mean 50.22   p10 9   median 33   p90 112
//! seed 1234
//!   systems 1266   levels 72304   reachable 66986   entrances 1813 drawn / 1636 open
//!   systems with an open mouth 1266
//!   per-system reachable share   p10 84.27%   median 100.00%   p90 100.00%
//!   whole-world reachable share  92.64%   (66986 of 72304)
//!   head-lattice only            100.00%   (49970 of 49970)
//!   levels per system            mean 57.11   p10 12   median 39   p90 128
//! ```
//!
//! **Seed 42 moved from 1,496 reachable of 21,328 (7.01%) to 39,140 of 42,820
//! (91.41%) — levels itself roughly doubled (the coin used to also suppress
//! about half of every drawn floor from EXISTING at all, not only from being
//! reached) and the reachable count grew 26.2x.** Every seed now clears BOTH
//! of spec §6's preregistered intents: per-system median is 100.00% on all
//! three (the intent is a median at or above 95%), and the whole-world share
//! is 91.41-93.19% (the intent is at or above 90%). `systems with an open
//! mouth` now equals `systems` exactly on every seed — deleting the coin
//! means an entrance's own mouth address can no longer be the one floor that
//! failed to exist. This is the campaign's first measured result and it
//! lands squarely on the intent, not merely past the 50% floor spec §6 also
//! names.
//!
//! Wall time for the whole probe (three `BuildDepth::Terrain` worlds and
//! ~1.6M `chamber_exists` calls) is ~1.4 s in the optimized test profile.
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
use hornvale_kernel::{CellId, Seed};
use hornvale_terrain::{Cave, GeothermalGradient, TerrainPins, rungs};
use hornvale_worldgen::chamber::{
    BRANCHES_PER_SYSTEM, ChamberAddr, FLOORS_PER_RUN_CEILING, chamber_exists, entrance_count,
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
/// **Pinned as an equality on purpose, and it has now broken once, exactly as
/// designed.** Before Task 1 landed this held `(21328, 1496, 511)` — spec
/// §1's opening figure, reproduced here as a pin so a future run's departure
/// from it would be caught rather than silently absorbed. The Drift's Task 1
/// (spec §4.1) then deleted `chamber_exists`'s existence coin, which is
/// exactly the change this pin exists to be broken by; the module's own
/// "MEASURED VALUES — AFTER" block above records the new triple and the
/// movement in full. The pin was updated to `(42820, 39140, 1101)` in the
/// same commit as the deletion, per this campaign's own rule that a
/// re-baselining is a deliberate, reported act rather than a quiet number
/// change. A move from any OTHER cause from here on — a terrain change, a
/// stream relabelling, a lattice constant — is a determinism finding, and
/// this equality still catches that.
///
/// A band was considered and rejected: this is not a noisy statistic but a
/// deterministic count over a fixed seed, and a band around a deterministic
/// count only buys room for an undetected change.
const SEED_42_BASELINE: (usize, usize, usize) = (42820, 39140, 1101);

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
#[derive(Clone, Copy, Debug, Default)]
struct SystemReach {
    /// Apertures this system draws, whether or not they open onto anything.
    drawn_mouths: usize,
    /// Apertures whose named level exists — the witness's "open entrances".
    open_mouths: usize,
    /// Levels that exist, summed over every drawn entrance's sublattice.
    levels: usize,
    /// Levels reached by one walk from the union of the open mouths.
    reachable: usize,
    /// The same two counts restricted to entrance 0's sublattice — the
    /// secondary reading described in this module's header.
    levels_head: usize,
    /// Levels of entrance 0's sublattice reached by the union walk. A walk
    /// never crosses the entrance axis, so this equals the walk from the head
    /// mouth alone whenever that mouth is open, and 0 when it is shut.
    reachable_head: usize,
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

    // EXISTENCE, over the whole drawn entrance population. Bounded by the
    // LATTICE's own ceilings, never by a run's drawn length: bounding by the
    // draw would make `chamber_exists`'s floor gate unfalsifiable here, the
    // same trap `underworld_readout`'s module doc records.
    let mut levels = 0usize;
    let mut levels_head = 0usize;
    for entrance in 0..entrances {
        for &band in ranks {
            for branch in 0..BRANCHES_PER_SYSTEM {
                for floor in 0..FLOORS_PER_RUN_CEILING {
                    let addr = ChamberAddr {
                        cell,
                        entrance,
                        branch,
                        band,
                        floor,
                    };
                    if chamber_exists(seed, cave, gradient, addr) {
                        levels += 1;
                        if entrance == 0 {
                            levels_head += 1;
                        }
                    }
                }
            }
        }
    }

    // THE MOUTHS. `EntranceMouth` is a plain struct of `branch`/`band`/`floor`
    // with no accessors, so the address is assembled from its fields; the
    // entrance index comes from the loop, because the mouth type does not
    // carry the aperture it belongs to.
    let mouths: Vec<ChamberAddr> = (0..entrances)
        .map(|entrance| {
            let mouth = entrance_mouth(seed, cell, entrance);
            ChamberAddr {
                cell,
                entrance,
                branch: mouth.branch,
                band: mouth.band,
                floor: mouth.floor,
            }
        })
        .filter(|&addr| chamber_exists(seed, cave, gradient, addr))
        .collect();

    let reached = reachable_union(seed, cave, gradient, &mouths);
    let reachable_head = reached.iter().filter(|a| a.entrance == 0).count();

    SystemReach {
        drawn_mouths: usize::from(entrances),
        open_mouths: mouths.len(),
        levels,
        reachable: reached.len(),
        levels_head,
        reachable_head,
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
    /// The entrance-0 sublattice's existing levels, world-wide.
    levels_head: usize,
    /// The entrance-0 sublattice's reachable levels, world-wide.
    reachable_head: usize,
}

impl ReachSummary {
    /// §6's whole-world quantity: the share of all existing levels reachable.
    fn world_share(&self) -> f64 {
        share(self.reachable, self.levels)
    }

    /// The same restricted to entrance 0's sublattice — a self-consistent
    /// secondary reading, since a walk never crosses the entrance axis.
    fn head_share(&self) -> f64 {
        share(self.reachable_head, self.levels_head)
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
            "  head-lattice only            {:.2}%   ({} of {})",
            100.0 * self.head_share(),
            self.reachable_head,
            self.levels_head
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
        levels_head: 0,
        reachable_head: 0,
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
        summary.levels_head += sys.levels_head;
        summary.reachable_head += sys.reachable_head;
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
