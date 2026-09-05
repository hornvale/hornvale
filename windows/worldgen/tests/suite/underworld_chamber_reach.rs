//! THE UNDERWORLD, Task 2: how much of the chamber lattice does a cave reach?
//!
//! Measurement only, and the evidence for the acceptance criterion Task 1b
//! parked (Ruling 8 in the campaign's SDD ledger): once a cave's depth became
//! a budget in metres capped at 3 km (spec §4.0), `Cave::deepest_horizon` stopped
//! reaching `Roots`, and `chamber_exists` still gated on `band_rank` — so
//! worlds could be carrying a silently reduced chamber count that nothing in
//! the suite objected to. Ruling 8 asked for the numbers rather than the
//! assumption; this is the instrument that takes them.
//!
//! Two arms per seed, over the same cave-bearing land vertices:
//!
//! - **band** — `band_rank(cave.deepest_horizon)`, the gate as it stood.
//! - **rung** — the delve ladder's rank for
//!   `rung_at_depth(cave.depth_reach_m, gradient)` (`hornvale_terrain::delve`),
//!   the ladder the gate is being re-pointed at.
//!
//! The comparable figure is **addressable** addresses,
//! `(rank + 1) * BRANCHES_PER_SYSTEM`: a pure lattice count with no draw in it, so
//! it survives the `chamber/v1` → `chamber/v2` key change that the re-point
//! forces. **Realized** — what `chamber_exists` actually admits — is printed
//! beside it, but a realized count before the epoch and one after it are drawn
//! from different streams and are only comparable in distribution, never
//! address by address. Realized tracking addressable is the check that the
//! re-point moved the *reach* and not the *density*.
//!
//! It asserts nothing beyond a vacuity guard: each seed must have caves, and
//! the two arms must not be constant (a constant rank would make every
//! comparison below trivially satisfied).
//!
//! ## Measured, 2026-08-17, seeds 42 / 7 / 1234
//!
//! The `realized` line is the only one that differs between the two runs
//! below, because it is the only one that consults the gate. Both arms'
//! histograms and addressable means are identical across the epoch, which is
//! the control: the worlds did not move, only the gate did.
//!
//! **Before** (`eb9921af`, band gate, `chamber/v1`):
//!
//! ```text
//! seed 42:   band hist=[0, 19, 855, 0, 0]    addr 11.913 | realized 5228   (5.982/cave)
//! seed 7:    band hist=[0, 46, 1635, 0, 0]   addr 11.891 | realized 10004  (5.951/cave)
//! seed 1234: band hist=[0, 13, 1253, 0, 0]   addr 11.959 | realized 7547   (5.961/cave)
//! ```
//!
//! **After** (delve gate, `chamber/v2`, `Deeps` beginning at 8 K):
//!
//! ```text
//! seed 42:   rung hist=[77, 131, 399, 53, 214]  addr 12.897 | realized 5604   (6.412/cave)
//! seed 7:    rung hist=[84, 599, 121, 150, 727] addr 13.992 | realized 11747  (6.988/cave)
//! seed 1234: rung hist=[91, 144, 366, 129, 536] addr 14.765 | realized 9384   (7.412/cave)
//! ```
//!
//! **After the `chamber/v3` epoch** (The Stope, Task 1, 2026-08-20 — `floor`
//! added, `slot` → `branch`, `Sunless` → `Nadir`, all three spelled into
//! `chamber_key`), measured over the floor-0 slice of the new lattice, which
//! is the same address set this probe enumerated before floors existed:
//!
//! ```text
//! seed 42:   rung hist=[77, 131, 399, 53, 214]  addr 12.897 | realized 5602   (6.410/cave)
//! seed 7:    rung hist=[84, 599, 121, 150, 727] addr 13.992 | realized 11754  (6.992/cave)
//! seed 1234: rung hist=[91, 144, 366, 129, 536] addr 14.765 | realized 9320   (7.362/cave)
//! ```
//!
//! **The control this file was built to print did its job.** Both arms'
//! histograms and both addressable means are byte-identical to the `chamber/v2`
//! reading above — the terrain did not move and the cave's REACH did not
//! move — while `realized` shifted by −0.04 / +0.06 / −0.68%. `realized /
//! addressable` is 0.497 / 0.500 / 0.499, still `EXISTENCE_DENSITY = 0.5`.
//! That is the exact signature of a re-keying: the same lattice geometry, the
//! same gate, different draws.
//!
//! **What that sentence must NOT be read as saying is that the worlds did not
//! move — they did, by a factor of twenty, and an earlier draft of this
//! paragraph said the opposite.** `chamber_exists` draws independently per
//! address INCLUDING `floor` (`chamber.rs`), and at Task 1 every in-budget run
//! admitted all `LEVELS_PER_BRANCH_CEILING` floors, so the chamber population of
//! a real world went from ~6.4 per cave to ~128 per cave in the same commit
//! that took this reading. The three figures above are a **floor-0 slice**,
//! chosen so they compare like with like against the `chamber/v2` row — which
//! is what makes them a clean re-keying control, and exactly what stops them
//! from being a census of the underworld.
//!
//! **Task 2 landed the draw, and the three figures above did not move by one
//! chamber** (`5602 / 11754 / 9320`, re-run 2026-08-20 — see the reading
//! below). That is not the run gate failing to bite; it is the one thing it
//! cannot bite on. Every band's frozen range has a minimum of **at least 1**
//! (`Undercroft` 1–5, `Shallows` 3–10, `Deeps` 5–20, `Underdeep` 5–10, `Nadir`
//! 1–5), so **floor 0 exists in every run that exists at all** and the
//! floor-0 slice is invariant under the draw by construction. What moved is
//! everything above it: `how_many_floors_does_a_run_realize` measured the
//! unsliced population at **42.8 / 43.6 / 49.2 realized chambers per cave**,
//! against the ~128 the ceiling was standing in for — a **3.0×/3.0×/2.6×
//! reduction**. Read the two readouts together or the first one will tell you
//! nothing happened.
//!
//! ## RE-READ, 2026-08-23 (The Drift) — every figure ABOVE is pre-Drift
//!
//! Two of this file's standing claims are falsified and neither test went red,
//! because this probe asserts only vacuity guards. Re-run at close:
//!
//! ```text
//! seed 42:   rung hist=[77, 131, 399, 53, 214]  addr 12.897 | realized 4512  (5.162/cave)
//! seed 7:    rung hist=[84, 599, 121, 150, 727] addr 13.992 | realized 9353  (5.564/cave)
//! seed 1234: rung hist=[91, 144, 366, 129, 536] addr 14.765 | realized 7372  (5.823/cave)
//! ```
//!
//! **The control still does its job and the reading it gives is the opposite
//! of the one above.** Both arms' histograms and both addressable means are
//! byte-identical across four epochs now — terrain has not moved and a cave's
//! REACH has not moved — while `realized / addressable` fell from **~0.500 to
//! 0.400 / 0.398 / 0.394**. That is not the coin drifting; The Drift deleted
//! the coin (spec §4.1), so the only per-address filter left at floor 0 is
//! `branch < branch_count_of(..)`, whose authored weights give a mean width of
//! 1.60 against `BRANCHES_PER_SYSTEM = 4` — exactly **0.400**. **So every
//! sentence below reading "still `EXISTENCE_DENSITY = 0.5`" describes a
//! constant that no longer exists**, and the ratio it names now measures a
//! branch-width draw.
//!
//! The unsliced population moved too, and downward rather than up: the
//! per-system realized median is **34 / 35 / 38** against the 42.8 / 43.6 /
//! 49.2 quoted above. Two changes push in opposite directions and the second
//! wins — deleting the coin roughly doubles what a realized branch contains,
//! while amendment A.3 collapses four per-entrance sublattices into one
//! lattice per system. **Absolute chamber counts are therefore not comparable
//! across The Drift**, which is amendment A.5's own warning; the comparable
//! quantity is the per-system reachable *share*, and it is 100.00%.
//!
//! The "after" arm was re-taken when review's finer re-bin moved
//! `DEEPS_TOP_K` from 10 K to 8 K (see that constant's own doc). The earlier
//! reading at 10 K was `[77, 149, 381, 53, 214]` / `[84, 639, 81, 150, 727]` /
//! `[91, 162, 348, 129, 536]`, addressable 12.815 / 13.896 / 14.708, realized
//! 5568 / 11664 / 9351 — kept here because a boundary that moved after
//! unblinding should leave both readings visible, not only the one that
//! survived.
//!
//! Two readings, and the second is the one that mattered.
//!
//! 1. **Realized rises 7.2 / 17.4 / 24.3%**, and `realized / addressable` is
//!    0.497 / 0.499 / 0.502 after against 0.502 / 0.500 / 0.499 before — the
//!    density is untouched at `EXISTENCE_DENSITY = 0.5` and the whole change
//!    is reach, exactly as the two figures were printed side by side to check.
//!
//! 2. **Ruling 8's premise is only half right, and the wrong half is the one
//!    it named.** "Materially fewer chambers" does not survive measurement:
//!    the pre-Task-1b band arm (computable from the first probe's own
//!    histograms) has mean addressable 12.15 / 11.72 / 12.71, against 11.91 /
//!    11.89 / 11.96 after — −2.0% / +1.4% / −5.9%, a mean of −2.2% and a rise
//!    on one of three seeds. What Task 1b actually broke is not the COUNT but
//!    the VARIANCE: `deepest_horizon` collapsed onto `Basement` for 97.3–99.0% of
//!    cave-bearing vertices, so every cave got the same three-rung lattice and
//!    the depth axis stopped distinguishing a shallow cave from a deep one at
//!    all. That is the defect the re-point closes, and it is a worse one than
//!    a count shortfall would have been: a count can be noticed, and a
//!    constant that happens to sit at the right average cannot.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Band;
use hornvale_terrain::{Horizon, TerrainPins, rung_at_depth};
use hornvale_worldgen::chamber::{
    BRANCHES_PER_SYSTEM, ChamberAddr, RunAddr, chamber_exists, levels_in_branch,
};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, WorldComponents, build_world_to_with_artifacts,
};

/// Seeds this campaign preregisters on (spec §5).
const SEEDS: [u64; 3] = [42, 7, 1234];

/// The band ladder's rank — a copy of `chamber.rs`'s private `band_rank`, so
/// the probe can measure the retired gate without widening that function's
/// visibility. Exhaustive: a sixth `Horizon` fails this to compile.
fn band_rank(band: Horizon) -> u8 {
    match band {
        Horizon::Regolith => 0,
        Horizon::Cover => 1,
        Horizon::Basement => 2,
        Horizon::Roots => 3,
        Horizon::Underneath => 4,
    }
}

/// The delve ladder's rank over its habitation rungs. `Surface` is not a
/// habitable rung and `rung_at_depth` never returns it, so it maps to `None`.
fn rung_rank(rung: Band) -> Option<u8> {
    match rung {
        Band::Surface => None,
        Band::Undercroft => Some(0),
        Band::Shallows => Some(1),
        Band::Deeps => Some(2),
        Band::Underdeep => Some(3),
        Band::Nadir => Some(4),
    }
}

/// claim: readout(off-gate, heavy:, prints only, no assertion beyond vacuity)
/// — how deep into the fixed chamber lattice a cave's depth budget reaches,
/// under the band gate and under the delve gate, on the same worlds.
#[test]
#[ignore = "probe: how far down the lattice a cave reaches (band vs. rung comparison); run by hand (The Underworld, Task 2, answered its question; demoted by The Governor 2026-08-28)"]
fn how_far_down_the_lattice_does_a_cave_reach() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    for seed_value in SEEDS {
        let seed = hornvale_kernel::Seed(seed_value);
        let artifacts = build_world_to_with_artifacts(
            seed,
            &SkyPins::default(),
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

        let mut caves = 0usize;
        let mut band_hist = [0usize; 5];
        let mut rung_hist = [0usize; 5];
        let mut band_addressable = 0usize;
        let mut rung_addressable = 0usize;
        let mut realized = 0usize;

        for vertex in geo.vertices() {
            if terrain.is_ocean(vertex) {
                continue;
            }
            let Some(cave) = terrain.cave_at(vertex) else {
                continue;
            };
            caves += 1;
            let gradient = terrain.geothermal_gradient_at(vertex);

            let b = band_rank(cave.deepest_horizon);
            let r = rung_rank(rung_at_depth(cave.depth_reach_m, gradient))
                .expect("rung_at_depth never returns Surface");
            band_hist[b as usize] += 1;
            rung_hist[r as usize] += 1;
            band_addressable += (b as usize + 1) * BRANCHES_PER_SYSTEM as usize;
            rung_addressable += (r as usize + 1) * BRANCHES_PER_SYSTEM as usize;

            // Realized under whichever gate this build carries. Scanning the
            // floor-0 slice of the lattice (every band, every branch) rather than
            // only the
            // in-budget part is deliberate: it means this loop measures the
            // gate rather than restating it.
            for &band in Band::habitation() {
                for branch in 0..BRANCHES_PER_SYSTEM {
                    if chamber_exists(
                        seed,
                        &cave,
                        gradient,
                        ChamberAddr {
                            vertex,
                            band,
                            branch,
                            level: 0,
                        },
                    ) {
                        realized += 1;
                    }
                }
            }
        }

        assert!(
            caves > 0,
            "seed {seed_value} has no caves — probe is vacuous"
        );
        assert!(
            band_hist.iter().filter(|c| **c > 0).count() > 1,
            "seed {seed_value}: the band arm is constant — the comparison below is vacuous"
        );
        assert!(
            rung_hist.iter().filter(|c| **c > 0).count() > 1,
            "seed {seed_value}: the rung arm is constant — the comparison below is vacuous"
        );

        let n = caves as f64;
        println!(
            "seed {seed_value}: caves={caves}\n  \
             band arm: hist={band_hist:?} mean addressable {:.3}\n  \
             rung arm: hist={rung_hist:?} mean addressable {:.3}\n  \
             realized (this build's gate): {realized}, {:.3} per cave",
            band_addressable as f64 / n,
            rung_addressable as f64 / n,
            realized as f64 / n,
        );
    }
}

/// Spec §3.1's floors-per-band ranges, restated as literals so this readout's
/// structural assertion does not read the same table the draw reads (which
/// would make it pass for any table at all). Indexed by delve-ladder rank.
const FROZEN_RANGES: [(usize, usize); 5] = [(1, 5), (3, 10), (5, 20), (5, 10), (1, 5)];

/// The middle value of a sorted sample. Even-length samples take the lower of
/// the two central values rather than averaging them — the sample is a count
/// of floors and a half-floor is not a thing the world has.
fn median(sorted: &[usize]) -> usize {
    if sorted.is_empty() {
        return 0;
    }
    sorted[(sorted.len() - 1) / 2]
}

/// The value at `q` of the way through a sorted sample (`q` in `[0, 1]`).
fn quantile(sorted: &[usize], q: f64) -> usize {
    if sorted.is_empty() {
        return 0;
    }
    let idx = ((sorted.len() - 1) as f64 * q).round() as usize;
    sorted[idx]
}

/// claim: readout(off-gate, heavy:, prints only, assertions are vacuity plus
/// the structural bound §3.1's own ranges imply) — **spec §4.2's subject: how
/// many floors does a run realize, and what does that make a system?**
///
/// **Three denominators, and naming which one you are reading is the whole
/// difficulty** (this readout prints all three, because §4.2 does not say
/// which it meant and the arm it lands in changes with the choice):
///
/// - **per-branch drawn** — one branch's floors summed over the bands its
///   cave's budget reaches. This is the arithmetic §3.1 itself did: its
///   ranges sum to exactly 15–50, the figure the sentence beside them quotes
///   for "a system that runs the full ladder". So §3.1 wrote "system" and
///   computed "branch".
/// - **per-system drawn** — the same, times [`BRANCHES_PER_SYSTEM`] = 4,
///   which is what the word *system* means everywhere else in this lattice.
/// - **per-system realized** — what [`chamber_exists`] actually admits, i.e.
///   the drawn floors thinned by the existence density. This is the only one
///   of the three that counts places a player could stand in.
///
/// The assertion is **not** §4.2's branch table: a preregistered outcome is a
/// finding to report, never a test to go red on. What is asserted is the
/// structural bound the frozen ranges imply — a per-branch total over bands
/// `0..=r` must lie between the summed minima and the summed maxima of those
/// bands — which would catch the draw drifting off its own table, plus the
/// vacuity guards (caves exist, the distribution is not constant).
///
/// ## Measured, 2026-08-20, seeds 42 / 7 / 1234
///
/// ```text
///                          seed 42      seed 7    seed 1234    §4.2 arm
///   per-system drawn  med        90         107          108    >50 on all three
///   per-system realiz med        46          52           53    HOLE / >50 / >50
///   per-branch drawn  med        22          24           27    5-25 / 5-25 / HOLE
///   realized per cave       42.848      43.577       49.174
/// ```
///
/// **§4.2's own denominator is the SYSTEM — "the distribution of total floors
/// per system" — and under it the arm is ">50 routinely: too many" on all
/// three seeds.** That is the preregistered reading and it is stated first,
/// because picking the denominator that passes after seeing the data is the
/// shape of retuning even when the reasoning behind it is sound.
///
/// **The per-branch row is a post-hoc resolution of an inconsistency INTERNAL
/// to the frozen spec, and it is flagged as post-hoc.** §3.1's sentence says
/// "15-50 floors in a system that runs the full ladder"; its parenthetical
/// says "the counts are drawn per branch"; and its arithmetic sums the five
/// ranges **once** — `1+3+5+5+1 = 15`, `5+10+20+10+5 = 50` — which is one
/// branch's total, not a system's. That arithmetic needs no measurement at
/// all: it is a property of the table as written. Whether the campaign wants
/// the intended range read per branch or per system is a fidelity call, and
/// it is Nathan's, not this readout's.
///
/// **Do not read the full-ladder branch min/max as evidence for that
/// resolution.** This test asserts `(lo_bound..=hi_bound).contains(&drawn)`
/// per branch, and for `deepest == 4` those bounds ARE 15 and 50 — so a
/// full-ladder branch is inside 15-50 by construction and the readout would
/// have gone red rather than reported otherwise. What is NOT forced, and is
/// therefore informative, is the **median: 32 / 32 / 33**, near the middle of
/// the range rather than piled at an edge.
///
/// **The frozen ranges are not retuned to move any of this** (the campaign's
/// standing rule, and amendment B.7's). The per-band means come out at 2.976 /
/// 6.490 / 12.460 / 7.517 / 3.028 on seed 42 against the uniform expectations
/// 3.0 / 6.5 / 12.5 / 7.5 / 3.0, which is the draw doing exactly what §3.1
/// specified and nothing else.
///
/// **§4.2's branch table has a hole between 25 and 50, and two of these nine
/// readings fell in it** — the same preregistration defect amendment B.8
/// recorded for §4.1's 15–25% gap. Named, not repaired: closing a table after
/// unblinding is what preregistration exists to prevent. See [`arm`].
///
/// **NONE OF THESE COUNTS IS PLAYER-REACHABLE TODAY.** `passages_from` never
/// varies `floor`, and `windows/vessel`'s `delve_at` enters at
/// `band: 0, floor: 0`, so a possession can reach exactly one chamber and no
/// floor above zero is traversable at all until spec §7's junction task. The
/// 42.8/cave figure is a count of what the lattice DERIVES, not of what
/// anyone can walk.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn how_many_floors_does_a_run_realize() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    for seed_value in SEEDS {
        let seed = hornvale_kernel::Seed(seed_value);
        let artifacts = build_world_to_with_artifacts(
            seed,
            &SkyPins::default(),
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

        let mut caves = 0usize;
        // Per-band drawn counts, pooled over every run in the world.
        let mut band_floors: [Vec<usize>; 5] = Default::default();
        let mut per_branch_drawn: Vec<usize> = Vec::new();
        let mut per_system_drawn: Vec<usize> = Vec::new();
        let mut per_system_realized: Vec<usize> = Vec::new();
        // Systems that run the whole ladder — the population §3.1's "15-50"
        // sentence is actually about.
        let mut full_ladder_branch_drawn: Vec<usize> = Vec::new();

        for vertex in geo.vertices() {
            if terrain.is_ocean(vertex) {
                continue;
            }
            let Some(cave) = terrain.cave_at(vertex) else {
                continue;
            };
            caves += 1;
            let gradient = terrain.geothermal_gradient_at(vertex);
            let deepest = rung_rank(rung_at_depth(cave.depth_reach_m, gradient))
                .expect("rung_at_depth never returns Surface");

            let mut system_drawn = 0usize;
            let mut system_realized = 0usize;
            for branch in 0..BRANCHES_PER_SYSTEM {
                let mut branch_drawn = 0usize;
                let (mut lo_bound, mut hi_bound) = (0usize, 0usize);
                for band_rank in 0..=deepest {
                    let band = Band::from_rank(band_rank)
                        .expect("0..=deepest are all real habitation ranks");
                    let run = RunAddr {
                        vertex,
                        branch,
                        band,
                    };
                    let drawn = usize::from(levels_in_branch(seed, run));
                    band_floors[band_rank as usize].push(drawn);
                    branch_drawn += drawn;
                    let (lo, hi) = FROZEN_RANGES[band_rank as usize];
                    lo_bound += lo;
                    hi_bound += hi;
                    for level in 0..drawn {
                        let addr = ChamberAddr {
                            vertex,
                            branch,
                            band,
                            level: level as u8,
                        };
                        if chamber_exists(seed, &cave, gradient, addr) {
                            system_realized += 1;
                        }
                    }
                }
                assert!(
                    (lo_bound..=hi_bound).contains(&branch_drawn),
                    "seed {seed_value} vertex {vertex:?} branch {branch}: {branch_drawn} \
                     drawn floors over bands 0..={deepest} is outside the \
                     {lo_bound}..={hi_bound} that spec §3.1's frozen ranges allow"
                );
                per_branch_drawn.push(branch_drawn);
                if deepest == 4 {
                    full_ladder_branch_drawn.push(branch_drawn);
                }
                system_drawn += branch_drawn;
            }
            per_system_drawn.push(system_drawn);
            per_system_realized.push(system_realized);
        }

        assert!(
            caves > 0,
            "seed {seed_value} has no caves — every figure below is vacuous"
        );
        assert!(
            per_branch_drawn.iter().any(|n| *n != per_branch_drawn[0]),
            "seed {seed_value}: every branch drew the same total — the draw is \
             not varying with the place"
        );
        assert!(
            per_system_realized.iter().sum::<usize>() > 0,
            "seed {seed_value}: no chamber exists anywhere, so the realized \
             column below says nothing about the gate"
        );

        per_branch_drawn.sort_unstable();
        per_system_drawn.sort_unstable();
        per_system_realized.sort_unstable();
        full_ladder_branch_drawn.sort_unstable();

        println!("======== seed {seed_value}: caves={caves} ========");
        println!("  per-band drawn floors (pooled over every run):");
        for (rank, sample) in band_floors.iter_mut().enumerate() {
            sample.sort_unstable();
            let (lo, hi) = FROZEN_RANGES[rank];
            let mean = sample.iter().sum::<usize>() as f64 / sample.len().max(1) as f64;
            println!(
                "    rank {rank} (range {lo}-{hi}): n={} min={} median={} max={} mean={mean:.3}",
                sample.len(),
                sample.first().copied().unwrap_or(0),
                median(sample),
                sample.last().copied().unwrap_or(0),
            );
        }
        for (name, sample) in [
            ("per-branch drawn ", &per_branch_drawn),
            ("per-system drawn ", &per_system_drawn),
            ("per-system realiz", &per_system_realized),
        ] {
            let mean = sample.iter().sum::<usize>() as f64 / sample.len().max(1) as f64;
            println!(
                "  {name}: n={} min={} p25={} MEDIAN={} p75={} max={} mean={mean:.3}",
                sample.len(),
                sample.first().copied().unwrap_or(0),
                quantile(sample, 0.25),
                median(sample),
                quantile(sample, 0.75),
                sample.last().copied().unwrap_or(0),
            );
        }
        println!(
            "  full-ladder branches only (§3.1's own population): n={} min={} \
             MEDIAN={} max={} (§3.1 intends 15-50)",
            full_ladder_branch_drawn.len(),
            full_ladder_branch_drawn.first().copied().unwrap_or(0),
            median(&full_ladder_branch_drawn),
            full_ladder_branch_drawn.last().copied().unwrap_or(0),
        );
        println!(
            "  §4.2 arm, per-branch drawn   median {} -> {}",
            median(&per_branch_drawn),
            arm(median(&per_branch_drawn))
        );
        println!(
            "  §4.2 arm, per-system drawn   median {} -> {}",
            median(&per_system_drawn),
            arm(median(&per_system_drawn))
        );
        println!(
            "  §4.2 arm, per-system realized median {} -> {}",
            median(&per_system_realized),
            arm(median(&per_system_realized))
        );
    }
}

/// Spec §4.2's branch table, as written, applied to a median.
///
/// **It has a hole between 25 and 50**, and this function reports that rather
/// than closing it — the same preregistration defect amendment B.8 recorded
/// for §4.1's 15–25% gap, found the same way (a measurement landed in it).
/// Repairing a branch table after unblinding is exactly what preregistration
/// exists to prevent, so the hole is named and left.
fn arm(median: usize) -> &'static str {
    if median < 5 {
        "median < 5: the ranges are not producing depth; report"
    } else if median <= 25 {
        "median in 5-25: proceed"
    } else if median > 50 {
        "> 50 routinely: too many"
    } else {
        "UNCLASSIFIED — §4.2's table has a hole between 25 and 50 (cf. amendment B.8)"
    }
}
