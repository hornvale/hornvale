//! THE UNDERWORLD, Task 2: how much of the chamber lattice does a cave reach?
//!
//! Measurement only, and the evidence for the acceptance criterion Task 1b
//! parked (Ruling 8 in the campaign's SDD ledger): once a cave's depth became
//! a budget in metres capped at 3 km (spec §4.0), `Cave::deepest_band` stopped
//! reaching `Roots`, and `chamber_exists` still gated on `band_rank` — so
//! worlds could be carrying a silently reduced chamber count that nothing in
//! the suite objected to. Ruling 8 asked for the numbers rather than the
//! assumption; this is the instrument that takes them.
//!
//! Two arms per seed, over the same cave-bearing land cells:
//!
//! - **band** — `band_rank(cave.deepest_band)`, the gate as it stood.
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
//! address INCLUDING `floor` (`chamber.rs`), and every in-budget run currently
//! admits all `FLOORS_PER_RUN_CEILING` floors, so the chamber population of a
//! real world went from ~6.4 per cave to ~128 per cave in the same commit that
//! took this reading. The three figures above are a **floor-0 slice**, chosen
//! so they compare like with like against the `chamber/v2` row — which is what
//! makes them a clean re-keying control, and exactly what stops them from
//! being a census of the underworld. The unsliced count is a Task 2 subject:
//! it draws the realized floors per run, and until it does, "20 floors
//! everywhere" is the lattice ceiling standing in for a distribution.
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
//!    the VARIANCE: `deepest_band` collapsed onto `Basement` for 97.3–99.0% of
//!    cave-bearing cells, so every cave got the same three-rung lattice and
//!    the depth axis stopped distinguishing a shallow cave from a deep one at
//!    all. That is the defect the re-point closes, and it is a worse one than
//!    a count shortfall would have been: a count can be noticed, and a
//!    constant that happens to sit at the right average cannot.
//!
//! Test fixture (decision 0092): calls the composition-root entry points
//! directly, the sanctioned posture for this crate's live-worldgen batteries.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_terrain::{BandKind, DelveRung, TerrainPins, rung_at_depth};
use hornvale_worldgen::chamber::{BRANCHES_PER_SYSTEM, ChamberAddr, chamber_exists};
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};

/// Seeds this campaign preregisters on (spec §5).
const SEEDS: [u64; 3] = [42, 7, 1234];

/// The band ladder's rank — a copy of `chamber.rs`'s private `band_rank`, so
/// the probe can measure the retired gate without widening that function's
/// visibility. Exhaustive: a sixth `BandKind` fails this to compile.
fn band_rank(band: BandKind) -> u8 {
    match band {
        BandKind::Regolith => 0,
        BandKind::Cover => 1,
        BandKind::Basement => 2,
        BandKind::Roots => 3,
        BandKind::Underneath => 4,
    }
}

/// The delve ladder's rank over its habitation rungs. `Surface` is not a
/// habitable rung and `rung_at_depth` never returns it, so it maps to `None`.
fn rung_rank(rung: DelveRung) -> Option<u8> {
    match rung {
        DelveRung::Surface => None,
        DelveRung::Undercroft => Some(0),
        DelveRung::Shallows => Some(1),
        DelveRung::Deeps => Some(2),
        DelveRung::Underdeep => Some(3),
        DelveRung::Nadir => Some(4),
    }
}

/// claim: readout(off-gate, heavy:, prints only, no assertion beyond vacuity)
/// — how deep into the fixed chamber lattice a cave's depth budget reaches,
/// under the band gate and under the delve gate, on the same worlds.
#[test]
#[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
fn how_far_down_the_lattice_does_a_cave_reach() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
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

        let mut caves = 0usize;
        let mut band_hist = [0usize; 5];
        let mut rung_hist = [0usize; 5];
        let mut band_addressable = 0usize;
        let mut rung_addressable = 0usize;
        let mut realized = 0usize;

        for cell in geo.cells() {
            if terrain.is_ocean(cell) {
                continue;
            }
            let Some(cave) = terrain.cave_at(cell) else {
                continue;
            };
            caves += 1;
            let gradient = terrain.geothermal_gradient_at(cell);

            let b = band_rank(cave.deepest_band);
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
            for band in 0..=4u8 {
                for branch in 0..BRANCHES_PER_SYSTEM {
                    if chamber_exists(
                        seed,
                        &cave,
                        gradient,
                        ChamberAddr {
                            cell,
                            entrance: 0,
                            band,
                            branch,
                            floor: 0,
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
