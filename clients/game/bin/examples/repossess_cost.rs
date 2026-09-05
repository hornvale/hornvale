//! Repossession-cost bench (The Quire, Task 10).
//!
//! `driver.rs` builds a [`hornvale_vessel::WorldContext`] exactly once per
//! `Driver::start` and every session borrows it — but nothing in the
//! interactive game or its tests exercises release-and-repossess (`Driver`'s
//! public surface is deliberately just `start`/`handle`/`snapshot`, per the
//! containment `driver.rs`'s module doc holds). This bench answers the
//! question directly: with the context already built, what does a second
//! `Session::start_in` cost, next to a cold `Session::start` (which
//! re-derives the context every time — the design spec §5 measured that at
//! a **0.73 s** median before the hoist)?
//!
//! INFORMATIVE, never a gate — nothing in `make game-check` runs this.
//!
//! Run: `cargo run --manifest-path clients/game/bin/Cargo.toml --release
//! --example repossess_cost`. ALWAYS `--release`: a debug build measures the
//! optimizer, not the code (`windows/vessel/examples/turn_cost.rs` notes The
//! Blocking's own spike measured ~10x slower in debug). Check `uptime`
//! first: this campaign already discarded one measurement taken at load
//! average 50 as contended, and a busy box makes any number here
//! meaningless.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_vessel::{PossessOpts, Session, WorldContext};
use hornvale_worldgen::{SettlementPins, build_world};

/// Runs per measured quantity — matches `turn_cost.rs`'s five.
const RUNS: usize = 5;

fn median(mut xs: Vec<f64>) -> f64 {
    xs.sort_by(f64::total_cmp);
    xs[xs.len() / 2]
}

fn main() {
    // `#[allow]` because the root `clippy.toml`'s `disallowed-types` bans
    // `Instant` workspace-wide (decision 0001: time is `WorldTime`) and
    // clippy's config lookup walks up the directory tree regardless of
    // workspace membership, so it reaches this crate too even though
    // `clients/CLAUDE.md` says the workspace rules don't bind here. A bench
    // is the sanctioned exception `windows/vessel/examples/turn_cost.rs`
    // already takes for the identical reason.
    #[allow(clippy::disallowed_types)]
    use std::time::Instant;

    let world = build_world(
        Seed(42),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");

    // Paid once, outside the timed loop — this is the campaign's whole
    // point: a repossession must not pay this again.
    let ctx = WorldContext::build(&world).expect("seed 42's context derives");

    let mut cold_starts = Vec::new();
    let mut repossessions = Vec::new();

    for _ in 0..RUNS {
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t0 = Instant::now();
        let (session, _opening) =
            Session::start(&world, &PossessOpts::default()).expect("Session::start");
        cold_starts.push(t0.elapsed().as_secs_f64() * 1000.0);
        drop(session);

        #[allow(clippy::disallowed_types)] // benchmark harness
        let t1 = Instant::now();
        let (session, _opening) =
            Session::start_in(&ctx, &PossessOpts::default()).expect("Session::start_in");
        repossessions.push(t1.elapsed().as_secs_f64() * 1000.0);
        drop(session);
    }

    println!(
        "Session::start    (re-derives WorldContext)  median {:8.3} ms  {cold_starts:?}",
        median(cold_starts.clone())
    );
    println!(
        "Session::start_in (reuses WorldContext)       median {:8.3} ms  {repossessions:?}",
        median(repossessions.clone())
    );
}
