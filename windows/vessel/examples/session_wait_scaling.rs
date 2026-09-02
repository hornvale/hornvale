//! The one instrument that times `Session::wait` itself (The Roll, spec §8
//! M2). `agent_scaling` and `session_length_scaling` drive
//! `DriveMovements::step_with_occupancy` directly — one walk, warm caches,
//! no roster derivation, no `turned-hostile` pass, no narration — so they
//! see about half of what a `wait` costs. M2 is a budget on the wait.
//!
//! Run: `cargo run --release -p hornvale-vessel --example session_wait_scaling`
//! on a quiet box (all three load averages under 4, The Repose). Paste the
//! output into the `## Measured` block below with the date, box and SHA.
//!
//! ## Measured
//!
//! 2026-09-01, MacBookPro, `0d72f37b7693c681bbe14f2005f038df822e22d3`.
//!
//! **CONTENDED (load: 5.92 10.13 15.40).** The box never quieted under
//! The Repose's rule (all three load averages < 4) across a ~20-minute
//! foreground poll — the 1-minute average fell as low as 4.15 once but the
//! 5- and 15-minute averages stayed elevated throughout, and load then rose
//! again before falling. The reading below was taken anyway per the task
//! brief's fallback and should be re-taken on a quiet box before being
//! treated as M2's baseline; the per-wait series climbing from ~30 ms to
//! ~204 ms over the 20 waits (rather than settling after a cold first tick,
//! as `agent_scaling`'s cache-warming rows do) is itself consistent with
//! rising contention during the run, not a property of `Session::wait`.
//!
//! ```text
//! session_wait_scaling: seed 42, 20 waits
//!   bodies  on_roll    ms/wait  facts/wait
//!        7        7    152.521      12.65
//! per-wait ms: [29.997667, 43.276584, 68.462375, 81.492583, 95.9015,
//! 124.301541, 147.092541, 159.676083, 176.37625, 193.813542, 183.184209,
//! 177.998292, 178.363667, 195.675042, 198.852250, 204.560125, 189.897958,
//! 193.732708, 203.708458, 204.049000]
//! ```
//!
//! (Task 13 appends the post-roll, post-tick-stage reading.)

// The wall-clock is the instrument here, never sim logic -- exempt from the
// wall-clock ban (clippy.toml / decision 0001), same pattern as
// `windows/vessel/examples/turn_cost.rs` and `agent_scaling.rs`.
#[allow(clippy::disallowed_types)]
use std::time::Instant;

use hornvale_vessel::{PossessOpts, Session};

const SEED: u64 = 42;
const TICKS: usize = 20;

/// The seed-42 world under default pins, copied verbatim from
/// `agent_scaling.rs`'s `main` — examples cannot see `tests/common`.
fn build(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 must build under default pins")
}

fn main() {
    let world = build(SEED); // copied from agent_scaling.rs, not invented
    let opts = PossessOpts {
        wild_agents: true,
        ..PossessOpts::default()
    };
    let (mut session, _) = Session::start(&world, &opts).expect("seed 42 starts");
    let bodies = session.bodies().len();
    let mut ms = Vec::with_capacity(TICKS);
    let mut facts = Vec::with_capacity(TICKS);
    for _ in 0..TICKS {
        let before = session.committed_fact_count();
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t0 = Instant::now();
        session.handle("wait");
        ms.push(t0.elapsed().as_secs_f64() * 1000.0);
        facts.push((session.committed_fact_count() - before) as f64);
    }
    let mean = |v: &[f64]| v.iter().sum::<f64>() / v.len() as f64;
    println!("session_wait_scaling: seed {SEED}, {TICKS} waits");
    println!(
        "{:>8} {:>8} {:>10} {:>10}",
        "bodies", "on_roll", "ms/wait", "facts/wait"
    );
    println!(
        "{:>8} {:>8} {:>10.3} {:>10.2}",
        bodies,
        bodies,
        mean(&ms),
        mean(&facts)
    );
    println!("per-wait ms: {ms:?}");
}
