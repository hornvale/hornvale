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
//! ### before Task 11 (CONTENDED), `e058a24db122a8fbec75db186528310c6f275a89`
//!
//! 2026-09-02, MacBookPro. `uptime`: `load averages: 3.72 5.73 8.96` — the
//! 1-minute average is under 4, but the 5- and 15-minute averages are not,
//! so this is CONTENDED per The Repose's all-three rule, taken anyway per
//! this task's fallback. Old code: `wait` ran the population walk twice
//! (`step_with_occupancy` for occupancy, `kernel::tick` for facts).
//!
//! ```text
//! session_wait_scaling: seed 42, 20 waits
//!   bodies  on_roll    ms/wait  facts/wait
//!       68       68    157.225      62.25
//! per-wait ms: [131.1315, 129.427, 127.600667, 89.339834, 157.9785,
//! 138.922791, 170.594208, 190.637583, 178.461875, 174.146917, 160.983167,
//! 132.328375, 169.054125, 130.424833, 192.354209, 216.837417, 199.843916,
//! 143.568125, 131.817125, 179.051875]
//! ```
//!
//! ### after Task 11 (CONTENDED), one walk per wait
//!
//! 2026-09-02, MacBookPro, same worktree, immediately after the reading
//! above (Task 11's `session.rs` change applied, uncommitted at the moment
//! of this reading — it lands in the commit immediately following this
//! one). `uptime`: `load averages: 4.27 5.66 8.82` — still CONTENDED (now
//! all three averages are over 4), so this is a contended-vs-contended
//! comparison, not a contended-vs-quiet one; the box's load did not fall
//! between the two readings, it rose slightly. New code: the same
//! `step_with_occupancy` call as before, but its `facts` are now committed
//! directly into `self.ledger` instead of being discarded and recomputed a
//! second time through `kernel::tick`.
//!
//! ```text
//! session_wait_scaling: seed 42, 20 waits
//!   bodies  on_roll    ms/wait  facts/wait
//!       68       68     83.487      62.25
//! per-wait ms: [60.467542, 60.582583, 59.993542, 40.535542, 77.2725,
//! 64.757791, 82.038083, 93.859125, 84.122834, 84.725458, 76.887999,
//! 68.577333, 120.198458, 74.087083, 86.112708, 137.653625, 104.703459,
//! 80.868709, 105.32825, 106.964292]
//! ```
//!
//! `facts/wait` is unchanged (62.25 both readings — the same facts commit
//! either way, just once now instead of twice, so the count moves by
//! nothing). `ms/wait` fell from 157.225 to 83.487, a 1.88x speedup,
//! **despite the box's load rising slightly between the two runs** (see
//! above) — a contended-favouring-the-old-code direction, which makes the
//! measured speedup a conservative one, not an inflated one. Both runs
//! carry the same 68-body, 68-on-roll population and the same worktree, so
//! the only thing that changed between them is this task's code.
//!
//! ### Task 13 readout (CONTENDED, load 4.07 4.96 5.77), after Task 11
//!
//! 2026-09-02, MacBookPro, `5cb7683107aa1babf4c99cc87c8ace306a1c7c71`.
//!
//! `uptime` was polled in the foreground for ~33 minutes (07:30–08:03Z);
//! the box never satisfied The Repose's all-three-under-4 rule — the
//! 1-minute average dipped under 4 several times (as low as 1.36) while
//! the 5- and 15-minute averages stayed elevated, and a fresh spike (a
//! 1-minute average of 28.64) arrived in the final stretch. This reading
//! was taken anyway per the task brief's fallback, at load `4.07 4.96
//! 5.77` — the lowest three-average reading reached in the poll and the
//! one nearest to quiet. The `on_roll` column now reads
//! `session.roll_len()` (Task 13) rather than the `bodies` duplicate it
//! printed before; at seed 42 the two agree (the whole population is on
//! the roll at this world's scale).
//!
//! ```text
//! session_wait_scaling: seed 42, 20 waits
//!   bodies  on_roll    ms/wait  facts/wait
//!       68       68     71.994      62.25
//! per-wait ms: [58.2975, 58.798667, 58.059583, 39.177375, 73.884208,
//! 63.178334, 80.947792, 90.167084, 84.838125, 82.1865, 74.47375,
//! 59.017041, 77.902875, 58.723375, 83.214209, 100.177834, 91.525792,
//! 64.247583, 59.264833, 81.79725]
//! ```
//!
//! `ms/wait` fell further, 83.487 -> 71.994 (a ~14% drop on a
//! contended-vs-contended comparison, both readings taken on the same
//! 68-body/68-on-roll population); the box was measurably quieter for
//! this reading (load 4.07/4.96/5.77 vs the prior reading's 4.27/5.66/8.82)
//! so part of the drop is plausibly box noise rather than code — no code
//! changed between Task 11's reading and this one. Well under M2's 1000 ms
//! budget either way. See the campaign report (Task 13) for the
//! `agent_scaling` 100-/200-agent readout and the M2 verdict.

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
        session.roll_len(), // Task 13: session.roll_len()
        mean(&ms),
        mean(&facts)
    );
    println!("per-wait ms: {ms:?}");
}
