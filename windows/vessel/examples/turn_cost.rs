//! The session-level turn-cost bench (The Panes, Task 1).
//!
//! INFORMATIVE, NEVER A GATE — the gate is `cli/tests/session_cost.rs`. This
//! exists to settle two questions with numbers instead of extrapolation:
//! what one turn costs, and what `Session::snapshot()` adds to it.
//!
//! `CLIENT-four-clocks` records the 4.75 ms no-op turn floor as STALE (per-
//! tick behaviour landed with The Action Clock) and says the re-measurement
//! "wants a session-level benchmark nobody has built". This is it.
//!
//! Run: `cargo run --release -p hornvale-vessel --example turn_cost`
//! ALWAYS `--release`: a debug build measures the optimizer, not the code.
//! The Blocking measured its own spike ~10x slower in debug.
//!
//! ## Measured — baseline (before the spatial channel)
//!
//! Date: 2026-08-06. Box: `MacBookPro` (`hostname -s`). Profile: `--release`.
//! Verbatim output:
//!
//! ```text
//! Session::start   median 1451.026 ms
//! handle(verb)     median    1.071 ms
//! snapshot()+json  median    0.173 ms
//! snapshot bytes   walk 4235, chamber 4064
//! ```

use hornvale_kernel::Seed;
use hornvale_vessel::{PossessOpts, Session};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

/// The fixed verb sequence every reading uses. Deliberately mixed: verbs
/// that move the possession (`enter`, `out`), verbs that advance the day
/// (`wait`), and verbs that do neither (`look`, `examine`) — because the
/// memo Task 4 may add only helps the third kind, and a sequence of only
/// `look` would flatter it.
const SEQUENCE: &[&str] = &[
    "look",
    "map",
    "examine me",
    "wait 1",
    "look",
    "enter",
    "map",
    "look",
    "out",
    "look",
];

/// How many times to run the sequence. Medians of repeated runs, per the
/// Rose Window metaplan §5's own measurement discipline.
const RUNS: usize = 5;

fn main() {
    // `#[allow]` because `clippy.toml` bans `Instant` workspace-wide
    // (decision 0001: time is `WorldTime`). A bench is the sanctioned
    // exception, the same one `cli/tests/scene_cost.rs` takes.
    #[allow(clippy::disallowed_types)] // benchmark harness
    use std::time::Instant;

    let world = build_world(
        Seed(42),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");

    let mut starts = Vec::new();
    let mut turns = Vec::new();
    let mut snaps = Vec::new();

    for _ in 0..RUNS {
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t0 = Instant::now();
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        starts.push(t0.elapsed().as_secs_f64() * 1000.0);

        for line in SEQUENCE {
            #[allow(clippy::disallowed_types)] // benchmark harness
            let t1 = Instant::now();
            let _ = session.handle(line);
            turns.push(t1.elapsed().as_secs_f64() * 1000.0);

            #[allow(clippy::disallowed_types)] // benchmark harness
            let t2 = Instant::now();
            let snap = session.snapshot().expect("a live session snapshots");
            snaps.push(t2.elapsed().as_secs_f64() * 1000.0);

            // Serialize too: the emit is part of the per-turn cost the
            // client actually pays, and measuring construction alone would
            // under-report it.
            let json = hornvale_vessel::snapshot_json(&snap);
            std::hint::black_box(&json);
        }
    }

    println!("Session::start   median {:8.3} ms", median(&mut starts));
    println!("handle(verb)     median {:8.3} ms", median(&mut turns));
    println!("snapshot()+json  median {:8.3} ms", median(&mut snaps));

    // The byte figure the spec priced by radius. Printed per band so the
    // walk/chamber asymmetry is visible rather than averaged away.
    let (mut session, _) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
    let walk = hornvale_vessel::snapshot_json(&session.snapshot().unwrap()).len();
    session.handle("enter");
    let chamber = hornvale_vessel::snapshot_json(&session.snapshot().unwrap()).len();
    println!("snapshot bytes   walk {walk}, chamber {chamber}");
}

/// The median of `xs`, which this sorts in place. `total_cmp` rather than
/// `partial_cmp().unwrap()`: the workspace sorts floats deterministically
/// and never panics on a NaN it did not expect.
fn median(xs: &mut [f64]) -> f64 {
    xs.sort_by(|a, b| a.total_cmp(b));
    xs[xs.len() / 2]
}
