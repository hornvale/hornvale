//! Per-verb cost of MOVING at both scales after The Roll — a diagnostic,
//! never a gate. `turn_cost` classes verbs coarsely; this times the exact
//! sequence a player types: room-scale `go`, `enter`, chamber-scale `go`,
//! `look`, `map`, `examine <name>`, `needs`, and `snapshot()` after each,
//! then again after twenty waits to see what grows with session length.
//!
//! Run: `cargo run --release -p hornvale-vessel --example move_cost`
//! (and once WITHOUT `--release`, because `cargo run -p hornvale -- possess`
//! is a debug build and that is what a player at the CLI actually feels).
//!
//! ## Measured
//!
//! 2026-09-02, MacBookPro, `2c34f9e4c64fae3230a751cfca061c4bcdf3bb37`, `--release`.
//! **CONTENDED** (`uptime`: `load averages: 9.27 12.44 9.86` — all three
//! over The Repose's quiet-box threshold of 4; taken anyway per the
//! fallback `session_wait_scaling.rs` documents, since this is a diagnostic
//! rather than a preregistered gate).
//!
//! ```text
//! move_cost: seed 42, profile release; build_world 2410 ms
//! Session::start 828 ms
//! --- fresh session: bodies 68 on roll 68 facts 21932
//!                   look handle     0.238 ms  snapshot    32.242 ms    69252 B
//!                    map handle     4.080 ms  snapshot    31.136 ms    70454 B
//!                   go n handle     0.525 ms  snapshot     4.227 ms    64685 B
//!                   go n handle     0.509 ms  snapshot     4.351 ms    66601 B
//!                   back handle     0.427 ms  snapshot     4.423 ms    66927 B
//!                   back handle     0.422 ms  snapshot    31.345 ms    73176 B
//! examine Dvoashngashngo handle     4.019 ms  snapshot    32.235 ms    72816 B
//!                  needs handle    27.559 ms  snapshot    31.705 ms    74911 B
//!                  enter handle    33.941 ms  snapshot    45.914 ms    24703 B
//!                   look handle    16.897 ms  snapshot    44.374 ms    24704 B
//!                    map handle     0.020 ms  snapshot    44.215 ms    25007 B
//!                   go n handle     0.007 ms  snapshot    44.147 ms    24475 B
//!                   go e handle     0.006 ms  snapshot    43.826 ms    24620 B
//!                   go s handle     0.006 ms  snapshot    45.162 ms    24763 B
//!                   go w handle     0.007 ms  snapshot    44.548 ms    24566 B
//!                   look handle    17.675 ms  snapshot    46.196 ms    24702 B
//!                    out handle     0.309 ms  snapshot    31.896 ms    73180 B
//! 20 waits 1859 ms
//! --- after 20 waits: bodies 68 on roll 68 facts 23487
//! (per-verb pattern repeats; omitted here — see the module's `## Measured`
//! sibling files for the full-run convention)
//! ```

use hornvale_vessel::{PossessOpts, Session, Turn};
#[allow(clippy::disallowed_types)] // wall-clock is the instrument here, never sim logic
use std::time::Instant;

const SEED: u64 = 42;

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

fn out(t: Turn) -> String {
    match t {
        Turn::Out(s) => s,
        _ => String::new(),
    }
}

fn timed(session: &mut Session<'_>, verb: &str) {
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t0 = Instant::now();
    let reply = out(session.handle(verb));
    let handle_ms = t0.elapsed().as_secs_f64() * 1000.0;
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t1 = Instant::now();
    let snap = session
        .snapshot()
        .map(|s| serde_json::to_string(&s).unwrap_or_default());
    let snap_ms = t1.elapsed().as_secs_f64() * 1000.0;
    let bytes = snap.as_ref().map(|s| s.len()).unwrap_or(0);
    let first = reply
        .lines()
        .next()
        .unwrap_or("")
        .chars()
        .take(60)
        .collect::<String>();
    println!(
        "{:>22} handle {:>9.3} ms  snapshot {:>9.3} ms  {:>7} B  | {}",
        verb, handle_ms, snap_ms, bytes, first
    );
}

fn block(session: &mut Session<'_>, label: &str) {
    println!(
        "--- {label}: bodies {} on roll {} facts {}",
        session.bodies().len(),
        session.roll_len(),
        session.committed_fact_count()
    );
    let name = session
        .bodies()
        .iter()
        .find(|b| b.village.is_some() && b.entity != session.driven_body().entity)
        .map(|b| b.label.clone())
        .unwrap_or_default();
    for v in ["look", "map", "go n", "go n", "back", "back"] {
        timed(session, v);
    }
    timed(session, &format!("examine {name}"));
    timed(session, "needs");
    timed(session, "enter");
    for v in ["look", "map", "go n", "go e", "go s", "go w", "look"] {
        timed(session, v);
    }
    timed(session, "out");
}

fn main() {
    let profile = if cfg!(debug_assertions) {
        "DEBUG"
    } else {
        "release"
    };
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t0 = Instant::now();
    let world = build(SEED);
    println!(
        "move_cost: seed {SEED}, profile {profile}; build_world {:.0} ms",
        t0.elapsed().as_secs_f64() * 1000.0
    );
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t1 = Instant::now();
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("seed 42 starts");
    println!(
        "Session::start {:.0} ms",
        t1.elapsed().as_secs_f64() * 1000.0
    );
    block(&mut session, "fresh session");
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t2 = Instant::now();
    for _ in 0..20 {
        session.handle("wait");
    }
    println!("20 waits {:.0} ms", t2.elapsed().as_secs_f64() * 1000.0);
    block(&mut session, "after 20 waits");
}
