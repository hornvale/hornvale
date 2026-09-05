//! The Detent, spec §3 rule 2: what does the affect replay actually COST,
//! in wall time, as a share of the whole-roster hazard read?
//!
//! INFORMATIVE, NEVER A GATE — the same standing as
//! `windows/vessel/examples/turn_cost.rs` and
//! `windows/vessel/examples/agent_scaling.rs`. Nothing asserts on this
//! output; it is the readout instrument for one decision-rule branch.
//!
//! Run: `cargo run --release -p hornvale-vessel --example detent_rule2_probe`
//! ALWAYS `--release`: a debug build measures the optimizer, not the code.
//!
//! ## Why this exists beside the counting witness
//!
//! `windows/vessel/tests/suite/the_detent.rs`'s
//! `rule_two_witness_the_affect_replay_share_of_the_hazard_read` counts the
//! same shape — seed 6, `PossessOpts::default()`, four `wait`s, then two
//! whole-roster `Session::hazard_memories()` calls — and prints replays and
//! room-memo lookups PER BODY READ. Those are counts, and spec §3 rule 2 is
//! phrased as a *share of the hazard read*, which is a time. A count cannot
//! be turned into a share without a cost per unit, and tests in this project
//! may not time (`Instant` is banned there). So the count lives in the
//! witness, where it is deterministic and gated, and the timing lives here,
//! where it is `--release` and explicitly not a gate. Same shape, same
//! script, two instruments — read them together.
//!
//! ## What is measured
//!
//! Three whole-roster `hazard_memories()` calls after four `wait`s:
//!
//! - **cold** — the first one. The four `wait`s have already warmed the room
//!   memo and the verdict index for the rooms the walk touched, so "cold"
//!   here means *whatever the whole roster still has un-memoised*, not a
//!   fresh session. It is the honest upper bound for a read on this shape.
//! - **warm 1**, **warm 2** — the second and third, over a memo and index
//!   nothing evicts. These are the steady-state per-read cost the campaign
//!   ships.
//!
//! Each is reported as total elapsed and as µs per body, alongside the
//! `resident_alarm_replays()` delta over the same call — so replays per read
//! and µs per read sit in one row and rule 2's branch can be read off them.
//! The replay path's own reads are inside `emitter_arousal -> affect_of`;
//! the room-memo hit/miss deltas beside them are the memo lookups the read
//! makes, which is the term the replay share is a share OF.

// The build-sites roster (`cli/tests/fixtures/world-build-sites.tsv`) scans
// `src/` and `tests/` only — `examples/` is invisible to it by construction
// (see `cli/tests/suite/world_build_sites.rs`), so this file's `build_world`
// call needs no row there.

use hornvale_kernel::Seed;
use hornvale_vessel::{PossessOpts, Session};
use hornvale_worldgen::build_world;

/// The emitter seed — the same one `ledger_hash_witness.rs` searched to and
/// `the_detent.rs`'s rule-2 witness counts on, so the two instruments read
/// the same world.
const EMITTER_SEED: u64 = 6;

/// Waits before the reads, matching the counting witness's `RULE_TWO_WAITS`.
const WAITS: usize = 4;

/// One timed whole-roster read: elapsed, per-body cost, and the counter
/// deltas taken across exactly the same call.
#[allow(clippy::disallowed_types)] // benchmark harness
fn timed_read(session: &Session<'_>, label: &str, bodies: usize) {
    let replays_before = session.resident_alarm_replays();
    let hits_before = session.resident_ground_hits();
    let misses_before = session.resident_ground_misses();
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t0 = std::time::Instant::now();
    let memories = session.hazard_memories();
    let elapsed = t0.elapsed();
    let replays = session.resident_alarm_replays() - replays_before;
    let hits = session.resident_ground_hits() - hits_before;
    let misses = session.resident_ground_misses() - misses_before;
    let shunned: usize = memories.iter().map(|(_, m)| m.shunned.len()).sum();
    let total_us = elapsed.as_secs_f64() * 1e6;
    println!(
        "{label:>7}: {total_us:10.1} us total, {:9.3} us/read over {bodies} bodies, \
         replays +{replays} ({:.4}/read), memo lookups +{} ({:.4}/read), shunned {shunned}",
        total_us / bodies as f64,
        replays as f64 / bodies as f64,
        hits + misses,
        (hits + misses) as f64 / bodies as f64,
    );
}

fn main() {
    let world = build_world(
        Seed(EMITTER_SEED),
        &Default::default(),
        &Default::default(),
        &Default::default(),
    )
    .expect("the emitter seed builds");
    let (mut session, _opening) =
        Session::start(&world, &PossessOpts::default()).expect("the emitter seed starts a session");

    println!("--- The Detent rule 2 probe: seed {EMITTER_SEED}, {WAITS} waits, --release ---");
    for _ in 0..WAITS {
        session.handle("wait");
    }
    let bodies = session.bodies().len();
    println!("roster: {bodies} bodies after {WAITS} waits");

    timed_read(&session, "cold", bodies);
    timed_read(&session, "warm 1", bodies);
    timed_read(&session, "warm 2", bodies);

    println!(
        "totals: alarm replays {}, ground hits {}, ground misses {}",
        session.resident_alarm_replays(),
        session.resident_ground_hits(),
        session.resident_ground_misses(),
    );
    // M1 (spec §4) on the possession shape, as COUNTS. `Session` owns the two
    // structures privately and exposes no byte accessor — only
    // `session_length_scaling.rs`, which owns its own `OwnedGround`, can call
    // `GroundHazards::held_bytes` / `FrighteningGround::held_bytes`. So this
    // shape contributes entry counts to M1 and the bench contributes bytes;
    // adding a byte accessor to `Session` for a readout's sake would widen the
    // production surface for no other caller.
    println!(
        "M1 on the possession shape (counts, not bytes): room memo holds {} rooms, \
         verdict index holds {} judged entries",
        session.resident_ground_len(),
        session.resident_ground_judged_entries(),
    );
}
