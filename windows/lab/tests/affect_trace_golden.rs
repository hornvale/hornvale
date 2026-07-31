//! A value-pinned affect-trace fixture (the-waymark, Task 4 carry-over from
//! Task 3's re-review): the health pins (`the_null_control_reads_no_chronic_
//! distress`, the stuck/recovery/self-determinism batteries) can only detect
//! a handful of SHAPE properties — none of them can catch a sampling
//! regression that shifts WHICH affect a creature reads on WHICH tick while
//! every existing assertion still passes. This commits a byte-stable digest
//! of `simulate_world`'s seed-42 output (species + per-tick affect label/
//! arousal/valence/object, one line per creature-tick) as a committed golden
//! (the `session_snapshot` precedent, `hornvale_kernel::golden::assert_
//! golden`), so the-waymark's plan-cache/geometry-memo work — and any future
//! sim performance work — has a standing byte witness that VALUES, not just
//! shapes, did not move.
//!
//! Generated at the-waymark's Task 4 base commit (5e2de827, before the
//! plan-cache change), which The Waymark's own A/B already proved
//! trace-equivalent to pre-campaign (Task 3's re-reviewer, ledger #7/#10).
//! `seed_42_affect_trace_reproduces_the_pinned_bytes` is the task's own
//! acceptance evidence: the plan cache must reproduce this digest bit for
//! bit, since it is a pure-function result cache — a search runs less
//! often, never differently.

use hornvale_lab::health::simulate_world;

fn world() -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap()
}

/// A byte-stable digest of every derived creature's full affect trace:
/// species, then one line per simulated tick naming the label, the
/// arousal/valence (Debug-formatted — full-precision, deterministic), and
/// the object drive. Line-oriented so a future diff (on a REBASELINE accept)
/// reads as "creature N, tick T changed", not an opaque blob.
fn digest() -> String {
    let traces = simulate_world(&world());
    let mut out = String::new();
    for (i, trace) in traces.iter().enumerate() {
        out.push_str(&format!("creature {i} species={}\n", trace.species));
        for (t, affect) in trace.affects.iter().enumerate() {
            out.push_str(&format!(
                "  tick {t}: label={:?} arousal={:?} valence={:?} object={:?}\n",
                affect.label, affect.arousal, affect.valence, affect.object
            ));
        }
    }
    out
}

#[test]
fn seed_42_affect_trace_reproduces_the_pinned_bytes() {
    hornvale_kernel::golden::assert_golden(
        std::path::Path::new(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/fixtures/affect-trace-seed-42.txt"
        )),
        &digest(),
        "the seed-42 affect trace moved — the-waymark's plan-cache/geometry- \
         memo work must be VALUE-preserving (a cache changes WHEN a search \
         runs, never WHAT it returns); a diff here means some change altered \
         what a creature feels, not just how fast, and needs investigation \
         before acceptance",
    );
}

#[test]
fn the_affect_trace_digest_is_itself_deterministic() {
    assert_eq!(
        digest(),
        digest(),
        "same seed, same code ⇒ same bytes, twice in a row"
    );
}
