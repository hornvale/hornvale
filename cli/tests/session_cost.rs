//! The session turn's **cost gate** (The Panes, Task 4): what one possessed
//! turn costs now that every snapshot carries the band's spatial cells.
//!
//! Budgets here are **falsification ceilings, not targets**, in the sense
//! `graph_cost.rs` established and `scene_cost.rs` restates: set above the
//! measured value so only a real regression trips them.
//!
//! **Ceilings ratchet DOWN freely. Raising one is an explicit, reviewed act**
//! and must be recorded in that constant's doc comment with the reason.
//!
//! The instrument that produced these numbers is
//! `windows/vessel/examples/turn_cost.rs`, which holds the full matched-pair
//! reading (before and after the channel) and the per-verb-class split. That
//! bench times `handle` and `snapshot()+json` separately so the split is
//! visible per class; this gate times them together (`TURN_BUDGET_MS`) as
//! the brief specifies, since a client pays both on every turn regardless of
//! which verb caused them. Re-run the bench, not this test, when you want to
//! know what moved or which verb class is responsible.
//!
//! Read a red run as contention before suspecting the code: every ceiling
//! here is a wall time, and `scene_cost.rs`'s documented failure mode — all
//! metrics inflating together by roughly the same factor — is the machine,
//! not a regression. A real regression is local.

use hornvale_kernel::Seed;
use hornvale_vessel::{PossessOpts, Session};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

// The measurement harness times derivation calls for a diagnostic (never sim
// logic, never a fact, never seeded from wall-clock) -- exempt from the
// wall-clock ban (clippy.toml / decision 0001), same pattern as
// `cli/tests/scene_cost.rs` and `cli/tests/graph_cost.rs`.
#[allow(clippy::disallowed_types)]
use std::time::Instant;

/// The fixed verb sequence, mirrored verbatim from
/// `windows/vessel/examples/turn_cost.rs::SEQUENCE`: verbs that move the
/// possession (`enter`, `out`), a verb that advances the day (`wait`), and
/// verbs that do neither (`look`, `map`, `examine`).
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

/// How many times to run the sequence. Medians of repeated runs, mirrored
/// from the bench.
const RUNS: usize = 5;

/// Ceiling for `Session::start`, ms.
///
/// **Dev profile is the ceiling basis** (this box, `MacBookPro`, `cargo test
/// -p hornvale --test session_cost -- --ignored --nocapture`, 2026-08-06),
/// the same convention `scene_cost.rs` follows, since `make gate-full`
/// (`scripts/gate-full-heavy.sh`) runs the heavy tier via plain `cargo
/// nextest run`, without `--release`. Three runs gave 3442.192, 3177.372,
/// 3289.123 ms — the slowest, 3442.192, is the ceiling basis.
///
/// **Budgeted at ~3x, not the usual ~2x**, because `Session::start` is
/// wall-clock noisy on this shared box: it re-sculpts terrain from scratch
/// on every call (`terrain_of` -> `hornvale_terrain::generate`, uncached at
/// the `World` level), and Task 1's own baseline reading swung from 1451 ms
/// to 2280 ms between two runs taken minutes apart (see Task 1's report,
/// `--release`) — a ~1.6x swing on its own. A ~2x ceiling on the slowest of
/// three same-session runs would still be a coin flip against a swing that
/// size; ~3x gives headroom against contention without hiding a real
/// regression, which would need to be far larger than this noise band to
/// trip it.
///
/// **Re-measured, kept unchanged (The Sighting, Task 6), 2026-08-06,
/// `MacBookPro`, quiet box (`uptime` 1-min load 3.8-4.7 throughout, after
/// waiting out another session's `nextest run` that had it at 19-42).**
/// Three fresh dev-profile runs gave 2757.136, 2712.546, 2803.291 ms —
/// slowest 2803.291, well *under* the 3442.192 ms basis above, not over it.
/// That is not evidence the ceiling can safely ratchet down: this session's
/// three runs were themselves tightly clustered (656-705 ms on the parallel
/// `--release` bench, run back to back), yet the cross-session record for
/// this same constant now spans roughly 657-3442 ms — a >5x range — purely
/// from terrain-resculpt noise on a shared box, not from any code this
/// campaign touched (Tasks 2-5 added spatial/creature-sighting cost to
/// `snapshot()`, never to genesis). Recomputing the multiplier from a
/// single quiet session's tight cluster would fit *this* session's noise
/// band and nothing else's; the ceiling stays at its previous value, and
/// this paragraph exists so the next re-measurement has both endpoints of
/// the observed range rather than just the newest one.
const START_BUDGET_MS: f64 = 10500.0;

/// Ceiling for one `handle` + `snapshot` + serialize, ms — the combined
/// per-turn cost a client actually pays, pooled across all ten verbs in
/// [`SEQUENCE`] (moving, day-advancing, and neither alike).
///
/// **Dev profile is the ceiling basis**, same box/command/date as
/// [`START_BUDGET_MS`]. Three runs of this test's own combined-timer
/// measurement gave a pooled median of 3.906, 3.543, 3.740 ms — slowest
/// 3.906 ms. Budgeted at ~2x, rounded up.
///
/// The bench (`windows/vessel/examples/turn_cost.rs`) times `handle` and
/// `snapshot()+json` separately, under `--release`, and additionally splits
/// by verb class; its release-profile pooled `snapshot()+json` reading
/// (slowest of three runs: 1.249 ms) is NOT comparable to this ceiling —
/// different profile, different thing measured (split vs combined) — see
/// that file's own doc comment for the release-profile numbers and the
/// per-class split.
///
/// **Re-measured, kept unchanged (The Sighting, Task 6), 2026-08-06, same
/// quiet box as [`START_BUDGET_MS`].** Three fresh dev-profile runs of this
/// test's own pooled combined timer gave 3.769, 3.721, 3.939 ms — slowest
/// 3.939 ms, essentially flat against the 3.906 ms basis above (+0.8%) even
/// though Tasks 2-5 added real per-turn work (the anchor embedding, the
/// shadowcast, creature marks) inside `snapshot()`. `2 * 3.939 = 7.878`,
/// which still rounds up to the existing 8.0 — no change needed.
///
/// **Why the pooled figure looks flat while the release-profile bench does
/// not**: it is dominated by the `Neither` class (35 of 50 pooled samples),
/// and this gate is not designed to see a per-class shift — its own header
/// comment says so ("Re-run the bench, not this test, when you want to know
/// what moved or which verb class is responsible"). The `--release` bench
/// (below) found the real signal: `moving`-class `snapshot()+json` grew
/// 1.259 ms -> 3.706 ms (**2.94x**), because `enter` is where the possession
/// first stands inside a chamber and `snapshot()` derives `sighting()`
/// fresh, uncached, on every call. A single indoor `enter` turn's combined
/// `handle` (11.093 ms) + `snapshot()+json` (3.706 ms) is 14.799 ms under
/// `--release` — already over this ceiling's *dev-profile* 8.0 ms figure by
/// comparison, though that comparison mixes profiles and isn't the number
/// this test gates on. The pooled dev-profile median stays comfortably
/// under budget only because 33 of the 35 `Neither` samples never touch a
/// chamber; a `SEQUENCE` weighted more toward chamber turns would surface
/// this in the pooled figure too.
const TURN_BUDGET_MS: f64 = 8.0;

/// Ceiling for one walk-band snapshot's serialized bytes. The spec measured
/// `scene/surrounds/v1` at 7,049 bytes at radius 4; this bounds the whole
/// snapshot, which is larger.
///
/// Basis: Task 4's reading, 2026-08-06 — 11,582 bytes, unchanged across
/// three dev-profile runs of this test and three release-profile runs of
/// the bench alike (deterministic on seed 42; profile and repetition affect
/// only wall time, never content). Budgeted at ~2x, rounded up.
///
/// **Corrected and re-based (The Sighting, Task 6), 2026-08-06.** The basis
/// above was already stale before this task: `d36a6a79` ("carry the v2
/// chart into The Panes' spatial channel", same day) grew the walk band to
/// 12,273 bytes, which is what the committed fixture
/// (`windows/vessel/tests/fixtures/snapshot-seed-42-walk.json`) and this
/// task's own fresh reads both give — three dev-profile runs of this test
/// (12273, 12273, 12273 B) and three release-profile runs of the bench
/// (12273, 12273, 12273 B) alike, so the walk band itself is untouched by
/// Tasks 2-5's chamber-only work (anchors/shadowcast/marks apply only
/// indoors). The old ceiling (23200) still happens to pass, but at only
/// `23200 / 12273 = 1.89x` headroom, not the stated ~2x — an unnoticed
/// consequence of the same staleness, not a deliberate tightening. Re-based
/// on the confirmed 12,273-byte figure: `2 * 12273 = 24546`, rounded up.
/// This is a ceiling **raise** (23200 -> 24600), recorded here as the
/// required deliberate act: the reason is restoring the intended ~2x
/// margin against a basis that was already wrong, not a growth this
/// campaign's chamber work caused in the walk band.
const WALK_BYTES_BUDGET: usize = 24600;

#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn a_possessed_turn_stays_within_its_ceilings() {
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

    for _ in 0..RUNS {
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t0 = Instant::now();
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        #[allow(clippy::disallowed_types)] // benchmark harness
        let start_ms = t0.elapsed().as_secs_f64() * 1000.0;
        starts.push(start_ms);

        for line in SEQUENCE {
            #[allow(clippy::disallowed_types)] // benchmark harness
            let t1 = Instant::now();
            let _ = session.handle(line);
            let snap = session.snapshot().expect("a live session snapshots");
            // Serialize too: the emit is part of the per-turn cost the
            // client actually pays, and measuring construction alone would
            // under-report it.
            let json = hornvale_vessel::snapshot_json(&snap);
            std::hint::black_box(&json);
            #[allow(clippy::disallowed_types)] // benchmark harness
            let turn_ms = t1.elapsed().as_secs_f64() * 1000.0;
            turns.push(turn_ms);
        }
    }

    let start_median = median(&mut starts);
    let turn_median = median(&mut turns);

    // The byte figure the spec priced by radius, taken on a fresh session
    // outside the possession (the walk band, the larger of the two bands).
    let (mut session, _) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
    let walk_bytes = hornvale_vessel::snapshot_json(&session.snapshot().unwrap()).len();
    std::hint::black_box(session.handle("enter"));

    println!("Session::start        {start_median:9.3} ms (budget {START_BUDGET_MS})");
    println!("handle+snapshot+json  {turn_median:9.3} ms (budget {TURN_BUDGET_MS})");
    println!("walk snapshot bytes   {walk_bytes:9} B  (budget {WALK_BYTES_BUDGET})");

    assert!(
        start_median < START_BUDGET_MS,
        "Session::start took {start_median:.3} ms, over the {START_BUDGET_MS} ms ceiling"
    );
    assert!(
        turn_median < TURN_BUDGET_MS,
        "one handle+snapshot+serialize took {turn_median:.3} ms, over the \
         {TURN_BUDGET_MS} ms ceiling"
    );
    assert!(
        walk_bytes < WALK_BYTES_BUDGET,
        "a walk-band snapshot serialized to {walk_bytes} bytes, over the \
         {WALK_BYTES_BUDGET} byte ceiling"
    );
}

/// The median of `xs`, which this sorts in place. `total_cmp` rather than
/// `partial_cmp().unwrap()`: the workspace sorts floats deterministically
/// and never panics on a NaN it did not expect.
fn median(xs: &mut [f64]) -> f64 {
    xs.sort_by(|a, b| a.total_cmp(b));
    xs[xs.len() / 2]
}
