//! The Rack's budget tests: `TurnWork`'s positive control (Task 1) and the
//! future-state assertions Task 4 is expected to turn green.
//!
//! **Not in the commit gate today.** A new vessel test is invisible to
//! `gate-commit` until the next green chamber job rewrites
//! `docs/timings/subfloor-roster.tsv` (that file's own header says so); this
//! module runs in the stage gate from its first commit.
//!
//! Three of the four tests here are `#[ignore]`d, as future-state assertions
//! against the rack Task 4 lands — landing the instrument before the change
//! it measures is what makes its positive control real: run these
//! `--ignored` and the numbers they print are the measured "before" this
//! campaign's Task 4 is claiming to change, not an assumed one. **Two of the
//! three are RED against today's tree** (`Session::snapshot` and
//! `Session::needs` still re-fold every present body's drives on every
//! call): `a_snapshot_performs_no_folds` reads 67 affect folds, 137 position
//! folds and 0 plan searches where it asserts 0/0/0; `a_wait_folds_the_roll_
//! and_nothing_more`'s SECOND assertion (the snapshot after the wait) reads
//! the same 67 where it asserts 0. **The third, `a_chamber_step_performs_no_
//! folds_and_one_shadowcast`, already passes today** — measured, not
//! assumed: a within-room `go n` step never calls `Session::snapshot` or
//! `needs` at all, so its own `affect_folds` is 0 today already, and
//! `Session::sighting` (the shadowcast source) is called at most once per
//! turn on this path already. It stays `#[ignore]`d anyway for uniformity
//! with its two siblings and because it shares their `claim:` shape and
//! their seed-42 cost, not because it is red.

use crate::common;
use hornvale_vessel::{PossessOpts, Session};

/// P2 (spec §4): a snapshot at seed 42's flagship performs ZERO drive folds,
/// ZERO position folds and ZERO plan searches once the rack is read (Task 4).
/// Today it performs one fold per present body; run with `--ignored` before
/// Task 4 and paste the red into the ledger — that red is the positive
/// control the spec preregisters.
///
/// Not in the commit gate until the next green chamber job rewrites
/// `docs/timings/subfloor-roster.tsv`; runs in the stage gate from this commit.
///
/// MUTATION THIS MUST FAIL AGAINST: (after Task 4) reinstate one
/// `affect_of_memo_occupied` call in `snapshot` — `affect_folds` reads 67.
#[test]
#[ignore = "claim: structural(seed: 42); red until The Rack Task 4 reads the rack — the preregistered positive control"]
fn a_snapshot_performs_no_folds() {
    let world = common::build(42).expect("seed 42 builds");
    let (session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let w = session.snapshot_work();
    assert_eq!(
        w.affect_folds, 0,
        "snapshot folded {} drives",
        w.affect_folds
    );
    assert_eq!(
        w.position_folds, 0,
        "snapshot folded {} positions",
        w.position_folds
    );
    assert_eq!(
        w.plan_searches, 0,
        "snapshot ran {} plan searches",
        w.plan_searches
    );
}

/// A chamber step performs no drive folds and derives at most one shadowcast
/// per turn (the `look` after it may derive one) — the rack's own claim,
/// checked both on the step's own turn-work read and on a snapshot taken
/// right after it, so a caller cannot pass by counting the wrong call.
///
/// **Already green today, measured rather than assumed**: a within-room
/// `go n` never calls `snapshot`/`needs` (`step.affect_folds == 0` already)
/// and `sighting` is derived at most once on this path already
/// (`step.shadowcasts` reads 0; `snap.shadowcasts` reads 1, both `<= 1`).
/// Kept `#[ignore]`d and in this module for uniformity with its two red
/// siblings — same `claim:` shape, same seed-42 cost — not because it is
/// red; Task 4 must not regress it.
///
/// Not in the commit gate until the next green chamber job rewrites
/// `docs/timings/subfloor-roster.tsv`; runs in the stage gate from this commit.
#[test]
#[ignore = "claim: structural(seed: 42); red until The Rack Task 4 — same control as above"]
fn a_chamber_step_performs_no_folds_and_one_shadowcast() {
    let world = common::build(42).expect("seed 42 builds");
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    common::step_inside(&mut session);
    session.handle("go n");
    let step = session.turn_work();
    assert_eq!(
        step.affect_folds, 0,
        "a chamber step folded {} drives",
        step.affect_folds
    );
    assert!(
        step.shadowcasts <= 1,
        "a chamber step derived {} shadowcasts",
        step.shadowcasts
    );
    let snap = session.snapshot_work();
    assert!(
        snap.shadowcasts <= 1,
        "the snapshot after the step derived {} shadowcasts",
        snap.shadowcasts
    );
}

/// A wait performs at most `roll_len()` folds (the walk) and the snapshot
/// after it none — the counters separate the tick's own work from the
/// turn's, which is the split Task 4 is meant to preserve while zeroing out
/// the turn's half.
///
/// **RED today on its second assertion, measured**: `wait` itself never
/// calls `affect_of_memo_occupied` (only `snapshot`/`needs` do, at seed 42
/// this reads `tick.affect_folds == 0`, so the first assertion holds
/// trivially against `roll_len() == 68`), but the `snapshot_work()` taken
/// right after it still folds every present body — 67, against the 0
/// asserted.
///
/// Not in the commit gate until the next green chamber job rewrites
/// `docs/timings/subfloor-roster.tsv`; runs in the stage gate from this commit.
#[test]
#[ignore = "claim: structural(seed: 42); red until The Rack Task 4 — same control as above"]
fn a_wait_folds_the_roll_and_nothing_more() {
    let world = common::build(42).expect("seed 42 builds");
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    let roll_len = session.roll_len();
    session.handle("wait");
    let tick = session.turn_work();
    assert!(
        (tick.affect_folds as usize) <= roll_len,
        "wait folded {} drives against a roll of {}",
        tick.affect_folds,
        roll_len
    );
    let snap = session.snapshot_work();
    assert_eq!(
        snap.affect_folds, 0,
        "the snapshot after a wait folded {} drives",
        snap.affect_folds
    );
}

/// The instrument itself is live TODAY: a snapshot at the flagship reads one
/// affect fold per present body (67 at seed 42) — pinned here so Task 4's
/// change to zero is a measured move, not a claim. Deleted by Task 4.
///
/// MUTATION THIS MUST FAIL AGAINST: comment out the
/// `TurnWork::bump(&self.turn_work.affect_folds);` line at the top of
/// `Session::snapshot`'s `.map(|npc| { ... })` closure
/// (`windows/vessel/src/session.rs`, immediately before the
/// `affect_of_memo_occupied` call) and re-run `cargo test -p hornvale-vessel
/// --test suite -- turn_budget::today_a_snapshot_folds_every_present_body
/// --nocapture`. Verified: `left: 0` (the mutated instrument) `right: 67`
/// (the real present count) — "snapshot folded 0 drives against 67 present
/// bodies".
#[test]
fn today_a_snapshot_folds_every_present_body() {
    let world = common::build(42).expect("seed 42 builds");
    let (session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    // `TurnWork` starts at its `Default` (all zero) and nothing has called
    // `reset` yet (`Session::start` never does), so one bare `snapshot()`
    // call followed by `turn_work()` reads exactly what THAT call folded —
    // no separate `snapshot_work()` reset-and-retake needed.
    let snapshot = session.snapshot().expect("a live session snapshots");
    let present = snapshot.sensed.present.len();
    let w = session.turn_work();
    assert_eq!(
        w.affect_folds as usize, present,
        "snapshot folded {} drives against {} present bodies",
        w.affect_folds, present
    );
}
