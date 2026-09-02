//! The Rack's budget tests (spec §3.5): the per-turn work counters, asserted
//! as COUNTS rather than as a clock, so they are deterministic on every box
//! and cannot flap.
//!
//! **Not in the commit gate today.** A new vessel test is invisible to
//! `gate-commit` until the next green chamber job rewrites
//! `docs/timings/subfloor-roster.tsv` (that file's own header says so); this
//! module runs in the stage gate from its first commit.
//!
//! **The positive controls were measured, not assumed** — that is what Task 1
//! landing the instrument before Task 4 landed the change was for. Run
//! `--ignored` against the pre-Task-4 tree, the three future-state assertions
//! read:
//!
//! | test | before Task 4 | after |
//! | --- | --- | --- |
//! | `a_snapshot_performs_no_folds` | 67 affect folds, 137 position folds | 0, 0 |
//! | `a_wait_folds_the_roll_and_nothing_more` (2nd assertion) | 67 affect folds | 0 |
//! | `a_chamber_step_performs_no_folds_and_one_shadowcast` | already green | green |
//!
//! The 137 is worth keeping: `colocated_npcs` folded TWICE per other body
//! (once for the body, once for the possession, both inside the filter) plus
//! one for the vantage — 68·2 + 1 — every time anything asked who was here.
//!
//! A fourth test, `today_a_snapshot_folds_every_present_body`, pinned the
//! instrument's own liveness against the pre-change tree (a snapshot folds
//! exactly one drive per present body; **67** at seed 42's flagship). It is
//! deleted with the reader it measured; the number is recorded here and in
//! the chronicle rather than left as a test that can now only assert zero.
//!
//! **The `#[ignore]`s are gone with the reds.** The three carried
//! `claim: structural(seed: 42)` reasons — decision 0093's shape
//! (`cli/tests/suite/claim_shape.rs`'s sanctioned `SHAPES`) — which was
//! always about being *future-state*, never about cost, so nothing survives
//! the change that made them green.

use crate::common;
use hornvale_vessel::{PossessOpts, Session};

/// P2 (spec §4): a snapshot at seed 42's flagship performs ZERO drive folds,
/// ZERO position folds and ZERO plan searches, now that the turn reads the
/// rack (Task 4). Before it, the same call folded 67 drives and 137
/// positions — the positive control the spec preregisters, measured.
///
/// The `position_folds == 0` half is a claim about a SNAPSHOT, not about the
/// counter: `Session::position_of` still bumps it (see that method's doc),
/// so a zero here means "this call folded nothing", never "nothing can".
///
/// Not in the commit gate until the next green chamber job rewrites
/// `docs/timings/subfloor-roster.tsv`; runs in the stage gate from this commit.
///
/// MUTATION THIS MUST FAIL AGAINST: give `Session::position` its old body
/// back (`self.turn_work.bump_position_folds(); agent_position(&self.ledger,
/// self.driven_body(), self.day)`). Run and observed: `snapshot folded 4
/// positions / left: 4 / right: 0` — four, not one, because `snapshot` reads
/// the possession's room at four separate points (the vantage, the
/// co-location scan's comparand, the `SelfChannel.room` pack, and the
/// chamber sighting's own room).
#[test]
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
/// **Green before Task 4 as well as after, measured rather than assumed**: a
/// within-room `go n` never calls `snapshot`/`needs` (`step.affect_folds ==
/// 0` already) and `sighting` was derived at most once on this path already.
/// It is here because Task 4 must not REGRESS it, which is a different job
/// from turning it green.
///
/// `snap.shadowcasts` comes from `snapshot_work`, which forgets the memo
/// before it measures (its own doc says why), so this reads a cold
/// snapshot's one derivation rather than the step's leftover.
/// `a_turn_derives_one_shadowcast` is the test for the sharing itself.
///
/// Not in the commit gate until the next green chamber job rewrites
/// `docs/timings/subfloor-roster.tsv`; runs in the stage gate from this commit.
///
/// MUTATION THIS MUST FAIL AGAINST: bump `shadowcasts` twice in
/// `Session::derive_sighting`. Run and observed: `the snapshot after the step
/// derived 2 shadowcasts`.
#[test]
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
/// **Its second assertion was the red, measured**: the `snapshot_work()`
/// taken right after the wait folded every present body — 67, against the 0
/// asserted. The first assertion has always held at seed 42, and holds for
/// a reason worth stating: the tick's own folds are the roster's APPEND
/// seeding (`Session::seed_felts`, the one surviving `affect_of*` call on
/// the session path), and seed 42's flagship roster never grows, so the
/// count is 0 against a roll of 68. A seed whose `wait` brings a settlement
/// within call folds once per newly appended body, which is what the bound
/// is for.
///
/// Not in the commit gate until the next green chamber job rewrites
/// `docs/timings/subfloor-roster.tsv`; runs in the stage gate from this commit.
///
/// MUTATION THIS MUST FAIL AGAINST: make `Session::felt_of` fold as well as
/// read (`let _ = self.seed_felts(std::slice::from_ref(body));` as its first
/// statement — the same stateless read the append seeds with). Run and
/// observed: `the snapshot after a wait folded 67 drives / left: 67 /
/// right: 0`.
#[test]
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

/// `Session::sighting` is derived AT MOST ONCE PER TURN (spec §3.3): a
/// chamber `look` — which reads the sighting for its presence line — followed
/// by the `snapshot()` the client takes on the same turn reads exactly one
/// shadowcast between them, not two.
///
/// It uses `snapshot()` and `turn_work()` rather than `snapshot_work()` on
/// purpose: `snapshot_work` forgets the memo first, by design (its own doc
/// says why), so it can only ever measure a COLD snapshot. What this test
/// asserts is the other half — that within one turn the second reader pays
/// nothing.
///
/// Not in the commit gate until the next green chamber job rewrites
/// `docs/timings/subfloor-roster.tsv`; runs in the stage gate from this commit.
///
/// MUTATION THIS MUST FAIL AGAINST: clear the memo inside `Session::sighting`
/// itself (`*self.sighting_memo.borrow_mut() = None;` as its first statement)
/// instead of leaving it to the key comparison — the memo is then written and
/// immediately discarded, so every reader derives its own. Run and observed:
/// `a chamber look plus its snapshot derived 2 shadowcasts / left: 2 /
/// right: 1`.
#[test]
fn a_turn_derives_one_shadowcast() {
    let world = common::build(42).expect("seed 42 builds");
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    common::step_inside(&mut session);
    session.handle("look");
    let _ = session.snapshot().expect("a live session snapshots");
    let w = session.turn_work();
    assert_eq!(
        w.shadowcasts, 1,
        "a chamber look plus its snapshot derived {} shadowcasts",
        w.shadowcasts
    );
}

/// ...and the memo MUST NOT straddle the tick inside a `wait`, which is the
/// one turn that reads the sighting on both sides of a world that moved.
///
/// **This is the test for the defect a naive per-turn memo would have
/// introduced, and nothing else here would have caught it.** `Session::wait`
/// asks who could be sensed BEFORE it advances the day (a departure is
/// narrated about a creature that is no longer here, so that is the last
/// moment the question is answerable) and asks again AFTER, for arrivals. A
/// memo keyed on the turn alone would hand the second question the first
/// question's answer — and both halves would then agree, so every chamber
/// arrival and departure would silently stop being narrated with no
/// assertion anywhere going red. `SightingKey` carries the day for exactly
/// this reason.
///
/// Not in the commit gate until the next green chamber job rewrites
/// `docs/timings/subfloor-roster.tsv`; runs in the stage gate from this commit.
///
/// MUTATION THIS MUST FAIL AGAINST: neutralise `day` in the key
/// (`day: WorldTime::from_ticks(0)` in `Session::sighting_key`), so the
/// post-tick read hits the pre-tick memo. Run and observed: `a chamber wait
/// derived 1 shadowcasts; the memo straddled its own tick`.
#[test]
fn a_chamber_wait_derives_a_shadowcast_on_each_side_of_its_tick() {
    let world = common::build(42).expect("seed 42 builds");
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("starts");
    common::step_inside(&mut session);
    session.handle("wait");
    let w = session.turn_work();
    assert!(
        w.shadowcasts >= 2,
        "a chamber wait derived {} shadowcasts; the memo straddled its own tick",
        w.shadowcasts
    );
}
