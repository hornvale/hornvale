//! The Coercion, Task 1: possession is an OPEN/CLOSE fact pair, because the
//! ledger is append-only and both terminators are events rather than
//! schedules. Sleep — the gate's only other row — sidesteps this by being
//! derived and self-terminating; a committed fact does not.
//!
//! The fold's own open/close/reopen coverage (`possessor_of`) lives
//! in-module in `windows/vessel/src/session.rs`'s `#[cfg(test)] mod tests`,
//! not here: `Session` gains no public ledger reader and no test-only commit
//! for this task (resolved against the tree before dispatch — see the
//! task-1 report), so this file can only exercise the one new public read,
//! [`Session::possessor`], from outside the crate.
//!
//! **Task 4 adds the imposition seam**, so this file gains its first two
//! tests that actually OPEN and CLOSE a possession — through
//! [`Session::handle`], the only public surface, exactly the way a real
//! player would. See `handle_ooc` in `session.rs` for the `!possess` /
//! `!unpossess` arms themselves.

use crate::body_fields::seed_42;
use hornvale_vessel::{PossessOpts, Session};

#[test]
fn a_fresh_session_has_no_possessor() {
    let (world, _ctx) = seed_42();
    let (session, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    assert_eq!(
        session.possessor(),
        None,
        "every body is free until an imposition seam opens a possessed-by"
    );
}

/// The acceptance shape (spec §7, H1's own scaffold): the OOC seam opens a
/// possession, and the possessor's own option — `!unpossess` — closes it
/// again (spec §6, "release is reachable").
#[test]
fn the_ooc_seam_opens_and_closes_a_possession() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    assert_eq!(s.possessor(), None, "starts free");

    let _ = s.handle("!possess");
    assert!(
        s.possessor().is_some(),
        "the seam must commit a possessed-by fact"
    );

    let _ = s.handle("!unpossess");
    assert_eq!(
        s.possessor(),
        None,
        "unpossess closes it — the possessor's option, spec section 6"
    );
}

/// Release is idempotent: closing an already-closed possession must not
/// commit a second `possession-ended`, or a body could accumulate unbounded
/// terminators for one opening.
#[test]
fn releasing_a_free_body_commits_nothing() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    let before = s.committed_fact_count();
    let _ = s.handle("!unpossess");
    assert_eq!(
        s.committed_fact_count(),
        before,
        "no fact for a no-op release"
    );
}

/// The footgun Task 1 flagged: `Ledger::commit`'s idempotent dedup compares
/// the WHOLE `Fact` envelope, so a possess -> unpossess -> possess sequence
/// whose second `possessed-by` matched the first in every field (including
/// `provenance`) would silently commit nothing on the reopen, and
/// `Session::possessor()` would then read `None` right after a verb that
/// reported success. Same day throughout — nothing here advances the clock —
/// which is exactly the case that would expose a static provenance string.
#[test]
fn reopening_a_possession_on_the_same_day_is_not_a_silent_no_op() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();

    let _ = s.handle("!possess");
    assert!(s.possessor().is_some(), "first possession opens");
    let _ = s.handle("!unpossess");
    assert_eq!(s.possessor(), None, "closes");
    let _ = s.handle("!possess");
    assert!(
        s.possessor().is_some(),
        "the reopen, same day, must not silently no-op"
    );
}
