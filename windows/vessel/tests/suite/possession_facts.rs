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
//! [`Session::possessor`], from outside the crate. It has nothing to commit
//! `possessed-by`/`possession-ended` with yet — that is Tasks 3-4's imposition
//! seam — so the only thing provable here is the free-body default.

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
