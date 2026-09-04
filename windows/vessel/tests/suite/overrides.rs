//! The Reticence, Task 2: the conduct half. `suppressed_drives` is a
//! per-decision read overwritten by every `advance_one` iteration
//! (`liveness.rs:4996-5010`). This accumulates it across the possession, so the
//! axis the rider overrides is the axis the host can later go quiet on.

use crate::body_fields::seed_42;
use hornvale_vessel::{PossessOpts, Session};

#[test]
fn overrides_accumulate_across_ticks_rather_than_being_overwritten() {
    let (world, _ctx) = seed_42();
    let (mut s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    s.handle("!wait 30");
    let after_first: u32 = s.override_record().values().sum();
    s.handle("!wait 30");
    let after_second: u32 = s.override_record().values().sum();
    assert!(
        after_second > after_first,
        "the record must ACCUMULATE, not be overwritten: {after_first} then {after_second}"
    );
    // A THIRD tick. The two readings above cannot by themselves tell
    // accumulation apart from "each decision resets its suppressed drives to
    // a fixed 1": the record is empty before the first wait ever suppresses
    // anything, so the very first non-empty reading looks identical either
    // way, and the sum keeps climbing under a constant-write bug too as long
    // as NEW drives keep entering the map. The discriminator is a drive
    // suppressed on more than one decision — at seed 42, Fatigue and Hunger
    // both are, on the second and third `!wait` — whose OWN count must then
    // exceed 1. A bug that always writes 1 can never produce that, no matter
    // how many distinct keys accumulate.
    s.handle("!wait 30");
    let max_count = s.override_record().values().copied().max().unwrap_or(0);
    assert!(
        max_count > 1,
        "a drive suppressed on more than one decision must accumulate past 1, got record {:?}",
        s.override_record()
    );
}

#[test]
fn a_fresh_session_has_overridden_nothing() {
    let (world, _ctx) = seed_42();
    let (s, _) = Session::start(&world, &PossessOpts::default()).unwrap();
    assert!(
        s.override_record().is_empty(),
        "nothing is overridden before the first wait"
    );
}
