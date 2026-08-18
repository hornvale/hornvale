//! Spec §5.1: a community that had already ended cannot hold a claim about a
//! later event. Spec §3.5 measured 1,959 of 164,822 holders (1.19%) that do,
//! confirmed on named instances up to ~500 years dead.

mod common;

use common::{eid, ledger_with, put};
use hornvale_hearsay::clock::{Clock, admits};
use hornvale_kernel::ledger::Value;

#[test]
fn a_community_that_ended_before_the_event_is_refused() {
    let mut led = ledger_with(&[(1, None)]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(100.0),
    );

    assert!(
        !admits(Clock::Alive, &led, eid(1), 200.0),
        "ended day 100, event day 200: refused"
    );
    assert!(
        admits(Clock::Alive, &led, eid(1), 50.0),
        "ended day 100, event day 50: admitted"
    );
    assert!(
        admits(Clock::Off, &led, eid(1), 200.0),
        "Clock::Off admits everything -- it is today's behaviour"
    );
}

#[test]
fn a_community_that_never_ended_is_always_admitted() {
    let led = ledger_with(&[(1, None)]);
    assert!(
        admits(Clock::Alive, &led, eid(1), 1.0e9),
        "no occ-ended fact: still standing, admitted at any day"
    );
}

/// The boundary is exact, not a band. A community ending on precisely the
/// event's day IS admitted: it was there. Spec §5.1 inherits the exact-equality
/// discipline `witnesses_of` already uses for survivor refoundings, where 477
/// of 562 pairs sit at a gap of exactly 0.0 -- so there is no threshold to
/// tune and no near-miss band to argue about.
#[test]
fn ending_on_the_event_day_is_admitted() {
    let mut led = ledger_with(&[(1, None)]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(100.0),
    );
    assert!(
        admits(Clock::Alive, &led, eid(1), 100.0),
        "the subject of an ending is a witness to its own ending"
    );
}
