//! Stance: where a community stands relative to one event.

mod common;

use common::{eid, ledger_with, put};
use hornvale_hearsay::lineage::lineage_of;
use hornvale_hearsay::stance::{Stance, is_lossy, stance_of};
use hornvale_kernel::ledger::Value;

/// 1 is raided by 4; 2 and 3 descend from 1; 5 is unrelated.
fn raid() -> hornvale_kernel::ledger::Ledger {
    let mut led = ledger_with(&[(2, Some(1)), (3, Some(2)), (4, None), (5, None)]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(500.0),
    );
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(4)),
    );
    led
}

#[test]
fn the_attacker_is_the_perpetrator() {
    let led = raid();
    let lin = lineage_of(&led);
    assert_eq!(stance_of(&led, &lin, eid(1), eid(4)), Stance::Perpetrator);
}

#[test]
fn the_subject_and_its_descendants_are_the_victim_line() {
    let led = raid();
    let lin = lineage_of(&led);
    assert_eq!(stance_of(&led, &lin, eid(1), eid(1)), Stance::VictimLine);
    assert_eq!(stance_of(&led, &lin, eid(1), eid(2)), Stance::VictimLine);
    assert_eq!(
        stance_of(&led, &lin, eid(1), eid(3)),
        Stance::VictimLine,
        "a grandchild is still the victim's line"
    );
}

#[test]
fn an_unrelated_community_is_a_bystander() {
    let led = raid();
    let lin = lineage_of(&led);
    assert_eq!(stance_of(&led, &lin, eid(1), eid(5)), Stance::Bystander);
}

#[test]
fn a_retelling_is_lossy_exactly_when_the_two_stances_differ() {
    let led = raid();
    let lin = lineage_of(&led);
    // victim -> its own descendant: same stance, frictionless.
    assert!(!is_lossy(&led, &lin, eid(1), eid(1), eid(2)));
    // perpetrator -> the victim's line: different stances, lossy.
    assert!(is_lossy(&led, &lin, eid(1), eid(4), eid(2)));
    // bystander -> perpetrator: different, lossy.
    assert!(is_lossy(&led, &lin, eid(1), eid(5), eid(4)));
    // bystander -> bystander: same, frictionless.
    assert!(!is_lossy(&led, &lin, eid(1), eid(5), eid(5)));
}

#[test]
fn stance_fires_on_an_event_with_no_attacker() {
    // The peaceful case: no occ-ended-by, so nobody is a perpetrator, but the
    // victim line and everyone else STILL differ. A predicate that went inert
    // here could never distort a world without raids.
    let mut led = ledger_with(&[(2, Some(1)), (5, None)]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(500.0),
    );
    let lin = lineage_of(&led);
    assert_eq!(stance_of(&led, &lin, eid(1), eid(2)), Stance::VictimLine);
    assert_eq!(stance_of(&led, &lin, eid(1), eid(5)), Stance::Bystander);
    assert!(is_lossy(&led, &lin, eid(1), eid(5), eid(2)));
}
