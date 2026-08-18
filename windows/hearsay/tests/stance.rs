//! Stance: where a community stands relative to one event.

mod common;

use common::{eid, ledger_with, put};
use hornvale_hearsay::lineage::lineage_of;
use hornvale_hearsay::stance::{Perpetration, Stance, is_lossy, stance_of};
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
    assert_eq!(
        stance_of(&led, &lin, Perpetration::Singleton, eid(1), eid(4)),
        Stance::Perpetrator
    );
}

#[test]
fn the_subject_and_its_descendants_are_the_victim_line() {
    let led = raid();
    let lin = lineage_of(&led);
    assert_eq!(
        stance_of(&led, &lin, Perpetration::Singleton, eid(1), eid(1)),
        Stance::VictimLine
    );
    assert_eq!(
        stance_of(&led, &lin, Perpetration::Singleton, eid(1), eid(2)),
        Stance::VictimLine
    );
    assert_eq!(
        stance_of(&led, &lin, Perpetration::Singleton, eid(1), eid(3)),
        Stance::VictimLine,
        "a grandchild is still the victim's line"
    );
}

#[test]
fn an_unrelated_community_is_a_bystander() {
    let led = raid();
    let lin = lineage_of(&led);
    assert_eq!(
        stance_of(&led, &lin, Perpetration::Singleton, eid(1), eid(5)),
        Stance::Bystander
    );
}

#[test]
fn a_retelling_is_lossy_exactly_when_the_two_stances_differ() {
    let led = raid();
    let lin = lineage_of(&led);
    // victim -> its own descendant: same stance, frictionless.
    assert!(!is_lossy(
        &led,
        &lin,
        Perpetration::Singleton,
        eid(1),
        eid(1),
        eid(2)
    ));
    // perpetrator -> the victim's line: different stances, lossy.
    assert!(is_lossy(
        &led,
        &lin,
        Perpetration::Singleton,
        eid(1),
        eid(4),
        eid(2)
    ));
    // bystander -> perpetrator: different, lossy.
    assert!(is_lossy(
        &led,
        &lin,
        Perpetration::Singleton,
        eid(1),
        eid(5),
        eid(4)
    ));
    // bystander -> bystander: same, frictionless.
    assert!(!is_lossy(
        &led,
        &lin,
        Perpetration::Singleton,
        eid(1),
        eid(5),
        eid(5)
    ));
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
    assert_eq!(
        stance_of(&led, &lin, Perpetration::Singleton, eid(1), eid(2)),
        Stance::VictimLine
    );
    assert_eq!(
        stance_of(&led, &lin, Perpetration::Singleton, eid(1), eid(5)),
        Stance::Bystander
    );
    assert!(is_lossy(
        &led,
        &lin,
        Perpetration::Singleton,
        eid(1),
        eid(5),
        eid(2)
    ));
}

/// Under `Inherited`, a raider's own child is still a `Perpetrator`; under
/// `Singleton` it is not. This is spec §5.2's entire behavioural difference.
#[test]
fn inherited_perpetration_closes_the_attacker_under_descent() {
    // 1 is the victim; 5 is the attacker; 6 is the attacker's child.
    let mut led = ledger_with(&[(1, None), (5, None), (6, Some(5))]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(10.0),
    );
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(5)),
    );
    let lin = lineage_of(&led);

    // The attacker itself is a Perpetrator under BOTH arms.
    for arm in Perpetration::ALL {
        assert_eq!(
            stance_of(&led, &lin, arm, eid(1), eid(5)),
            Stance::Perpetrator,
            "the named attacker is a Perpetrator under {}",
            arm.label()
        );
    }

    // Its child is where the arms part company.
    assert_eq!(
        stance_of(&led, &lin, Perpetration::Singleton, eid(1), eid(6)),
        Stance::Bystander,
        "Singleton: the raider's child is not implicated"
    );
    assert_eq!(
        stance_of(&led, &lin, Perpetration::Inherited, eid(1), eid(6)),
        Stance::Perpetrator,
        "Inherited: the deed stays the line's own"
    );

    // And therefore the raider's first retelling step is lossy under
    // Singleton and free under Inherited -- the asymmetry spec §3.6 measured.
    assert!(
        is_lossy(&led, &lin, Perpetration::Singleton, eid(1), eid(5), eid(6)),
        "Singleton: attacker -> own child crosses a stance boundary"
    );
    assert!(
        !is_lossy(&led, &lin, Perpetration::Inherited, eid(1), eid(5), eid(6)),
        "Inherited: attacker -> own child does not"
    );
}

/// `Singleton` must reproduce the shipped behaviour exactly, including the
/// precedence rule that a victim's own child which IS the named attacker
/// reads `Perpetrator` rather than `VictimLine`. Spec §3.6 measured 124 such
/// foundings, and they are the whole of the victim line's 4.06%.
#[test]
fn the_attacker_label_still_beats_the_victim_line_label() {
    // 2 is founded FROM 1 and is also what destroyed 1.
    let mut led = ledger_with(&[(1, None), (2, Some(1))]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(10.0),
    );
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(2)),
    );
    let lin = lineage_of(&led);
    for arm in Perpetration::ALL {
        assert_eq!(
            stance_of(&led, &lin, arm, eid(1), eid(2)),
            Stance::Perpetrator,
            "attacker precedence holds under {}",
            arm.label()
        );
    }
}
