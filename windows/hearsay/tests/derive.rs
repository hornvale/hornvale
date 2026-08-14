mod common;

use common::{eid, ledger_with, put};
use hornvale_hearsay::derive::{claims_about, witnesses_of};
use hornvale_hearsay::lineage::lineage_of;
use hornvale_kernel::ledger::{Ledger, Value};
use hornvale_kernel::provenance::Provenance;

/// 1 (root) ends on day 100; 2 founded from 1; 3 founded from 2. A second,
/// unrelated lineage (10 root, 11 founded from 10) exists so
/// `an_unrelated_lineage_holds_nothing_about_it` has a real cross-lineage
/// holder to prove absent, rather than an entity id nothing ever committed.
fn world() -> Ledger {
    let mut led = ledger_with(&[
        (1, None),
        (2, Some(1)),
        (3, Some(2)),
        (10, None),
        (11, Some(10)),
    ]);
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(100.0),
    );
    led
}

#[test]
fn the_subject_of_an_event_witnessed_it() {
    let led = world();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let own = claims
        .iter()
        .find(|c| c.holder == eid(1))
        .expect("self holds it");
    assert_eq!(own.grade, Provenance::Witnessed);
    assert_eq!(own.hops, 0);
}

#[test]
fn descendants_inherit_it_taught_with_growing_hops() {
    let led = world();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let two = claims
        .iter()
        .find(|c| c.holder == eid(2))
        .expect("2 holds it");
    let three = claims
        .iter()
        .find(|c| c.holder == eid(3))
        .expect("3 holds it");
    assert_eq!(two.grade, Provenance::Taught);
    assert_eq!(two.hops, 1);
    assert_eq!(three.grade, Provenance::Taught);
    assert_eq!(three.hops, 2);
}

#[test]
fn content_is_carried_unchanged() {
    let led = world();
    let lin = lineage_of(&led);
    for c in claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED) {
        assert_eq!(c.object, Value::Number(100.0), "distortion is campaign 2");
        assert_eq!(c.subject, eid(1));
    }
}

#[test]
fn an_unrelated_lineage_holds_nothing_about_it() {
    let led = world();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let holders: Vec<_> = claims.iter().map(|c| c.holder).collect();
    // The real lineage (1's own event, inherited by its descendants 2 and 3)
    // is present...
    assert!(holders.contains(&eid(1)));
    assert!(holders.contains(&eid(2)));
    assert!(holders.contains(&eid(3)));
    // ...and the second, genuinely unrelated lineage rooted at 10 is not —
    // 10 and 11 are real, committed entities that never descend from 1.
    assert!(!holders.contains(&eid(10)));
    assert!(!holders.contains(&eid(11)));
}

/// A raid: village 1 (founded day 0) is ended on day 100 by village 50.
/// Survivors found 2 and 3 on day 100 — they were there. Village 4 was
/// founded from 1 back on day 10 and was elsewhere when it happened.
/// Village 51 is founded from the attacker 50, later.
fn raid() -> Ledger {
    let mut led = ledger_with(&[
        (1, None),
        (50, None),
        (2, Some(1)),
        (3, Some(1)),
        (4, Some(1)),
        (51, Some(50)),
    ]);
    for (occ, day) in [
        (1, 0.0),
        (50, 0.0),
        (2, 100.0),
        (3, 100.0),
        (4, 10.0),
        (51, 200.0),
    ] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_FOUNDED,
            Value::Number(day),
        );
    }
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(100.0),
    );
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED_BY,
        Value::Entity(eid(50)),
    );
    led
}

#[test]
fn the_victim_the_survivors_and_the_attacker_all_witnessed_it() {
    let led = raid();
    let lin = lineage_of(&led);
    let w = witnesses_of(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    assert_eq!(w, vec![eid(1), eid(2), eid(3), eid(50)]);
}

#[test]
fn a_child_founded_before_the_ending_was_elsewhere() {
    let led = raid();
    let lin = lineage_of(&led);
    let w = witnesses_of(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    assert!(
        !w.contains(&eid(4)),
        "4 was founded on day 10, not at the ending"
    );
}

#[test]
fn a_survivor_is_a_witness_not_an_inheritor() {
    // The trap: 2 is a DESCENDANT of 1 and would otherwise inherit at hops 1.
    // It saw the raid. Witness wins over inheritance.
    let led = raid();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let two = claims
        .iter()
        .find(|c| c.holder == eid(2))
        .expect("2 holds it");
    assert_eq!(two.grade, Provenance::Witnessed);
    assert_eq!(two.hops, 0);
}

#[test]
fn the_ordinary_child_inherits_at_one_hop() {
    let led = raid();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let four = claims
        .iter()
        .find(|c| c.holder == eid(4))
        .expect("4 holds it");
    assert_eq!(four.grade, Provenance::Taught);
    assert_eq!(four.hops, 1);
}

#[test]
fn the_attackers_line_holds_it_too() {
    let led = raid();
    let lin = lineage_of(&led);
    let claims = claims_about(&led, &lin, eid(1), hornvale_history::OCC_ENDED);
    let fifty = claims
        .iter()
        .find(|c| c.holder == eid(50))
        .expect("50 holds it");
    let fifty_one = claims
        .iter()
        .find(|c| c.holder == eid(51))
        .expect("51 holds it");
    assert_eq!(fifty.grade, Provenance::Witnessed);
    assert_eq!(fifty.hops, 0);
    assert_eq!(fifty_one.grade, Provenance::Taught);
    assert_eq!(fifty_one.hops, 1);
}

#[test]
fn a_non_ending_predicate_has_only_its_subject_as_witness() {
    let led = raid();
    let lin = lineage_of(&led);
    let w = witnesses_of(&led, &lin, eid(1), hornvale_history::OCC_FOUNDED);
    assert_eq!(w, vec![eid(1)], "only an ending has other parties");
}
