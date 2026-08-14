mod common;

use common::{eid, ledger_with, put};
use hornvale_hearsay::derive::claims_about;
use hornvale_hearsay::lineage::lineage_of;
use hornvale_kernel::ledger::{Ledger, Value};
use hornvale_kernel::provenance::Provenance;

/// 1 (root) ends on day 100; 2 founded from 1; 3 founded from 2.
fn world() -> Ledger {
    let mut led = ledger_with(&[(1, None), (2, Some(1)), (3, Some(2))]);
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
    assert!(claims.iter().all(|c| c.holder != eid(99)));
}
