//! Hand-built ledgers for the hearsay tests: these are milliseconds and pin
//! the shape exactly.
//!
//! Shared by all three test files (Tasks 4 and 5 declare `mod common;` rather
//! than copying it); not every helper is used by every consuming test
//! binary, so this module allows dead code the same way
//! `windows/vessel/tests/common/mod.rs` does.
#![allow(dead_code)]

use hornvale_kernel::ledger::{EntityId, Fact, Ledger, Value};
use hornvale_kernel::registry::ConceptRegistry;

/// `EntityId` from a small integer.
pub fn eid(n: u64) -> EntityId {
    EntityId::new(n).expect("nonzero")
}

/// A ledger holding one `occ-founded-from` per entry: `Some(parent)` is a
/// `Founding::From` edge, `None` is a `Founding::Genesis` root (Number-valued,
/// a site id — NOT an ancestor).
pub fn ledger_with(chain: &[(u64, Option<u64>)]) -> Ledger {
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(hornvale_history::OCC_FOUNDED_FROM, true, "founding")
        .expect("register");
    let mut led = Ledger::default();
    for (child, parent) in chain {
        let object = match parent {
            Some(p) => Value::Entity(eid(*p)),
            None => Value::Number(7449.0),
        };
        led.commit(
            Fact {
                subject: eid(*child),
                predicate: hornvale_history::OCC_FOUNDED_FROM.to_string(),
                object,
                place: None,
                day: None,
                provenance: "test".to_string(),
            },
            &reg,
        )
        .expect("commit");
    }
    led
}

/// Commit one extra fact onto an existing ledger, registering its predicate.
pub fn put(led: &mut Ledger, subject: u64, predicate: &str, object: Value) {
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(predicate, true, "test predicate")
        .expect("register");
    led.commit(
        Fact {
            subject: eid(subject),
            predicate: predicate.to_string(),
            object,
            place: None,
            day: None,
            provenance: "test".to_string(),
        },
        &reg,
    )
    .expect("commit");
}

/// Commit one extra fact onto an existing ledger, registering its predicate
/// as NON-FUNCTIONAL — unlike [`put`], which registers functional. Several
/// facts (e.g. one `moon-period-std` per moon) can then land on one subject
/// without the registry rejecting the second as a contradiction.
pub fn put_on(led: &mut Ledger, subject: u64, predicate: &str, object: Value) {
    let mut reg = ConceptRegistry::default();
    reg.register_predicate(predicate, false, "test predicate")
        .expect("register");
    led.commit(
        Fact {
            subject: eid(subject),
            predicate: predicate.to_string(),
            object,
            place: None,
            day: None,
            provenance: "test".to_string(),
        },
        &reg,
    )
    .expect("commit");
}

/// A single descent chain of six occupations (1 -> 2 -> 3 -> 4 -> 5 -> 6),
/// each `occ-people` `"human"`, with widening (roughly doubling)
/// `occ-founded` gaps, an `occ-ended` on the root, and a real sky (day
/// length, one moon, a year — the same trio `tests/derive.rs`'s stance-
/// boundary tests commit on entity 9) so a [`hornvale_hearsay::ladder::PrecisionLadder`]
/// has astronomical rungs regardless of which social durations a caller
/// supplies on top.
///
/// Built for Task 5's accumulating derivation, which needs founding days and
/// a people on every occupation to produce any generational span at all.
/// `ledger_with` alone cannot serve it: it commits only `occ-founded-from`,
/// so [`hornvale_hearsay::amplitude::gen_span`] would read no founding day
/// anywhere and return `0.0` on every step, leaving every claim at
/// `Precision::FINEST` no matter what the accumulation rule does with it.
pub fn chain_with_foundings() -> Ledger {
    let mut led = ledger_with(&[
        (1, None),
        (2, Some(1)),
        (3, Some(2)),
        (4, Some(3)),
        (5, Some(4)),
        (6, Some(5)),
    ]);
    for (occ, day) in [
        (1, 0.0),
        (2, 200.0),
        (3, 600.0),
        (4, 1400.0),
        (5, 3000.0),
        (6, 6200.0),
    ] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_FOUNDED,
            Value::Number(day),
        );
    }
    for occ in [1u64, 2, 3, 4, 5, 6] {
        put(
            &mut led,
            occ,
            hornvale_history::OCC_PEOPLE,
            Value::Text("human".to_string()),
        );
    }
    put(
        &mut led,
        1,
        hornvale_history::OCC_ENDED,
        Value::Number(6300.0),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::DAY_LENGTH_STD,
        Value::Number(1.0),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::MOON_PERIOD_STD,
        Value::Number(41.7),
    );
    put_on(
        &mut led,
        9,
        hornvale_astronomy::facts::YEAR_LENGTH_STD,
        Value::Number(372.4),
    );
    led
}
