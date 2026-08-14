//! Hand-built ledgers for the hearsay tests. A world build at Settlements
//! depth is minutes; these are milliseconds and pin the shape exactly.
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
