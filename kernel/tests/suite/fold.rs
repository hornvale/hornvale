//! The incremental-fold primitive: advancing over a ledger prefix gives the
//! same answer as folding it from scratch (FOLD equals SCAN), and the position
//! guard makes a skipped or repeated fact loud rather than silent.

use hornvale_kernel::fold::{Folded, LedgerFold};
use hornvale_kernel::ledger::{EntityId, Fact, Ledger, Value};
use hornvale_kernel::registry::ConceptRegistry;

/// An ORDER-SENSITIVE test fold. `count` alone would pass even if facts were
/// absorbed in the wrong order, which is precisely the bug class this
/// primitive can have — so `rolling` mixes each fact's predicate length in a
/// way that does not commute.
#[derive(Debug, PartialEq)]
struct Probe {
    count: u64,
    rolling: u64,
}

impl LedgerFold for Probe {
    fn empty() -> Self {
        Probe {
            count: 0,
            rolling: 1,
        }
    }
    fn absorb(&mut self, fact: &Fact) {
        self.count += 1;
        self.rolling = self
            .rolling
            .wrapping_mul(31)
            .wrapping_add(fact.predicate.len() as u64);
    }
}

/// Two predicates of DIFFERENT name length, so `rolling` distinguishes their
/// order. Equal lengths would make the fold commutative and every test below
/// vacuous.
const SHORT: &str = "a";
const LONGER: &str = "abcd";

fn registry() -> ConceptRegistry {
    let mut r = ConceptRegistry::default();
    r.register_predicate(SHORT, false, "test").unwrap();
    r.register_predicate(LONGER, false, "test").unwrap();
    r
}

fn fact(predicate: &str, n: u64) -> Fact {
    Fact {
        subject: EntityId::new(n).expect("n is non-zero"),
        predicate: predicate.to_string(),
        object: Value::Flag(true),
        place: None,
        day: None,
        provenance: "test".to_string(),
    }
}

/// A ledger whose predicate sequence is deliberately mixed, so an order bug
/// moves `rolling`.
fn ledger_of(len: u64) -> Ledger {
    let reg = registry();
    let mut l = Ledger::default();
    for i in 1..=len {
        let p = if i % 3 == 0 { LONGER } else { SHORT };
        l.commit(fact(p, i), &reg).expect("a test fact commits");
    }
    l
}

#[test]
fn the_probe_fold_is_order_sensitive() {
    // Guards every other test in this file from being vacuous: a commutative
    // probe would pass them all under an ordering bug.
    let mut forward = Probe::empty();
    forward.absorb(&fact(SHORT, 1));
    forward.absorb(&fact(LONGER, 2));

    let mut backward = Probe::empty();
    backward.absorb(&fact(LONGER, 2));
    backward.absorb(&fact(SHORT, 1));

    assert_ne!(forward, backward, "the probe fold must not commute");
}

#[test]
fn advancing_in_two_steps_equals_folding_from_scratch() {
    let l = ledger_of(20);
    let scan: Folded<Probe> = Folded::rebuild(&l);

    // Advance in two bites, the way a tick does.
    let mut incremental: Folded<Probe> = Folded::new();
    let half = ledger_of(9);
    incremental.advance_to(&half);
    incremental.advance_to(&l);

    assert_eq!(incremental.state(), scan.state());
    assert_eq!(incremental.position(), scan.position());
}

#[test]
fn one_fact_at_a_time_equals_folding_from_scratch() {
    let l = ledger_of(20);
    let scan: Folded<Probe> = Folded::rebuild(&l);

    let mut one_by_one: Folded<Probe> = Folded::new();
    for (i, f) in l.iter().enumerate() {
        one_by_one.absorb_at(i as u64, f);
    }

    assert_eq!(one_by_one.state(), scan.state());
    assert_eq!(one_by_one.position(), scan.position());
}
