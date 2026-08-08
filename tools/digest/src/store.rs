//! The compacting fact store.

use hornvale_kernel::ledger::{EntityId, Fact, Value};
use hornvale_kernel::registry::ConceptRegistry;

/// Why an assertion was refused.
#[derive(Debug, PartialEq)]
pub enum StoreError {
    /// The predicate is not in the project vocabulary.
    UnknownPredicate(String),
    /// The fact carried a `place` or `day`. Project time is git's (spec §4.2).
    TimeIsGits,
}

/// Build a project fact. `place` and `day` are always `None` by construction.
pub fn fact(subject: EntityId, predicate: &str, object: Value) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: None,
        provenance: "asserted".to_string(),
    }
}

/// A compacted, time-free ledger of facts about the project.
pub struct ProjectLedger {
    registry: ConceptRegistry,
    facts: Vec<Fact>,
}

impl ProjectLedger {
    /// A ledger over the given project vocabulary.
    pub fn new(registry: ConceptRegistry) -> Self {
        Self {
            registry,
            facts: Vec::new(),
        }
    }

    /// Assert a fact. A *functional* predicate replaces any existing fact for
    /// the same `(subject, predicate)` — that replacement IS the compaction.
    /// A non-functional predicate accumulates.
    pub fn assert(&mut self, f: Fact) -> Result<(), StoreError> {
        if f.place.is_some() || f.day.is_some() {
            return Err(StoreError::TimeIsGits);
        }
        let def = self
            .registry
            .predicate(&f.predicate)
            .ok_or_else(|| StoreError::UnknownPredicate(f.predicate.clone()))?;
        if def.functional {
            self.facts
                .retain(|e| !(e.subject == f.subject && e.predicate == f.predicate));
        }
        self.facts.push(f);
        Ok(())
    }

    /// Every fact currently in force.
    pub fn facts(&self) -> &[Fact] {
        &self.facts
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::ledger::{EntityId, Value};
    use std::num::NonZeroU64;

    fn eid(n: u64) -> EntityId {
        EntityId(NonZeroU64::new(n).expect("nonzero"))
    }

    fn registry() -> hornvale_kernel::registry::ConceptRegistry {
        let mut r = hornvale_kernel::registry::ConceptRegistry::default();
        r.register_predicate("status", true, "a decision's current status")
            .expect("register");
        r.register_predicate("supersedes", false, "this decision supersedes another")
            .expect("register");
        r
    }

    #[test]
    fn asserting_a_functional_predicate_replaces_its_predecessor() {
        let mut led = ProjectLedger::new(registry());
        led.assert(fact(eid(1), "status", Value::Text("accepted".into())))
            .expect("first assert");
        led.assert(fact(eid(1), "status", Value::Text("superseded".into())))
            .expect("second assert");
        assert_eq!(led.facts().len(), 1, "functional predicate must compact");
        assert_eq!(
            led.facts()[0].object,
            Value::Text("superseded".into()),
            "the later assertion wins"
        );
    }

    #[test]
    fn a_non_functional_predicate_accumulates() {
        let mut led = ProjectLedger::new(registry());
        led.assert(fact(eid(1), "supersedes", Value::Text("0026".into())))
            .expect("a");
        led.assert(fact(eid(1), "supersedes", Value::Text("0043".into())))
            .expect("b");
        assert_eq!(led.facts().len(), 2, "non-functional predicates accumulate");
    }

    #[test]
    fn an_unregistered_predicate_is_rejected() {
        let mut led = ProjectLedger::new(registry());
        let err = led
            .assert(fact(eid(1), "invented", Value::Flag(true)))
            .expect_err("unregistered predicate must be rejected");
        assert!(matches!(err, StoreError::UnknownPredicate(_)));
    }

    #[test]
    fn a_fact_carrying_time_is_rejected() {
        let mut led = ProjectLedger::new(registry());
        let mut f = fact(eid(1), "status", Value::Text("accepted".into()));
        f.day = Some(1.0);
        let err = led.assert(f).expect_err("time must be rejected");
        assert!(matches!(err, StoreError::TimeIsGits));
    }
}
