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
    /// A JSONL line did not parse as a `Fact`.
    Malformed,
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

    /// Serialize as JSONL, one fact per line, stable-ordered by
    /// `(subject, predicate)`. `Fact` has no `Ord` (it holds an `f64`), so the
    /// comparator is explicit. Stable order is what keeps a single assertion a
    /// one-line diff, which is what keeps `git log -p` readable (decision 0088).
    pub fn to_jsonl(&self) -> String {
        let mut sorted: Vec<&Fact> = self.facts.iter().collect();
        sorted.sort_by(|a, b| {
            a.subject
                .cmp(&b.subject)
                .then_with(|| a.predicate.cmp(&b.predicate))
        });
        let mut out = String::new();
        for f in sorted {
            out.push_str(&serde_json::to_string(f).expect("Fact serializes"));
            out.push('\n');
        }
        out
    }

    /// Parse JSONL produced by [`ProjectLedger::to_jsonl`].
    pub fn from_jsonl(text: &str, registry: ConceptRegistry) -> Result<Self, StoreError> {
        let mut led = Self::new(registry);
        for line in text.lines().filter(|l| !l.trim().is_empty()) {
            let f: Fact = serde_json::from_str(line).map_err(|_| StoreError::Malformed)?;
            led.assert(f)?;
        }
        Ok(led)
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

    #[test]
    fn jsonl_is_stable_ordered_by_subject_then_predicate() {
        let mut led = ProjectLedger::new(registry());
        led.assert(fact(eid(2), "status", Value::Text("b".into())))
            .unwrap();
        led.assert(fact(eid(1), "supersedes", Value::Text("x".into())))
            .unwrap();
        led.assert(fact(eid(1), "status", Value::Text("a".into())))
            .unwrap();
        let text = led.to_jsonl();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 3);
        assert!(lines[0].contains("\"subject\":1") && lines[0].contains("\"status\""));
        assert!(lines[1].contains("\"subject\":1") && lines[1].contains("\"supersedes\""));
        assert!(lines[2].contains("\"subject\":2"));
    }

    #[test]
    fn replacing_one_fact_changes_exactly_one_line() {
        let mut led = ProjectLedger::new(registry());
        for n in 1..=20u64 {
            led.assert(fact(eid(n), "status", Value::Text("accepted".into())))
                .unwrap();
        }
        let before: Vec<String> = led.to_jsonl().lines().map(str::to_string).collect();

        led.assert(fact(eid(7), "status", Value::Text("superseded".into())))
            .unwrap();
        let after: Vec<String> = led.to_jsonl().lines().map(str::to_string).collect();

        assert_eq!(
            before.len(),
            after.len(),
            "compaction must not grow the file"
        );
        let changed = before.iter().zip(&after).filter(|(a, b)| a != b).count();
        assert_eq!(
            changed, 1,
            "a single assertion must be a one-line diff (S3)"
        );
    }

    #[test]
    fn jsonl_round_trips() {
        let mut led = ProjectLedger::new(registry());
        led.assert(fact(eid(1), "status", Value::Text("accepted".into())))
            .unwrap();
        let text = led.to_jsonl();
        let back = ProjectLedger::from_jsonl(&text, registry()).expect("round trip");
        assert_eq!(back.facts(), led.facts());
    }
}
