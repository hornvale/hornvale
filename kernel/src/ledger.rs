//! The fact ledger: the append-only posterior. Once committed, a fact is
//! true forever (spec §3.1, §3.3). The envelope is deliberately dumb;
//! predicates carry meaning via the concept registry.

use crate::registry::ConceptRegistry;
use serde::{Deserialize, Serialize};

/// Opaque entity handle. Minted by the ledger, never reused.
/// type-audit: bare-ok(constructor-edge)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct EntityId(pub u64);

/// The stable identity of a *kind* — the authored label a kind is known by
/// ("red-dragon", "kobold"). A kind's identity is its label, never its
/// position in any registry (decision 0015: a name is its own key). When a
/// kind is referenced in the ledger it is referenced by this label (a
/// `Value::Text`); a deliberate change to a kind's authored traits that must
/// not alias the old kind takes an epoch suffix ("red-dragon/v2"), never a
/// rename. Build-state: never serialized — the label enters the save as
/// `Value::Text`, not as a `KindId`.
/// type-audit: bare-ok(identifier-text)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KindId(pub &'static str);

/// A fact's object. `Number` values are quantized to a platform-stable
/// canonical form at commit (see the `quantize` module), so bitwise-exact
/// f64 equality is meaningful across platforms — the ledger's serialized
/// bytes are identical on macOS and Linux for the same seed.
/// type-audit: bare-ok(envelope)
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Value {
    /// A reference to another entity.
    Entity(EntityId),
    /// Free-form text.
    Text(String),
    /// A finite floating-point number. Non-finite values are rejected at
    /// ledger check time.
    Number(f64),
    /// A boolean flag.
    Flag(bool),
}

/// The dumb envelope (spec §3.1.6): subject, predicate, object, place,
/// time, provenance. Semantics live in the concept registry.
/// type-audit: bare-ok(envelope: predicate), waiver(decision-0014: day), bare-ok(prose: provenance)
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Fact {
    /// The entity this fact is about.
    pub subject: EntityId,
    /// The predicate name, resolved against the concept registry.
    pub predicate: String,
    /// The value asserted for (subject, predicate).
    pub object: Value,
    /// The entity where this fact was observed, if location-bound.
    pub place: Option<EntityId>,
    /// The simulated day this fact was observed, if time-bound.
    pub day: Option<f64>,
    /// Free-form description of what produced this fact.
    pub provenance: String,
}

/// Ledger validation error.
/// type-audit: bare-ok(identifier-text)
#[derive(Debug)]
pub enum LedgerError {
    /// The predicate is not registered in the concept registry.
    UnknownPredicate {
        /// The unrecognized predicate name.
        predicate: String,
    },
    /// A functional predicate already holds a different value for this subject.
    Contradiction {
        /// The entity holding the conflicting value.
        subject: EntityId,
        /// The functional predicate in conflict.
        predicate: String,
    },
    /// A fact's object or day was a non-finite f64 (NaN or infinity).
    NonFiniteNumber {
        /// The entity the offending fact is about.
        subject: EntityId,
        /// The predicate the offending fact was committed under.
        predicate: String,
    },
}

impl std::fmt::Display for LedgerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LedgerError::UnknownPredicate { predicate } => {
                write!(f, "predicate '{predicate}' is not in the concept registry")
            }
            LedgerError::Contradiction { subject, predicate } => write!(
                f,
                "contradiction: entity {} already holds a different '{predicate}'",
                subject.0
            ),
            LedgerError::NonFiniteNumber { subject, predicate } => write!(
                f,
                "entity {} '{predicate}': non-finite numbers cannot be committed",
                subject.0
            ),
        }
    }
}

impl std::error::Error for LedgerError {}

/// Append-only fact store. Facts are never mutated or removed.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Ledger {
    facts: Vec<Fact>,
    next_entity: u64,
    /// Derived permutation indexes — never serialized; rebuilt on first use
    /// after load, maintained incrementally on commit. Absent-or-complete.
    #[serde(skip)]
    index: Option<crate::fact_index::FactIndex>,
}

impl Ledger {
    /// Mint a fresh entity id. Ids start at 1; 0 is reserved as "never valid".
    pub fn mint_entity(&mut self) -> EntityId {
        self.next_entity += 1;
        EntityId(self.next_entity)
    }

    /// Ensure the derived index exists and is current (rebuild-if-absent).
    fn ensure_index(&mut self) {
        if self.index.is_none() {
            let mut idx = crate::fact_index::FactIndex::default();
            idx.rebuild(&self.facts);
            self.index = Some(idx);
        }
    }

    // --- naive reference impls: the O(n) truth the index refines. Kept for the
    // INDEX≡SCAN property test and the heavy-tier before/after micro-bench.
    pub(crate) fn naive_has_conflict(&self, fact: &Fact) -> bool {
        self.facts.iter().any(|f| {
            f.subject == fact.subject && f.predicate == fact.predicate && f.object != fact.object
        })
    }
    pub(crate) fn naive_contains(&self, fact: &Fact) -> bool {
        self.facts.contains(fact)
    }
    pub(crate) fn naive_facts_about(&self, subject: EntityId) -> Vec<usize> {
        (0..self.facts.len())
            .filter(|&p| self.facts[p].subject == subject)
            .collect()
    }
    /// Position accessor for tests/benches (the naive refs return positions).
    #[cfg(test)]
    pub(crate) fn fact_at(&self, pos: usize) -> &Fact {
        &self.facts[pos]
    }
    pub(crate) fn naive_find(&self, predicate: &str) -> Vec<usize> {
        (0..self.facts.len())
            .filter(|&p| self.facts[p].predicate == predicate)
            .collect()
    }
    pub(crate) fn naive_value_of(&self, subject: EntityId, predicate: &str) -> Option<&Value> {
        self.facts
            .iter()
            .find(|f| f.subject == subject && f.predicate == predicate)
            .map(|f| &f.object)
    }
    pub(crate) fn naive_query_by_object(&self, object: &Value) -> Vec<usize> {
        (0..self.facts.len())
            .filter(|&p| &self.facts[p].object == object)
            .collect()
    }

    /// Would this fact be accepted? Used by the refinement engine to test
    /// candidates without committing.
    pub fn check(&self, fact: &Fact, registry: &ConceptRegistry) -> Result<(), LedgerError> {
        let def =
            registry
                .predicate(&fact.predicate)
                .ok_or_else(|| LedgerError::UnknownPredicate {
                    predicate: fact.predicate.clone(),
                })?;
        let object_is_non_finite = matches!(fact.object, Value::Number(n) if !n.is_finite());
        let day_is_non_finite = matches!(fact.day, Some(d) if !d.is_finite());
        if object_is_non_finite || day_is_non_finite {
            return Err(LedgerError::NonFiniteNumber {
                subject: fact.subject,
                predicate: fact.predicate.clone(),
            });
        }
        if def.functional {
            let clash = match &self.index {
                Some(idx) => idx.has_conflicting_object(fact, &self.facts),
                None => self.naive_has_conflict(fact),
            };
            if clash {
                return Err(LedgerError::Contradiction {
                    subject: fact.subject,
                    predicate: fact.predicate.clone(),
                });
            }
        }
        Ok(())
    }

    /// Commit a fact. Ok(true) = appended; Ok(false) = identical fact
    /// already present (idempotent no-op).
    /// type-audit: bare-ok(flag)
    pub fn commit(
        &mut self,
        mut fact: Fact,
        registry: &ConceptRegistry,
    ) -> Result<bool, LedgerError> {
        // Canonicalize numeric objects and days to a platform-stable form
        // *before* the idempotency and contradiction checks, so dedup compares
        // canonical values (see the `quantize` module: last-ULP libm divergence
        // between platforms otherwise reaches the serialized ledger and breaks
        // cross-platform byte-identity). Integer-valued facts (cell ids,
        // populations, counts) are unaffected — they quantize to themselves.
        if let Value::Number(n) = fact.object {
            fact.object = Value::Number(crate::quantize::quantize(n));
        }
        fact.day = fact.day.map(crate::quantize::quantize);
        self.ensure_index(); // fast contradiction/dedup for the rest of this build
        self.check(&fact, registry)?;
        let dup = match &self.index {
            Some(idx) => idx.contains_full(&fact, &self.facts),
            None => self.naive_contains(&fact),
        };
        if dup {
            return Ok(false);
        }
        let pos = self.facts.len();
        if let Some(idx) = self.index.as_mut() {
            idx.insert(pos, &fact);
        }
        self.facts.push(fact);
        Ok(true)
    }

    /// All facts with this subject.
    pub fn facts_about(&self, subject: EntityId) -> impl Iterator<Item = &Fact> {
        let positions = match &self.index {
            Some(idx) => idx.positions_for_subject(subject),
            None => self.naive_facts_about(subject),
        };
        positions.into_iter().map(move |p| &self.facts[p])
    }

    /// All facts with this predicate.
    /// type-audit: bare-ok(identifier-text)
    pub fn find(&self, predicate: &str) -> impl Iterator<Item = &Fact> {
        let positions = match &self.index {
            Some(idx) => idx.positions_for_predicate(predicate),
            None => self.naive_find(predicate),
        };
        positions.into_iter().map(move |p| &self.facts[p])
    }

    /// First object for (subject, predicate). For functional predicates
    /// this is the unique value.
    /// type-audit: bare-ok(identifier-text)
    pub fn value_of(&self, subject: EntityId, predicate: &str) -> Option<&Value> {
        match &self.index {
            Some(idx) => {
                // first fact (commit order) for (subject, predicate)
                let first = idx
                    .positions_for_subject(subject)
                    .into_iter()
                    .find(|&p| self.facts[p].predicate == predicate);
                first.map(|p| &self.facts[p].object)
            }
            None => self.naive_value_of(subject, predicate),
        }
    }

    /// All facts whose object equals `object`, in commit order (the O-shape
    /// query the flat ledger could not answer). O(log n + k) via the OSP index.
    pub fn query_by_object(&self, object: &Value) -> impl Iterator<Item = &Fact> {
        let positions = match &self.index {
            Some(idx) => idx.positions_for_object(object),
            None => self.naive_query_by_object(object),
        };
        positions.into_iter().map(move |p| &self.facts[p])
    }

    /// The text value of (subject, predicate), if present and textual.
    /// type-audit: bare-ok(identifier-text: predicate), bare-ok(envelope: return)
    pub fn text_of(&self, subject: EntityId, predicate: &str) -> Option<&str> {
        match self.value_of(subject, predicate) {
            Some(Value::Text(t)) => Some(t.as_str()),
            _ => None,
        }
    }

    /// Number of facts in the ledger.
    /// type-audit: bare-ok(count)
    pub fn len(&self) -> usize {
        self.facts.len()
    }

    /// True if the ledger is empty.
    /// type-audit: bare-ok(flag)
    pub fn is_empty(&self) -> bool {
        self.facts.is_empty()
    }

    /// The maximum entity id referenced by any fact (subjects, `Value::Entity`
    /// objects, and `place` fields), or 0 if the ledger is empty.
    /// type-audit: pending(wave-1)
    pub fn max_entity_id(&self) -> u64 {
        self.facts
            .iter()
            .flat_map(|f| {
                let object_id = match f.object {
                    Value::Entity(e) => Some(e.0),
                    _ => None,
                };
                [Some(f.subject.0), object_id, f.place.map(|p| p.0)]
            })
            .flatten()
            .max()
            .unwrap_or(0)
    }

    /// Valid when no future mint can collide with an entity id already
    /// referenced in a fact.
    /// type-audit: bare-ok(flag)
    pub fn minting_is_valid(&self) -> bool {
        self.next_entity >= self.max_entity_id()
    }

    /// Iterate over every committed fact, in commit order.
    pub fn iter(&self) -> impl Iterator<Item = &Fact> {
        self.facts.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn registry() -> ConceptRegistry {
        let mut r = ConceptRegistry::default();
        r.register_predicate("name", true, "canonical name")
            .unwrap();
        r.register_predicate("located-in", false, "spatial containment")
            .unwrap();
        r
    }

    fn named(ledger: &mut Ledger, name: &str) -> Fact {
        let e = ledger.mint_entity();
        Fact {
            subject: e,
            predicate: "name".to_string(),
            object: Value::Text(name.to_string()),
            place: None,
            day: None,
            provenance: "test".to_string(),
        }
    }

    #[test]
    fn mint_entity_yields_distinct_ids() {
        let mut l = Ledger::default();
        assert_ne!(l.mint_entity(), l.mint_entity());
    }

    #[test]
    fn commit_and_query_roundtrip() {
        let r = registry();
        let mut l = Ledger::default();
        let f = named(&mut l, "Zaggrak");
        let subject = f.subject;
        assert!(l.commit(f, &r).unwrap());
        assert_eq!(
            l.value_of(subject, "name"),
            Some(&Value::Text("Zaggrak".to_string()))
        );
        assert_eq!(l.facts_about(subject).count(), 1);
        assert_eq!(l.find("name").count(), 1);
    }

    #[test]
    fn unknown_predicate_is_rejected() {
        let r = registry();
        let mut l = Ledger::default();
        let e = l.mint_entity();
        let f = Fact {
            subject: e,
            predicate: "unregistered".to_string(),
            object: Value::Flag(true),
            place: None,
            day: None,
            provenance: "test".to_string(),
        };
        assert!(matches!(
            l.commit(f, &r),
            Err(LedgerError::UnknownPredicate { .. })
        ));
    }

    #[test]
    fn functional_contradiction_is_rejected() {
        let r = registry();
        let mut l = Ledger::default();
        let f = named(&mut l, "Zaggrak");
        let subject = f.subject;
        l.commit(f, &r).unwrap();
        let contradiction = Fact {
            subject,
            predicate: "name".to_string(),
            object: Value::Text("Bolnar".to_string()),
            place: None,
            day: None,
            provenance: "test".to_string(),
        };
        assert!(matches!(
            l.commit(contradiction, &r),
            Err(LedgerError::Contradiction { .. })
        ));
    }

    #[test]
    fn identical_recommit_is_idempotent() {
        let r = registry();
        let mut l = Ledger::default();
        let f = named(&mut l, "Zaggrak");
        l.commit(f.clone(), &r).unwrap();
        assert!(!l.commit(f, &r).unwrap());
        assert_eq!(l.len(), 1);
    }

    #[test]
    fn non_functional_predicate_allows_multiple_objects() {
        let r = registry();
        let mut l = Ledger::default();
        let village = l.mint_entity();
        let vale = l.mint_entity();
        let forest = l.mint_entity();
        for container in [vale, forest] {
            l.commit(
                Fact {
                    subject: village,
                    predicate: "located-in".to_string(),
                    object: Value::Entity(container),
                    place: None,
                    day: None,
                    provenance: "test".to_string(),
                },
                &r,
            )
            .unwrap();
        }
        assert_eq!(l.facts_about(village).count(), 2);
    }

    #[test]
    fn non_finite_number_object_is_rejected() {
        let r = registry();
        let mut l = Ledger::default();
        let e = l.mint_entity();
        let f = Fact {
            subject: e,
            predicate: "name".to_string(),
            object: Value::Number(f64::NAN),
            place: None,
            day: None,
            provenance: "test".to_string(),
        };
        assert!(matches!(
            l.commit(f, &r),
            Err(LedgerError::NonFiniteNumber { .. })
        ));
    }

    #[test]
    fn non_finite_day_is_rejected() {
        let r = registry();
        let mut l = Ledger::default();
        let e = l.mint_entity();
        let f = Fact {
            subject: e,
            predicate: "name".to_string(),
            object: Value::Text("Zaggrak".to_string()),
            place: None,
            day: Some(f64::INFINITY),
            provenance: "test".to_string(),
        };
        assert!(matches!(
            l.commit(f, &r),
            Err(LedgerError::NonFiniteNumber { .. })
        ));
    }

    #[test]
    fn committed_numbers_and_days_are_quantized() {
        use crate::quantize::quantize;
        let r = registry();
        let mut l = Ledger::default();
        let e = l.mint_entity();
        let raw = 210.2242156495795_f64;
        l.commit(
            Fact {
                subject: e,
                predicate: "name".to_string(),
                object: Value::Number(raw),
                place: None,
                day: Some(raw),
                provenance: "test".to_string(),
            },
            &r,
        )
        .unwrap();
        let stored = l.iter().next().unwrap();
        assert_eq!(stored.object, Value::Number(quantize(raw)));
        assert_eq!(stored.day, Some(quantize(raw)));
        assert_ne!(
            stored.object,
            Value::Number(raw),
            "raw value must not survive"
        );
    }

    #[test]
    fn finite_numbers_still_commit() {
        let r = registry();
        let mut l = Ledger::default();
        let e = l.mint_entity();
        let f = Fact {
            subject: e,
            predicate: "name".to_string(),
            object: Value::Number(42.5),
            place: None,
            day: Some(3.0),
            provenance: "test".to_string(),
        };
        assert!(l.commit(f, &r).unwrap());
    }

    #[test]
    fn ledger_serializes_roundtrip_including_minting_state() {
        let r = registry();
        let mut l = Ledger::default();
        let f = named(&mut l, "Zaggrak");
        l.commit(f, &r).unwrap();
        let json = serde_json::to_string(&l).unwrap();
        let mut l2: Ledger = serde_json::from_str(&json).unwrap();
        assert_eq!(l2.len(), 1);
        // Minting must resume without colliding with existing entities.
        let fresh = l2.mint_entity();
        assert!(l2.facts_about(fresh).count() == 0);
        assert!(fresh.0 > 1);
    }

    #[test]
    fn text_of_returns_text_values_only() {
        let r = registry();
        let mut l = Ledger::default();
        let f = named(&mut l, "Zaggrak");
        let subject = f.subject;
        l.commit(f, &r).unwrap();
        assert_eq!(l.text_of(subject, "name"), Some("Zaggrak"));
        assert_eq!(l.text_of(subject, "located-in"), None);
    }

    #[test]
    fn iter_yields_facts_in_commit_order() {
        let r = registry();
        let mut l = Ledger::default();
        let a = named(&mut l, "Zaggrak");
        let b = named(&mut l, "Bolnar");
        l.commit(a, &r).unwrap();
        l.commit(b, &r).unwrap();
        let names: Vec<&Value> = l.iter().map(|f| &f.object).collect();
        assert_eq!(
            names,
            vec![
                &Value::Text("Zaggrak".to_string()),
                &Value::Text("Bolnar".to_string())
            ]
        );
    }

    #[test]
    fn kind_id_orders_by_label() {
        use crate::KindId;
        let mut ids = [KindId("kobold"), KindId("goblin"), KindId("bugbear")];
        ids.sort();
        assert_eq!(ids, [KindId("bugbear"), KindId("goblin"), KindId("kobold")]);
        assert_eq!(ids[0].0, "bugbear");
    }

    #[test]
    fn index_backed_commit_matches_naive_semantics() {
        // Idempotent recommit, functional contradiction, and non-functional
        // multi-object all behave exactly as the pre-index ledger did.
        let r = registry();
        let mut l = Ledger::default();
        let f = named(&mut l, "Zaggrak");
        let s = f.subject;
        assert!(l.commit(f.clone(), &r).unwrap()); // appended
        assert!(!l.commit(f, &r).unwrap()); // idempotent no-op
        assert_eq!(l.len(), 1);
        let clash = Fact {
            subject: s,
            predicate: "name".into(),
            object: Value::Text("Bolnar".into()),
            place: None,
            day: None,
            provenance: "t".into(),
        };
        assert!(matches!(
            l.commit(clash, &r),
            Err(LedgerError::Contradiction { .. })
        ));
    }

    #[test]
    fn facts_about_yields_commit_order_not_index_key_order() {
        // Commit-order preservation is a determinism contract: facts_about must
        // yield facts in ascending commit position, NOT in the (predicate, object)
        // key order the SPO index iterates. Construct a case where the two differ:
        // "located-in" interns first (symbol 0), "name" second (symbol 1); the two
        // located-in objects are committed high-id-then-low-id. Index-key order is
        // [C(low obj), A(high obj), B(name)]; commit order is [A, B, C]. This test
        // fails if positions_for_subject's sort is dropped.
        let r = registry();
        let mut l = Ledger::default();
        let s = l.mint_entity();
        let low = l.mint_entity(); // id 2
        let high = l.mint_entity(); // id 3, so ObjKey(Entity(low)) < ObjKey(Entity(high))
        let commit = |l: &mut Ledger, pred: &str, obj: Value| {
            l.commit(
                Fact {
                    subject: s,
                    predicate: pred.to_string(),
                    object: obj,
                    place: None,
                    day: None,
                    provenance: "t".into(),
                },
                &r,
            )
            .unwrap();
        };
        commit(&mut l, "located-in", Value::Entity(high)); // A, pos 0
        commit(&mut l, "name", Value::Text("Zaggrak".into())); // B, pos 1 (name is functional, one value)
        commit(&mut l, "located-in", Value::Entity(low)); // C, pos 2
        let objs: Vec<&Value> = l.facts_about(s).map(|f| &f.object).collect();
        assert_eq!(
            objs,
            vec![
                &Value::Entity(high),
                &Value::Text("Zaggrak".into()),
                &Value::Entity(low),
            ],
            "facts_about must yield commit order [A, B, C], not index-key order [C, A, B]"
        );
    }

    // Tiny deterministic PRNG — no dep (splitmix64). Same "roll our own" style as
    // the astronomy property batteries.
    fn splitmix(state: &mut u64) -> u64 {
        *state = state.wrapping_add(0x9E3779B97F4A7C15);
        let mut z = *state;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D049BB133111EB);
        z ^ (z >> 31)
    }

    fn random_ledger(seed: u64, n: usize) -> (Ledger, ConceptRegistry, Vec<EntityId>) {
        let r = registry(); // predicates: "name" (functional), "located-in" (non-functional)
        let mut l = Ledger::default();
        let subjects: Vec<EntityId> = (0..8).map(|_| l.mint_entity()).collect();
        let mut st = seed.wrapping_add(1);
        for _ in 0..n {
            let s = subjects[(splitmix(&mut st) as usize) % subjects.len()];
            // only use the non-functional predicate for bulk facts, so random
            // objects never trip the functional-contradiction reject
            let target = subjects[(splitmix(&mut st) as usize) % subjects.len()];
            let _ = l.commit(
                Fact {
                    subject: s,
                    predicate: "located-in".into(),
                    object: Value::Entity(target),
                    place: None,
                    day: None,
                    provenance: "t".into(),
                },
                &r,
            );
        }
        (l, r, subjects)
    }

    #[test]
    fn index_equals_scan_subject_and_predicate() {
        for seed in 0..64u64 {
            let (l, _r, subjects) = random_ledger(seed, 200);
            // S-shape: facts_about == naive scan (same facts, same commit order)
            for &s in &subjects {
                let idx: Vec<&Fact> = l.facts_about(s).collect();
                let scan: Vec<&Fact> = l
                    .naive_facts_about(s)
                    .iter()
                    .map(|&p| l.fact_at(p))
                    .collect();
                assert_eq!(idx, scan, "facts_about seed {seed} subj {s:?}");
                // value_of == naive_value_of for the same subject/predicate
                assert_eq!(
                    l.value_of(s, "located-in"),
                    l.naive_value_of(s, "located-in"),
                    "value_of seed {seed} subj {s:?}"
                );
            }
            // P-shape: find == naive scan
            let idx: Vec<&Fact> = l.find("located-in").collect();
            let scan: Vec<&Fact> = l
                .naive_find("located-in")
                .iter()
                .map(|&p| l.fact_at(p))
                .collect();
            assert_eq!(idx, scan, "find seed {seed}");
        }
    }

    #[test]
    fn index_equals_scan_object() {
        for seed in 0..64u64 {
            let (l, _r, subjects) = random_ledger(seed, 200);
            for &s in &subjects {
                let obj = Value::Entity(s);
                let idx: Vec<&Fact> = l.query_by_object(&obj).collect();
                let scan: Vec<&Fact> = l
                    .naive_query_by_object(&obj)
                    .iter()
                    .map(|&p| l.fact_at(p))
                    .collect();
                assert_eq!(idx, scan, "query_by_object seed {seed} obj {obj:?}");
            }
        }
    }

    #[test]
    fn query_by_object_finds_committed_facts() {
        let r = registry();
        let mut l = Ledger::default();
        let a = l.mint_entity();
        let hub = l.mint_entity();
        l.commit(
            Fact {
                subject: a,
                predicate: "located-in".into(),
                object: Value::Entity(hub),
                place: None,
                day: None,
                provenance: "t".into(),
            },
            &r,
        )
        .unwrap();
        let found: Vec<EntityId> = l
            .query_by_object(&Value::Entity(hub))
            .map(|f| f.subject)
            .collect();
        assert_eq!(found, vec![a]);
    }

    #[test]
    fn symbol_is_four_bytes() {
        // The interning space contract: a predicate key is a u32, not a String.
        assert_eq!(std::mem::size_of::<crate::fact_index::Symbol>(), 4);
    }

    #[test]
    fn index_is_absent_until_first_use_then_complete() {
        // The lifecycle invariant: a freshly-deserialized ledger has no index;
        // a query over it still returns the right answers (naive fallback), and a
        // commit builds it. (Byte-identity of the serialized form is covered by
        // ledger_serializes_roundtrip_including_minting_state.)
        let r = registry();
        let mut l = Ledger::default();
        let f = named(&mut l, "Zaggrak");
        let s = f.subject;
        l.commit(f, &r).unwrap();
        let json = serde_json::to_string(&l).unwrap();
        let l2: Ledger = serde_json::from_str(&json).unwrap();
        // index skipped on the wire => rebuilt-on-use; answers match
        assert_eq!(l2.facts_about(s).count(), 1);
        assert_eq!(l2.value_of(s, "name"), Some(&Value::Text("Zaggrak".into())));
    }

    // This is a wall-time micro-bench of `Ledger::commit`'s scaling, not a
    // live-worldgen battery — but `cli/tests/heavy_tier.rs` requires every
    // `#[ignore]` reason containing "heavy:" to be the one verbatim
    // canonical string (checked by `heavy_tier_reason_strings_are_canonical`),
    // so it shares that string with the other heavy-tier deferrals: both are
    // deferred from the commit gate to `make gate-full` for the same reason
    // (too slow to run every commit).
    #[test]
    #[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
    // Wall-clock time is banned everywhere the sim computes (decision 0001):
    // world time is `WorldTime`, never `Instant`. This test measures the
    // *build's* wall-clock cost of commits, off the sim compute path and
    // never serialized/gated — a justified, scoped exception.
    #[allow(clippy::disallowed_types)]
    fn bench_commit_scaling_before_vs_after_index() {
        use std::hint::black_box;
        use std::time::Instant;
        let r = registry();
        for n in [1_000usize, 5_000, 20_000] {
            // AFTER: index-backed commit (each commit maintains the index).
            let mut l = Ledger::default();
            let subj = l.mint_entity();
            let start = Instant::now();
            for i in 0..n {
                let target = l.mint_entity();
                let _ = black_box(l.commit(
                    Fact {
                        subject: subj,
                        predicate: "located-in".into(),
                        object: Value::Entity(target),
                        place: None,
                        day: None,
                        provenance: "b".into(),
                    },
                    &r,
                ));
                let _ = i;
            }
            let after = start.elapsed();
            // BEFORE (reference): the naive O(n) contradiction+dedup scans over the
            // same facts, showing the quadratic the index removes.
            let facts: Vec<Fact> = l.iter().cloned().collect();
            let scan_start = Instant::now();
            let probe = Ledger::default();
            for f in &facts {
                let _ = black_box(probe.naive_has_conflict(f) || probe.naive_contains(f));
                // (probe is not mutated; this measures the scan cost per fact)
            }
            let before = scan_start.elapsed();
            eprintln!(
                "n={n:>6}  after(indexed commit)={after:?}  before(naive scans/one pass)={before:?}"
            );
        }
    }
}
