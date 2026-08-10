//! The fact ledger: the append-only posterior. Once committed, a fact is
//! true forever (spec §3.1, §3.3). The envelope is deliberately dumb;
//! predicates carry meaning via the concept registry.

use crate::registry::ConceptRegistry;
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::num::NonZeroU64;

/// Opaque entity handle. Minted by the ledger, never reused. `NonZeroU64`:
/// 0 has always been reserved as "never valid", so the niche is free and
/// `Option<EntityId>` is 8 bytes. Serializes as the bare number.
/// type-audit: bare-ok(constructor-edge)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct EntityId(pub NonZeroU64);

impl EntityId {
    /// Construct from a raw id; `None` for the reserved 0.
    /// type-audit: bare-ok(constructor-edge)
    pub const fn new(raw: u64) -> Option<EntityId> {
        match NonZeroU64::new(raw) {
            Some(n) => Some(EntityId(n)),
            None => None,
        }
    }
    /// The raw id value.
    /// type-audit: bare-ok(constructor-edge: return)
    pub const fn get(self) -> u64 {
        self.0.get()
    }
    /// Smallest valid id (1) — index range sentinel.
    pub(crate) const MIN: EntityId = EntityId(NonZeroU64::MIN);
    /// Largest valid id — index range sentinel.
    pub(crate) const MAX: EntityId = EntityId(NonZeroU64::MAX);
}

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
    /// Every id minted so far — the collision guard's memory. Rebuilt from
    /// the facts on load, so it is `#[serde(skip)]` and never widens the
    /// save format.
    #[serde(skip)]
    minted: BTreeSet<EntityId>,
}

impl Ledger {
    /// Mint an entity whose identity derives from `lineage`.
    ///
    /// # Panics
    ///
    /// If `lineage` derives an id already minted in this ledger. That means
    /// either the same lineage was minted twice — a caller bug — or a
    /// 48-bit path-hash collision (p ~ 1.8e-9 per world at this entity
    /// population). Failing loudly turns silent identity corruption into a
    /// reproducible panic; never suppress this.
    pub fn mint_entity(&mut self, lineage: Lineage<'_>) -> EntityId {
        // Repopulate `minted` from the facts if this is the first mint since
        // a load: without this, a freshly-deserialized ledger's `minted` is
        // empty (it is `#[serde(skip)]`) and a mint could silently hand out
        // an id that collides with one already referenced by a loaded fact.
        self.ensure_index();
        let id = derive_entity_id(lineage);
        assert!(
            self.minted.insert(id),
            "entity id {:#x} already minted — lineage (parent {:?}, role {:?}, \
             ordinal {}) collides. Same lineage minted twice, or a path-hash \
             collision. Do not suppress: widen the lineage instead.",
            id.get(),
            lineage.parent,
            lineage.role,
            lineage.ordinal
        );
        self.next_entity += 1;
        id
    }

    /// The entity `lineage` derives to — minted if this ledger has never seen
    /// it, returned unchanged if it has.
    ///
    /// **This is not a softer [`Ledger::mint_entity`].** That one panics on a
    /// repeated lineage because a genesis path that mints one lineage twice
    /// has produced two entities wearing one identity. This is for the other
    /// case: a derivation that is legitimately RE-RUN over a ledger that may
    /// already hold its output, where the same input must yield the same
    /// entity rather than a second one. The live example is a possession
    /// session re-deriving the NPCs of a world it has already played and
    /// saved: the herder of a settlement is the same herder in every session,
    /// so re-deriving must find it. Under the counter that case could not even
    /// be expressed — the re-derivation silently minted a duplicate NPC on
    /// every reload — which is a defect deriving ids from lineage exposes.
    ///
    /// Reach for it only where re-derivation is genuinely idempotent. A
    /// genesis path minting a fresh entity wants `mint_entity` and its guard.
    pub fn reuse_or_mint_entity(&mut self, lineage: Lineage<'_>) -> EntityId {
        self.ensure_index();
        let id = derive_entity_id(lineage);
        if self.minted.insert(id) {
            self.next_entity += 1;
        }
        id
    }

    /// How many entities this ledger has minted — an accession count, never
    /// an identity. Ids are derived (see [`derive_entity_id`]); this only
    /// answers "how many".
    /// type-audit: bare-ok(count: return)
    pub fn entity_count(&self) -> u64 {
        self.next_entity
    }

    /// Ensure the derived index exists and is current (rebuild-if-absent).
    /// Also the sole place `minted` is repopulated after a load: `index` and
    /// `minted` are both `#[serde(skip)]`, both absent until this first
    /// rebuild, and rebuilt together from the same `facts` scan so the two
    /// stay in lockstep.
    fn ensure_index(&mut self) {
        if self.index.is_none() {
            let mut idx = crate::fact_index::FactIndex::default();
            idx.rebuild(&self.facts);
            self.index = Some(idx);
            self.minted = self.facts.iter().map(|f| f.subject).collect();
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
            // First fact in COMMIT order for (subject, predicate) — the
            // smallest position, which is what the old
            // `positions_for_subject().sort().find(predicate matches)` shape
            // computed the expensive way: it materialized and sorted every
            // position for the SUBJECT, then scanned for the predicate. The
            // SPO index is keyed on the predicate already, so `min` over the
            // pair's own postings is the same answer without the `Vec` or the
            // sort. Profiled at ~10% of a `hornvale-book` render test.
            Some(idx) => idx
                .positions_for_subject_predicate(subject, predicate)
                .min()
                .map(|p| &self.facts[p].object),
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

    /// Iterate over every committed fact, in commit order.
    pub fn iter(&self) -> impl Iterator<Item = &Fact> {
        self.facts.iter()
    }

    /// Mint a fresh entity and commit its `instance-of` fact in one
    /// operation — the sole writer of the predicate (single-writer by
    /// construction). The kernel is roster-blind: label validation is the
    /// composition root's job (worldgen).
    /// type-audit: bare-ok(identifier-text: kind_label), waiver(decision-0014: day), bare-ok(prose: provenance)
    pub fn mint_instance(
        &mut self,
        lineage: Lineage<'_>,
        kind_label: &str,
        day: Option<f64>,
        provenance: &str,
        registry: &ConceptRegistry,
    ) -> Result<EntityId, LedgerError> {
        let e = self.mint_entity(lineage);
        self.commit(
            Fact {
                subject: e,
                predicate: crate::world::INSTANCE_OF.to_string(),
                object: Value::Text(kind_label.to_string()),
                place: None,
                day,
                provenance: provenance.to_string(),
            },
            registry,
        )?;
        Ok(e)
    }

    /// Commit a kind-change fact for an existing entity (owlbear ->
    /// awakened-owlbear). Appends; never edits. No transition constraints
    /// here — guards ride the c6 capability schema.
    /// type-audit: bare-ok(identifier-text: kind_label), waiver(decision-0014: day), bare-ok(prose: provenance)
    pub fn change_kind(
        &mut self,
        e: EntityId,
        kind_label: &str,
        day: Option<f64>,
        provenance: &str,
        registry: &ConceptRegistry,
    ) -> Result<(), LedgerError> {
        self.commit(
            Fact {
                subject: e,
                predicate: crate::world::INSTANCE_OF.to_string(),
                object: Value::Text(kind_label.to_string()),
                place: None,
                day,
                provenance: provenance.to_string(),
            },
            registry,
        )
        .map(|_| ())
    }

    /// The LAST committed object for (subject, predicate) — the read for
    /// sim-mutable non-functional predicates, where commit order is time
    /// order. Contrast `value_of` (first object; the functional read).
    /// type-audit: bare-ok(identifier-text: predicate)
    pub fn latest_value_of(&self, e: EntityId, predicate: &str) -> Option<&Value> {
        match &self.index {
            // The `max` mirror of `value_of`'s `min` — same reasoning, same
            // saving. `facts_about(e).filter(...).last()` walked (and sorted)
            // every fact about the entity to keep the one at the end.
            Some(idx) => idx
                .positions_for_subject_predicate(e, predicate)
                .max()
                .map(|p| &self.facts[p].object),
            None => self
                .naive_facts_about(e)
                .into_iter()
                .rfind(|&p| self.facts[p].predicate == predicate)
                .map(|p| &self.facts[p].object),
        }
    }

    /// The entity's current kind label: the latest `instance-of` fact.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn kind_of(&self, e: EntityId) -> Option<&str> {
        match self.latest_value_of(e, crate::world::INSTANCE_OF) {
            Some(Value::Text(t)) => Some(t.as_str()),
            _ => None,
        }
    }
}

/// Where an entity comes from — the whole input to its derived identity.
/// An id is a function of this and nothing else; deliberately NOT of any
/// material fact, so two materially identical entities still differ.
/// type-audit: bare-ok(identifier-text: role), bare-ok(count: ordinal)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Lineage<'a> {
    /// The parent entity, or `None` for a root.
    pub parent: Option<EntityId>,
    /// The role this entity fills for its parent. A save-format contract:
    /// changing a role label renumbers that whole lineage.
    pub role: &'a str,
    /// Which sibling this is among that parent's children in that role.
    pub ordinal: u16,
}

/// The fixed root every parentless entity derives from. Not the world seed:
/// ids are a pure function of structure, exactly as today's 1, 2, 3... are.
const ENTITY_ROOT: u64 = 0x5369_676E_6574_0001;

/// Derive an entity's identity from its lineage: a 48-bit path hash over
/// (parent, role) in the high bits, the sibling ordinal in the low 16.
/// Siblings therefore share their high bits, which makes a lineage legible
/// in a hex dump. Routes through the `entity/identity/v1` leg before the
/// role, so this derivation space is namespaced away from any other
/// consumer that might derive off a seed whose value happens to equal an
/// entity id.
pub fn derive_entity_id(lineage: Lineage<'_>) -> EntityId {
    let base = crate::seed::Seed(lineage.parent.map_or(ENTITY_ROOT, EntityId::get));
    let hashed = base
        .derive(crate::streams::ENTITY_IDENTITY)
        .derive(crate::seed::StreamLabel::dynamic(lineage.role))
        .0;
    let raw = ((hashed >> 16) << 16) | u64::from(lineage.ordinal);
    // `raw` is zero only when the top 48 bits AND the ordinal are all zero
    // (p = 2^-48). Map that one case to 1 rather than panicking: 1 is a
    // legal id and the collision assert in Task 2 catches any clash it causes.
    EntityId::new(raw).unwrap_or(EntityId::MIN)
}

/// A distinct throwaway lineage for a test that only needs "some entity".
///
/// **Test support, never production.** Every real mint states the parent it
/// belongs to and the role it fills (see [`Lineage`]); passing `parent: None,
/// role: "test"` in world-building code would mis-key every fact about that
/// entity. It is `pub` rather than `#[cfg(test)]` only because the tests that
/// need it live in other crates, which cannot see a `#[cfg(test)]` item.
///
/// `n` distinguishes siblings: two calls with the same `n` derive the same id,
/// so two mints on ONE ledger need two different `n` — the collision assert in
/// [`Ledger::mint_entity`] is what tells you when they do not.
/// type-audit: bare-ok(count: n)
pub fn test_lineage(n: u16) -> Lineage<'static> {
    Lineage {
        parent: None,
        role: "test",
        ordinal: n,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_inserted_mint_does_not_move_an_unrelated_id() {
        // The campaign's whole point, at kernel scale: the star is minted first
        // in one ledger and second in the other. Under the old counter its id
        // moved; under lineage-derived ids it must not.
        let mut before = Ledger::default();
        let star_first = before.mint_entity(Lineage {
            parent: None,
            role: "star",
            ordinal: 0,
        });

        let mut after = Ledger::default();
        let _interloper = after.mint_entity(Lineage {
            parent: None,
            role: "interloper",
            ordinal: 0,
        });
        let star_second = after.mint_entity(Lineage {
            parent: None,
            role: "star",
            ordinal: 0,
        });

        assert_eq!(
            star_first, star_second,
            "minting an unrelated entity first must not move the star's id"
        );
    }

    #[test]
    fn minting_the_same_lineage_twice_is_a_hard_error() {
        let mut l = Ledger::default();
        let lin = Lineage {
            parent: None,
            role: "star",
            ordinal: 0,
        };
        let _first = l.mint_entity(lin);
        let again = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| l.mint_entity(lin)));
        assert!(
            again.is_err(),
            "minting one lineage twice must panic rather than hand out a duplicate identity"
        );
    }

    #[test]
    fn re_deriving_one_lineage_finds_the_same_entity_instead_of_minting_a_second() {
        let mut l = Ledger::default();
        let lin = Lineage {
            parent: None,
            role: "npc",
            ordinal: 0,
        };
        let first = l.reuse_or_mint_entity(lin);
        let again = l.reuse_or_mint_entity(lin);
        assert_eq!(
            first, again,
            "an idempotent re-derivation must return the entity it already made"
        );
        assert_eq!(
            l.entity_count(),
            1,
            "and must not book a second accession for it"
        );
        // It is still a real mint the collision guard knows about: the strict
        // form must now refuse the same lineage.
        let strict = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| l.mint_entity(lin)));
        assert!(
            strict.is_err(),
            "reuse must register the id, so `mint_entity` still sees the collision"
        );
    }

    #[test]
    fn the_accession_count_still_counts() {
        let mut l = Ledger::default();
        l.mint_entity(Lineage {
            parent: None,
            role: "star",
            ordinal: 0,
        });
        l.mint_entity(Lineage {
            parent: None,
            role: "plate",
            ordinal: 0,
        });
        assert_eq!(
            l.entity_count(),
            2,
            "next_entity survives as an accession count"
        );
    }

    #[test]
    fn an_id_is_a_function_of_lineage_not_of_call_order() {
        let parent = EntityId::new(7).expect("nonzero");
        let a = derive_entity_id(Lineage {
            parent: Some(parent),
            role: "occupation",
            ordinal: 0,
        });
        let b = derive_entity_id(Lineage {
            parent: Some(parent),
            role: "occupation",
            ordinal: 0,
        });
        assert_eq!(a, b, "the same lineage must always yield the same id");
    }

    #[test]
    fn siblings_differ_only_in_the_low_sixteen_bits() {
        let parent = EntityId::new(7).expect("nonzero");
        let a = derive_entity_id(Lineage {
            parent: Some(parent),
            role: "occupation",
            ordinal: 0,
        });
        let b = derive_entity_id(Lineage {
            parent: Some(parent),
            role: "occupation",
            ordinal: 1,
        });
        assert_ne!(a, b, "distinct siblings must not collide");
        assert_eq!(
            a.get() >> 16,
            b.get() >> 16,
            "siblings share their path hash, so a lineage is legible in a hex dump"
        );
        assert_eq!(a.get() & 0xFFFF, 0);
        assert_eq!(b.get() & 0xFFFF, 1);
    }

    #[test]
    fn a_different_parent_or_role_moves_the_path_hash() {
        let p7 = EntityId::new(7).expect("nonzero");
        let p8 = EntityId::new(8).expect("nonzero");
        let base = derive_entity_id(Lineage {
            parent: Some(p7),
            role: "occupation",
            ordinal: 0,
        });
        let other_parent = derive_entity_id(Lineage {
            parent: Some(p8),
            role: "occupation",
            ordinal: 0,
        });
        let other_role = derive_entity_id(Lineage {
            parent: Some(p7),
            role: "person",
            ordinal: 0,
        });
        assert_ne!(base.get() >> 16, other_parent.get() >> 16);
        assert_ne!(base.get() >> 16, other_role.get() >> 16);
    }

    #[test]
    fn a_root_needs_no_parent_and_no_world_seed() {
        let a = derive_entity_id(Lineage {
            parent: None,
            role: "star",
            ordinal: 0,
        });
        let b = derive_entity_id(Lineage {
            parent: None,
            role: "star",
            ordinal: 0,
        });
        assert_eq!(a, b);
        let other = derive_entity_id(Lineage {
            parent: None,
            role: "plate",
            ordinal: 0,
        });
        assert_ne!(a.get() >> 16, other.get() >> 16);
    }

    fn registry() -> ConceptRegistry {
        let mut r = ConceptRegistry::default();
        r.register_predicate("name", true, "canonical name")
            .unwrap();
        r.register_predicate("located-in", false, "spatial containment")
            .unwrap();
        r
    }

    fn named(ledger: &mut Ledger, name: &str) -> Fact {
        // `entity_count()` before the mint gives each successive call on the
        // same ledger a fresh ordinal, so repeated `named()` calls never
        // collide (same role, distinct siblings).
        let ordinal = ledger.entity_count() as u16;
        let e = ledger.mint_entity(Lineage {
            parent: None,
            role: "named-test-subject",
            ordinal,
        });
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
        let a = l.mint_entity(Lineage {
            parent: None,
            role: "a",
            ordinal: 0,
        });
        let b = l.mint_entity(Lineage {
            parent: None,
            role: "b",
            ordinal: 0,
        });
        assert_ne!(a, b);
    }

    #[test]
    fn option_entity_id_is_niche_packed() {
        // The c4-deferred perf contract: the NonZeroU64 niche halves Option<EntityId>.
        assert_eq!(std::mem::size_of::<Option<EntityId>>(), 8);
    }

    #[test]
    fn entity_id_zero_is_unrepresentable() {
        assert!(EntityId::new(0).is_none());
        assert_eq!(EntityId::new(7).unwrap().get(), 7);
        // A forged 0 in a save now fails loudly at deserialize.
        assert!(serde_json::from_str::<EntityId>("0").is_err());
        assert_eq!(
            serde_json::from_str::<EntityId>("7").unwrap(),
            EntityId::new(7).unwrap()
        );
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
        let e = l.mint_entity(Lineage {
            parent: None,
            role: "unknown-predicate-subject",
            ordinal: 0,
        });
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
        let village = l.mint_entity(Lineage {
            parent: None,
            role: "village",
            ordinal: 0,
        });
        let vale = l.mint_entity(Lineage {
            parent: None,
            role: "vale",
            ordinal: 0,
        });
        let forest = l.mint_entity(Lineage {
            parent: None,
            role: "forest",
            ordinal: 0,
        });
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
        let e = l.mint_entity(Lineage {
            parent: None,
            role: "non-finite-number-subject",
            ordinal: 0,
        });
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
        let e = l.mint_entity(Lineage {
            parent: None,
            role: "non-finite-day-subject",
            ordinal: 0,
        });
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
        let e = l.mint_entity(Lineage {
            parent: None,
            role: "quantize-subject",
            ordinal: 0,
        });
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
        let e = l.mint_entity(Lineage {
            parent: None,
            role: "finite-number-subject",
            ordinal: 0,
        });
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
        let lineage = Lineage {
            parent: None,
            role: "roundtrip-subject",
            ordinal: 0,
        };
        let e = l.mint_entity(lineage);
        l.commit(
            Fact {
                subject: e,
                predicate: "name".to_string(),
                object: Value::Text("Zaggrak".to_string()),
                place: None,
                day: None,
                provenance: "test".to_string(),
            },
            &r,
        )
        .unwrap();
        let json = serde_json::to_string(&l).unwrap();
        let mut l2: Ledger = serde_json::from_str(&json).unwrap();
        assert_eq!(l2.len(), 1);
        // `minted` is `#[serde(skip)]`, so this only holds if it is rebuilt
        // from the loaded facts before the first post-reload mint: re-minting
        // the SAME lineage that produced `e` must still collide.
        let again =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| l2.mint_entity(lineage)));
        assert!(
            again.is_err(),
            "a lineage already present in the loaded facts must still collide after reload"
        );
        // An unrelated lineage mints cleanly and does not collide.
        let fresh = l2.mint_entity(Lineage {
            parent: None,
            role: "post-reload-subject",
            ordinal: 0,
        });
        assert_eq!(l2.facts_about(fresh).count(), 0);
        assert_ne!(fresh, e);
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
        let s = l.mint_entity(Lineage {
            parent: None,
            role: "commit-order-subject",
            ordinal: 0,
        });
        let e1 = l.mint_entity(Lineage {
            parent: None,
            role: "commit-order-target",
            ordinal: 0,
        });
        let e2 = l.mint_entity(Lineage {
            parent: None,
            role: "commit-order-target",
            ordinal: 1,
        });
        // Ids are lineage-derived, not sequential, so which of the two sorts
        // lower is not knowable from mint order — sort by value instead of
        // assuming it, so ObjKey(Entity(low)) < ObjKey(Entity(high)) holds.
        let (low, high) = if e1.get() < e2.get() {
            (e1, e2)
        } else {
            (e2, e1)
        };
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
        let subjects: Vec<EntityId> = (0..8u16)
            .map(|ordinal| {
                l.mint_entity(Lineage {
                    parent: None,
                    role: "random-ledger-subject",
                    ordinal,
                })
            })
            .collect();
        let mut st = seed.wrapping_add(1);
        for _ in 0..n {
            let s = subjects[(splitmix(&mut st) as usize) % subjects.len()];
            // only use the non-functional predicate for bulk facts, so random
            // objects never trip the functional-contradiction reject
            let obj = if splitmix(&mut st).is_multiple_of(4) {
                match splitmix(&mut st) % 3 {
                    0 => Value::Number(0.0),
                    1 => Value::Number(-0.0),
                    _ => Value::Number((splitmix(&mut st) % 100) as f64),
                }
            } else {
                Value::Entity(subjects[(splitmix(&mut st) as usize) % subjects.len()])
            };
            let _ = l.commit(
                Fact {
                    subject: s,
                    predicate: "located-in".into(),
                    object: obj,
                    place: None,
                    day: None,
                    provenance: "t".into(),
                },
                &r,
            );
        }
        (l, r, subjects)
    }

    /// claim: invariant(forall-seed) — indexed facts_about/value_of agree with
    /// a naive scan
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

    /// claim: invariant(forall-seed) — indexed `latest_value_of` agrees with a
    /// naive scan.
    ///
    /// Its own test rather than a line inside
    /// `index_equals_scan_subject_and_predicate`, because it guards the
    /// opposite END of the posting list: `value_of` takes the first position
    /// and this takes the last, and only a non-functional predicate can tell
    /// the two apart. `random_ledger` commits bulk facts under `located-in`
    /// (non-functional) for exactly that reason, so a subject here really does
    /// carry many values and first != last.
    #[test]
    fn latest_value_of_equals_the_last_matching_fact_in_a_scan() {
        for seed in 0..64u64 {
            let (l, _r, subjects) = random_ledger(seed, 200);
            for &s in &subjects {
                let scan = l
                    .naive_facts_about(s)
                    .into_iter()
                    .map(|p| l.fact_at(p))
                    .rfind(|f| f.predicate == "located-in")
                    .map(|f| &f.object);
                assert_eq!(
                    l.latest_value_of(s, "located-in"),
                    scan,
                    "latest_value_of seed {seed} subj {s:?}"
                );
            }
        }
    }

    /// claim: invariant(forall-seed) — indexed query_by_object agrees with a
    /// naive scan
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
    fn index_equals_scan_handles_signed_zero_numbers() {
        // Regression: quantize preserves -0.0 and total_cmp orders -0.0 < 0.0, but
        // the naive path compares objects with IEEE == (-0.0 == 0.0). ObjKey
        // canonicalizes signed zero so the index buckets them together, keeping
        // INDEX == SCAN total over numeric objects.
        let r = registry();
        let mut l = Ledger::default();
        let s = l.mint_entity(Lineage {
            parent: None,
            role: "signed-zero-subject",
            ordinal: 0,
        });
        for obj in [Value::Number(0.0), Value::Number(-0.0), Value::Number(1.5)] {
            l.commit(
                Fact {
                    subject: s,
                    predicate: "located-in".into(),
                    object: obj,
                    place: None,
                    day: None,
                    provenance: "t".into(),
                },
                &r,
            )
            .unwrap();
        }
        // -0.0 dedups against 0.0 (IEEE) exactly as the pre-index ledger did:
        // 0.0 and 1.5 remain (2 facts), matching the naive scan.
        assert_eq!(l.facts_about(s).count(), l.naive_facts_about(s).len());
        assert_eq!(l.facts_about(s).count(), 2);
        // query_by_object under either zero spelling returns the naive set.
        for probe in [Value::Number(0.0), Value::Number(-0.0)] {
            let idx: Vec<&Fact> = l.query_by_object(&probe).collect();
            let scan: Vec<&Fact> = l
                .naive_query_by_object(&probe)
                .iter()
                .map(|&p| l.fact_at(p))
                .collect();
            assert_eq!(
                idx, scan,
                "query_by_object({probe:?}) must equal the naive scan"
            );
        }
    }

    #[test]
    fn query_by_object_finds_committed_facts() {
        let r = registry();
        let mut l = Ledger::default();
        let a = l.mint_entity(Lineage {
            parent: None,
            role: "query-by-object-subject",
            ordinal: 0,
        });
        let hub = l.mint_entity(Lineage {
            parent: None,
            role: "query-by-object-hub",
            ordinal: 0,
        });
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

    #[test]
    fn mint_instance_commits_an_instance_of_fact() {
        let mut w = crate::World::new(crate::Seed(1));
        let e = w
            .ledger
            .mint_instance(
                Lineage {
                    parent: None,
                    role: "owlbear-instance",
                    ordinal: 0,
                },
                "owlbear",
                Some(0.0),
                "test",
                &w.registry,
            )
            .unwrap();
        assert_eq!(w.ledger.kind_of(e), Some("owlbear"));
        assert_eq!(w.ledger.find(crate::INSTANCE_OF).count(), 1);
    }

    #[test]
    fn kind_change_is_a_fact_and_kind_of_is_latest_wins() {
        let mut w = crate::World::new(crate::Seed(1));
        let e = w
            .ledger
            .mint_instance(
                Lineage {
                    parent: None,
                    role: "owlbear-instance",
                    ordinal: 0,
                },
                "owlbear",
                Some(0.0),
                "test",
                &w.registry,
            )
            .unwrap();
        w.ledger
            .change_kind(
                e,
                "awakened-owlbear",
                Some(12.5),
                "test: the awakening",
                &w.registry,
            )
            .unwrap();
        // Current kind is the LATEST fact (contrast value_of's first-wins).
        assert_eq!(w.ledger.kind_of(e), Some("awakened-owlbear"));
        // The history is the ledger's native state machine: both transitions
        // survive, in commit order, day-stamped.
        let history: Vec<&Value> = w
            .ledger
            .facts_about(e)
            .filter(|f| f.predicate == crate::INSTANCE_OF)
            .map(|f| &f.object)
            .collect();
        assert_eq!(
            history,
            vec![
                &Value::Text("owlbear".to_string()),
                &Value::Text("awakened-owlbear".to_string())
            ]
        );
    }

    #[test]
    fn latest_value_of_returns_the_last_committed_value() {
        let mut w = crate::World::new(crate::Seed(1));
        let e = w
            .ledger
            .mint_instance(
                Lineage {
                    parent: None,
                    role: "owlbear-instance",
                    ordinal: 0,
                },
                "owlbear",
                None,
                "test",
                &w.registry,
            )
            .unwrap();
        w.ledger
            .change_kind(e, "corpse", None, "test", &w.registry)
            .unwrap();
        assert_eq!(
            w.ledger.latest_value_of(e, crate::INSTANCE_OF),
            Some(&Value::Text("corpse".to_string()))
        );
        assert_eq!(w.ledger.latest_value_of(e, "name"), None);
    }

    #[test]
    fn kind_of_survives_serialization_roundtrip() {
        let mut w = crate::World::new(crate::Seed(1));
        let e = w
            .ledger
            .mint_instance(
                Lineage {
                    parent: None,
                    role: "granite-instance",
                    ordinal: 0,
                },
                "granite",
                None,
                "test",
                &w.registry,
            )
            .unwrap();
        let json = serde_json::to_string(&w.ledger).unwrap();
        let l2: Ledger = serde_json::from_str(&json).unwrap();
        // Exercises the lazy index rebuild path on a fresh deserialize.
        assert_eq!(l2.kind_of(e), Some("granite"));
    }

    // This is a wall-time micro-bench of `Ledger::commit`'s scaling, not a
    // live-worldgen battery — but `cli/tests/heavy_tier.rs` requires every
    // `#[ignore]` reason containing "heavy:" to be the one verbatim
    // canonical string (checked by `heavy_tier_reason_strings_are_canonical`),
    // so it shares that string with the other heavy-tier deferrals: both are
    // deferred from the commit gate to `make gate-full` for the same reason
    // (too slow to run every commit).
    // Wall-clock time is banned everywhere the sim computes (decision 0001):
    // world time is `WorldTime`, never `Instant`. This test measures the
    // *build's* wall-clock cost of commits, off the sim compute path and
    // never serialized/gated — a justified, scoped exception.
    #[test]
    #[allow(clippy::disallowed_types)]
    #[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
    fn bench_commit_scaling_before_vs_after_index() {
        use std::hint::black_box;
        use std::time::Instant;
        let r = registry();
        for n in [1_000usize, 5_000, 20_000] {
            // AFTER: index-backed commit (each commit maintains the index).
            let mut l = Ledger::default();
            let subj = l.mint_entity(Lineage {
                parent: None,
                role: "bench-subject",
                ordinal: 0,
            });
            let start = Instant::now();
            for i in 0..n {
                let target = l.mint_entity(Lineage {
                    parent: None,
                    role: "bench-target",
                    ordinal: i as u16,
                });
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
