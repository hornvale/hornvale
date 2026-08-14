//! A claim: what somebody holds to be true, in decision 0100's *myth*
//! register. Derived, never serialized, never contradiction-checked.

use crate::ledger::{EntityId, Value};
use crate::provenance::Provenance;

/// One claim held by one holder.
///
/// **This type is deliberately not `Serialize`.** 0100 puts myth in the
/// derived register — free, evictable, and not required to be coherent with
/// fact or with other myth — and rule 5 forbids committing a balance.
/// A claim set is recomputed from committed facts, never stored.
///
/// The `holder` field is 0100 rule 2 in the type system: a myth without a
/// holder is malformed, so there is no way to build one here.
/// type-audit: bare-ok(identifier-text: predicate), bare-ok(count: hops)
#[derive(Clone, Debug, PartialEq)]
pub struct Claim {
    /// Who holds this claim. Never optional (0100 rule 2).
    pub holder: EntityId,
    /// What the claim is about.
    pub subject: EntityId,
    /// The predicate asserted, named as the concept registry names it.
    pub predicate: String,
    /// The value asserted for (subject, predicate).
    pub object: Value,
    /// How this holder came to hold it.
    pub grade: Provenance,
    /// Retellings between the original witness and this holder. 0 = witness.
    pub hops: u32,
}

impl Claim {
    /// The claim as a new holder receives it: same content, downgraded grade,
    /// one more hop. Content is carried unchanged — distortion is deliberately
    /// out of scope for this campaign (spec §2).
    pub fn inherited_by(&self, holder: EntityId) -> Claim {
        Claim {
            holder,
            subject: self.subject,
            predicate: self.predicate.clone(),
            object: self.object.clone(),
            grade: self.grade.on_transmission(),
            hops: self.hops.saturating_add(1),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::{EntityId, Value};

    fn eid(n: u64) -> EntityId {
        EntityId::new(n).expect("nonzero")
    }

    fn witnessed() -> Claim {
        Claim {
            holder: eid(1),
            subject: eid(99),
            predicate: "occ-ended".to_string(),
            object: Value::Number(63918.75),
            grade: Provenance::Witnessed,
            hops: 0,
        }
    }

    #[test]
    fn inheriting_a_claim_downgrades_its_grade() {
        let heir = witnessed().inherited_by(eid(2));
        assert_eq!(heir.grade, Provenance::Taught);
    }

    #[test]
    fn inheriting_a_claim_increments_hops() {
        assert_eq!(witnessed().inherited_by(eid(2)).hops, 1);
        assert_eq!(
            witnessed().inherited_by(eid(2)).inherited_by(eid(3)).hops,
            2
        );
    }

    #[test]
    fn inheriting_a_claim_moves_the_holder_and_nothing_else() {
        let original = witnessed();
        let heir = original.inherited_by(eid(2));
        assert_eq!(heir.holder, eid(2));
        assert_eq!(heir.subject, original.subject);
        assert_eq!(heir.predicate, original.predicate);
        assert_eq!(heir.object, original.object);
    }
}
