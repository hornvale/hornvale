//! A claim: what somebody holds to be true, in decision 0100's *myth*
//! register. Derived, never serialized, never contradiction-checked.

use crate::ledger::{EntityId, Value};
use crate::precision::Precision;
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
    /// Which rung of the world's ladder this holder remembers the day at.
    /// Witnesses hold [`Precision::FINEST`]; each lossy retelling descends.
    pub precision: Precision,
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
            precision: self.precision,
        }
    }

    /// The claim as a new holder receives it through a FRICTIONLESS retelling:
    /// teller and hearer share a frame, so content and precision pass
    /// unchanged. Behaviourally identical to [`Claim::inherited_by`], pinned
    /// by test.
    pub fn retold_by(&self, holder: EntityId) -> Claim {
        Claim {
            holder,
            subject: self.subject,
            predicate: self.predicate.clone(),
            object: self.object.clone(),
            grade: self.grade.on_transmission(),
            hops: self.hops.saturating_add(1),
            precision: self.precision,
        }
    }

    /// The claim as a new holder receives it through a LOSSY retelling.
    ///
    /// `precision` and `object` arrive already coarsened from the window that
    /// owns the world's ladder — the kernel performs no day arithmetic,
    /// because a duration at a `pub` boundary wants `StdDays` and that type
    /// lives in a domain the kernel may not depend on.
    pub fn retold_by_lossy(&self, holder: EntityId, precision: Precision, object: Value) -> Claim {
        Claim {
            holder,
            subject: self.subject,
            predicate: self.predicate.clone(),
            object,
            grade: self.grade.on_transmission(),
            hops: self.hops.saturating_add(1),
            precision,
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
            precision: Precision::FINEST,
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

    #[test]
    fn a_frictionless_retelling_carries_content_and_precision_unchanged() {
        let heir = witnessed().retold_by(eid(2));
        assert_eq!(heir.object, witnessed().object);
        assert_eq!(heir.precision, Precision::FINEST);
        assert_eq!(heir.hops, 1);
        assert_eq!(heir.grade, Provenance::Taught);
    }

    #[test]
    fn retold_by_agrees_with_inherited_by() {
        assert_eq!(
            witnessed().inherited_by(eid(2)),
            witnessed().retold_by(eid(2))
        );
    }

    #[test]
    fn a_lossy_retelling_takes_the_coarsened_value_it_is_given() {
        let heir = witnessed().retold_by_lossy(eid(2), Precision(1), Value::Number(63875.0));
        assert_eq!(heir.precision, Precision(1));
        assert_eq!(heir.object, Value::Number(63875.0));
        assert_eq!(heir.grade, Provenance::Taught);
    }

    #[test]
    fn hops_and_grade_advance_identically_on_both_paths() {
        // The constructors differ ONLY in content and precision. If an edit
        // makes one skip a hop or a downgrade, this fails.
        let a = witnessed().retold_by(eid(2));
        let b = witnessed().retold_by_lossy(eid(2), Precision(1), Value::Number(0.0));
        assert_eq!((a.hops, a.grade, a.holder), (b.hops, b.grade, b.holder));
        assert_eq!(
            (a.subject, a.predicate.clone()),
            (b.subject, b.predicate.clone())
        );
    }

    #[test]
    fn a_frictionless_retelling_never_sharpens_a_coarsened_claim() {
        // The one transition the model forbids.
        let c = witnessed()
            .retold_by_lossy(eid(2), Precision(2), Value::Number(0.0))
            .retold_by(eid(3));
        assert_eq!(c.precision, Precision(2));
        assert!(c.precision > Precision::FINEST);
    }
}
