//! Turning committed facts into held claims.

use crate::lineage::Lineage;
use hornvale_kernel::Claim;
use hornvale_kernel::ledger::{EntityId, Ledger};
use hornvale_kernel::provenance::Provenance;
use std::collections::BTreeMap;

/// Every claim held about `(subject, predicate)`, ascending by holder.
///
/// Two rules, and no others (spec §4.3): the subject of a committed event
/// witnessed it, and an occupation founded from a holder inherits it at one
/// more hop and a downgraded grade. Nothing here draws, mutates content, or
/// consults anything but the ledger.
///
/// type-audit: bare-ok(identifier-text: predicate)
pub fn claims_about(
    ledger: &Ledger,
    lineage: &Lineage,
    subject: EntityId,
    predicate: &str,
) -> Vec<Claim> {
    let Some(object) = ledger.value_of(subject, predicate) else {
        return Vec::new();
    };
    let witness = Claim {
        holder: subject,
        subject,
        predicate: predicate.to_string(),
        object: object.clone(),
        grade: Provenance::Witnessed,
        hops: 0,
    };
    // Walk every occupation the lineage knows and keep those descending from
    // the witness. BTreeMap keeps the result ascending and deterministic.
    let mut held: BTreeMap<EntityId, Claim> = BTreeMap::new();
    held.insert(subject, witness.clone());
    for holder in lineage.descendants_of(subject) {
        let hops = lineage
            .ancestry(holder)
            .iter()
            .position(|a| *a == subject)
            .unwrap_or(0) as u32;
        let mut c = witness.clone();
        c.holder = holder;
        c.grade = witness.grade.on_transmission();
        c.hops = hops;
        held.insert(holder, c);
    }
    held.into_values().collect()
}
