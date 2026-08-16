//! Turning committed facts into held claims.

use crate::lineage::Lineage;
use hornvale_kernel::Claim;
use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use hornvale_kernel::provenance::Provenance;
use std::collections::BTreeMap;

/// Everyone present when `(subject, predicate)` happened.
///
/// Only an ENDING has parties beyond its subject (spec §6.1). For an ending on
/// day `d`, that is the subject itself, every child of the subject founded on
/// exactly day `d` — the survivors who fled and refounded — and the occupation
/// named by `occ-ended-by` when it is `Entity`-valued. Any other predicate has
/// the subject alone.
///
/// The day comparison is exact equality, and deliberately so: the bake writes
/// a refounding at precisely its parent's ending day (477 of 562 such pairs on
/// seed 42, with all three gap quartiles at 0.0), so there is no threshold to
/// tune and no near-miss band to argue about.
///
/// type-audit: bare-ok(identifier-text: predicate)
pub fn witnesses_of(
    ledger: &Ledger,
    lineage: &Lineage,
    subject: EntityId,
    predicate: &str,
) -> Vec<EntityId> {
    let mut out = vec![subject];
    if predicate == hornvale_history::OCC_ENDED {
        if let Some(Value::Number(day)) = ledger.value_of(subject, hornvale_history::OCC_ENDED) {
            let day = *day;
            // Survivors are DIRECT children, not deeper kin — so ask for them
            // directly rather than filtering the whole descendant set down to
            // one hop, which is what this used to do.
            for child in lineage.children_of(subject) {
                if let Some(Value::Number(f)) =
                    ledger.value_of(*child, hornvale_history::OCC_FOUNDED)
                    && *f == day
                {
                    out.push(*child);
                }
            }
        }
        if let Some(Value::Entity(attacker)) =
            ledger.value_of(subject, hornvale_history::OCC_ENDED_BY)
        {
            out.push(*attacker);
        }
    }
    out.sort();
    out.dedup();
    out
}

/// Every claim held about `(subject, predicate)`, ascending by holder.
///
/// Every witness of the event (`witnesses_of`) holds it at `hops = 0` with
/// grade `Witnessed`; every witness's descendants inherit it, downgraded and
/// one hop further per retelling. Two rules keep the walk from producing a
/// wrong answer at the seams (spec §6.1): a holder that is itself a witness
/// is never demoted to an inheritor (a survivor saw the raid; it does not
/// merely hear about it from the village it fled), and a holder reachable
/// from two witnesses takes the nearer telling — the minimum hop count, not
/// whichever witness's walk happened to reach it first. Nothing here draws,
/// mutates content, or consults anything but the ledger.
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
    let base = Claim {
        holder: subject,
        subject,
        predicate: predicate.to_string(),
        object: object.clone(),
        grade: Provenance::Witnessed,
        hops: 0,
    };
    let witnesses = witnesses_of(ledger, lineage, subject, predicate);
    // BTreeMap keeps the result ascending and deterministic.
    let mut held: BTreeMap<EntityId, Claim> = BTreeMap::new();
    for w in &witnesses {
        let mut c = base.clone();
        c.holder = *w;
        held.insert(*w, c); // hops 0, Witnessed
    }
    for w in &witnesses {
        // The walk carries the hop count out with it, so the depth no longer
        // costs a per-descendant re-derivation of that descendant's ancestry.
        for (d, hops) in lineage.descendants_with_hops(*w) {
            if witnesses.contains(&d) {
                continue; // a witness is never demoted to an inheritor
            }
            let entry = held.entry(d).or_insert_with(|| {
                let mut c = base.clone();
                c.holder = d;
                c.grade = base.grade.on_transmission();
                c.hops = hops;
                c
            });
            // reachable from two witnesses: the NEARER telling is the one held
            if entry.hops > hops {
                entry.hops = hops;
            }
        }
    }
    held.into_values().collect()
}
