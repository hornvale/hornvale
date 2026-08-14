//! The hearsay window: what a community holds to be true, derived from the
//! committed ledger and nothing else.
//!
//! Decision 0100 puts myth in the derived register and says it "has no channel
//! today". This crate is that channel's read side. It draws nothing, commits
//! nothing, and owns no seed labels — it is a window, not a domain.
#![warn(missing_docs)]

pub mod derive;
pub mod lineage;

/// The hop count of every holder of a claim about `(subject, predicate)`.
/// Empty when the subject holds no such committed fact.
/// type-audit: bare-ok(identifier-text: predicate), bare-ok(count: return)
pub fn hops_about(
    ledger: &hornvale_kernel::ledger::Ledger,
    lineage: &lineage::Lineage,
    subject: hornvale_kernel::ledger::EntityId,
    predicate: &str,
) -> Vec<u32> {
    derive::claims_about(ledger, lineage, subject, predicate)
        .iter()
        .map(|c| c.hops)
        .collect()
}

/// Median hop count over every (event, holder) pair in the world for
/// `predicate`, or `None` when there are no pairs at all.
///
/// The median of an even-length population takes the lower of the two central
/// values — a deterministic tie-break, never an average, so the result is
/// always an observed hop count and never an interpolated one.
/// type-audit: bare-ok(identifier-text: predicate), bare-ok(count: return)
pub fn median_hops(
    ledger: &hornvale_kernel::ledger::Ledger,
    lineage: &lineage::Lineage,
    predicate: &str,
) -> Option<f64> {
    let mut hops: Vec<u32> = lineage
        .all()
        .into_iter()
        .flat_map(|s| hops_about(ledger, lineage, s, predicate))
        .collect();
    if hops.is_empty() {
        return None;
    }
    hops.sort_unstable();
    Some(f64::from(hops[hops.len() / 2]))
}

/// Witnesses over holders for one event, or `None` below three holders
/// (spec §6.2's qualifying threshold).
///
/// Range is `(0, 1]`: 1.0 when everyone holding the claim saw it happen,
/// low when a few witnesses are outnumbered by generations of inheritors.
/// No elimination pass is needed or wanted — witnesses and inheritors are
/// already separated by `derive::witnesses_of`, so a copy is never counted
/// as a source in the first place (spec §6.1).
/// type-audit: bare-ok(identifier-text: predicate), bare-ok(ratio: return)
pub fn echo_ratio(
    ledger: &hornvale_kernel::ledger::Ledger,
    lineage: &lineage::Lineage,
    subject: hornvale_kernel::ledger::EntityId,
    predicate: &str,
) -> Option<f64> {
    let holders = derive::claims_about(ledger, lineage, subject, predicate).len();
    if holders < 3 {
        return None;
    }
    let witnesses = derive::witnesses_of(ledger, lineage, subject, predicate).len();
    Some(witnesses as f64 / holders as f64)
}

/// The witnesses whose lines diverge: those with no other witness among their
/// ancestors, ascending.
///
/// Two such witnesses carry the event down lines that never inherited from one
/// another, so a later meeting between their descendants is a genuine
/// cross-check rather than an echo. A survivor community is a *descendant* of
/// the village it fled, so it does not diverge from it — it is still an
/// independent witness (it saw the raid), which is why this is a separate
/// question from how many witnesses there were.
pub fn divergent_witnesses(
    lineage: &lineage::Lineage,
    witnesses: &[hornvale_kernel::ledger::EntityId],
) -> Vec<hornvale_kernel::ledger::EntityId> {
    let mut out: Vec<_> = witnesses
        .iter()
        .copied()
        .filter(|w| {
            let anc = lineage.ancestry(*w);
            !witnesses.iter().any(|o| *o != *w && anc.contains(o))
        })
        .collect();
    out.sort();
    out.dedup();
    out
}
