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
