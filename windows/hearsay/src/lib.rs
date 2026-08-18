//! The hearsay window: what a community holds to be true, derived from the
//! committed ledger and nothing else.
//!
//! Decision 0100 puts myth in the derived register and says it "has no channel
//! today". This crate is that channel's read side. It draws nothing, commits
//! nothing, and owns no seed labels — it is a window, not a domain.
#![warn(missing_docs)]

pub mod accumulate;
pub mod amplitude;
pub mod clock;
pub mod contact;
pub mod derive;
pub mod divergence;
pub mod durations;
pub mod ladder;
pub mod lineage;
pub mod stance;

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

/// Spearman's rank correlation, or `None` when either side is constant.
///
/// A constant column has no ranks to correlate; returning `0.0` would read as
/// "no relationship" when the truth is "not measurable", which is the
/// distinction H3's NO VERDICT branch depends on.
/// type-audit: bare-ok(diagnostic-value: xs), bare-ok(diagnostic-value: ys), bare-ok(ratio: return)
pub fn spearman(xs: &[f64], ys: &[f64]) -> Option<f64> {
    if xs.len() != ys.len() || xs.len() < 2 {
        return None;
    }
    fn ranks(v: &[f64]) -> Option<Vec<f64>> {
        let mut idx: Vec<usize> = (0..v.len()).collect();
        idx.sort_by(|a, b| v[*a].total_cmp(&v[*b]));
        if v[idx[0]] == v[idx[idx.len() - 1]] {
            return None; // constant: no variance
        }
        let mut r = vec![0.0; v.len()];
        let mut i = 0;
        while i < idx.len() {
            let mut j = i;
            while j + 1 < idx.len() && v[idx[j + 1]] == v[idx[i]] {
                j += 1;
            }
            // average rank for ties, 1-based
            let avg = ((i + j) as f64) / 2.0 + 1.0;
            for k in i..=j {
                r[idx[k]] = avg;
            }
            i = j + 1;
        }
        Some(r)
    }
    let (rx, ry) = (ranks(xs)?, ranks(ys)?);
    let n = rx.len() as f64;
    let mx = rx.iter().sum::<f64>() / n;
    let my = ry.iter().sum::<f64>() / n;
    let mut num = 0.0;
    let mut dx = 0.0;
    let mut dy = 0.0;
    for i in 0..rx.len() {
        let (a, b) = (rx[i] - mx, ry[i] - my);
        num += a * b;
        dx += a * a;
        dy += b * b;
    }
    if dx == 0.0 || dy == 0.0 {
        return None;
    }
    Some(num / (dx * dy).sqrt())
}

/// How many distinct `(precision, object)` variants of one event are held, or
/// `None` when fewer than three holders qualify (§6's population rule).
/// type-audit: bare-ok(identifier-text: predicate), bare-ok(count: return)
pub fn variant_count(
    ledger: &hornvale_kernel::ledger::Ledger,
    lineage: &lineage::Lineage,
    ladder: &ladder::PrecisionLadder,
    subject: hornvale_kernel::ledger::EntityId,
    predicate: &str,
) -> Option<usize> {
    let vs = derive::variants_about(ledger, lineage, ladder, subject, predicate);
    if vs.len() < 3 {
        return None;
    }
    let mut seen: std::collections::BTreeSet<(hornvale_kernel::Precision, String)> =
        std::collections::BTreeSet::new();
    for v in &vs {
        seen.insert((v.precision, format!("{:?}", v.object)));
    }
    Some(seen.len())
}

/// The hop counts of holders still at the FINEST precision — H1's population.
/// type-audit: bare-ok(identifier-text: predicate), bare-ok(count: return)
pub fn finest_precision_hops(
    ledger: &hornvale_kernel::ledger::Ledger,
    lineage: &lineage::Lineage,
    ladder: &ladder::PrecisionLadder,
    subject: hornvale_kernel::ledger::EntityId,
    predicate: &str,
) -> Vec<u32> {
    derive::variants_about(ledger, lineage, ladder, subject, predicate)
        .into_iter()
        .filter(|c| c.precision == hornvale_kernel::Precision::FINEST)
        .map(|c| c.hops)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn spearman_is_one_on_a_perfectly_monotone_pair() {
        let xs = [1.0, 2.0, 3.0, 4.0];
        let ys = [10.0, 20.0, 30.0, 40.0];
        let r = spearman(&xs, &ys).expect("defined");
        assert!((r - 1.0).abs() < 1e-9, "got {r}");
    }

    #[test]
    fn spearman_is_minus_one_when_reversed() {
        let xs = [1.0, 2.0, 3.0, 4.0];
        let ys = [40.0, 30.0, 20.0, 10.0];
        let r = spearman(&xs, &ys).expect("defined");
        assert!((r + 1.0).abs() < 1e-9, "got {r}");
    }

    #[test]
    fn spearman_is_undefined_when_a_side_has_no_variance() {
        // A constant column has no ranks to correlate. Returning 0.0 here
        // would read as "no relationship" when the truth is "not measurable".
        assert_eq!(spearman(&[1.0, 1.0, 1.0], &[1.0, 2.0, 3.0]), None);
    }
}
