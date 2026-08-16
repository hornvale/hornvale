//! Divergent structure: the maximum antichain of a witness set.
//!
//! Deliberately NOT called corroboration. It measures the PRECONDITION —
//! whether two witnesses' accounts descend through communities that never
//! inherited from one another — not the event of confirmation.

use crate::lineage::Lineage;
use hornvale_kernel::ledger::EntityId;

/// The largest pairwise-incomparable subset of `witnesses`, ascending.
///
/// Corroboration is a SYMMETRIC relation; ancestry is a partial order and
/// therefore antisymmetric, so filtering a witness set by ancestry can never
/// express it. The symmetric relation available inside a partial order is
/// incomparability, and a pairwise-incomparable set is an antichain.
///
/// In a forest the maximum antichain of a set `S` is exactly the elements of
/// `S` with no strict `S`-descendant. It is an antichain: if `x` were an
/// ancestor of `y`, then `y` would be an `S`-descendant of `x`. It is maximum:
/// any antichain `A ⊆ S` injects into it by sending each `a` to a deepest
/// `S`-descendant of `a`, injectively because incomparable elements of a tree
/// have disjoint descendant sets. So no matching algorithm is needed.
///
/// For witnesses `{A, B, C}` with `B` and `C` survivors of `A`, this returns
/// `{B, C}`. The minimal elements would be `{A}`, which scores that scenario
/// zero — the error campaign 1 made.
pub fn maximum_antichain(lineage: &Lineage, witnesses: &[EntityId]) -> Vec<EntityId> {
    let mut out: Vec<EntityId> = witnesses
        .iter()
        .copied()
        .filter(|w| {
            !witnesses
                .iter()
                .any(|other| other != w && lineage.is_ancestor(*w, *other))
        })
        .collect();
    out.sort();
    out.dedup();
    out
}
