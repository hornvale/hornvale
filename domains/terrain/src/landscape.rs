//! Landscape features: individuated regions of the world with draw-free
//! identities. See `docs/superpowers/specs/2026-08-18-the-gazetteer-design.md`.

use hornvale_kernel::{CellId, Geosphere};
use std::collections::{BTreeSet, VecDeque};

/// Connected components of the cells satisfying `member`, under
/// [`Geosphere::neighbors`].
///
/// Returned in ascending order of each component's lowest cell id — the
/// identity the caller assigns — so no call site has to sort. Traversal only:
/// no draws, no transcendentals, integer comparisons, so cross-platform
/// byte-identical.
///
/// The components **partition** the members: every satisfying cell appears in
/// exactly one. That is what makes "lowest cell id" a valid identity, and it
/// is asserted rather than assumed.
/// type-audit: bare-ok(count: return)
pub fn components(geo: &Geosphere, member: impl Fn(CellId) -> bool) -> Vec<BTreeSet<CellId>> {
    let mut visited = vec![false; geo.cell_count()];
    let mut out = Vec::new();
    for start in geo.cells() {
        if visited[start.0 as usize] || !member(start) {
            continue;
        }
        visited[start.0 as usize] = true;
        let mut queue = VecDeque::from([start]);
        let mut set = BTreeSet::new();
        while let Some(cell) = queue.pop_front() {
            set.insert(cell);
            for &nb in geo.neighbors(cell) {
                if !visited[nb.0 as usize] && member(nb) {
                    visited[nb.0 as usize] = true;
                    queue.push_back(nb);
                }
            }
        }
        out.push(set);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    /// On a geosphere every cell neighbours another, so an all-true predicate
    /// must yield exactly one component holding every cell.
    #[test]
    fn an_all_true_predicate_yields_one_component_of_every_cell() {
        let geo = Geosphere::new(3);
        let comps = components(&geo, |_| true);
        assert_eq!(comps.len(), 1, "the whole sphere is connected");
        assert_eq!(comps[0].len(), geo.cell_count());
    }

    /// The empty case is a legitimate answer, not a panic.
    #[test]
    fn an_all_false_predicate_yields_no_components() {
        let geo = Geosphere::new(3);
        assert!(components(&geo, |_| false).is_empty());
    }

    /// Every member appears in exactly one component. THIS is what makes
    /// "lowest cell id" a valid identity — if a cell could appear twice, two
    /// features would claim it.
    #[test]
    fn components_partition_their_members() {
        let geo = Geosphere::new(3);
        let member = |c: CellId| !c.0.is_multiple_of(3);
        let comps = components(&geo, member);
        let mut seen = BTreeSet::new();
        for comp in &comps {
            for cell in comp {
                assert!(
                    seen.insert(*cell),
                    "cell {cell:?} appeared in two components"
                );
            }
        }
        let expected: BTreeSet<CellId> = geo.cells().filter(|c| member(*c)).collect();
        assert_eq!(seen, expected, "every member is in exactly one component");
    }

    /// Components arrive in identity order, so no call site has to sort.
    #[test]
    fn components_are_ordered_by_their_lowest_cell_id() {
        let geo = Geosphere::new(3);
        let comps = components(&geo, |c| c.0 % 5 < 3);
        let firsts: Vec<u32> = comps
            .iter()
            .map(|c| c.first().expect("nonempty").0)
            .collect();
        let mut sorted = firsts.clone();
        sorted.sort_unstable();
        assert_eq!(firsts, sorted, "components must arrive in identity order");
    }
}
