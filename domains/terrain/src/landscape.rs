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
    ///
    /// The predicate (`c.0 % 13 < 2`) is chosen to fragment the sphere into
    /// many small components (56 on `Geosphere::new(3)`, confirmed below) —
    /// a predicate yielding only one or two components could never exercise
    /// cross-component contamination, which is the failure mode this test
    /// exists to catch.
    #[test]
    fn components_partition_their_members() {
        let geo = Geosphere::new(3);
        let member = |c: CellId| c.0 % 13 < 2;
        let comps = components(&geo, member);
        assert!(
            comps.len() >= 30,
            "predicate must yield many components to exercise cross-component \
             contamination, got {} (expected 56 on Geosphere::new(3))",
            comps.len()
        );
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
    ///
    /// The predicate (`c.0 % 11 < 3`) is chosen to yield many components (49
    /// on `Geosphere::new(3)`, confirmed below) whose sizes are **not**
    /// monotonically related to their identity order — sizes run
    /// `[1, 1, 3, 1, 8, 13, 1, 9, 3, 2, 4, 7, ...]`, repeatedly rising and
    /// falling. The failure mode this test guards against is "components
    /// come back in size order, or insertion order, rather than identity
    /// order"; a fixture whose sizes happen to already be monotonic (or a
    /// two-component fixture, where insertion order and size order and
    /// identity order all trivially agree half the time) cannot distinguish
    /// identity order from those other orders.
    #[test]
    fn components_are_ordered_by_their_lowest_cell_id() {
        let geo = Geosphere::new(3);
        let comps = components(&geo, |c| c.0 % 11 < 3);
        assert!(
            comps.len() >= 30,
            "predicate must yield many components, got {} (expected 49 on Geosphere::new(3))",
            comps.len()
        );
        let sizes: Vec<usize> = comps.iter().map(BTreeSet::len).collect();
        let mut ascending = sizes.clone();
        ascending.sort_unstable();
        let mut descending = sizes.clone();
        descending.sort_unstable_by(|a, b| b.cmp(a));
        assert_ne!(
            sizes, ascending,
            "fixture sizes must not already be ascending, or this test cannot \
             distinguish identity order from size order"
        );
        assert_ne!(
            sizes, descending,
            "fixture sizes must not already be descending, or this test cannot \
             distinguish identity order from size order"
        );

        let firsts: Vec<u32> = comps
            .iter()
            .map(|c| c.first().expect("nonempty").0)
            .collect();
        let mut sorted = firsts.clone();
        sorted.sort_unstable();
        assert_eq!(firsts, sorted, "components must arrive in identity order");
    }
}
