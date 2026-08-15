mod common;

use common::{eid, ledger_with};
use hornvale_hearsay::lineage::lineage_of;

#[test]
fn a_number_valued_founding_is_a_root_not_a_parent() {
    let led = ledger_with(&[(1, None)]);
    let lin = lineage_of(&led);
    assert_eq!(
        lin.parent(eid(1)),
        None,
        "Genesis(cell) is not a parent link"
    );
    assert_eq!(lin.roots(), &[eid(1)]);
}

#[test]
fn an_entity_valued_founding_is_a_parent() {
    let led = ledger_with(&[(1, None), (2, Some(1))]);
    let lin = lineage_of(&led);
    assert_eq!(lin.parent(eid(2)), Some(eid(1)));
    assert_eq!(lin.roots(), &[eid(1)]);
}

#[test]
fn ancestry_runs_self_first_root_last() {
    let led = ledger_with(&[(1, None), (2, Some(1)), (3, Some(2))]);
    let lin = lineage_of(&led);
    assert_eq!(lin.ancestry(eid(3)), vec![eid(3), eid(2), eid(1)]);
}

#[test]
fn two_roots_stay_two_lineages() {
    let led = ledger_with(&[(1, None), (2, Some(1)), (10, None), (11, Some(10))]);
    let lin = lineage_of(&led);
    let mut roots = lin.roots().to_vec();
    roots.sort();
    assert_eq!(roots, vec![eid(1), eid(10)]);
}

#[test]
fn is_ancestor_agrees_with_walking_the_ancestry() {
    // The memo must not drift from the walk it replaces. Cross-checked over
    // every ordered pair rather than a sampled one.
    let led = ledger_with(&[(2, Some(1)), (3, Some(2)), (4, Some(1)), (5, None)]);
    let lin = lineage_of(&led);
    for a in lin.all() {
        for d in lin.all() {
            let walked = a != d && lin.ancestry(d).contains(&a);
            assert_eq!(
                lin.is_ancestor(a, d),
                walked,
                "is_ancestor({a:?}, {d:?}) disagrees with the ancestry walk"
            );
        }
    }
}

#[test]
fn a_node_is_not_its_own_ancestor() {
    let led = ledger_with(&[(2, Some(1))]);
    let lin = lineage_of(&led);
    assert!(!lin.is_ancestor(eid(1), eid(1)));
    assert!(lin.is_ancestor(eid(1), eid(2)));
    assert!(
        !lin.is_ancestor(eid(2), eid(1)),
        "ancestry is antisymmetric"
    );
}
