mod common;

use common::{eid, ledger_with};
use hornvale_hearsay::lineage::{Lineage, lineage_of};
use hornvale_kernel::ledger::EntityId;

/// THE PRE-BEGAT `descendants_of`, verbatim in behaviour, kept as an oracle.
///
/// The optimisation it replaced re-derived each node's ancestry to decide
/// descent; the shipped one walks a children map. Keeping the old form here
/// makes the equivalence a standing assertion instead of a one-time argument
/// in a review. It reads only the public surface, so it cannot drift into
/// sharing an implementation with the thing it checks.
fn oracle_descendants_of(lin: &Lineage, of: EntityId) -> Vec<EntityId> {
    let mut out: Vec<EntityId> = lin
        .all()
        .into_iter()
        .filter(|k| lin.parent(*k).is_some())
        .filter(|k| *k != of && lin.ancestry(*k).contains(&of))
        .collect();
    out.sort();
    out
}

/// The pre-Begat hop count: the position of `w` in `d`'s ancestry.
fn oracle_hops(lin: &Lineage, d: EntityId, w: EntityId) -> u32 {
    lin.ancestry(d)
        .iter()
        .position(|a| *a == w)
        .expect("caller passes a genuine descendant") as u32
}

/// Every shape the equivalence has to hold on, cycles included. `ledger_with`
/// takes `(child, Some(parent))` for a founding edge and `(child, None)` for a
/// Genesis root, so a cycle is just a set of edges with no root.
type Shape = (&'static str, Vec<(u64, Option<u64>)>);

fn shapes() -> Vec<Shape> {
    vec![
        ("empty", vec![]),
        ("lone root", vec![(1, None)]),
        (
            "chain",
            vec![(1, None), (2, Some(1)), (3, Some(2)), (4, Some(3))],
        ),
        (
            "branching tree",
            vec![
                (1, None),
                (2, Some(1)),
                (3, Some(1)),
                (4, Some(2)),
                (5, Some(2)),
                (6, Some(3)),
                (7, Some(6)),
            ],
        ),
        (
            "two roots",
            vec![(1, None), (2, Some(1)), (10, None), (11, Some(10))],
        ),
        ("two-cycle", vec![(1, Some(2)), (2, Some(1))]),
        (
            "three-cycle",
            vec![(1, Some(2)), (2, Some(3)), (3, Some(1))],
        ),
        (
            "cycle with a tail hanging off it",
            vec![
                (1, Some(2)),
                (2, Some(3)),
                (3, Some(1)),
                (4, Some(1)),
                (5, Some(4)),
            ],
        ),
        (
            "rho: a chain running into a cycle",
            vec![
                (1, Some(2)),
                (2, Some(3)),
                (3, Some(1)),
                (7, Some(3)),
                (8, Some(7)),
                (9, Some(8)),
            ],
        ),
    ]
}

#[test]
fn the_children_walk_returns_exactly_what_re_deriving_ancestry_returned() {
    for (label, edges) in shapes() {
        let lin = lineage_of(&ledger_with(&edges));
        for node in lin.all() {
            assert_eq!(
                lin.descendants_of(node),
                oracle_descendants_of(&lin, node),
                "descendants_of({node:?}) diverged from the ancestry-derived \
                 oracle on the {label} shape"
            );
        }
    }
}

#[test]
fn the_hop_count_carried_out_of_the_walk_matches_the_ancestry_position() {
    for (label, edges) in shapes() {
        let lin = lineage_of(&ledger_with(&edges));
        for node in lin.all() {
            for (d, hops) in lin.descendants_with_hops(node) {
                assert_eq!(
                    hops,
                    oracle_hops(&lin, d, node),
                    "hops from {d:?} up to {node:?} diverged from the ancestry \
                     position on the {label} shape"
                );
            }
        }
    }
}

#[test]
fn children_of_is_the_one_hop_slice_of_the_descendant_set() {
    for (label, edges) in shapes() {
        let lin = lineage_of(&ledger_with(&edges));
        for node in lin.all() {
            let one_hop: Vec<EntityId> = lin
                .descendants_with_hops(node)
                .into_iter()
                .filter(|(_, h)| *h == 1)
                .map(|(k, _)| k)
                .collect();
            assert_eq!(
                lin.children_of(node),
                one_hop.as_slice(),
                "children_of({node:?}) is not the 1-hop set on the {label} shape"
            );
        }
    }
}

/// The guard that keeps the two tests above from being vacuous: the cyclic
/// shapes must actually produce descendants, or the equivalence is only ever
/// checked on acyclic input and the hard case goes unmeasured.
#[test]
fn the_cyclic_shapes_are_not_vacuous() {
    let lin = lineage_of(&ledger_with(&[(1, Some(2)), (2, Some(3)), (3, Some(1))]));
    assert_eq!(
        lin.descendants_of(eid(1)),
        vec![eid(2), eid(3)],
        "a three-cycle must report the other two members as descendants"
    );
    assert_eq!(
        lin.descendants_with_hops(eid(1)),
        vec![(eid(2), 2), (eid(3), 1)],
        "and at the hop counts the ancestry walk's first occurrence gives"
    );
}

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
