//! Integration tests for `ConnectionGraph`: construction, edge lookup, and
//! `reachable_regions` connected-components over a conductance threshold.

use hornvale_kernel::CellId;
use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};
use std::collections::BTreeSet;

/// A 4-cell graph: 0-1 an adjacency edge, 1-2 a water route, 3 isolated.
fn four_cell_graph() -> ConnectionGraph {
    let mut graph = ConnectionGraph::new(4);
    graph.add_edge(
        CellId(0),
        Edge {
            to: CellId(1),
            kind: EdgeKind::Adjacency,
            conductance: 1.0,
        },
    );
    graph.add_edge(
        CellId(1),
        Edge {
            to: CellId(2),
            kind: EdgeKind::WaterRoute,
            conductance: 0.5,
        },
    );
    graph
}

#[test]
fn edges_reports_both_kinds_at_the_shared_node() {
    let graph = four_cell_graph();
    let kinds: Vec<EdgeKind> = graph.edges(CellId(1)).iter().map(|e| e.kind).collect();
    assert!(kinds.contains(&EdgeKind::Adjacency));
    assert!(kinds.contains(&EdgeKind::WaterRoute));
}

#[test]
fn add_edge_is_undirected() {
    let graph = four_cell_graph();
    // 0-1 was added from 0's side; 1's adjacency list must see it too.
    let from_one: Vec<CellId> = graph.edges(CellId(1)).iter().map(|e| e.to).collect();
    assert!(from_one.contains(&CellId(0)));
}

#[test]
fn reachable_regions_splits_by_conductance_threshold() {
    let graph = four_cell_graph();
    let regions = graph.reachable_regions(0.0);
    assert_eq!(regions.len(), 2);
    assert_eq!(
        regions[0],
        BTreeSet::from([CellId(0), CellId(1), CellId(2)])
    );
    assert_eq!(regions[1], BTreeSet::from([CellId(3)]));
}

#[test]
fn reachable_regions_orders_components_by_min_cell_id() {
    let graph = four_cell_graph();
    let regions = graph.reachable_regions(0.0);
    // Component 0 (containing CellId(0)) sorts before component 1
    // (containing only CellId(3)), regardless of insertion order.
    let mins: Vec<CellId> = regions
        .iter()
        .map(|region| *region.iter().next().unwrap())
        .collect();
    assert_eq!(mins, vec![CellId(0), CellId(3)]);
}

#[test]
fn a_high_conductance_threshold_isolates_the_water_route() {
    let graph = four_cell_graph();
    // Only the 0-1 adjacency (conductance 1.0) survives a 0.75 threshold;
    // the 1-2 water route (conductance 0.5) does not.
    let regions = graph.reachable_regions(0.75);
    assert_eq!(regions.len(), 3);
    assert_eq!(regions[0], BTreeSet::from([CellId(0), CellId(1)]));
    assert_eq!(regions[1], BTreeSet::from([CellId(2)]));
    assert_eq!(regions[2], BTreeSet::from([CellId(3)]));
}

#[test]
fn nodes_iterates_every_node_added_at_construction() {
    let graph = ConnectionGraph::new(4);
    let mut ids: Vec<CellId> = graph.nodes().collect();
    ids.sort();
    assert_eq!(ids, vec![CellId(0), CellId(1), CellId(2), CellId(3)]);
}
