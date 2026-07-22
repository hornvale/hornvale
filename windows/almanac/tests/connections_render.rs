//! The legibility surface's acceptance test (The Connection Graph, Task 6):
//! `render_connections` on a hand-built graph with a known water route, a
//! known land route, and a smaller region cut off from the larger one.
//!
//! No `hornvale-worldgen` needed here (unlike `history_render.rs`, which
//! moved to `cli/tests/` to dodge an almanac<->worldgen dev-dependency
//! cycle): `render_connections` takes a pre-derived `&ConnectionGraph`
//! rather than deriving one itself (`windows/worldgen` already depends on
//! `windows/almanac`, so the dependency can't run the other way), and
//! `hornvale-topology`/`hornvale-settlement` are already normal dependencies
//! of `hornvale-almanac`, so this integration test reaches them for free.

use hornvale_almanac::connections::{render_connections, render_overview};
use hornvale_kernel::{CellId, Seed, World};
use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};

/// Five cells: 0-1 a sea-lane, 0-2 a land route (both real natural routes),
/// and a separate 3-4 pair connected only to each other -- a smaller region,
/// cut off from the 0/1/2 region.
fn fixture_graph() -> ConnectionGraph {
    let mut graph = ConnectionGraph::new(5);
    graph.add_edge(
        CellId(0),
        Edge {
            to: CellId(1),
            kind: EdgeKind::WaterRoute,
            conductance: 0.5,
        },
    );
    graph.add_edge(
        CellId(0),
        Edge {
            to: CellId(2),
            kind: EdgeKind::LandRoute,
            conductance: 0.1,
        },
    );
    graph.add_edge(
        CellId(3),
        Edge {
            to: CellId(4),
            kind: EdgeKind::Adjacency,
            conductance: 1.0,
        },
    );
    graph
}

fn fixture_world() -> World {
    World::new(Seed(1))
}

#[test]
fn a_site_with_a_water_and_land_route_names_both() {
    let world = fixture_world();
    let graph = fixture_graph();
    let text = render_connections(&world, CellId(0), &graph);
    assert!(text.contains("sea-lane"), "names the sea-lane:\n{text}");
    assert!(text.contains("route"), "names a route at all:\n{text}");
    assert!(text.contains("by land"), "names the land route:\n{text}");
}

#[test]
fn a_site_in_the_smaller_region_reads_as_cut_off() {
    let world = fixture_world();
    let graph = fixture_graph();
    // Cell 3's region ({3, 4}, size 2) is smaller than cell 0's region
    // ({0, 1, 2}, size 3) -- it must read as isolated.
    let text = render_connections(&world, CellId(3), &graph);
    assert!(
        text.contains("cut off"),
        "an isolated region's site names its isolation:\n{text}"
    );
}

#[test]
fn a_site_in_the_largest_region_does_not_read_as_cut_off() {
    let world = fixture_world();
    let graph = fixture_graph();
    let text = render_connections(&world, CellId(0), &graph);
    assert!(
        !text.contains("cut off"),
        "the largest region must not read as isolated:\n{text}"
    );
}

#[test]
fn rendering_is_deterministic() {
    let world = fixture_world();
    let graph = fixture_graph();
    assert_eq!(
        render_connections(&world, CellId(0), &graph),
        render_connections(&world, CellId(0), &graph)
    );
}

#[test]
fn a_site_with_no_routes_at_all_says_so_honestly() {
    let world = fixture_world();
    // CellId(4) is only adjacency-linked to 3 (no water or land route of its
    // own reaches it), so its route paragraph must not fabricate one.
    let graph = fixture_graph();
    let text = render_connections(&world, CellId(4), &graph);
    assert!(
        !text.contains("linked by sea-lane"),
        "cell 4 must not be claimed to have a sea-lane of its own:\n{text}"
    );
    // It still must read as cut off (same small region as cell 3).
    assert!(text.contains("cut off"), "{text}");
}

#[test]
fn a_known_settlement_name_stands_in_for_a_bare_cell_id() {
    let mut world = fixture_world();
    world
        .registry
        .register_predicate(hornvale_settlement::IS_SETTLEMENT, false, "is a settlement")
        .unwrap();
    world
        .registry
        .register_predicate(hornvale_settlement::CELL_ID, true, "cell id")
        .unwrap();
    // `NAME` is already registered by `World::new`; re-registering an
    // identical definition is idempotent (`register_predicate`'s contract).
    world
        .registry
        .register_predicate(hornvale_kernel::NAME, true, "canonical name of an entity")
        .unwrap();
    let village = hornvale_kernel::EntityId::new(1).unwrap();
    let fact = |predicate: &str, object: hornvale_kernel::Value| hornvale_kernel::Fact {
        subject: village,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: None,
        provenance: "test fixture".to_string(),
    };
    let registry = world.registry.clone();
    world
        .ledger
        .commit(
            fact(
                hornvale_settlement::IS_SETTLEMENT,
                hornvale_kernel::Value::Flag(true),
            ),
            &registry,
        )
        .unwrap();
    world
        .ledger
        .commit(
            fact(
                hornvale_kernel::NAME,
                hornvale_kernel::Value::Text("Harrowgate".to_string()),
            ),
            &registry,
        )
        .unwrap();
    world
        .ledger
        .commit(
            fact(
                hornvale_settlement::CELL_ID,
                hornvale_kernel::Value::Number(1.0),
            ),
            &registry,
        )
        .unwrap();

    let graph = fixture_graph();
    let text = render_connections(&world, CellId(0), &graph);
    assert!(
        text.contains("Harrowgate"),
        "a named settlement replaces its bare cell id:\n{text}"
    );
    assert!(
        !text.contains("cell 1"),
        "the named endpoint must not also show as a bare cell id:\n{text}"
    );
}

#[test]
fn overview_names_the_largest_region_and_the_smaller_ones() {
    let graph = fixture_graph();
    let text = render_overview(&graph);
    // The {0,1,2} region (size 3) is the largest; the {3,4} region (size 2)
    // is the only other one -- both meet MIN_NOTABLE_REGION_SIZE (2).
    assert!(
        text.contains('3'),
        "names the largest region's size:\n{text}"
    );
    assert!(
        text.contains('2'),
        "names the smaller region's size:\n{text}"
    );
}

#[test]
fn overview_is_deterministic() {
    let graph = fixture_graph();
    assert_eq!(render_overview(&graph), render_overview(&graph));
}

#[test]
fn overview_reports_one_region_as_fully_connected() {
    // A single connected graph (everything reaches everything) must not
    // claim a "cut off" region exists.
    let mut graph = ConnectionGraph::new(3);
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
            kind: EdgeKind::Adjacency,
            conductance: 1.0,
        },
    );
    let text = render_overview(&graph);
    assert!(
        !text.contains("cut off"),
        "a single connected region must not read as isolated:\n{text}"
    );
}
