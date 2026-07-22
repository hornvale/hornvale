//! The Connection Graph's determinism contract (Task 7): the campaign's core
//! claim is that the derived transport topology adds legibility WITHOUT
//! changing the world. Two assertions, on a real seed-42 world built to
//! [`BuildDepth::Settlements`] (deep enough for `connection_graph_of` to read
//! terrain, climate, and every committed settlement's cell):
//!
//! 1. **Byte-identity** — deriving the graph twice from the same world (same
//!    seed, same pins) yields identical edge sets for every node. The
//!    derivation is a total, seedless function of already-committed state
//!    (`windows/worldgen/src/graph_derive.rs`'s module doc), so this is the
//!    same guarantee `history_byte_identity.rs` proves for the history bake,
//!    applied to the graph.
//! 2. **Derived, not committed** — deriving the graph never mutates the
//!    world's ledger (no epoch: the graph is computed on demand, like a
//!    field, never written as a fact), and no predicate in the committed
//!    ledger names an edge/route/graph/connection concept. This is the
//!    "no epoch" design decision (`.superpowers/sdd/decision-ledger.md` #3)
//!    stated as a test.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, GraphConfig, SettlementPins, SkyChoice, WorldComponents, build_world_to,
    connection_graph_of,
};

/// Build seed 42 to `BuildDepth::Settlements` — deep enough for
/// `connection_graph_of` to reconstruct terrain, climate, and read every
/// committed settlement's `cell-id` fact. Mirrors
/// `windows/worldgen/tests/history_byte_identity.rs`'s `build_settlements`
/// and `cli/tests/graph_cost.rs`'s own build, exactly.
fn build_settlements() -> hornvale_kernel::World {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    build_world_to(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Settlements,
    )
    .expect("seed 42 builds to Settlements")
}

#[test]
fn connection_graph_is_byte_identical_across_two_derivations() {
    let world = build_settlements();
    let cfg = GraphConfig::default();

    let a = connection_graph_of(&world, &cfg);
    let b = connection_graph_of(&world, &cfg);

    let nodes_a: Vec<_> = a.nodes().collect();
    let nodes_b: Vec<_> = b.nodes().collect();
    assert_eq!(
        nodes_a, nodes_b,
        "the node set itself must be identical across two derivations from \
         the same world"
    );

    let mut edge_count = 0usize;
    for node in nodes_a {
        let edges_a = a.edges(node);
        let edges_b = b.edges(node);
        assert_eq!(
            edges_a, edges_b,
            "cell {}'s edges diverged across two derivations of the same \
             world's connection graph — the derivation must be a total, \
             seedless function of already-committed state",
            node.0
        );
        edge_count += edges_a.len();
    }
    // A non-trivial sanity check that this test is exercising a real graph,
    // not two empty ones vacuously agreeing.
    assert!(
        edge_count > 0,
        "expected seed 42's connection graph to have edges; got an empty graph"
    );
}

#[test]
fn connection_graph_is_derived_not_committed() {
    let world = build_settlements();
    let before = serde_json::to_string(&world.ledger).unwrap();

    // Deriving the graph must be a pure read: no mutation to the world at
    // all, so the world stays byte-identical whether or not anything ever
    // asks for its connection graph (the campaign's "no epoch" decision).
    let cfg = GraphConfig::default();
    let graph = connection_graph_of(&world, &cfg);
    // Touch the result so the derivation isn't optimized away, without
    // asserting anything about its shape here (that's the other test).
    assert!(graph.nodes().count() > 0, "sanity: the graph has nodes");

    let after = serde_json::to_string(&world.ledger).unwrap();
    assert_eq!(
        before, after,
        "deriving the connection graph must never mutate the world's ledger \
         -- it is DERIVED (computed on demand from terrain + currents + \
         settlements), never committed as a fact. No epoch."
    );

    // Belt-and-suspenders: no ledger predicate names an edge/route/graph
    // concept at all -- the connection graph was never given the chance to
    // sneak in as a fact under a different-looking name.
    let suspect_terms = ["edge", "route", "graph", "connection", "conductance"];
    for fact in world.ledger.iter() {
        let lower = fact.predicate.to_lowercase();
        for term in suspect_terms {
            assert!(
                !lower.contains(term),
                "found a committed ledger predicate {:?} containing {:?} -- \
                 the connection graph (and its vocabulary) must never be \
                 committed as a fact; it is derived at the worldgen root, \
                 with no epoch",
                fact.predicate,
                term
            );
        }
    }
}
