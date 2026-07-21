//! Derive the four kinds of synthetic state from (seed, config).

use crate::config::{EdgeKind, NodeId, SoundingConfig, SpeciesId};
use crate::streams;
use crate::world::{Community, Edge, SpeciesStub, World};
use std::collections::BTreeMap;

/// Build the initial world (before the epoch loop runs).
pub(crate) fn seed_world(config: &SoundingConfig) -> World {
    let z = config.communities as usize;

    // Species table. Always at least one entry: a `species == 0` config
    // still needs a synthetic species so index 0 (used as the fallback in
    // `seed_world` and `deliver`) is never out of bounds.
    let mut s = config.seed.derive(streams::SPECIES).stream();
    let species_count = config.species.max(1);
    let species: Vec<SpeciesStub> = (0..species_count)
        .map(|_| SpeciesStub {
            carrying_need: 0.5 + s.next_f64(), // 0.5..1.5
            frequency_weight: s.next_f64(),    // 0..1
        })
        .collect();

    // Per-node capacity (one node per initial community; foundings reuse nodes).
    let mut cap = config.seed.derive(streams::CAPACITY).stream();
    let capacity: Vec<f64> = (0..z).map(|_| 50.0 + 150.0 * cap.next_f64()).collect();

    // Communities: a species, a starting population, its own node.
    let mut cs = config.seed.derive(streams::COMMUNITIES).stream();
    let communities: Vec<Community> = (0..z)
        .map(|i| Community {
            species: SpeciesId(if config.species == 0 {
                0
            } else {
                cs.range_u32(0, config.species - 1)
            }),
            population: 10.0 + 40.0 * cs.next_f64(),
            node: NodeId(i as u32),
            biography: Vec::new(),
            alive: true,
        })
        .collect();

    let graph = build_graph(config, z);

    World {
        species,
        communities,
        graph,
        capacity,
        ruins: Vec::new(),
    }
}

/// A sparse connection graph: each node gets ~avg_degree edges, a
/// `long_range_fraction` of which jump to a distant node (Route/Portal).
fn build_graph(config: &SoundingConfig, z: usize) -> BTreeMap<NodeId, Vec<Edge>> {
    let mut g: BTreeMap<NodeId, Vec<Edge>> = BTreeMap::new();
    if z == 0 {
        return g;
    }
    let mut e = config.seed.derive(streams::GRAPH).stream();
    let deg = config.avg_degree.max(0.0).round() as u32;
    for i in 0..z {
        let mut edges = Vec::new();
        for _ in 0..deg {
            let long_range = e.next_f64() < config.long_range_fraction;
            let (to, kind, lag) = if long_range {
                // range_u32 is inclusive of hi, so bound at z - 1 to stay in [0, z).
                let j = e.range_u32(0, z as u32 - 1);
                // Half of long-range edges are instant portals, half lagged routes.
                if e.next_f64() < 0.5 {
                    (j, EdgeKind::Portal, 0)
                } else {
                    (j, EdgeKind::Route, 1 + e.range_u32(0, 3))
                }
            } else {
                // Local: an adjacent index (wrap), zero-ish lag.
                let j = ((i + 1 + e.range_u32(0, 3) as usize) % z) as u32;
                (j, EdgeKind::Adjacent, 0)
            };
            edges.push(Edge {
                to: NodeId(to),
                lag,
                kind,
            });
        }
        g.insert(NodeId(i as u32), edges);
    }
    g
}
