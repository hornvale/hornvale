//! The epoch loop: pure, deterministic `run(&SoundingConfig) -> World`.
//!
//! Coupling note (the headline the benchmark exists to expose): a raid
//! delivers population to the community occupying a graph-neighbour node.
//! Finding "the community at node N" by a linear scan is O(Z) per delivery
//! and O(Z²·A) overall — a quadratic coupling that made the top sweep point
//! intractable on first run. The fix here is the standard one: a
//! `node_index: BTreeMap<NodeId, usize>` giving O(log Z) lookup, restoring
//! near-linear scaling. The real engine must carry the same index.

use crate::config::{EventKind, NodeId, RoleHandle, SoundingConfig};
use crate::generate::seed_world;
use crate::world::{BioEntry, Community, Ruin, World};
use std::collections::BTreeMap;

/// Run the whole `A`-epoch history and return the baked world.
pub fn run(config: &SoundingConfig) -> World {
    let mut world = seed_world(config);
    // O(log Z) "which alive community occupies node N" index — the current
    // owner of each node, kept in step with founding/collapse/refounding.
    let mut node_index: BTreeMap<NodeId, usize> = world
        .communities
        .iter()
        .enumerate()
        .map(|(i, c)| (c.node, i))
        .collect();
    // Bound the placeholder founding instability so total communities stay
    // O(Z) (the real engine balances founding against collapse; here a hard
    // cap keeps memory proportional to Z instead of compounding).
    let founding_cap = (config.communities as usize).saturating_mul(3).max(1);

    // Pending displacements: (arrival_epoch -> [(target_node, population)]).
    let mut pending: BTreeMap<u32, Vec<(NodeId, f64)>> = BTreeMap::new();
    let mut ev = config.seed.derive("chronicle/events").stream();
    let mut deliver_ev = config.seed.derive("chronicle/deliver").stream();

    for epoch in 0..config.epochs {
        let n = world.communities.len();
        for i in 0..n {
            if !world.communities[i].alive {
                continue;
            }
            let c = &world.communities[i];
            let cap = world.capacity[c.node.0 as usize % world.capacity.len().max(1)];
            let need = world.species[c.species.0 as usize].carrying_need;
            let pressure = (c.population * need) / cap.max(1.0);
            let handle = RoleHandle(ev.next_u64());

            if pressure < 0.6 {
                // Slack: grow, occasionally found a daughter along an edge.
                world.communities[i].population *= 1.05;
                world.communities[i].biography.push(BioEntry {
                    epoch,
                    event: EventKind::Grew,
                    actor: handle,
                });
                if ev.next_f64() < 0.02 && world.communities.len() < founding_cap {
                    found_daughter(&mut world, &mut node_index, i, epoch, &mut ev);
                }
            } else if pressure > 1.0 {
                // Overshoot: raid a neighbour (coupling) or collapse.
                if let Some((target, lag)) =
                    pick_neighbour(&world.graph, world.communities[i].node, &mut ev)
                {
                    let taken = world.communities[i].population * 0.25;
                    world.communities[i].population -= taken;
                    world.communities[i].biography.push(BioEntry {
                        epoch,
                        event: EventKind::Raided,
                        actor: handle,
                    });
                    let arrival = epoch.saturating_add(lag);
                    if arrival < config.epochs {
                        pending.entry(arrival).or_default().push((target, taken));
                    }
                } else {
                    world.communities[i].alive = false;
                    let node = world.communities[i].node;
                    world.communities[i].biography.push(BioEntry {
                        epoch,
                        event: EventKind::Collapsed,
                        actor: handle,
                    });
                    world.ruins.push(Ruin { node, epoch });
                    // The node is now vacant unless another owner took it.
                    if node_index.get(&node) == Some(&i) {
                        node_index.remove(&node);
                    }
                }
            } // 0.6..=1.0: stable, no event this epoch.
        }

        // Deliver displacements that arrive this epoch (graph-coupled,
        // possibly lagged) after the community loop, so a same-epoch
        // (lag == 0) raid enqueued above is delivered before the epoch ends.
        if let Some(arrivals) = pending.remove(&epoch) {
            for (node, pop) in arrivals {
                deliver(
                    &mut world,
                    &mut node_index,
                    &mut deliver_ev,
                    node,
                    pop,
                    epoch,
                );
            }
        }
    }
    world
}

/// A displaced population arriving at a node: it flees into the community
/// there (found in O(log Z) via `node_index`), or refounds if none stands.
fn deliver(
    world: &mut World,
    node_index: &mut BTreeMap<NodeId, usize>,
    ev: &mut hornvale_kernel::Stream,
    node: NodeId,
    pop: f64,
    epoch: u32,
) {
    let handle = RoleHandle(ev.next_u64() ^ ((node.0 as u64) << 32) ^ epoch as u64);
    match node_index.get(&node).copied() {
        Some(i) if world.communities[i].alive => {
            world.communities[i].population += pop;
            world.communities[i].biography.push(BioEntry {
                epoch,
                event: EventKind::Fled,
                actor: handle,
            });
        }
        _ => {
            let species = world
                .communities
                .first()
                .map(|c| c.species)
                .unwrap_or(crate::config::SpeciesId(0));
            let idx = world.communities.len();
            world.communities.push(Community {
                species,
                population: pop,
                node,
                biography: vec![BioEntry {
                    epoch,
                    event: EventKind::Fled,
                    actor: handle,
                }],
                alive: true,
            });
            node_index.insert(node, idx);
        }
    }
}

/// Found a daughter community on a graph neighbour's node, recording it as
/// that node's current owner in `node_index`.
fn found_daughter(
    world: &mut World,
    node_index: &mut BTreeMap<NodeId, usize>,
    parent: usize,
    epoch: u32,
    ev: &mut hornvale_kernel::Stream,
) {
    let handle = RoleHandle(ev.next_u64());
    let node = pick_neighbour(&world.graph, world.communities[parent].node, ev)
        .map(|(t, _)| t)
        .unwrap_or(world.communities[parent].node);
    let species = world.communities[parent].species;
    let seed_pop = world.communities[parent].population * 0.2;
    world.communities[parent].population -= seed_pop;
    world.communities[parent].biography.push(BioEntry {
        epoch,
        event: EventKind::Founded,
        actor: handle,
    });
    let idx = world.communities.len();
    world.communities.push(Community {
        species,
        population: seed_pop,
        node,
        biography: Vec::new(),
        alive: true,
    });
    node_index.insert(node, idx);
}

/// Pick a graph neighbour deterministically; returns (target_node, lag).
fn pick_neighbour(
    graph: &BTreeMap<NodeId, Vec<crate::world::Edge>>,
    from: NodeId,
    ev: &mut hornvale_kernel::Stream,
) -> Option<(NodeId, u32)> {
    let edges = graph.get(&from)?;
    if edges.is_empty() {
        return None;
    }
    // range_u32 is inclusive of hi, so bound at len - 1 to stay in bounds.
    let idx = ev.range_u32(0, edges.len() as u32 - 1) as usize;
    Some((edges[idx].to, edges[idx].lag))
}
