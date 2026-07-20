//! The epoch loop: pure, deterministic `run(&SoundingConfig) -> World`.

use crate::config::{EventKind, NodeId, RoleHandle, SoundingConfig};
use crate::generate::seed_world;
use crate::world::{BioEntry, Ruin, World};
use std::collections::BTreeMap;

/// Run the whole `A`-epoch history and return the baked world.
pub fn run(config: &SoundingConfig) -> World {
    let mut world = seed_world(config);
    // Pending displacements: (arrival_epoch, target_node, incoming_population).
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
                if ev.next_f64() < 0.02 {
                    found_daughter(&mut world, config, i, epoch, &mut ev);
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
                    world.communities[i].biography.push(BioEntry {
                        epoch,
                        event: EventKind::Collapsed,
                        actor: handle,
                    });
                    world.ruins.push(Ruin {
                        node: world.communities[i].node,
                        epoch,
                    });
                }
            } // 0.6..=1.0: stable, no event this epoch.
        }

        // Deliver displacements that arrive this epoch (graph-coupled,
        // possibly lagged) after the community loop, so a same-epoch
        // (lag == 0) raid enqueued above is delivered before the epoch ends.
        if let Some(arrivals) = pending.remove(&epoch) {
            for (node, pop) in arrivals {
                deliver(&mut world, &mut deliver_ev, node, pop, epoch);
            }
        }
    }
    world
}

/// A displaced population arriving at a node: it flees into the community
/// there (or refounds if none stands), appending to the biography.
fn deliver(
    world: &mut World,
    ev: &mut hornvale_kernel::Stream,
    node: NodeId,
    pop: f64,
    epoch: u32,
) {
    let handle = RoleHandle(ev.next_u64() ^ ((node.0 as u64) << 32) ^ epoch as u64);
    if let Some(i) = world
        .communities
        .iter()
        .position(|c| c.alive && c.node == node)
    {
        world.communities[i].population += pop;
        world.communities[i].biography.push(BioEntry {
            epoch,
            event: EventKind::Fled,
            actor: handle,
        });
    } else {
        let species = world
            .communities
            .first()
            .map(|c| c.species)
            .unwrap_or(crate::config::SpeciesId(0));
        world.communities.push(crate::world::Community {
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
    }
}

/// Found a daughter community on a graph neighbour's node.
fn found_daughter(
    world: &mut World,
    _config: &SoundingConfig,
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
    world.communities.push(crate::world::Community {
        species,
        population: seed_pop,
        node,
        biography: Vec::new(),
        alive: true,
    });
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
