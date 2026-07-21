//! The epoch loop: pure, deterministic `run_with(&SoundingConfig, mode)`.
//!
//! Coupling note (the headline the benchmark exists to expose): a raid
//! delivers population to the community occupying a graph-neighbour node.
//! Finding "the community at node N" is the coupling's per-event cost. Under
//! `DeliveryMode::Scan` it is a linear scan — O(Z) per delivery, O(Z²·A)
//! overall; under `DeliveryMode::Index` it is a `BTreeMap` lookup — O(log Z).
//! One alive community per node is enforced (a founding/refounding onto an
//! occupied node is skipped), so the two modes find the *same* community and
//! produce byte-identical worlds, differing only in speed — which is what
//! lets the sweep exhibit the O(Z²)→O(Z·log Z) crossover honestly.

use crate::config::{DeliveryMode, EventKind, NodeId, RoleHandle, SoundingConfig};
use crate::generate::seed_world;
use crate::streams;
use crate::world::{BioEntry, Community, Ruin, World};
use std::collections::BTreeMap;

/// Run the whole history with the index-based delivery (the shipping path).
pub fn run(config: &SoundingConfig) -> World {
    run_with(config, DeliveryMode::Index)
}

/// Run the whole `A`-epoch history under an explicit delivery mode and return
/// the baked world. The mode changes only the delivery *lookup*, never the
/// resulting world (one alive community per node makes scan and index agree).
pub fn run_with(config: &SoundingConfig, mode: DeliveryMode) -> World {
    let mut world = seed_world(config);
    // node -> the unique alive community occupying it (the invariant the
    // whole scan-vs-index comparison rests on).
    let mut node_index: BTreeMap<NodeId, usize> = world
        .communities
        .iter()
        .enumerate()
        .map(|(i, c)| (c.node, i))
        .collect();
    let founding_cap = (config.communities as usize).saturating_mul(3).max(1);

    let mut pending: BTreeMap<u32, Vec<(NodeId, f64)>> = BTreeMap::new();
    let mut ev = config.seed.derive_typed(streams::EVENTS).stream();
    let mut deliver_ev = config.seed.derive_typed(streams::DELIVER).stream();

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

            if pressure >= 2.0 {
                // Severe overshoot (usually a big delivery landed here):
                // collapse into a ruin.
                collapse(&mut world, &mut node_index, i, epoch, handle);
            } else if pressure >= 1.0 {
                // Overshoot: raid a neighbour (the coupling) — or collapse if
                // isolated. Growth runs right up to this threshold, so most
                // communities cycle grow -> overshoot -> raid steadily, which
                // is what exercises the coupling at volume.
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
                    collapse(&mut world, &mut node_index, i, epoch, handle);
                }
            } else {
                // Slack (pressure < 1.0): grow toward the overshoot threshold,
                // occasionally found a daughter on a *vacant* neighbour node.
                world.communities[i].population *= 1.05;
                world.communities[i].biography.push(BioEntry {
                    epoch,
                    event: EventKind::Grew,
                    actor: handle,
                });
                if ev.next_f64() < 0.02 && world.communities.len() < founding_cap {
                    found_daughter(&mut world, &mut node_index, i, epoch, &mut ev);
                }
            }
        }

        // Deliver displacements that arrive this epoch after the community
        // loop, so a same-epoch (lag == 0) raid is delivered before it ends.
        if let Some(arrivals) = pending.remove(&epoch) {
            for (node, pop) in arrivals {
                deliver(
                    &mut world,
                    &mut node_index,
                    mode,
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

/// Mark community `i` collapsed, leaving a ruin and vacating its node.
fn collapse(
    world: &mut World,
    node_index: &mut BTreeMap<NodeId, usize>,
    i: usize,
    epoch: u32,
    handle: RoleHandle,
) {
    world.communities[i].alive = false;
    let node = world.communities[i].node;
    world.communities[i].biography.push(BioEntry {
        epoch,
        event: EventKind::Collapsed,
        actor: handle,
    });
    world.ruins.push(Ruin { node, epoch });
    if node_index.get(&node) == Some(&i) {
        node_index.remove(&node);
    }
}

/// Find the alive community at `node` under the chosen mode. `Scan` walks the
/// communities linearly; `Index` reads the node index. One alive community
/// per node makes the two agree.
fn find_at_node(
    world: &World,
    node_index: &BTreeMap<NodeId, usize>,
    mode: DeliveryMode,
    node: NodeId,
) -> Option<usize> {
    match mode {
        DeliveryMode::Index => node_index
            .get(&node)
            .copied()
            .filter(|&i| world.communities[i].alive),
        DeliveryMode::Scan => world
            .communities
            .iter()
            .position(|c| c.alive && c.node == node),
    }
}

/// A displaced population arriving at a node: it flees into the community
/// there (located per `mode`), or refounds if the node is vacant.
#[allow(clippy::too_many_arguments)]
fn deliver(
    world: &mut World,
    node_index: &mut BTreeMap<NodeId, usize>,
    mode: DeliveryMode,
    ev: &mut hornvale_kernel::Stream,
    node: NodeId,
    pop: f64,
    epoch: u32,
) {
    let handle = RoleHandle(ev.next_u64() ^ ((node.0 as u64) << 32) ^ epoch as u64);
    match find_at_node(world, node_index, mode, node) {
        Some(i) => {
            world.communities[i].population += pop;
            world.communities[i].biography.push(BioEntry {
                epoch,
                event: EventKind::Fled,
                actor: handle,
            });
        }
        None => {
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

/// Found a daughter on a graph neighbour's node — but only if that node is
/// *vacant*, preserving the one-alive-community-per-node invariant.
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
    // Skip if the node already holds an alive community (invariant).
    if node_index
        .get(&node)
        .is_some_and(|&i| world.communities[i].alive)
    {
        return;
    }
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
