//! The derived synthetic state and its stable text digest.

use crate::config::{EventKind, NodeId, RoleHandle, SpeciesId};
use std::collections::BTreeMap;
use std::fmt::Write as _;

/// A synthetic species: placeholder trait weights, the two the dynamics read.
#[derive(Clone, Copy, Debug)]
pub struct SpeciesStub {
    /// Per-head resource need (scales the pressure threshold).
    pub carrying_need: f64,
    /// Global commonality weight (rank-abundance placeholder).
    pub frequency_weight: f64,
}

/// A connection-graph edge out of a node.
#[derive(Clone, Copy, Debug)]
pub struct Edge {
    /// The destination node.
    pub to: NodeId,
    /// Propagation lag in epochs (0 for a portal, more for a route).
    pub lag: u32,
    /// The edge's kind.
    pub kind: crate::config::EdgeKind,
}

/// One dated biography entry.
#[derive(Clone, Copy, Debug)]
pub struct BioEntry {
    /// The epoch the event occurred.
    pub epoch: u32,
    /// What happened.
    pub event: EventKind,
    /// The (lazily-expandable) individual the event implies.
    pub actor: RoleHandle,
}

/// A community: a persistent entity with a dated biography.
#[derive(Clone, Debug)]
pub struct Community {
    /// Which synthetic species.
    pub species: SpeciesId,
    /// Current population (an abstract head-count).
    pub population: f64,
    /// The graph node it occupies.
    pub node: NodeId,
    /// Its dated history.
    pub biography: Vec<BioEntry>,
    /// Whether it still stands (false once collapsed).
    pub alive: bool,
}

/// The trace a collapsed community leaves behind.
#[derive(Clone, Copy, Debug)]
pub struct Ruin {
    /// The node where it died.
    pub node: NodeId,
    /// The epoch of collapse.
    pub epoch: u32,
}

/// A fully-derived synthetic world (past baked, present at the final epoch).
#[derive(Clone, Debug)]
pub struct World {
    /// The synthetic species table (Y entries).
    pub species: Vec<SpeciesStub>,
    /// The synthetic communities (grows past Z as foundings fire).
    pub communities: Vec<Community>,
    /// The sparse connection graph (adjacency list).
    pub graph: BTreeMap<NodeId, Vec<Edge>>,
    /// The per-node synthetic carrying capacity.
    pub capacity: Vec<f64>,
    /// Ruins left by collapses.
    pub ruins: Vec<Ruin>,
}

/// A stable, byte-deterministic text rendering of every biography — the
/// determinism-test surface (timings are NOT part of this).
pub fn biography_digest(world: &World) -> String {
    let mut out = String::new();
    for (i, c) in world.communities.iter().enumerate() {
        let _ = write!(
            out,
            "c{i} sp{} node{} pop{} alive{}",
            c.species.0, c.node.0, c.population as i64, c.alive
        );
        for e in &c.biography {
            let _ = write!(out, " [{}:{:?}:{}]", e.epoch, e.event, e.actor.0);
        }
        out.push('\n');
    }
    out
}
