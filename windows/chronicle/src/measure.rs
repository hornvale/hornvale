//! The three instrumented cost directions. Timing uses `std::time::Instant`
//! under a scoped allow — this is the BENCHMARK HARNESS, not sim logic; the
//! core loop (`simulate.rs`) contains no wall-clock.

use crate::config::{DeliveryMode, NodeId, RoleHandle, SoundingConfig};
use crate::simulate::run_with;
use crate::streams;
use crate::world::{BioEntry, Community, Edge, Ruin, World};
use hornvale_kernel::Seed;

/// Metrics for the genesis-bake direction.
/// type-audit: bare-ok(diagnostic-value: nanos), bare-ok(count: peak_bytes), bare-ok(count: total_bio_bytes), bare-ok(count: communities), bare-ok(count: events)
#[derive(Clone, Copy, Debug)]
pub struct BakeMetrics {
    /// Wall-time to derive the whole history, in nanoseconds.
    pub nanos: u128,
    /// A proxy for peak memory: bytes held by the final world, INCLUDING the
    /// connection graph and ruins (the structures the density axes inflate).
    pub peak_bytes: usize,
    /// Total bytes of biography (Σ entries × size_of::<BioEntry>).
    pub total_bio_bytes: usize,
    /// Final community count.
    pub communities: usize,
    /// Total events appended.
    pub events: usize,
}

/// Derive the world with the shipping (index) delivery and its bake metrics.
pub fn bake(config: &SoundingConfig) -> (World, BakeMetrics) {
    bake_with(config, DeliveryMode::Index)
}

/// Derive the world under an explicit delivery mode and measure the bake.
/// `Scan` and `Index` produce byte-identical worlds; only `nanos` differs —
/// this is the pair the sweep contrasts to exhibit O(Z²) vs O(Z·log Z).
pub fn bake_with(config: &SoundingConfig, mode: DeliveryMode) -> (World, BakeMetrics) {
    #[allow(clippy::disallowed_types)] // benchmark harness: measuring the loop, not sim logic
    let start = std::time::Instant::now();
    let world = run_with(config, mode);
    #[allow(clippy::disallowed_types)] // benchmark harness
    let nanos = start.elapsed().as_nanos();

    let events: usize = world.communities.iter().map(|c| c.biography.len()).sum();
    let total_bio_bytes = events * std::mem::size_of::<BioEntry>();
    let communities = world.communities.len();
    let graph_bytes: usize = world
        .graph
        .values()
        .map(|v| v.len() * std::mem::size_of::<Edge>())
        .sum::<usize>()
        + world.graph.len() * (std::mem::size_of::<NodeId>() + std::mem::size_of::<Vec<Edge>>());
    let peak_bytes = communities * std::mem::size_of::<Community>()
        + total_bio_bytes
        + world.capacity.len() * std::mem::size_of::<f64>()
        + graph_bytes
        + world.ruins.len() * std::mem::size_of::<Ruin>();
    (
        world,
        BakeMetrics {
            nanos,
            peak_bytes,
            total_bio_bytes,
            communities,
            events,
        },
    )
}

/// Hot-loop average nanoseconds per read op (sample a biography + expand a
/// role-handle into a synthetic persona hash). Returns ns/op.
/// type-audit: bare-ok(count: ops), bare-ok(ratio: return)
pub fn read_nanos_per_op(world: &World, ops: u32, seed: Seed) -> f64 {
    if world.communities.is_empty() || ops == 0 {
        return 0.0;
    }
    let mut s = seed.stream();
    #[allow(clippy::disallowed_types)] // benchmark harness
    let start = std::time::Instant::now();
    let mut sink: u64 = 0;
    for _ in 0..ops {
        let i = s.range_u32(0, world.communities.len() as u32 - 1) as usize;
        let bio = &world.communities[i].biography;
        if let Some(e) = bio.last() {
            sink ^= expand_handle(e.actor); // "expand a role-handle to a persona"
            sink ^= e.epoch as u64;
        }
    }
    #[allow(clippy::disallowed_types)] // benchmark harness
    let nanos = start.elapsed().as_nanos();
    std::hint::black_box(sink);
    nanos as f64 / ops as f64
}

/// A synthetic role-handle → persona expansion (a few hashes; stands in for
/// deriving a name/traits/drives from the handle).
fn expand_handle(h: RoleHandle) -> u64 {
    let mut x = h.0.wrapping_mul(0x9E3779B97F4A7C15);
    x ^= x >> 29;
    x = x.wrapping_mul(0xBF58476D1CE4E5B9);
    x ^ (x >> 32)
}

/// Time deriving one graph-neighbourhood forward `delta_epochs` under the
/// intervention (a killed community). The living-present cost.
/// type-audit: bare-ok(count: delta_epochs), bare-ok(diagnostic-value: return)
pub fn replay_nanos(
    world: &World,
    region: NodeId,
    delta_epochs: u32,
    config: &SoundingConfig,
) -> u128 {
    #[allow(clippy::disallowed_types)] // benchmark harness
    let start = std::time::Instant::now();
    let _ = replay_event_count(world, region, delta_epochs, config, true);
    #[allow(clippy::disallowed_types)] // benchmark harness
    start.elapsed().as_nanos()
}

/// The deterministic core of a replay: derive the `region`'s neighbourhood
/// forward `delta_epochs` under the same churning dynamics (grow → overshoot →
/// raid a *local* neighbour → collapse), and return the number of events
/// appended. `intervene` kills one neighbour at the start (the player delta);
/// because the local raid couples the neighbourhood, that kill *propagates* —
/// the event count with and without the intervention genuinely differ. Pure.
/// type-audit: bare-ok(count: delta_epochs), bare-ok(flag: intervene), bare-ok(count: return)
pub fn replay_event_count(
    world: &World,
    region: NodeId,
    delta_epochs: u32,
    config: &SoundingConfig,
    intervene: bool,
) -> usize {
    // Gather the region: `region` plus its graph neighbours.
    let mut nodes: Vec<NodeId> = vec![region];
    if let Some(edges) = world.graph.get(&region) {
        for e in edges {
            if !nodes.contains(&e.to) {
                nodes.push(e.to);
            }
        }
    }
    let mut local: Vec<Community> = world
        .communities
        .iter()
        .filter(|c| c.alive && nodes.contains(&c.node))
        .cloned()
        .collect();
    if intervene && local.len() > 1 {
        local[1].alive = false; // the player intervention
    }
    let mut ev = config.seed.derive(streams::REPLAY).stream();
    let mut appended = 0usize;
    for _epoch in config.epochs..config.epochs.saturating_add(delta_epochs) {
        let m = local.len();
        for i in 0..m {
            if !local[i].alive {
                continue;
            }
            let cap = world.capacity[local[i].node.0 as usize % world.capacity.len().max(1)];
            let need = world.species[local[i].species.0 as usize].carrying_need;
            let pressure = (local[i].population * need) / cap.max(1.0);
            if pressure >= 2.0 {
                local[i].alive = false;
                appended += 1;
            } else if pressure >= 1.0 {
                // Raid a local neighbour — the coupling that makes the
                // intervention propagate through the neighbourhood.
                let taken = local[i].population * 0.25;
                local[i].population -= taken;
                if let Some(j) = (0..m).find(|&j| j != i && local[j].alive) {
                    local[j].population += taken;
                }
                appended += 1;
            } else {
                local[i].population *= 1.05;
                appended += 1;
            }
            let _ = ev.next_u64();
        }
    }
    appended
}
