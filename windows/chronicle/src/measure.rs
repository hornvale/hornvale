//! The three instrumented cost directions. Timing uses `std::time::Instant`
//! under a scoped allow — this is the BENCHMARK HARNESS, not sim logic; the
//! core loop (`simulate.rs`) contains no wall-clock.

use crate::config::{NodeId, RoleHandle, SoundingConfig};
use crate::simulate::run;
use crate::world::{BioEntry, World};
use hornvale_kernel::Seed;

/// Metrics for the genesis-bake direction.
#[derive(Clone, Copy, Debug)]
pub struct BakeMetrics {
    /// Wall-time to derive the whole history, in nanoseconds.
    pub nanos: u128,
    /// A cheap proxy for peak memory: bytes held by the final world.
    pub peak_bytes: usize,
    /// Total bytes of biography (Σ entries × size_of::<BioEntry>).
    pub total_bio_bytes: usize,
    /// Final community count.
    pub communities: usize,
    /// Total events appended.
    pub events: usize,
}

/// Derive the world and its bake metrics.
pub fn bake(config: &SoundingConfig) -> (World, BakeMetrics) {
    #[allow(clippy::disallowed_types)] // benchmark harness: measuring the loop, not sim logic
    let start = std::time::Instant::now();
    let world = run(config);
    #[allow(clippy::disallowed_types)] // benchmark harness
    let nanos = start.elapsed().as_nanos();

    let events: usize = world.communities.iter().map(|c| c.biography.len()).sum();
    let total_bio_bytes = events * std::mem::size_of::<BioEntry>();
    // Hoisted before the move below (world.communities.len() can't follow it).
    let communities = world.communities.len();
    let peak_bytes = communities * std::mem::size_of::<crate::world::Community>()
        + total_bio_bytes
        + world.capacity.len() * std::mem::size_of::<f64>();
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

/// Derive one graph-neighbourhood forward `delta_epochs` from the baked
/// world, applying a synthetic intervention delta (kill one neighbour), and
/// return the wall-time. The living-present cost.
pub fn replay_nanos(
    world: &World,
    region: NodeId,
    delta_epochs: u32,
    config: &SoundingConfig,
) -> u128 {
    #[allow(clippy::disallowed_types)] // benchmark harness
    let start = std::time::Instant::now();
    let _ = replay_event_count(world, region, delta_epochs, config);
    #[allow(clippy::disallowed_types)] // benchmark harness
    start.elapsed().as_nanos()
}

/// The deterministic core of a replay: how many events does deriving the
/// `region`'s neighbourhood forward `delta_epochs` produce (with one
/// neighbour killed as the intervention). Pure — the assertion surface.
pub fn replay_event_count(
    world: &World,
    region: NodeId,
    delta_epochs: u32,
    config: &SoundingConfig,
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
    // Local copy of the communities on those nodes; kill the first neighbour
    // (the synthetic player intervention).
    let mut local: Vec<crate::world::Community> = world
        .communities
        .iter()
        .filter(|c| c.alive && nodes.contains(&c.node))
        .cloned()
        .collect();
    if local.len() > 1 {
        local[1].alive = false; // the intervention delta
    }
    // Replay the same pressure rule forward, counting appended events.
    let mut ev = config.seed.derive("chronicle/replay").stream();
    let mut appended = 0usize;
    for epoch in config.epochs..config.epochs.saturating_add(delta_epochs) {
        for c in local.iter_mut().filter(|c| c.alive) {
            let cap = world.capacity[c.node.0 as usize % world.capacity.len().max(1)];
            let need = world.species[c.species.0 as usize].carrying_need;
            let pressure = (c.population * need) / cap.max(1.0);
            let _ = RoleHandle(ev.next_u64());
            if pressure < 0.6 {
                c.population *= 1.05;
                c.biography.push(BioEntry {
                    epoch,
                    event: crate::config::EventKind::Grew,
                    actor: RoleHandle(ev.next_u64()),
                });
                appended += 1;
            } else if pressure > 1.0 {
                c.alive = false;
                appended += 1;
            }
        }
    }
    appended
}
