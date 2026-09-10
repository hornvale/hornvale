//! Bounded Waterworld migration and propagation values.

use crate::waterworld::{VentState, WaterFields, WaterSubstrate};
use hornvale_kernel::{Geosphere, Vertex};

/// Counts work at the bounded Waterworld loops that actually perform it.
/// type-audit: bare-ok(count: candidate_ring), bare-ok(count: stock), bare-ok(count: propagation), bare-ok(count: refresh), bare-ok(count: observation)
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct WaterWorkCounters {
    /// Candidate-ring entries inspected or selected.
    pub candidate_ring: usize,
    /// Aggregate stock rows derived.
    pub stock: usize,
    /// Current-following neighbour candidates inspected.
    pub propagation: usize,
    /// Ambient field rows refreshed for a temporal snapshot.
    pub refresh: usize,
    /// Observation rows consumed; Task 4 owns that loop.
    pub observation: usize,
}

/// Anchor plus at most four one-hop marine candidates.
/// plumb: pending(wave-1)
pub(crate) const VENT_CANDIDATE_LIMIT: usize = 5;

/// Build the stable candidate ring in ascending vertex-key order.
pub(crate) fn build_vent_candidate_ring(
    geosphere: &Geosphere,
    marine_vertices: &[Vertex],
    anchor: Vertex,
    candidate_count: &mut usize,
) -> Vec<Vertex> {
    let mut candidates = geosphere
        .neighbors(anchor)
        .iter()
        .copied()
        .inspect(|_| *candidate_count += 1)
        .filter(|candidate| marine_vertices.binary_search(candidate).is_ok())
        .take(VENT_CANDIDATE_LIMIT - 1)
        .collect::<Vec<_>>();
    candidates.push(anchor);
    candidates.sort();
    candidates
}

/// Select zero or one influence position without drawing or moving substrate.
pub(crate) fn select_vent_position(
    candidate_ring: &[Vertex],
    state: VentState,
    cycle_index: i64,
) -> Option<Vertex> {
    if candidate_ring.is_empty() || matches!(state, VentState::Absent | VentState::Failed) {
        return None;
    }
    let state_step = match state {
        VentState::Nascent => 0,
        VentState::Active => 1,
        VentState::Weakening => 2,
        VentState::Absent | VentState::Failed => unreachable!(),
    };
    let index = (cycle_index + state_step).rem_euclid(candidate_ring.len() as i64) as usize;
    Some(candidate_ring[index])
}

/// One sample in a bounded vertical Waterworld trajectory.
/// type-audit: bare-ok(diagnostic-value: depth_m), bare-ok(diagnostic-value: current)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct WaterTrajectorySample {
    /// Stable substrate vertex.
    pub vertex: Vertex,
    /// Sample depth in metres.
    pub depth_m: f64,
    /// Existing current vector.
    pub current: [f64; 3],
}

/// Sparse Waterworld propagation state.
/// type-audit: bare-ok(count: candidate_count), bare-ok(count: accepted_count), bare-ok(ratio: transported_influence)
#[derive(Clone, Debug, Default, PartialEq)]
pub struct WaterPropagation {
    /// Ordered accepted samples.
    pub samples: Vec<WaterTrajectorySample>,
    /// Number of substrate candidates examined.
    pub candidate_count: usize,
    /// Number of samples accepted from candidates.
    pub accepted_count: usize,
    /// Transported aggregate influence aligned with substrate rows.
    pub transported_influence: Vec<f64>,
    /// Actual-loop work performed for this propagation read.
    pub counters: WaterWorkCounters,
}

impl WaterPropagation {
    /// Build ordered, bounded samples directly at the actual substrate loop.
    pub fn from_substrate(substrate: &[WaterSubstrate], fields: &[WaterFields]) -> Self {
        let mut samples = Vec::new();
        let mut candidate_count = 0;
        for (sample, field) in substrate.iter().zip(fields) {
            candidate_count += 1;
            if field.current != [0.0; 3] {
                samples.push(WaterTrajectorySample {
                    vertex: sample.vertex,
                    depth_m: sample.depth_m,
                    current: field.current,
                });
            }
        }
        Self {
            accepted_count: samples.len(),
            samples,
            candidate_count,
            transported_influence: vec![0.0; substrate.len()],
            counters: WaterWorkCounters {
                refresh: candidate_count,
                ..WaterWorkCounters::default()
            },
        }
    }

    /// Derive a finite ordered transport readout from present current fields.
    /// type-audit: bare-ok(ratio: local_influence), bare-ok(count: max_hops), bare-ok(ratio: attenuation)
    pub fn transport(
        geosphere: &Geosphere,
        substrate: &[WaterSubstrate],
        fields: &[WaterFields],
        local_influence: &[f64],
        max_hops: usize,
        attenuation: f64,
    ) -> Self {
        assert_eq!(substrate.len(), local_influence.len());
        assert_eq!(substrate.len(), fields.len());
        assert!((0.0..=1.0).contains(&attenuation));
        let mut transported_influence = vec![0.0; substrate.len()];
        let mut samples = Vec::new();
        let mut propagation_count = 0;
        for (source, &source_influence) in local_influence.iter().enumerate() {
            let mut influence = source_influence;
            if influence <= 0.0 || max_hops == 0 {
                continue;
            }
            let mut current_index = source;
            let mut previous_vertex = None;
            for _ in 0..max_hops {
                let vertex = substrate[current_index].vertex;
                let position = geosphere.position(vertex);
                let current = fields[current_index].current;
                let mut best = None::<(f64, Vertex, usize)>;
                for &candidate in geosphere.neighbors(vertex) {
                    propagation_count += 1;
                    if Some(candidate) == previous_vertex {
                        continue;
                    }
                    let Some(candidate_index) = seabed_index(substrate, candidate) else {
                        continue;
                    };
                    let candidate_position = geosphere.position(candidate);
                    let direction = [
                        candidate_position[0] - position[0],
                        candidate_position[1] - position[1],
                        candidate_position[2] - position[2],
                    ];
                    let alignment = current[0] * direction[0]
                        + current[1] * direction[1]
                        + current[2] * direction[2];
                    let choice = (alignment, candidate, candidate_index);
                    if best.is_none_or(|prior| {
                        alignment.total_cmp(&prior.0).is_gt()
                            || (alignment == prior.0 && candidate < prior.1)
                    }) {
                        best = Some(choice);
                    }
                }
                let Some((_, next_vertex, next_index)) = best else {
                    break;
                };
                influence *= attenuation * (0.5 + 0.5 * current[0].clamp(-1.0, 1.0));
                if influence <= 0.0 {
                    break;
                }
                transported_influence[next_index] = (transported_influence[next_index]
                    + (1.0 - transported_influence[next_index]) * influence)
                    .clamp(0.0, 1.0);
                samples.push(WaterTrajectorySample {
                    vertex: next_vertex,
                    depth_m: substrate[next_index].depth_m,
                    current: fields[current_index].current,
                });
                previous_vertex = Some(vertex);
                current_index = next_index;
            }
        }
        Self {
            candidate_count: propagation_count,
            accepted_count: samples.len(),
            transported_influence,
            counters: WaterWorkCounters {
                propagation: propagation_count,
                ..WaterWorkCounters::default()
            },
            samples,
        }
    }
}

fn seabed_index(substrate: &[WaterSubstrate], vertex: Vertex) -> Option<usize> {
    let start = substrate.partition_point(|sample| sample.vertex < vertex);
    substrate[start..]
        .iter()
        .take_while(|sample| sample.vertex == vertex)
        .position(|sample| sample.is_seabed)
        .map(|offset| start + offset)
}
