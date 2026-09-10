//! Bounded Waterworld migration and propagation values.

use crate::waterworld::{VentState, WaterFields, WaterSubstrate};
use hornvale_kernel::{Geosphere, Vertex};

/// Anchor plus at most four one-hop marine candidates.
/// plumb: pending(wave-1)
pub(crate) const VENT_CANDIDATE_LIMIT: usize = 5;

/// Build the stable candidate ring in ascending vertex-key order.
pub(crate) fn build_vent_candidate_ring(
    geosphere: &Geosphere,
    marine_vertices: &[Vertex],
    anchor: Vertex,
) -> Vec<Vertex> {
    let mut candidates = geosphere
        .neighbors(anchor)
        .iter()
        .copied()
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
/// type-audit: bare-ok(count: candidate_count), bare-ok(count: accepted_count)
#[derive(Clone, Debug, Default, PartialEq)]
pub struct WaterPropagation {
    /// Ordered accepted samples.
    pub samples: Vec<WaterTrajectorySample>,
    /// Number of substrate candidates examined.
    pub candidate_count: usize,
    /// Number of samples accepted from candidates.
    pub accepted_count: usize,
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
        }
    }
}
