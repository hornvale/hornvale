//! Bounded Waterworld propagation values, populated in Stage 3.

use crate::waterworld::{WaterFields, WaterSubstrate};
use hornvale_kernel::Vertex;

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
