//! Bounded Waterworld propagation values, populated in Stage 3.

/// One sample in a bounded vertical Waterworld trajectory.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct WaterTrajectorySample;

/// Sparse Waterworld propagation state.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct WaterPropagation;
