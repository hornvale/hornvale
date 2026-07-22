//! Topology: the world's derived transport connection graph — a legible,
//! byte-identical structure over the geosphere's cells (routes, not roads).
#![warn(missing_docs)]

pub mod graph;

pub use graph::{ConnectionGraph, Edge, EdgeKind};
