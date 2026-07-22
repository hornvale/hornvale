//! Topology: the world's derived transport connection graph — a legible,
//! byte-identical structure over the geosphere's cells (routes, not roads).
#![warn(missing_docs)]

pub mod graph;
pub mod route;

pub use graph::{ConnectionGraph, Edge, EdgeKind};
pub use route::{CellRoute, least_cost};
