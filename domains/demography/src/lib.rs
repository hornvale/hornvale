//! Hornvale demography domain: a carrying-capacity field derived from climate
//! and terrain, and the flow-condensation that reads discrete settlements off
//! it as conserved attractors of a population flow. Kernel-only: the
//! composition root supplies each cell's bare climate/terrain inputs; this
//! crate never imports a climate or terrain crate.
//!
//! Condensation is the same field-to-fact projection the codebase performs
//! for biomes (Whittaker classification) and rivers (`terrain::drainage`);
//! lifting all three onto one kernel primitive is future work — see the
//! design spec's scope boundary.
#![warn(missing_docs)]

pub mod carrying_capacity;
pub mod condense;
pub mod flow;
pub use carrying_capacity::{CarryingInput, carrying_capacity};
pub use condense::{Condensation, condense};
pub use flow::{Flow, flow};
