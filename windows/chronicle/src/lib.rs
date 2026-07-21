//! The Sounding — a computational feasibility spike of the derived-history
//! engine (frontier "the living community"). Synthetic inputs, placeholder
//! dynamics, real data structures and constitution — enough to measure the
//! loop's SCALING (especially the inter-community coupling), not the engine.
#![warn(missing_docs)]

mod config;
mod generate;
mod simulate;
mod world;

pub mod measure;
pub mod sweep;

pub use config::{
    DeliveryMode, EdgeKind, EventKind, NodeId, RoleHandle, SoundingConfig, SpeciesId,
};
pub use simulate::{run, run_with};
pub use world::{
    BioEntry, Census, Community, Edge, Ruin, SpeciesStub, World, biography_digest, census,
};
