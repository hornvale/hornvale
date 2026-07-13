#![warn(missing_docs)]
//! The vessel window: possess an agent minted from the world and walk the
//! frozen locale mesh through a read-only verb loop (The Seam, Chunk 0 of
//! The Walk).

mod agent;
mod focalize;
mod knowledge;
pub mod streams;
mod vantage;
pub use agent::{Agent, AgentId, mint_flagship, walk_depth};
pub use focalize::*;
pub use knowledge::*;
pub use streams::stream_labels;
pub use vantage::*;

/// Why a possession could not begin or proceed.
/// type-audit: bare-ok(prose: NoSpecies.0), bare-ok(prose: NoPosition.0), bare-ok(prose: Build.0)
#[derive(Debug, Clone, PartialEq)]
pub enum VesselError {
    /// The world has no settlements to mint from.
    NoSettlement,
    /// The settlement's species is unknown to the registry.
    NoSpecies(String),
    /// The settlement has no committed position fact.
    NoPosition(String),
    /// The locale window could not describe a room.
    Locale(hornvale_locale::LocaleError),
    /// Building a coarse-world view failed (worldgen).
    Build(String),
}

impl std::fmt::Display for VesselError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VesselError::NoSettlement => write!(f, "no settlement to mint an agent from"),
            VesselError::NoSpecies(m) => write!(f, "no species known for {m}"),
            VesselError::NoPosition(m) => write!(f, "no position: {m}"),
            VesselError::Locale(e) => write!(f, "locale: {e}"),
            VesselError::Build(m) => write!(f, "building the coarse world: {m}"),
        }
    }
}
