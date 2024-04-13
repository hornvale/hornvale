//! # World
//!
//! `world` is tasked with creating and managing the game world of _Hornvale_.
//!
//! The world is an infinite, procedurally-generated 3D grid of regions, each
//! of which is a 3D grid of rooms. These regions are loaded and unloaded as
//! the player moves around the world.
//!
//! Passage between regions is handled by corridors; a region may have any of
//! its six faces connected to any of the six faces of its neighboring regions.
//! These corridors are generated deterministically based on the coordinates of
//! the regions they connect.

/// Corridors connect regions.
pub mod corridor;
/// An error type.
pub mod error;
/// Passages connect rooms.
pub mod passage;
/// Regions are points in the 3D grid.
pub mod region;
/// Rooms are points in the 3D grid within a region.
pub mod room;

/// The prelude.
pub mod prelude {
  pub use crate::corridor::{direction::CorridorDirection, finder::CorridorFinder, kind::CorridorKind};
  pub use crate::error::WorldError;
  pub use crate::passage::{condition::PassageCondition, kind::PassageKind};
  pub use crate::region::{
    generator::{manager::RegionGeneratorManager, registry::RegionGeneratorRegistry, RegionGenerator},
    Region,
  };
  pub use crate::room::Room;
}
