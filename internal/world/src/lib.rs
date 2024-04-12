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
/// A 3-dimensional point in the world.
pub mod point;
/// Regions are 3-dimensional grids of rooms.
pub mod region;

/// The prelude.
pub mod prelude {
  pub use crate::corridor::direction::CorridorDirection;
  pub use crate::corridor::finder::CorridorFinder;
  pub use crate::corridor::kind::CorridorKind;
  pub use crate::point::Point;
}
