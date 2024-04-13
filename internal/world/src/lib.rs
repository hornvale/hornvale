//! # World
//!
//! `world` is tasked with creating and managing the game world of _Hornvale_.
//!
//! The world is an infinite, procedurally-generated 3D grid. Within this, each
//! point in the grid is a region. Each region is connected to its neighbors by
//! corridors.
//!
//! Associated with each region are rooms, which are points within the region.
//! A room may have one or more passages that connect it to other rooms. This
//! passage and the relationship between the rooms is not necessarily spatially
//! coherent, i.e. a passage may connect two rooms that are not adjacent.
//!
//! Note that there is not an actual "passage" struct or "corridor" struct, and
//! the "region" and "room" structs only contain their spatial coordinates; as
//! this is an ECS architecture, it would be more appropriate to think of these
//! as something like:
//! - `Passage` = `(Region, Room, PassageDirection, PassageKind)`
//! - `Corridor` = `(Region, CorridorDirection, CorridorKind)`
//! - `Region` = everything tagged with a `Region` component
//! - `Room` = everything tagged with a `Room` component
//!
//! In addition, this crate provides some basic components that can be used to
//! describe regions, rooms, and other entities.

/// Some basic components.
pub mod component;
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
  pub use crate::component::{description::Description, name::Name};
  pub use crate::corridor::{
    direction::CorridorDirection, finder::CorridorFinder, kind::CorridorKind, origin::CorridorOrigin,
    terminus::CorridorTerminus,
  };
  pub use crate::error::WorldError;
  pub use crate::passage::{condition::PassageCondition, direction::PassageDirection, kind::PassageKind};
  pub use crate::region::{
    generator::{manager::RegionGeneratorManager, registry::RegionGeneratorRegistry, RegionGenerator},
    generators::{
      compass_rose::CompassRoseRegionGenerator, empty::EmptyRegionGenerator, fail::FailRegionGenerator,
      null::NullRegionGenerator,
    },
    Region,
  };
  pub use crate::room::Room;
}
