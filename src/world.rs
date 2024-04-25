//! # World
//!
//! Shared code used throughout the _Hornvale_ project.
//!
//! This crate contains code that is used in multiple parts of the project, such
//! as type definitions, traits, etc.
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

/// Common components.
pub mod components;
/// Corridors connect regions.
pub mod corridor;
/// A direction in 4D space.
pub mod direction;
/// An error type.
pub mod error;
/// Passages connect rooms.
pub mod passage;
/// Functions for querying the world.
pub mod query;
/// Regions are points in the 3D grid.
pub mod region;
/// Rooms are points in the 3D grid within a region.
pub mod room;
/// A 4D integer vector.
pub mod vector4d;
/// Additions to the world.
#[allow(clippy::module_inception)]
pub mod world;

/// The prelude.
pub mod prelude {
  pub use super::components::{adjectives::Adjectives, description::Description, name::Name, quit_flag::QuitFlag};
  pub use super::corridor::{
    direction::CorridorDirection, finder::CorridorFinder, kind::CorridorKind, origin::CorridorOrigin,
    terminus::CorridorTerminus,
  };
  pub use super::direction::{traits::descriptors::DirectionDescriptors, Direction};
  pub use super::error::WorldError;
  pub use super::passage::{
    condition::PassageCondition, direction::PassageDirection, is_a_passage::IsAPassage, kind::PassageKind,
  };
  pub use super::query as world_query;
  pub use super::region::{
    generator::{manager::RegionGeneratorManager, registry::RegionGeneratorRegistry, RegionGenerator},
    generators::{
      compass_rose::CompassRoseRegionGenerator, empty::EmptyRegionGenerator, fail::FailRegionGenerator,
      null::NullRegionGenerator,
    },
    is_a_region::IsARegion,
    Region,
  };
  pub use super::room::{is_a_room::IsARoom, Room};
  pub use super::vector4d::Vector4D;
  pub use super::world::traits::{
    get_region_and_room_containing_entity::GetRegionAndRoomContainingEntity, get_room_entity::GetRoomEntity,
    get_room_entity_containing_entity::GetRoomEntityContainingEntity,
    get_room_entity_region_and_room::GetRoomEntityRegionAndRoom,
    get_room_name_and_description::GetRoomNameAndDescription, get_room_passage_directions::GetRoomPassageDirections,
    get_room_passage_entities::GetRoomPassageEntities,
    get_room_passage_entity_in_direction::GetRoomPassageEntityInDirection, is_quit_flag_set::IsQuitFlagSet,
    set_quit_flag::SetQuitFlag,
  };
}
