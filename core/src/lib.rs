//! # Core
//!
//! Shared code used throughout the _Hornvale_ project.
//!
//! This crate contains code that is used in multiple parts of the project, such
//! as type definitions, traits, etc.

/// The Command trait.
pub mod command;
/// Common components.
pub mod components;
/// Corridors connect regions.
pub mod corridor;
/// A direction in 4D space.
pub mod direction;
/// Passages connect rooms.
pub mod passage;
/// The region component, which can be used to determine the region an entity is in.
pub mod region;
/// The room component, which can be used to determine the room an entity is in.
pub mod room;
/// A 4-D vector.
pub mod vector4d;
/// Additions to the World type.
pub mod world;

/// The prelude.
pub mod prelude {
  pub use super::command::{form::CommandForm, function::CommandFunction, Command};
  pub use super::components::{adjectives::Adjectives, description::Description, name::Name, quit_flag::QuitFlag};
  pub use super::corridor::{
    components::{is_corridor_origin::IsCorridorOrigin, is_corridor_terminus::IsCorridorTerminus},
    direction::CorridorDirection,
    kind::CorridorKind,
  };
  pub use super::direction::{traits::descriptors::DirectionDescriptors, Direction};
  pub use super::passage::{
    components::is_a_passage::IsAPassage, condition::PassageCondition, direction::PassageDirection, kind::PassageKind,
  };
  pub use super::region::{components::is_a_region::IsARegion, Region};
  pub use super::room::{components::is_a_room::IsARoom, Room};
  pub use super::vector4d::Vector4D;
  pub use super::world::traits::{
    get_region_and_room_containing_entity::GetRegionAndRoomContainingEntity, get_room_entity::GetRoomEntity,
    get_room_entity_containing_entity::GetRoomEntityContainingEntity,
    get_room_name_and_description::GetRoomNameAndDescription, get_room_passage_directions::GetRoomPassageDirections,
    get_room_passage_entities::GetRoomPassageEntities,
    get_room_passage_entity_in_direction::GetRoomPassageEntityInDirection,
    get_room_region_and_room::GetRoomRegionAndRoom, is_quit_flag_set::IsQuitFlagSet,
  };
}
