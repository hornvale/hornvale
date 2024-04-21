//! # Core
//!
//! Shared code used throughout the _Hornvale_ project.
//!
//! This crate contains code that is used in multiple parts of the project, such
//! as type definitions, traits, etc.

/// The Command trait.
pub mod command;
/// Common components.
pub mod component;
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
  pub use super::component::{adjectives::Adjectives, description::Description, name::Name, quit_flag::QuitFlag};
  pub use super::corridor::{direction::CorridorDirection, kind::CorridorKind};
  pub use super::direction::Direction;
  pub use super::region::{components::is_a_region::IsARegion, Region};
  pub use super::room::{components::is_a_room::IsARoom, Room};
  pub use super::vector4d::Vector4D;
  pub use super::world::traits::is_quit_flag_set::IsQuitFlagSet;
}
