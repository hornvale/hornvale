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
/// A direction in 4D space.
pub mod direction;
/// Additions to the World type.
pub mod world;

/// The prelude.
pub mod prelude {
  pub use crate::command::{form::CommandForm, function::CommandFunction, Command};
  pub use crate::component::{
    adjectives::Adjectives, description::Description, name::Name, quit_flag::QuitFlag, region::Region, room::Room,
  };
  pub use crate::direction::Direction;
  pub use crate::world::traits::is_quit_flag_set::IsQuitFlagSet;
}
