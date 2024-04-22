//! # Core
//!
//! Shared code used throughout the _Hornvale_ project.
//!
//! This crate contains code that is used in multiple parts of the project, such
//! as type definitions, traits, etc.

/// Common components.
pub mod component;
/// A direction in 4D space.
pub mod direction;

/// The prelude.
pub mod prelude {
  pub use crate::component::{adjectives::Adjectives, description::Description, name::Name};
  pub use crate::direction::Direction;
}
