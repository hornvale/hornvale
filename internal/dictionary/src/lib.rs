//! # Dictionary
//!
//! This module provides dictionary functionality for the _Hornvale_ library.
//! It is used to store and retrieve words, phrases, and other language-related
//! data.
//!

/// An Adjectives component for listing adjectives relevant to a noun.
pub mod adjectives;
/// A Description component for storing the default description of an entity.
pub mod description;
/// A Name component for storing the canonical name of an entity.
pub mod name;

/// The prelude.
pub mod prelude {
  pub use super::adjectives::Adjectives;
  pub use super::description::Description;
  pub use super::name::Name;
}
