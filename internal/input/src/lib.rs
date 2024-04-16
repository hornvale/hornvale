//! # Input
//!
//! This crate provides functionality for reading input from the player. It
//! is intentionally minimalistic, containing only the necessary components
//! for reading input, splitting it into one or more distinct strings that
//! can be parsed, and enqueueing those strings for parsing.

/// An error type for input.
pub mod error;
/// A double-ended queue for storing input.
pub mod queue;
/// An object that manages the process of reading and storing input.
pub mod reader;
/// A source trait for defining where input comes from.
pub mod source;
/// Some built-in input sources.
pub mod sources;

/// The prelude.
pub mod prelude {
  pub use crate::error::InputError;
  pub use crate::queue::Queue;
  pub use crate::reader::Reader;
  pub use crate::source::Source;
  pub use crate::sources::{
    generic::{FileSource, GenericSource, StdinSource},
    string::StringSource,
  };
}
