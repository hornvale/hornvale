//! # Input
//!
//! This crate provides functionality for reading input from the player. It
//! is intentionally minimalistic, containing only the necessary components
//! for reading input, splitting it into one or more distinct strings that
//! can be parsed, and enqueueing those strings for parsing.
//!
//! At present, I envision a few different types of input, which are handled
//! by different parsing systems:
//! - Commands: These are the primary way that the player interacts with the
//!   game. Commands are parsed and executed by the command system.
//! - Dialog: Dialog is used for conversations between the player and NPCs.
//!   Dialog is parsed and executed by the dialog system.
//! - Scripting: Scripts can be used in various places throughout the system.
//!   Scripts are parsed and executed by the scripting system.
//! - Magic Words: Magic words are special commands that can be used to debug,
//!   test, develop, or otherwise interact with the game in ways that are not
//!   available to the player. Magic words are parsed and executed by the magic
//!   word system.
//! - Questions: There are two different types of questions: those that are
//!   asked by the game and those that are asked by the player. Questions are
//!   parsed and executed by the question system.

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
  pub use crate::queue::InputQueue;
  pub use crate::reader::InputReader;
  pub use crate::source::InputSource;
  pub use crate::sources::{
    generic::{FileSource, GenericSource, StdinSource},
    string::StringSource,
  };
}
