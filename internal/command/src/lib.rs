//! # Command
//!
//! The command module provides a way to define and execute commands in the
//! game.
//!
//! This crate is intentionally minimalistic, providing only the core traits
//! and a few basic commands.

/// The `Command` trait.
pub mod command;
/// A collection of core commands.
pub mod commands;
/// An error type.
pub mod error;
/// Things relating to objects, direct and indirect.
pub mod object;
/// Syntax for the commands.
pub mod syntax;

/// The prelude.
pub mod prelude {
  pub use crate::command::Command;
  pub use crate::commands::{fail::FailCommand, no_op::NoOpCommand};
  pub use crate::error::CommandError;
  pub use crate::object::qualifier::ObjectQualifier;
  pub use crate::syntax::{element::SyntaxElement, pattern::SyntaxPattern};
}
