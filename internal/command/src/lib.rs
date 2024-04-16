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
/// A flag to set when the game should quit.
pub mod quit_flag;
/// A registry for commands.
pub mod registry;

/// The prelude.
pub mod prelude {
  pub use crate::command::{context::CommandContext, function::CommandFunction, Command};
  pub use crate::commands::{fail::FailCommand, no_op::NoOpCommand};
  pub use crate::error::CommandError;
  pub use crate::quit_flag::QuitFlag;
  pub use crate::registry::CommandRegistry;
}
