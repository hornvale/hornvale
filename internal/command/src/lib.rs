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
/// The context in which a command is executed.
pub mod context;
/// An error type.
pub mod error;
/// A function that can be executed as a command.
pub mod function;
/// A flag to set when the game should quit.
pub mod quit_flag;
/// A registry for commands.
pub mod registry;

/// The prelude.
pub mod prelude {
  pub use super::command::Command;
  pub use super::commands::{fail::FailCommand, no_op::NoOpCommand, quit::QuitCommand};
  pub use super::context::CommandContext;
  pub use super::error::CommandError;
  pub use super::function::CommandFunction;
  pub use super::quit_flag::QuitFlag;
  pub use super::registry::CommandRegistry;
}
