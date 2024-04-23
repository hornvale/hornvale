//! # Command
//!
//! The command module provides a way to define and execute commands in the
//! game.
//!
//! This crate is intentionally minimalistic, providing only the core traits
//! and a few basic commands.

/// The `Command` trait and related concepts.
pub mod command;
/// A collection of core commands.
pub mod commands;
/// General components of use.
pub mod components;
/// An error type.
pub mod error;
/// A registry for commands.
pub mod registry;
/// Additions to the world to ease operations.
pub mod world;

/// The prelude.
pub mod prelude {
  pub use super::command::{
    arity::CommandArity, function::CommandFunction, modifier::CommandModifier, syntax::CommandSyntax, Command,
  };
  pub use super::commands::{fail::FailCommand, no_op::NoOpCommand, quit::QuitCommand};
  pub use super::components::quit_flag::QuitFlag;
  pub use super::error::CommandError;
  pub use super::registry::CommandRegistry;
  pub use super::world::traits::{is_quit_flag_set::IsQuitFlagSet, set_quit_flag::SetQuitFlag};
}
