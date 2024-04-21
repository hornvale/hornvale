//! # Command
//!
//! The command module provides a way to define and execute commands in the
//! game.
//!
//! This crate is intentionally minimalistic, providing only the core traits
//! and a few basic commands.

/// A collection of core commands.
pub mod commands;
/// An error type.
pub mod error;
/// A registry for commands.
pub mod registry;

/// The prelude.
pub mod prelude {
  pub use super::commands::{fail::FailCommand, no_op::NoOpCommand, quit::QuitCommand};
  pub use super::error::CommandError;
  pub use super::registry::CommandRegistry;
}
