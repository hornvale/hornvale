//! # World Commands
//!
//! This crate provides a collection of general world-facing commands.

/// Commands that can be executed within the world.
pub mod commands;

/// The prelude.
pub mod prelude {
  pub use crate::commands::{
    go_direction::GoDirectionCommand, look_direction::LookDirectionCommand, look_here::LookHereCommand,
  };
}
