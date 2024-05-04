//! # Command Event
//!
//! This module contains the command event plugin, which is used to handle
//! commands that the player can issue to the game.

/// The catalog of commands.
pub mod catalog;
/// The command event plugin.
pub mod plugin;

/// The prelude.
pub mod prelude {
  pub use super::catalog::prelude::*;
  pub use super::plugin::CommandEventPlugin;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
}
