//! # Input
//!
//! This module contains the input handling for the player.

/// The input plugin.
pub mod plugin;

/// The prelude.
pub mod prelude {
  pub use super::plugin::InputPlugin;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
}
