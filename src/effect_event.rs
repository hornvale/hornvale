//! # Effect Event
//!
//! This module contains `Effect` events, which indicate changes to the world.

/// A catalog of effects.
pub mod catalog;
/// The EffectEvent plugin.
pub mod plugin;

/// The prelude.
pub mod prelude {
  pub use super::catalog::prelude::*;
  pub use super::plugin::EffectEventPlugin;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
}
