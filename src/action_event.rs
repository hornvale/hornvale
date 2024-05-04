//! # Action Event
//!
//! This module contains `Action` events, which indicate an entity's intent.

/// A catalog of actions.
pub mod catalog;
/// The ActionEvent plugin.
pub mod plugin;

/// The prelude.
pub mod prelude {
  pub use super::catalog::prelude::*;
  pub use super::plugin::ActionEventPlugin;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;
}
