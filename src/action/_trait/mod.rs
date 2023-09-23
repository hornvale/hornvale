use std::fmt::Debug;

use crate::action::ActionContext;
use crate::action::ActionError;

/// The `Action` trait.
///
/// This trait represents an action that can be executed by any entity within
/// the game.
pub trait Action: Debug + Send + Sync {
  /// Executes this action.
  fn execute(&self, context: &mut ActionContext) -> Result<(), ActionError>;
}
