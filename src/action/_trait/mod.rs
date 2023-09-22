use std::fmt::Debug;

use crate::action::ActionContext;
use crate::action::ActionError;
use crate::action::ActionResult;

/// The `Action` trait.
///
/// This trait represents an action that can be executed by any entity within
/// the game.
pub trait Action: Debug + Send + Sync {
  /// Executes this action.
  ///
  /// This method returns a `Result` that indicates whether the action was
  /// executed successfully, and if so, whether it produced any output.
  fn execute(&self, _context: &mut ActionContext) -> Result<ActionResult, ActionError>;
}
