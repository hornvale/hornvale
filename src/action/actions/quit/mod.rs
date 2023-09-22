use crate::action::Action;
use crate::action::ActionContext;
use crate::action::ActionError;
use crate::action::ActionResult;

/// The `Quit` action struct.
#[derive(Clone, Debug, Default)]
pub struct Quit {}

impl Action for Quit {
  fn execute(&self, _context: &mut ActionContext) -> Result<ActionResult, ActionError> {
    Ok(ActionResult::SucceededWithOutput("Goodbye!".to_string()))
  }
}
