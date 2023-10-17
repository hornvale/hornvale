use crate::action::ActionData;
use crate::action::ActionError;
use crate::action::ActionType;

/// The `Action` type.
#[derive(Builder, Clone, Debug, Default, PartialEq)]
pub struct Action {
  /// The `Action` type.
  pub action_type: ActionType,
  /// A backtrace.
  #[builder(default = "Vec::new()")]
  pub backtrace: Vec<String>,
}

impl Action {
  pub fn attempt(&self, data: &mut ActionData) -> Result<(), Box<ActionError>> {
    debug!("Attempting {:#?} action.", self);
    self.action_type.attempt(self, data)?;
    Ok(())
  }
}
