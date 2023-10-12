use crate::action::ActionData;
use crate::action::ActionError;
use crate::action::ActionType;
use crate::entity_uuid::ActionUuid;

/// The `Action` type.
#[derive(Builder, Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct Action {
  /// The `Action` type.
  pub action_type: ActionType,
  /// The `Action`'s UUID.
  #[builder(default = "ActionUuid::default()")]
  pub uuid: ActionUuid,
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
