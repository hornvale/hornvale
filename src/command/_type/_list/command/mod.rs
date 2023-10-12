use crate::command::CommandData;
use crate::command::CommandType;
use crate::entity_uuid::CommandUuid;
use anyhow::Error as AnyError;

/// The `Command` type.
#[derive(Builder, Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Command {
  /// The `Command` type.
  pub command_type: CommandType,
  /// The `Command`'s UUID.
  #[builder(default = "CommandUuid::default()")]
  pub uuid: CommandUuid,
  /// A backtrace.
  #[builder(default = "Vec::new()")]
  pub backtrace: Vec<String>,
}

impl Command {
  pub fn execute(&self, data: &mut CommandData) -> Result<(), AnyError> {
    debug!("Executing {:#?} command.", self.command_type);
    if let Some(error) = self.command_type.execute(self, data).err() {
      error!("Error executing command:\n{:#?}\n{:#?}", self.clone(), error);
    }
    Ok(())
  }

  /// Is this a diegetic command or not?
  pub fn is_diegetic(&self) -> bool {
    self.command_type.is_diegetic()
  }
}
