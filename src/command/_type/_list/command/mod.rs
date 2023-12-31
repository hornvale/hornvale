use anyhow::Error as AnyError;

use crate::command::CommandData;
use crate::command::CommandType;

/// The `Command` type.
#[derive(Builder, Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Command {
  /// The `Command` type.
  pub command_type: CommandType,
  /// A backtrace.
  #[builder(default = "Vec::new()")]
  pub backtrace: Vec<String>,
}

impl Command {
  pub fn execute(&self, data: &mut CommandData) -> Result<(), AnyError> {
    debug!("Executing {:#?} command.", self);
    if let Some(error) = self.command_type.execute(self, data).err() {
      error!("Error executing command:\n{:#?}\n{:#?}", self.clone(), error);
    } else if self.is_diegetic() {
      data.advance_flag_resource.0 = true;
    }
    Ok(())
  }

  /// Is this a diegetic command or not?
  pub fn is_diegetic(&self) -> bool {
    self.command_type.is_diegetic()
  }
}
