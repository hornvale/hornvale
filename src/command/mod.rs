use uuid::Uuid;

use crate::game_state::GameState;
use crate::game_state::OutputQueueTrait;

pub mod error;
pub use error::Error as CommandError;
pub mod command_type;
pub use command_type::Type as CommandType;

/// The `Command` struct.
#[derive(Clone, Debug, PartialEq)]
pub struct Command {
  /// The `Command` type.
  pub command_type: CommandType,
  /// The `Command`'s UUID.
  pub uuid: Uuid,
  /// A backtrace.
  pub backtrace: Vec<String>,
}

impl Command {
  pub fn new(command_type: CommandType) -> Self {
    let uuid = Uuid::new_v4();
    let backtrace = vec![format!("Command {:?}:{}", command_type, uuid)];
    Self {
      command_type,
      uuid,
      backtrace,
    }
  }

  pub fn execute(&self, game_state: &mut GameState) -> Result<(), CommandError> {
    debug!("Executing {:#?} command.", self.command_type);
    if let Some(error) = self.command_type.execute(self, game_state).err() {
      error!("Error executing command:\n{:#?}\n{:#?}", self.clone(), error);
      game_state.enqueue_output(&error.to_string());
    }
    Ok(())
  }
}
