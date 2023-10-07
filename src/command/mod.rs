use uuid::Uuid;

use crate::game_state::GameState;
use crate::game_state::OutputQueueTrait;

pub mod error;
pub use error::Error as CommandError;
pub mod r#type;
pub use r#type::Type as CommandType;

/// The `Command` struct.
#[derive(Clone, Debug, PartialEq)]
pub struct Command {
  /// The `Command` type.
  pub r#type: CommandType,
  /// The `Command`'s UUID.
  pub uuid: Uuid,
  /// A backtrace.
  pub backtrace: Vec<String>,
}

impl Command {
  pub fn new(r#type: CommandType) -> Self {
    let uuid = Uuid::new_v4();
    let backtrace = vec![format!("Command {:?}:{}", r#type, uuid)];
    Self {
      r#type,
      uuid,
      backtrace,
    }
  }

  pub fn execute(&self, game_state: &mut GameState) -> Result<(), CommandError> {
    debug!("Executing {:#?} command.", self.r#type);
    if let Some(error) = self.r#type.execute(self, game_state).err() {
      error!("Error executing command:\n{:#?}\n{:#?}", self.clone(), error);
      game_state.enqueue_output(&error.to_string());
    }
    Ok(())
  }
}
