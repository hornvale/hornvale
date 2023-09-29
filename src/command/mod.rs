use anyhow::Error as AnyError;
use uuid::Uuid;

use crate::game_state::GameState;

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

  pub fn execute(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Executing {:?} command.", self.r#type);
    self.r#type.execute(self, game_state)?;
    Ok(())
  }
}
