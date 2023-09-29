use anyhow::Error as AnyError;
use uuid::Uuid;

use crate::game_state::GameState;

pub mod r#type;
pub use r#type::Type as ActionType;

/// The `Action` struct.
#[derive(Clone, Debug, PartialEq)]
pub struct Action {
  /// The `Action` type.
  pub r#type: ActionType,
  /// The `Action`'s UUID.
  pub uuid: Uuid,
  /// A backtrace.
  pub backtrace: Vec<String>,
}

impl Action {
  pub fn new(r#type: ActionType, backtrace: Vec<String>) -> Self {
    let uuid = Uuid::new_v4();
    let mut backtrace = backtrace;
    backtrace.push(format!("Action {:?}:{}", r#type, uuid));
    Self {
      r#type,
      uuid,
      backtrace,
    }
  }

  pub fn attempt(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Attempting {:?} action.", self.r#type);
    self.r#type.attempt(self, game_state)?;
    Ok(())
  }
}
