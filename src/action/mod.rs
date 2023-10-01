use uuid::Uuid;

use crate::game_state::GameState;

pub mod error;
pub use error::Error as ActionError;
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

  pub fn attempt(&self, game_state: &mut GameState) -> Result<(), Box<ActionError>> {
    debug!("Attempting {:#?} action.", self.r#type);
    self.r#type.attempt(self, game_state)?;
    Ok(())
  }
}

#[cfg(test)]
mod tests {

  use super::*;

  use crate::game_state::GameState;
  use crate::test::init;

  #[test]
  fn test_new() {
    init();
    let action = Action::new(ActionType::NoOp, vec![]);
    assert_eq!(action.r#type, ActionType::NoOp);
    assert_eq!(action.backtrace.len(), 1);
  }

  #[test]
  fn test_attempt() {
    init();
    let mut game_state = GameState::new();
    let action = Action::new(ActionType::NoOp, vec![]);
    action.attempt(&mut game_state).unwrap();
  }
}
