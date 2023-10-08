use uuid::Uuid;

use crate::game_state::GameState;

pub mod error;
pub use error::Error as ActionError;
pub mod action_type;
pub use action_type::Type as ActionType;

/// The `Action` struct.
#[derive(Clone, Debug, PartialEq)]
pub struct Action {
  /// The `Action` type.
  pub action_type: ActionType,
  /// The `Action`'s UUID.
  pub uuid: Uuid,
  /// A backtrace.
  pub backtrace: Vec<String>,
}

impl Action {
  pub fn new(action_type: ActionType, backtrace: Vec<String>) -> Self {
    let uuid = Uuid::new_v4();
    let mut backtrace = backtrace;
    backtrace.push(format!("Action {:?}:{}", action_type, uuid));
    Self {
      action_type,
      uuid,
      backtrace,
    }
  }

  pub fn attempt(&self, game_state: &mut GameState) -> Result<(), Box<ActionError>> {
    debug!("Attempting {:#?} action.", self.action_type);
    self.action_type.attempt(self, game_state)?;
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
    assert_eq!(action.action_type, ActionType::NoOp);
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
