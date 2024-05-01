use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The `Restart` state, for restarting the game.
#[derive(Clone, Copy, Debug)]
pub struct Restart;

impl SessionStateValue for Restart {
  fn on_enter(&mut self) {
    println!("Entering the Restart state.");
  }

  fn on_exit(&mut self) {
    println!("Exiting the Restart state.");
  }

  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError> {
    Ok(Some(SessionStateMachine::Load(Load)))
  }
}
