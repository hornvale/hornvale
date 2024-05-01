use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The `Save` state,  which handles saving the game.
///
/// In typical roguelike fashion, the game will allow the player to save only
/// when the player quits the game.
#[derive(Clone, Copy, Debug)]
pub struct Save;

impl SessionStateValue for Save {
  fn on_enter(&mut self) {
    println!("Entering the Save state.");
  }

  fn on_exit(&mut self) {
    println!("Exiting the Save state.");
  }

  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError> {
    Ok(Some(SessionStateMachine::Shutdown(Shutdown)))
  }
}
