use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The `Active` state, representing the main game loop.
///
/// Valid previous states:
/// - `CleanSlate` if the player has never played before.
/// - `Creation` if the player is creating a character.
///
/// Valid transitions:
/// - `Save` if the player saves the game.
/// - `Quit` if the player quits the game without saving.
#[derive(Clone, Copy, Debug)]
pub struct Active;

impl SessionStateValue for Active {
  fn on_enter(&mut self) {
    println!("Entering the Active state.");
  }

  fn on_exit(&mut self) {
    println!("Exiting the Active state.");
  }

  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError> {
    Ok(Some(SessionStateMachine::Save(Save)))
  }
}
