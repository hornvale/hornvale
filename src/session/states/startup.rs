use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The `Startup` state, representing the startup state.
///
/// This state is entered when the player starts the game.
///
/// The game will check the file system to determine whether the player has an
/// existing profile. If the player has an existing profile, the game will
/// transition to the `Load` state. If the player does not, the game will
/// transition to `CleanSlate`.
///
/// Valid previous states: None
///
/// Valid transitions:
/// - `CleanSlate` if the player does not have an existing profile.
/// - `Load` if the player has an existing profile.
#[derive(Clone, Copy, Debug)]
pub struct Startup;

impl SessionStateValue for Startup {
  fn on_enter(&mut self) {
    println!("Entering the Startup state.");
  }

  fn on_exit(&mut self) {
    println!("Exiting the Startup state.");
  }

  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError> {
    Ok(Some(SessionStateMachine::Load(Load)))
  }
}
