use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The `LoadFailed` state,  which reports and handles a failed load.
///
/// When `LoadFailed` is entered, the game will report the failure to load the
/// player's profile and prompt the player to either start a new game or quit.
///
/// Valid previous states:
/// - `Load`, if the game fails to load the player's profile.
///
/// Valid transitions:
/// - `CleanSlate` if the player chooses to start a new game from scratch.
/// - `Creation` if the player chooses to create a new character.
/// - `Quit` if the player chooses to quit the game.
#[derive(Clone, Copy, Debug)]
pub struct LoadFailed;

impl SessionStateValue for LoadFailed {
  fn on_enter(&mut self) {
    println!("Entering the LoadFailed state.");
  }

  fn on_exit(&mut self) {
    println!("Exiting the LoadFailed state.");
  }

  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError> {
    Ok(Some(SessionStateMachine::CleanSlate(CleanSlate)))
  }
}

impl From<SessionStateWrapper<Load>> for SessionStateWrapper<LoadFailed> {
  fn from(_state: SessionStateWrapper<Load>) -> Self {
    SessionStateWrapper::<LoadFailed>(LoadFailed)
  }
}
