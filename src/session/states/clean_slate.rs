use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The `CleanSlate` state, representing a first-time player.
///
/// This state is entered when the player has no existing profile.
///
/// Valid previous states:
/// - `Startup` if the player has never played before.
/// - `LoadFailed` if the attempt to load the player's profile failed.
///
/// Valid transitions:
/// - `Save` if the player saves the game.
/// - `Quit` if the player quits the game without saving.
#[derive(Clone, Copy, Debug)]
pub struct CleanSlate;

impl SessionStateValue for CleanSlate {
  fn on_enter(&mut self) {
    println!("Entering the CleanSlate state.");
  }

  fn on_exit(&mut self) {
    println!("Exiting the CleanSlate state.");
  }

  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError> {
    Ok(Some(SessionStateMachine::Save(Save)))
  }
}
