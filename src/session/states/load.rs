use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The `Load` state,  which handles loading a saved game.
///
/// When `Load` is entered, the game will load the player's profile from disk
/// and transition to the `Active` state.
///
/// Valid previous states: None
///
/// Valid transitions:
/// - `Active` if the load succeeded.
/// - `LoadFailed` if the game fails to load the player's profile.
#[derive(Clone, Copy, Debug)]
pub struct Load;

impl SessionStateValue for Load {
  fn on_enter(&mut self) {
    println!("Entering the Load state.");
  }

  fn on_exit(&mut self) {
    println!("Exiting the Load state.");
  }

  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError> {
    Ok(Some(SessionStateMachine::Active(Active)))
  }
}
