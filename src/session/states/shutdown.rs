use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The `Shutdown` state,  which is the final state.
///
/// Valid previous states:
/// - `Save` if the player saves the game.
/// - `Quit` if the player quits the game without saving.
///
/// Valid transitions: None
#[derive(Clone, Copy, Debug)]
pub struct Shutdown;

impl SessionStateValue for Shutdown {
  fn on_enter(&mut self) {
    println!("Entering the Shutdown state.");
  }

  fn on_exit(&mut self) {
    println!("Exiting the Shutdown state.");
  }

  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError> {
    Ok(None)
  }
}

impl From<SessionStateWrapper<Save>> for SessionStateWrapper<Shutdown> {
  fn from(_state: SessionStateWrapper<Save>) -> Self {
    SessionStateWrapper::<Shutdown>(Shutdown)
  }
}

impl From<SessionStateWrapper<Quit>> for SessionStateWrapper<Shutdown> {
  fn from(_state: SessionStateWrapper<Quit>) -> Self {
    SessionStateWrapper::<Shutdown>(Shutdown)
  }
}
