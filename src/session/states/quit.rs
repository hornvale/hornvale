use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The `Quit` state,  which handles quitting the game.
#[derive(Clone, Copy, Debug)]
pub struct Quit;

impl SessionStateValue for Quit {
  fn on_enter(&mut self) {
    println!("Entering the Quit state.");
  }

  fn on_exit(&mut self) {
    println!("Exiting the Quit state.");
  }

  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError> {
    Ok(None)
  }
}
