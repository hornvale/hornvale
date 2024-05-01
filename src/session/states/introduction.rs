use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The `Introduction` state,  which sets up the "newbie" experience.
#[derive(Clone, Copy, Debug)]
pub struct Introduction;

impl SessionStateValue for Introduction {
  fn on_enter(&mut self) {
    println!("Entering the Introduction state.");
  }

  fn on_exit(&mut self) {
    println!("Exiting the Introduction state.");
  }

  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError> {
    Ok(Some(SessionStateMachine::Active(Active)))
  }
}
