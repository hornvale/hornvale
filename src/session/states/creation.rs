use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The `Creation` state, representing character creation.
#[derive(Clone, Copy, Debug)]
pub struct Creation;

impl SessionStateValue for Creation {
  fn on_enter(&mut self) {
    println!("Entering the Creation state.");
  }

  fn on_exit(&mut self) {
    println!("Exiting the Creation state.");
  }

  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError> {
    Ok(Some(SessionStateMachine::Active(Active)))
  }
}
