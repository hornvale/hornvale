use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The `Death` state, representing character death.
#[derive(Clone, Copy, Debug)]
pub struct Death;

impl SessionStateValue for Death {
  fn on_enter(&mut self) {
    println!("Entering the Death state.");
  }

  fn on_exit(&mut self) {
    println!("Exiting the Death state.");
  }

  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError> {
    Ok(Some(SessionStateMachine::Quit(Quit)))
  }
}
