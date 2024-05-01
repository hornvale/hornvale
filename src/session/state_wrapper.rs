use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The SessionStateWrapper struct wraps the session state.
#[derive(Debug)]
pub struct SessionStateWrapper<S: SessionStateValue>(pub S);

impl<T> SessionStateValue for SessionStateWrapper<T>
where
  T: SessionStateValue,
{
  fn on_enter(&mut self) {
    self.0.on_enter();
  }

  fn on_exit(&mut self) {
    self.0.on_exit();
  }

  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError> {
    self.0.next()
  }
}
