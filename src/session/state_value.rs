use crate::session::prelude_internal::*;
use anyhow::Error as AnyError;

/// The state value trait.
///
/// Each state in the state machine is represented by a value that implements
/// this trait.
pub trait SessionStateValue {
  /// On entering the state.
  fn on_enter(&mut self);
  /// On exiting the state.
  fn on_exit(&mut self);
  /// Return the next state.
  fn next(&self) -> Result<Option<SessionStateMachine>, AnyError>;
}
