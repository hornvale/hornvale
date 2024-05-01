use crate::session::prelude_internal::*;

/// The `SessionStateMachine` enum.
///
/// This enum represents the state machine for the player's session.
#[derive(Clone, Copy, Debug)]
pub enum SessionStateMachine {
  /// The `Active` state.
  /// Next: `Save`, `Quit`
  Active(Active),
  /// The `CleanSlate` state.
  /// Next: `Introduction`
  CleanSlate(CleanSlate),
  /// The `Creation` state.
  /// Next: `Active`, `Quit`
  Creation(Creation),
  /// The `Death` state.
  /// Next: `CleanSlate`, `Creation`, `Quit`
  Death(Death),
  /// The `Introduction` state.
  /// Next: `Active`
  Introduction(Introduction),
  /// The `Load` state.
  /// Next: `Active`, `LoadFailed`
  Load(Load),
  /// The `LoadFailed` state.
  /// Next: `CleanSlate`, `Creation`, `Quit`
  LoadFailed(LoadFailed),
  /// The `Quit` state.
  /// Next: `Shutdown`
  Quit(Quit),
  /// The `Restart` state.
  /// Next: `CleanSlate`, `Creation`
  Restart(Restart),
  /// The `Save` state.
  /// Next: `Shutdown`
  Save(Save),
  /// The `Shutdown` state.
  /// Next: None
  Shutdown(Shutdown),
  /// The `Startup` state.
  /// Next: `Load`, `CleanSlate`
  Startup(Startup),
}

impl Default for SessionStateMachine {
  fn default() -> Self {
    Self::new()
  }
}

impl SessionStateMachine {
  /// Constructor.
  pub fn new() -> Self {
    SessionStateMachine::Startup(Startup)
  }

  /// Update the state machine.
  pub fn step(self, event: SessionEvent) -> Self {
    let (_, _) = (&self, event);
    self
  }
}
