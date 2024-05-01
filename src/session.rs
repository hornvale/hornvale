//! # Session State
//!
//! The player's session is managed by the "Session State" state machine.
//!
//! _Hornvale_ does not have a main menu or a title screen; the game starts
//! immediately when the program is run, dumping the new player into the game
//! world.
//!
//! At launch, the current state of the file system (e.g. a clean slate, an
//! existing profile, etc) is checked and used to set the state accordingly.
//! From there, player commands, actions, the death of the player character,
//! and so forth will cause the session state to transition.

/// Events that can occur during the session.
pub mod event;
/// The session state machine.
pub mod state_machine;
/// The state value trait.
pub mod state_value;
/// The state wrapper.
pub mod state_wrapper;
/// Possible states for the state machine.
pub mod states;

/// The prelude.
pub mod prelude {
  pub use super::state_machine::SessionStateMachine;
}

/// The internal prelude.
pub mod prelude_internal {
  pub use super::prelude::*;

  pub use super::event::SessionEvent;
  pub use super::state_value::SessionStateValue;
  pub use super::state_wrapper::SessionStateWrapper;
  pub use super::states::prelude::*;
}
