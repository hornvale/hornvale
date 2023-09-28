use crate::action::ActionTrait;
use crate::command::CommandTrait;
use crate::effect::EffectTrait;
use crate::event::EventTrait;
use crate::game_state::GameStateTrait;

/// The `EventStackEntry` struct.
///
/// This struct is used to represent an entry in the event stack.
#[derive(Clone, Debug, Display, PartialEq)]
pub struct StackEntry {}

impl StackEntry {
  /// Creates a new `StackEntry` from an `Action`.
  pub fn from_action<T: GameStateTrait, A: 'static + ActionTrait<T>>(_action: A) -> Self {
    Self {}
  }
  /// Creates a new `StackEntry` from a `Command`.
  pub fn from_command<T: GameStateTrait, C: 'static + CommandTrait<T>>(_command: C) -> Self {
    Self {}
  }
  /// Creates a new `StackEntry` from an `Effect`.
  pub fn from_effect<T: GameStateTrait, E: 'static + EffectTrait<T>>(_effect: E) -> Self {
    Self {}
  }
  /// Creates a new `StackEntry` from an `Event`.
  pub fn from_event<T: GameStateTrait, E: 'static + EventTrait<T>>(_event: E) -> Self {
    Self {}
  }
}
