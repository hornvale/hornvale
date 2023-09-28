use anyhow::Error as AnyError;

use crate::game_state::GameStateTrait;

/// The `Action` trait.
pub trait Action<T: GameStateTrait> {
  fn get_name(&self) -> &'static str;
  /// Runs the `Action`.
  fn execute(&self, game_state: &mut T) -> Result<(), AnyError>;
}
