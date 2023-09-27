use anyhow::Error as AnyError;

use crate::game_state::GameStateTrait;

/// The `Action` trait.
pub trait Action<T: GameStateTrait> {
  /// Runs the `Action`.
  fn run(&self, game_state: &mut T) -> Result<(), AnyError>;
}
