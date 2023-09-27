use anyhow::Error as AnyError;

use crate::game_state::GameStateTrait;

/// The `Effect` trait.
pub trait Effect<T: GameStateTrait> {
  /// Runs the `Effect`.
  fn apply(&self, game_state: &mut T) -> Result<(), AnyError>;
}
