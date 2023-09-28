use anyhow::Error as AnyError;

use crate::game_state::GameStateTrait;

/// The `Effect` trait.
pub trait Effect<T: GameStateTrait> {
  /// Get the name of this effect.
  fn get_name(&self) -> &'static str;
  /// Runs the `Effect`.
  fn apply(&self, game_state: &mut T) -> Result<(), AnyError>;
}
