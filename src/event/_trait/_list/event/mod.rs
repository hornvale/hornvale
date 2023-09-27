use anyhow::Error as AnyError;

use crate::game_state::GameStateTrait;

/// The `Event` trait.
pub trait Event<T: GameStateTrait> {
  /// Processes the `Event`.
  fn process(&self, game_state: &mut T) -> Result<(), AnyError>;
}
