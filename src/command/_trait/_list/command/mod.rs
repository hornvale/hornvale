use anyhow::Error as AnyError;

use crate::game_state::GameStateTrait;

/// The `Command` trait.
pub trait Command<T: GameStateTrait> {
  /// Runs the `Command`.
  fn run(&self, game_state: &mut T) -> Result<(), AnyError>;
}
