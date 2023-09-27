use anyhow::Error as AnyError;

use crate::game_state::GameStateTrait;

/// The `Command` trait.
pub trait Command<T: GameStateTrait> {
  /// Is this diegetic?
  fn is_diegetic(&self) -> bool;
  /// Runs the `Command`.
  fn execute(&self, game_state: &mut T) -> Result<(), AnyError>;
}
