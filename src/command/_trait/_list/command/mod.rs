use anyhow::Error as AnyError;

use crate::game_state::GameStateTrait;

/// The `Command` trait.
pub trait Command<T: GameStateTrait> {
  /// Get the name of this command.
  fn get_name(&self) -> &'static str;
  /// Is this diegetic?
  fn is_diegetic(&self) -> bool;
  /// Runs the `Command`.
  fn execute(&self, game_state: &mut T) -> Result<(), AnyError>;
}
