use anyhow::Error as AnyError;

use crate::command::CommandTrait;
use crate::game_state::GameState;

/// The `NoOp` struct.
#[derive(Debug, Default)]
pub struct NoOp {}

impl NoOp {
  /// Creates a new `NoOp`.
  pub fn new() -> Self {
    Self {}
  }
}

impl CommandTrait<GameState> for NoOp {
  /// Runs the `NoOp` command.
  fn run(&self, _game_state: &mut GameState) -> Result<(), AnyError> {
    Ok(())
  }
}
