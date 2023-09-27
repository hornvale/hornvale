use anyhow::Error as AnyError;

use crate::action::ActionTrait;
use crate::game_state::GameState;

/// The `NoOp` action.
#[derive(Debug, Default)]
pub struct NoOp {}

impl NoOp {
  /// Creates a new `NoOp`.
  pub fn new() -> Self {
    Self {}
  }
}

impl ActionTrait<GameState> for NoOp {
  /// Runs the `NoOp` action.
  fn run(&self, _game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Running no-op action.");
    Ok(())
  }
}
