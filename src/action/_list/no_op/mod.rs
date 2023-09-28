use anyhow::Error as AnyError;

use crate::action::ActionTrait;
use crate::effect::EffectTrait;
use crate::effect::NoOpEffect;
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
  fn get_name(&self) -> &'static str {
    "NoOp"
  }
  /// Runs the `NoOp` action.
  fn execute(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Running no-op action.");
    NoOpEffect::new().apply(game_state)?;
    Ok(())
  }
}
