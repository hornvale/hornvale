use anyhow::Error as AnyError;

use crate::effect::EffectTrait;
use crate::game_state::GameState;

/// The `NoOp` effect.
#[derive(Debug, Default)]
pub struct NoOp {}

impl NoOp {
  /// Creates a new `NoOp`.
  pub fn new() -> Self {
    Self {}
  }
}

impl EffectTrait<GameState> for NoOp {
  /// Runs the `NoOp` effect.
  fn apply(&self, _game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Applying no-op effect.");
    Ok(())
  }
}
