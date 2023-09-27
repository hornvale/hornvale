use anyhow::Error as AnyError;

use crate::effect::EffectTrait;
use crate::effect::NoOpEffect;
use crate::event::EventTrait;
use crate::game_state::GameState;

/// The `NoOp` event.
#[derive(Debug, Default)]
pub struct NoOp {}

impl NoOp {
  /// Creates a new `NoOp`.
  pub fn new() -> Self {
    Self {}
  }
}

impl EventTrait<GameState> for NoOp {
  /// Processes the `NoOp` event.
  fn process(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Applying no-op event.");
    NoOpEffect::new().apply(game_state)?;
    Ok(())
  }
}
