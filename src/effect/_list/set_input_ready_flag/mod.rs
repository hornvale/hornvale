use anyhow::Error as AnyError;

use crate::effect::EffectTrait;
use crate::game_state::GameState;
use crate::game_state::InputReadyFlagTrait;

/// The `SetInputReadyFlag` effect.
#[derive(Debug, Default)]
pub struct SetInputReadyFlag {
  /// The intended value.
  pub value: bool,
}

impl SetInputReadyFlag {
  /// Creates a new `Effect`.
  pub fn new(value: bool) -> Self {
    Self { value }
  }
}

impl EffectTrait<GameState> for SetInputReadyFlag {
  /// Applies this effect.
  fn apply(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Applying set-input-ready-flag effect.");
    game_state.set_input_ready_flag(self.value);
    Ok(())
  }
}
