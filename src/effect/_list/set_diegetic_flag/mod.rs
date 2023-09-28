use anyhow::Error as AnyError;

use crate::effect::EffectTrait;
use crate::game_state::DiegeticFlagTrait;
use crate::game_state::GameState;

/// The `SetDiegeticFlag` effect.
#[derive(Debug, Default)]
pub struct SetDiegeticFlag {
  /// The intended value.
  pub value: bool,
}

impl SetDiegeticFlag {
  /// Creates a new `Effect`.
  pub fn new(value: bool) -> Self {
    Self { value }
  }
}

impl EffectTrait<GameState> for SetDiegeticFlag {
  /// Get the name of this effect.
  fn get_name(&self) -> &'static str {
    "SetDiegeticFlag"
  }
  /// Applies this effect.
  fn apply(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Applying set-diegetic-flag effect.");
    game_state.set_diegetic_flag(self.value);
    Ok(())
  }
}
