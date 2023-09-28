use anyhow::Error as AnyError;

use crate::effect::EffectTrait;
use crate::game_state::GameState;
use crate::game_state::QuitFlagTrait;

/// The `SetQuitFlag` effect.
#[derive(Debug, Default)]
pub struct SetQuitFlag {
  /// The intended value.
  pub value: bool,
}

impl SetQuitFlag {
  /// Creates a new `Effect`.
  pub fn new(value: bool) -> Self {
    Self { value }
  }
}

impl EffectTrait<GameState> for SetQuitFlag {
  /// Get the name of this effect.
  fn get_name(&self) -> &'static str {
    "SetQuitFlag"
  }
  /// Applies this effect.
  fn apply(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Applying set-quit-flag effect.");
    game_state.set_quit_flag(self.value);
    Ok(())
  }
}
