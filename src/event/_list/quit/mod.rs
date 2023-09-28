use anyhow::Error as AnyError;

use crate::effect::EffectTrait;
use crate::effect::SetQuitFlagEffect;
use crate::event::EventTrait;
use crate::game_state::GameState;

/// The `Quit` event.
#[derive(Clone, Debug, Default)]
pub struct Quit {}

impl Quit {
  /// Creates a new `Quit`.
  pub fn new() -> Self {
    Self {}
  }
}

impl EventTrait<GameState> for Quit {
  /// Get the name of this event.
  fn get_name(&self) -> &'static str {
    "Quit"
  }

  /// Processes the `Quit` event.
  fn process(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Applying quit event.");
    SetQuitFlagEffect::new(true).apply(game_state)?;
    Ok(())
  }
}
