use anyhow::Error as AnyError;
use uuid::Uuid;

use crate::effect::EffectTrait;
use crate::effect::SetQuitFlagEffect;
use crate::event::EventTrait;
use crate::game_state::GameState;

/// The `Quit` event.
#[derive(Clone, Debug, Default)]
pub struct Quit {
  pub uuid: Uuid,
}

impl Quit {
  /// Creates a new `Quit`.
  pub fn new() -> Self {
    Self { uuid: Uuid::new_v4() }
  }
}

impl EventTrait<GameState> for Quit {
  /// Get the name of this event.
  fn get_name(&self) -> &'static str {
    "Quit"
  }

  /// Get the UUID of this event.
  fn get_uuid(&self) -> uuid::Uuid {
    self.uuid
  }

  /// Processes the `Quit` event.
  fn process(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Applying quit event.");
    SetQuitFlagEffect::new(true).apply(game_state)?;
    Ok(())
  }
}
