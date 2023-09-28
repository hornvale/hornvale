use anyhow::Error as AnyError;

use crate::effect::EffectTrait;
use crate::effect::SetQuitFlagEffect;
use crate::event::EventMetadata;
use crate::event::EventTrait;
use crate::game_state::GameState;

/// The `Quit` event.
#[derive(Clone, Debug, Default)]
pub struct Quit {
  metadata: EventMetadata,
}

impl Quit {
  /// Creates a new `Quit`.
  pub fn new() -> Self {
    let metadata = EventMetadata::new();
    Self { metadata }
  }
}

impl EventTrait<GameState> for Quit {
  /// Get the name of this event.
  fn get_name(&self) -> &'static str {
    "Quit"
  }

  /// Get the metadata for this event.
  fn get_metadata(&self) -> &EventMetadata {
    &self.metadata
  }

  /// Get (mutably) the metadata for this event.
  fn get_metadata_mut(&mut self) -> &mut EventMetadata {
    &mut self.metadata
  }

  /// Processes the `Quit` event.
  fn process(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Applying quit event.");
    SetQuitFlagEffect::new(true).apply(game_state)?;
    Ok(())
  }
}
