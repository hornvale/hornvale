use anyhow::Error as AnyError;

use crate::event::EventMetadata;
use crate::event::EventTrait;
use crate::game_state::GameState;

/// The `StartGame` event.
#[derive(Clone, Debug, Default)]
pub struct StartGame {
  metadata: EventMetadata,
}

impl StartGame {
  /// Creates a new `StartGame`.
  pub fn new() -> Self {
    let metadata = EventMetadata::new();
    Self { metadata }
  }
}

impl EventTrait<GameState> for StartGame {
  /// Get the name of this event.
  fn get_name(&self) -> &'static str {
    "StartGame"
  }

  /// Get the metadata for this event.
  fn get_metadata(&self) -> &EventMetadata {
    &self.metadata
  }

  /// Get (mutably) the metadata for this event.
  fn get_metadata_mut(&mut self) -> &mut EventMetadata {
    &mut self.metadata
  }

  /// Processes the `StartGame` event.
  ///
  /// This does not do anything by itself, but its subscribers will perform
  /// actions to start the game.
  fn process(&self, _game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Applying start-game event.");
    Ok(())
  }
}
