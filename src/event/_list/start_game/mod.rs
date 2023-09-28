use anyhow::Error as AnyError;
use uuid::Uuid;

use crate::event::EventTrait;
use crate::game_state::GameState;

/// The `StartGame` event.
#[derive(Clone, Debug, Default)]
pub struct StartGame {
  pub uuid: Uuid,
}

impl StartGame {
  /// Creates a new `StartGame`.
  pub fn new() -> Self {
    Self { uuid: Uuid::new_v4() }
  }
}

impl EventTrait<GameState> for StartGame {
  /// Get the name of this event.
  fn get_name(&self) -> &'static str {
    "StartGame"
  }

  /// Get the UUID of this event.
  fn get_uuid(&self) -> uuid::Uuid {
    self.uuid
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
