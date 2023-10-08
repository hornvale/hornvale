use anyhow::Error as AnyError;

use crate::event::EventBuilder;
use crate::event::EventType;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameFileServiceTrait;
use crate::game_state::GameState;

/// Implementation of the `GameFileService` trait.
impl GameFileServiceTrait for GameState {
  /// Create the local game file directory.
  fn create_game_file_directory(&mut self) -> Result<(), AnyError> {
    self.game_file_service.create_game_file_directory()?;
    let event = EventBuilder::new()
      .event_type(EventType::GameFileDirectoryIsCreated)
      .build();
    self.enqueue_event(event);
    Ok(())
  }
}
