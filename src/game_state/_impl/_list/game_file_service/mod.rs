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

#[cfg(test)]
mod tests {
  use super::*;

  use crate::event::DEFAULT_PRIORITY;
  use crate::test::init;

  #[test]
  fn test_create_game_file_directory() {
    init();
    let mut game_state = GameState::new();
    game_state.create_game_file_directory().unwrap();
    assert_eq!(game_state.event_queue.len(), 1);
    let event = game_state.dequeue_event().unwrap();
    assert_eq!(event.event_type, EventType::GameFileDirectoryIsCreated);
    assert_eq!(event.priority, DEFAULT_PRIORITY);
  }
}
