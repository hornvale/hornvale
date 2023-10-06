use anyhow::Error as AnyError;

use crate::event::Event;
use crate::event::EventType;
use crate::event::DEFAULT_PRIORITY;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;
use crate::game_state::StartTrait;

/// Implementation of the `ChunkManagement` trait.
impl StartTrait for GameState {
  /// Start the game.
  fn start(&mut self) -> Result<(), AnyError> {
    let start_game_event = Event::new(EventType::StartsGame, DEFAULT_PRIORITY + 10000, Vec::new(), Vec::new());
    self.enqueue_event(start_game_event);
    Ok(())
  }
}
