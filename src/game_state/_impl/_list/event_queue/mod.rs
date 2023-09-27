use crate::event::EventTrait;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;

impl EventQueueTrait for GameState {
  /// Enqueue an event.
  fn enqueue_event(&mut self, event: Box<dyn EventTrait<GameState>>) {
    self.event_queue.push_back(event);
  }

  /// Dequeue an event.
  fn dequeue_event(&mut self) -> Option<Box<dyn EventTrait<GameState>>> {
    self.event_queue.pop_front()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::event::NoOpEvent;
  use crate::test::init;

  #[test]
  fn test_enqueue_event() {
    init();
    let mut game_state = GameState::new();
    let event = Box::new(NoOpEvent::new());
    game_state.enqueue_event(event);
    assert_eq!(game_state.event_queue.len(), 1);
  }

  #[test]
  fn test_dequeue_event() {
    init();
    let mut game_state = GameState::new();
    let event = Box::new(NoOpEvent::new());
    game_state.enqueue_event(event);
    let _event = game_state.dequeue_event();
  }
}
