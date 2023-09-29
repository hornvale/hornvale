use crate::event::Event;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;

impl EventQueueTrait for GameState {
  /// Enqueue an event.
  fn enqueue_event(&mut self, event: Event) {
    self.event_queue.push(event);
  }

  /// Dequeue an event.
  fn dequeue_event(&mut self) -> Option<Event> {
    self.event_queue.pop()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::event::EventType;
  use crate::event::DEFAULT_PRIORITY;
  use crate::test::init;

  #[test]
  fn test_enqueue_event() {
    init();
    let mut game_state = GameState::new();
    let event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, Vec::new());
    game_state.enqueue_event(event);
    assert_eq!(game_state.event_queue.len(), 1);
  }

  #[test]
  fn test_dequeue_event() {
    init();
    let mut game_state = GameState::new();
    let event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, Vec::new());
    game_state.enqueue_event(event);
    game_state.dequeue_event().unwrap();
    assert_eq!(game_state.event_queue.len(), 0);
  }
}
