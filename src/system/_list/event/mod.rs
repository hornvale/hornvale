use crate::event::EventPublisher;
use crate::game_state::EventQueueTrait;
use crate::game_state::GameState;
use crate::system::SystemTrait;

/// The `Event` system.
///
/// This system dequeues events from the `GameState` and runs them.
#[derive(Debug, Default)]
pub struct Event {
  pub event_publisher: EventPublisher,
}

impl Event {
  /// Creates a new `Event` system.
  pub fn new() -> Self {
    Self {
      event_publisher: EventPublisher::new(),
    }
  }
}

impl SystemTrait<GameState> for Event {
  /// Runs the `Event`s.
  fn run(&mut self, game_state: &mut GameState) {
    debug!("Running event system.");
    while let Some(mut event) = game_state.dequeue_event() {
      self
        .event_publisher
        .publish_event(&mut event, game_state)
        .map_err(|error| error!("Error running event: {}", error))
        .ok();
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::event::Event;
  use crate::event::EventType;
  use crate::event::DEFAULT_PRIORITY;
  use crate::game_state::EventQueueTrait;
  use crate::game_state::GameState;
  use crate::game_state::QuitFlagTrait;
  use crate::system::EventSystem;
  use crate::system::SystemTrait;

  #[test]
  fn test_run() {
    let mut game_state = GameState::new();
    let mut event_system = EventSystem::new();
    game_state.set_quit_flag(false);
    game_state.enqueue_event(Event::new(EventType::QuitGame, DEFAULT_PRIORITY, vec![]));
    event_system.run(&mut game_state);
    assert_eq!(game_state.get_quit_flag(), true);
  }
}
