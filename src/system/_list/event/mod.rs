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
