use uuid::Uuid;

use crate::event::Event;
use crate::event::EventFilterRule;
use crate::event::EventSubscriberTrait;
use crate::game_state::GameState;

/// The `DebugLogger` event subscriber.
#[derive(Clone, Debug, Default)]
pub struct DebugLogger {
  id: String,
}

impl DebugLogger {
  /// Creates a new `DebugLogger`.
  pub fn new() -> Self {
    let id = Uuid::new_v4().to_string();
    Self { id }
  }
}

impl EventSubscriberTrait<GameState> for DebugLogger {
  /// Get the subscriber's ID.
  fn get_id(&self) -> &str {
    &self.id
  }

  /// Get the filter rule.
  fn get_filter_rule(&self) -> &EventFilterRule {
    &EventFilterRule::Always
  }

  /// Ask if this subscriber prevents, ensures, or allows an event.
  ///
  /// Returns:
  /// - `Some(true)` if this subscriber ensures the event.
  /// - `Some(false)` if this subscriber prevents the event.
  /// - `None` if this subscriber allows the event.
  fn should_process(&self, _event: &Event, _game_state: &GameState) -> Option<bool> {
    None
  }

  /// Notify this subscriber that an event will occur.
  fn will_process(&mut self, event: &mut Event, _game_state: &GameState) {
    debug!("Event {:#?} will process.", event);
  }

  /// Notify this subscriber that an event has occurred.
  fn did_process(&mut self, event: &Event, _game_state: &mut GameState) {
    debug!("Event {:#?} did process.", event);
  }
}
