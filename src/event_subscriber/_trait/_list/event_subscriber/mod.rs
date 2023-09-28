use crate::event::EventTrait;
use crate::event_filter_rule::EventFilterRule;
use crate::game_state::GameStateTrait;

/// The `EventSubscriber` trait.
///
/// By implementing this, an object is assuring that it will not throw an
/// exception when it is notified of or consulted about an event.
pub trait EventSubscriber<T: GameStateTrait> {
  /// Get a unique ID for this subscriber.
  fn get_id(&self) -> &str;
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
  fn should_occur(&self, _event: &dyn EventTrait<T>, _game_state: &T) -> Option<bool> {
    None
  }
  /// Notify this subscriber that an event will occur.
  fn will_occur(&mut self, _event: &mut dyn EventTrait<T>, _game_state: &T) {}
  /// Notify this subscriber that an event has occurred.
  fn did_occur(&mut self, _event: &dyn EventTrait<T>, _game_state: &mut T) {}
}
