use std::cmp::Ordering;
use std::sync::Arc;
use uuid::Uuid;

use crate::event::EventFilterRule;
use crate::event::EventType;
use crate::event::DEFAULT_PRIORITY;

pub mod _type;
pub use _type::*;
pub mod builder;
pub use builder::Builder as EventSubscriberBuilder;

/// The `EventSubscriber` struct.
///
/// This is a generic event subscriber built around closures.
#[derive(Clone, Derivative)]
#[derivative(Debug)]
pub struct EventSubscriber {
  /// The UUID of the subscriber.
  pub uuid: Uuid,
  /// A human-friendly name for the subscriber.
  pub name: String,
  /// The priority of the subscriber.
  pub priority: i64,
  /// The event type that this subscriber is interested in.
  pub event_type: EventType,
  /// The filter rule.
  pub filter_rule: EventFilterRule,
  /// The closure that determines if this subscriber should process an event.
  #[derivative(Debug = "ignore")]
  pub should_process: ShouldProcessFn,
  /// The closure that is called when an event is about to be processed.
  #[derivative(Debug = "ignore")]
  pub will_process: WillProcessFn,
  /// The closure that is called after an event has been processed.
  #[derivative(Debug = "ignore")]
  pub did_process: DidProcessFn,
  /// Whether this is enabled or not.
  pub is_enabled: bool,
}

impl EventSubscriber {
  /// Creates a new `EventSubscriber`.
  #[allow(clippy::too_many_arguments)]
  pub fn new(
    name: String,
    priority: i64,
    event_type: EventType,
    filter_rule: EventFilterRule,
    should_process: ShouldProcessFn,
    will_process: WillProcessFn,
    did_process: DidProcessFn,
    is_enabled: bool,
  ) -> Self {
    assert!(event_type != EventType::None, "Event type cannot be None.");
    Self {
      uuid: Uuid::new_v4(),
      name,
      priority,
      event_type,
      filter_rule,
      should_process,
      will_process,
      did_process,
      is_enabled,
    }
  }
}

impl Default for EventSubscriber {
  /// Creates a new `EventSubscriber`.
  fn default() -> Self {
    Self {
      uuid: Uuid::new_v4(),
      name: String::from("Default EventSubscriber"),
      priority: DEFAULT_PRIORITY,
      event_type: EventType::default(),
      filter_rule: EventFilterRule::Always,
      should_process: Arc::new(|_, _| Ok(None)),
      will_process: Arc::new(|_, _| Ok(())),
      did_process: Arc::new(|_, _| Ok(())),
      is_enabled: true,
    }
  }
}

impl PartialEq for EventSubscriber {
  fn eq(&self, other: &Self) -> bool {
    self.event_type == other.event_type && self.priority == other.priority && self.uuid == other.uuid
  }
}

impl Eq for EventSubscriber {}

impl PartialOrd for EventSubscriber {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    match other.priority.partial_cmp(&self.priority) {
      Some(Ordering::Equal) => self.uuid.partial_cmp(&other.uuid),
      other => other,
    }
  }
}

impl Ord for EventSubscriber {
  fn cmp(&self, other: &Self) -> Ordering {
    match other.priority.cmp(&self.priority) {
      Ordering::Equal => self.uuid.cmp(&other.uuid),
      other => other,
    }
  }
}
