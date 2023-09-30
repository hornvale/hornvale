use std::cmp::Ordering;
use std::sync::Arc;
use uuid::Uuid;

use crate::event::EventFilterRule;
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
}

impl EventSubscriber {
  /// Creates a new `EventSubscriber`.
  pub fn new(
    name: String,
    priority: i64,
    filter_rule: EventFilterRule,
    should_process: ShouldProcessFn,
    will_process: WillProcessFn,
    did_process: DidProcessFn,
  ) -> Self {
    Self {
      uuid: Uuid::new_v4(),
      name,
      priority,
      filter_rule,
      should_process,
      will_process,
      did_process,
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
      filter_rule: EventFilterRule::Always,
      should_process: Arc::new(|_, _| None),
      will_process: Arc::new(|_, _| {}),
      did_process: Arc::new(|_, _| {}),
    }
  }
}

impl PartialEq for EventSubscriber {
  fn eq(&self, other: &Self) -> bool {
    self.priority == other.priority && self.uuid == other.uuid
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
