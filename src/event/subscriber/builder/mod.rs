use std::sync::Arc;
use uuid::Uuid;

use crate::event::DidProcessFn;
use crate::event::EventFilterRule;
use crate::event::EventSubscriber;
use crate::event::EventType;
use crate::event::ShouldProcessFn;
use crate::event::WillProcessFn;
use crate::event::DEFAULT_PRIORITY;

/// The `EventSubscriberBuilder` struct.
#[derive(Clone, Default, Derivative)]
#[derivative(Debug)]
pub struct Builder {
  /// A human-friendly name for the subscriber.
  name: Option<String>,
  /// The priority of the subscriber.
  priority: Option<i64>,
  /// The event type that this subscriber is interested in.
  event_type: Option<EventType>,
  /// The filter rule.
  filter_rule: Option<EventFilterRule>,
  /// The closure that determines if this subscriber should process an event.
  #[derivative(Debug = "ignore")]
  should_process: Option<ShouldProcessFn>,
  /// The closure that is called when an event is about to be processed.
  #[derivative(Debug = "ignore")]
  will_process: Option<WillProcessFn>,
  /// The closure that is called after an event has been processed.
  #[derivative(Debug = "ignore")]
  did_process: Option<DidProcessFn>,
  /// Whether this is enabled or not.
  is_enabled: Option<bool>,
}

impl Builder {
  pub fn new() -> Self {
    Builder {
      name: None,
      priority: None,
      event_type: None,
      filter_rule: None,
      should_process: None,
      will_process: None,
      did_process: None,
      is_enabled: None,
    }
  }

  pub fn name(mut self, name: String) -> Self {
    self.name = Some(name);
    self
  }

  pub fn priority(mut self, priority: i64) -> Self {
    self.priority = Some(priority);
    self
  }

  pub fn event_type(mut self, event_type: EventType) -> Self {
    self.event_type = Some(event_type);
    self
  }

  pub fn filter_rule(mut self, filter_rule: EventFilterRule) -> Self {
    self.filter_rule = Some(filter_rule);
    self
  }

  pub fn should_process(mut self, should_process: ShouldProcessFn) -> Self {
    self.should_process = Some(should_process);
    self
  }

  pub fn will_process(mut self, will_process: WillProcessFn) -> Self {
    self.will_process = Some(will_process);
    self
  }

  pub fn did_process(mut self, did_process: DidProcessFn) -> Self {
    self.did_process = Some(did_process);
    self
  }

  pub fn is_enabled(mut self, is_enabled: bool) -> Self {
    self.is_enabled = Some(is_enabled);
    self
  }

  pub fn build(self) -> EventSubscriber {
    EventSubscriber {
      uuid: Uuid::new_v4(),
      name: self.name.unwrap_or_else(|| String::from("Default EventSubscriber")),
      priority: self.priority.unwrap_or(DEFAULT_PRIORITY),
      event_type: self.event_type.unwrap_or(EventType::default()),
      filter_rule: self.filter_rule.unwrap_or(EventFilterRule::Always),
      should_process: self.should_process.unwrap_or_else(|| Arc::new(|_, _| Ok(None))),
      will_process: self.will_process.unwrap_or_else(|| Arc::new(|_, _| Ok(()))),
      did_process: self.did_process.unwrap_or_else(|| Arc::new(|_, _| Ok(()))),
      is_enabled: self.is_enabled.unwrap_or(true),
    }
  }
}
