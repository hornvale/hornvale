use uuid::Uuid;

use crate::event::Event;
use crate::event::EventTag;
use crate::event::EventType;

/// The `EventBuilder` struct.
#[derive(Clone, Debug)]
pub struct EventBuilder {
  pub uuid: Option<Uuid>,
  pub r#type: Option<EventType>,
  pub priority: Option<i64>,
  pub backtrace: Option<Vec<String>>,
  pub tags: Option<Vec<EventTag>>,
}

impl EventBuilder {
  pub fn new() -> Self {
    EventBuilder {
      uuid: None,
      r#type: None,
      priority: None,
      backtrace: None,
      tags: None,
    }
  }

  pub fn uuid(mut self, uuid: Uuid) -> Self {
    self.uuid = Some(uuid);
    self
  }

  pub fn r#type(mut self, r#type: EventType) -> Self {
    self.r#type = Some(r#type);
    self
  }

  pub fn priority(mut self, priority: i64) -> Self {
    self.priority = Some(priority);
    self
  }

  pub fn backtrace(mut self, backtrace: Vec<String>) -> Self {
    self.backtrace = Some(backtrace);
    self
  }

  pub fn tags(mut self, tags: Vec<EventTag>) -> Self {
    self.tags = Some(tags);
    self
  }

  pub fn add_tag(mut self, tag: EventTag) -> Self {
    let mut tags = self.tags.unwrap_or_default();
    tags.push(tag);
    self.tags = Some(tags);
    self
  }

  pub fn build(self) -> Event {
    let uuid = self.uuid.unwrap_or_else(Uuid::new_v4);
    let r#type = self.r#type.unwrap_or_default();
    let priority = self.priority.unwrap_or_else(|| r#type.get_priority());
    let backtrace = self.backtrace.unwrap_or_default();
    let tags = self.tags.unwrap_or_default();
    Event {
      uuid,
      r#type,
      priority,
      backtrace,
      tags,
    }
  }
}

impl Default for EventBuilder {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_event_builder() {
    let event = EventBuilder::new().build();
    assert_eq!(event.uuid.get_version_num(), 4);
    assert_eq!(event.r#type, EventType::default());
    assert_eq!(event.priority, 0);
    assert_eq!(event.backtrace, Vec::<String>::new());
  }

  #[test]
  fn test_event_builder_uuid() {
    let uuid = Uuid::new_v4();
    let event = EventBuilder::new().uuid(uuid).build();
    assert_eq!(event.uuid, uuid);
    assert_eq!(event.r#type, EventType::default());
    assert_eq!(event.priority, 0);
    assert_eq!(event.backtrace, Vec::<String>::new());
  }

  #[test]
  fn test_event_builder_type() {
    let r#type = EventType::NoOp;
    let event = EventBuilder::new().r#type(r#type.clone()).build();
    assert_eq!(event.uuid.get_version_num(), 4);
    assert_eq!(event.r#type, r#type);
    assert_eq!(event.priority, 0);
    assert_eq!(event.backtrace, Vec::<String>::new());
  }

  #[test]
  fn test_event_builder_priority() {
    let priority = 1;
    let event = EventBuilder::new().priority(priority).build();
    assert_eq!(event.uuid.get_version_num(), 4);
    assert_eq!(event.r#type, EventType::default());
    assert_eq!(event.priority, priority);
    assert_eq!(event.backtrace, Vec::<String>::new());
  }
}
