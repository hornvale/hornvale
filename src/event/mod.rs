use anyhow::Error as AnyError;
use std::cmp::Ordering;
use uuid::Uuid;

use crate::game_state::GameState;

pub mod _constant;
pub use _constant::*;
pub mod filter_rule;
pub use filter_rule::FilterRule as EventFilterRule;
pub mod logger;
pub use logger::*;
pub mod publisher;
pub use publisher::*;
pub mod subscriber;
pub use subscriber::*;
pub mod tag;
pub use tag::Tag as EventTag;
pub mod r#type;
pub use r#type::Type as EventType;

/// The `Event` struct.
#[derive(Debug, Default)]
pub struct Event {
  /// The `Event`'s type.
  pub r#type: EventType,
  /// The `Event`'s UUID.
  pub uuid: Uuid,
  /// A backtrace.
  pub backtrace: Vec<String>,
  /// Event priority.
  pub priority: i64,
}

impl Event {
  pub fn new(r#type: EventType, priority: i64, backtrace: Vec<String>) -> Self {
    let uuid = Uuid::new_v4();
    let mut backtrace = backtrace;
    backtrace.push(format!("Event {:?}:{}", r#type, uuid));
    Self {
      r#type,
      uuid,
      priority,
      backtrace,
    }
  }

  pub fn process(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Processing {:#?} event.", self.r#type);
    self.r#type.process(self, game_state)?;
    Ok(())
  }
}

impl PartialEq for Event {
  fn eq(&self, other: &Self) -> bool {
    self.r#type == other.r#type && self.uuid == other.uuid
  }
}

impl Eq for Event {}

impl PartialOrd for Event {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for Event {
  fn cmp(&self, other: &Self) -> Ordering {
    let priority_order = self.priority.cmp(&other.priority);
    if priority_order == Ordering::Equal {
      let type_order = self.r#type.cmp(&other.r#type);
      if type_order == Ordering::Equal {
        self.uuid.cmp(&other.uuid)
      } else {
        type_order
      }
    } else {
      priority_order
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::game_state::GameState;
  use std::collections::BinaryHeap;

  #[test]
  fn test_new() {
    let event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, vec![]);
    assert_eq!(event.r#type, EventType::NoOp);
    assert_eq!(event.backtrace.len(), 1);
  }

  #[test]
  fn test_process() {
    let mut game_state = GameState::new();
    let event = Event::new(EventType::NoOp, DEFAULT_PRIORITY, vec![]);
    event.process(&mut game_state).unwrap();
  }

  #[test]
  fn test_priority_queue() {
    let event_1 = Event::new(EventType::NoOp, 1, vec![]);
    let event_2 = Event::new(EventType::NoOp, 2, vec![]);
    let event_3 = Event::new(EventType::NoOp, 3, vec![]);
    let event_4 = Event::new(EventType::NoOp, 4, vec![]);
    let event_5 = Event::new(EventType::NoOp, 5, vec![]);
    let event_6 = Event::new(EventType::NoOp, 6, vec![]);
    let event_7 = Event::new(EventType::NoOp, 7, vec![]);
    let event_8 = Event::new(EventType::NoOp, 8, vec![]);
    let event_9 = Event::new(EventType::NoOp, 9, vec![]);
    let event_10 = Event::new(EventType::NoOp, 10, vec![]);
    let event_11 = Event::new(EventType::NoOp, 11, vec![]);
    let event_12 = Event::new(EventType::NoOp, 12, vec![]);
    let event_13 = Event::new(EventType::NoOp, 13, vec![]);
    let mut priority_queue = BinaryHeap::new();
    // Insert in semi-random order.
    priority_queue.push(event_4);
    priority_queue.push(event_11);
    priority_queue.push(event_7);
    priority_queue.push(event_12);
    priority_queue.push(event_3);
    priority_queue.push(event_10);
    priority_queue.push(event_2);
    priority_queue.push(event_6);
    priority_queue.push(event_1);
    priority_queue.push(event_5);
    priority_queue.push(event_9);
    priority_queue.push(event_13);
    priority_queue.push(event_8);
    // Pop in priority order.
    assert_eq!(priority_queue.pop().unwrap().priority, 13);
    assert_eq!(priority_queue.pop().unwrap().priority, 12);
    assert_eq!(priority_queue.pop().unwrap().priority, 11);
    assert_eq!(priority_queue.pop().unwrap().priority, 10);
    assert_eq!(priority_queue.pop().unwrap().priority, 9);
    assert_eq!(priority_queue.pop().unwrap().priority, 8);
    assert_eq!(priority_queue.pop().unwrap().priority, 7);
    assert_eq!(priority_queue.pop().unwrap().priority, 6);
    assert_eq!(priority_queue.pop().unwrap().priority, 5);
    assert_eq!(priority_queue.pop().unwrap().priority, 4);
    assert_eq!(priority_queue.pop().unwrap().priority, 3);
    assert_eq!(priority_queue.pop().unwrap().priority, 2);
    assert_eq!(priority_queue.pop().unwrap().priority, 1);
  }
}
