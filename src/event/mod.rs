use anyhow::Error as AnyError;
use std::cmp::Ordering;
use uuid::Uuid;

use crate::game_state::GameState;

pub mod _constant;
pub use _constant::*;
pub mod filter_rule;
pub use filter_rule::FilterRule as EventFilterRule;
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
