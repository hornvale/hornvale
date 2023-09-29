use anyhow::Error as AnyError;
use uuid::Uuid;

use crate::game_state::GameState;

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
}

impl Event {
  pub fn new(r#type: EventType, backtrace: Vec<String>) -> Self {
    let uuid = Uuid::new_v4();
    let mut backtrace = backtrace;
    backtrace.push(format!("Event {:?}:{}", r#type, uuid));
    Self {
      r#type,
      uuid,
      backtrace,
    }
  }

  pub fn process(&self, game_state: &mut GameState) -> Result<(), AnyError> {
    debug!("Processing {:#?} event.", self.r#type);
    self.r#type.process(self, game_state)?;
    Ok(())
  }
}
