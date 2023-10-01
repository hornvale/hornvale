use crate::event::EventSubscriber;

pub mod manager;
pub use manager::Manager as GameRuleManager;
pub mod r#type;
pub use r#type::Type as GameRuleType;

/// The `GameRule` struct.
#[derive(Clone, Debug, PartialEq)]
pub struct GameRule {
  pub r#type: GameRuleType,
  pub subscriber: EventSubscriber,
}

impl GameRule {
  pub fn new(r#type: GameRuleType, subscriber: EventSubscriber) -> Self {
    GameRule { r#type, subscriber }
  }

  pub fn enable(&mut self) {
    self.subscriber.is_enabled = true;
  }

  pub fn disable(&mut self) {
    self.subscriber.is_enabled = false;
  }

  pub fn is_enabled(&self) -> bool {
    self.subscriber.is_enabled
  }
}
