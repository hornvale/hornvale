use std::cell::RefCell;
use std::rc::Rc;

use crate::event::EventSubscriber;
use crate::event::EventSubscriberBuilder;

pub mod manager;
pub use manager::Manager as GameRuleManager;
pub mod r#type;
pub use r#type::Type as GameRuleType;

/// The `GameRule` struct.
#[derive(Clone, Debug, PartialEq)]
pub struct GameRule {
  pub r#type: GameRuleType,
  pub subscriber: Rc<RefCell<EventSubscriber>>,
}

impl GameRule {
  pub fn new(r#type: GameRuleType) -> Self {
    let subscriber = EventSubscriberBuilder::new()
      .name(format!("GameRule::{}", r#type))
      .priority(r#type.get_priority())
      .event_type(r#type.get_event_type())
      .filter_rule(r#type.get_filter_rule())
      .should_process(r#type.get_should_process())
      .will_process(r#type.get_will_process())
      .did_process(r#type.get_did_process())
      .is_enabled(r#type.is_enabled())
      .build();
    let subscriber = Rc::new(RefCell::new(subscriber));
    GameRule { r#type, subscriber }
  }

  pub fn enable(&mut self) {
    self.subscriber.borrow_mut().is_enabled = true;
  }

  pub fn disable(&mut self) {
    self.subscriber.borrow_mut().is_enabled = false;
  }

  pub fn is_enabled(&self) -> bool {
    self.subscriber.borrow().is_enabled
  }
}
