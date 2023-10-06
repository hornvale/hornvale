use std::cell::RefCell;
use std::rc::Rc;

use crate::event::EventSubscriber;
use crate::event::EventSubscriberBuilder;

pub mod manager;
pub use manager::Manager as ChunkRuleManager;
pub mod r#type;
pub use r#type::Type as ChunkRuleType;

/// The `ChunkRule` struct.
#[derive(Clone, Debug, PartialEq)]
pub struct ChunkRule {
  pub r#type: ChunkRuleType,
  pub subscriber: Rc<RefCell<EventSubscriber>>,
}

impl ChunkRule {
  pub fn new(r#type: ChunkRuleType) -> Self {
    let mut r#type = r#type;
    let subscriber = EventSubscriberBuilder::new()
      .name(format!("ChunkManagerRule::{}", r#type))
      .priority(r#type.get_priority())
      .event_type(r#type.get_event_type())
      .filter_rule(r#type.get_filter_rule())
      .should_process(r#type.get_should_process())
      .will_process(r#type.get_will_process())
      .did_process(r#type.get_did_process())
      .is_enabled(r#type.is_enabled())
      .build();
    let subscriber = Rc::new(RefCell::new(subscriber));
    ChunkRule { r#type, subscriber }
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
