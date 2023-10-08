use std::cell::RefCell;
use std::rc::Rc;

use crate::event::EventSubscriber;
use crate::event::EventSubscriberBuilder;

pub mod manager;
pub use manager::Manager as ChunkRuleManager;
pub mod chunk_rule_type;
pub use chunk_rule_type::Type as ChunkRuleType;

/// The `ChunkRule` struct.
#[derive(Clone, Debug, PartialEq)]
pub struct ChunkRule {
  pub chunk_rule_type: ChunkRuleType,
  pub subscriber: Rc<RefCell<EventSubscriber>>,
}

impl ChunkRule {
  pub fn new(chunk_rule_type: ChunkRuleType) -> Self {
    let mut chunk_rule_type = chunk_rule_type;
    let subscriber = EventSubscriberBuilder::new()
      .name(format!("ChunkManagerRule::{}", chunk_rule_type))
      .priority(chunk_rule_type.get_priority())
      .event_type(chunk_rule_type.get_event_type())
      .filter_rule(chunk_rule_type.get_filter_rule())
      .should_process(chunk_rule_type.get_should_process())
      .will_process(chunk_rule_type.get_will_process())
      .did_process(chunk_rule_type.get_did_process())
      .is_enabled(chunk_rule_type.is_enabled())
      .build();
    let subscriber = Rc::new(RefCell::new(subscriber));
    ChunkRule {
      chunk_rule_type,
      subscriber,
    }
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
