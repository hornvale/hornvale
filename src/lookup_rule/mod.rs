use std::cell::RefCell;
use std::rc::Rc;

use crate::event::EventSubscriber;
use crate::event::EventSubscriberBuilder;

pub mod manager;
pub use manager::Manager as LookupRuleManager;
pub mod lookup_rule_type;
pub use lookup_rule_type::Type as LookupRuleType;

/// The `LookupRule` struct.
#[derive(Clone, Debug, PartialEq)]
pub struct LookupRule {
  pub lookup_rule_type: LookupRuleType,
  pub subscriber: Rc<RefCell<EventSubscriber>>,
}

impl LookupRule {
  pub fn new(lookup_rule_type: LookupRuleType) -> Self {
    let mut lookup_rule_type = lookup_rule_type;
    let subscriber = EventSubscriberBuilder::new()
      .name(format!("LookupRule::{}", lookup_rule_type))
      .priority(lookup_rule_type.get_priority())
      .event_type(lookup_rule_type.get_event_type())
      .filter_rule(lookup_rule_type.get_filter_rule())
      .should_process(lookup_rule_type.get_should_process())
      .will_process(lookup_rule_type.get_will_process())
      .did_process(lookup_rule_type.get_did_process())
      .is_enabled(lookup_rule_type.is_enabled())
      .build();
    let subscriber = Rc::new(RefCell::new(subscriber));
    LookupRule {
      lookup_rule_type,
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
