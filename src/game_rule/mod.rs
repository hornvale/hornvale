use std::cell::RefCell;
use std::rc::Rc;

use crate::event::EventSubscriber;
use crate::event::EventSubscriberBuilder;

pub mod manager;
pub use manager::Manager as GameRuleManager;
pub mod game_rule_type;
pub use game_rule_type::Type as GameRuleType;

/// The `GameRule` struct.
#[derive(Clone, Debug, PartialEq)]
pub struct GameRule {
  pub game_rule_type: GameRuleType,
  pub subscriber: Rc<RefCell<EventSubscriber>>,
}

impl GameRule {
  pub fn new(game_rule_type: GameRuleType) -> Self {
    let mut game_rule_type = game_rule_type;
    let subscriber = EventSubscriberBuilder::new()
      .name(format!("GameRule::{}", game_rule_type))
      .priority(game_rule_type.get_priority())
      .event_type(game_rule_type.get_event_type())
      .filter_rule(game_rule_type.get_filter_rule())
      .should_process(game_rule_type.get_should_process())
      .will_process(game_rule_type.get_will_process())
      .did_process(game_rule_type.get_did_process())
      .is_enabled(game_rule_type.is_enabled())
      .build();
    let subscriber = Rc::new(RefCell::new(subscriber));
    GameRule {
      game_rule_type,
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
