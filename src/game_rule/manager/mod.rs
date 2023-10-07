use std::collections::HashMap;

use crate::event::EventPublisher;
use crate::game_rule::GameRule;
use crate::game_rule::GameRuleType;

/// The `GameRuleManager` struct.
#[derive(Clone, Debug, PartialEq)]
pub struct Manager {
  rules: HashMap<GameRuleType, GameRule>,
}

impl Manager {
  pub fn new() -> Self {
    Manager { rules: HashMap::new() }
  }

  pub fn add_rule(&mut self, rule: GameRule) {
    self.rules.insert(rule.r#type, rule);
  }

  pub fn enable_rule(&mut self, rule_type: &GameRuleType) {
    if let Some(rule) = self.rules.get_mut(rule_type) {
      rule.enable();
    }
  }

  pub fn disable_rule(&mut self, rule_type: &GameRuleType) {
    if let Some(rule) = self.rules.get_mut(rule_type) {
      rule.disable();
    }
  }

  pub fn is_rule_enabled(&self, rule_type: &GameRuleType) -> bool {
    self.rules.get(rule_type).map_or(false, |rule| rule.is_enabled())
  }

  pub fn get_rules(&self) -> Vec<&GameRule> {
    self.rules.values().collect()
  }

  pub fn insert_stock_rules(&mut self) {
    GameRuleType::iterator().for_each(|rule_type| {
      self.add_rule(GameRule::new(rule_type));
    });
  }

  pub fn inject_rule_subscribers(&self, event_publisher: &mut EventPublisher) {
    self.rules.values().for_each(|rule| {
      event_publisher.add_subscriber(rule.subscriber.clone());
    });
  }
}

impl Default for Manager {
  fn default() -> Self {
    Self::new()
  }
}
