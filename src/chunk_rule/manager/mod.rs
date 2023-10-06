use std::collections::HashMap;

use crate::chunk_rule::ChunkRule;
use crate::chunk_rule::ChunkRuleType;
use crate::event::EventPublisher;

/// The `ChunkRuleManager` struct.
#[derive(Clone, Debug, Default)]
pub struct Manager {
  /// The `ChunkRule` rules.
  rules: HashMap<ChunkRuleType, ChunkRule>,
}

impl Manager {
  /// Creates a new `ChunkRuleManager`.
  pub fn new() -> Self {
    let rules = HashMap::new();
    Self { rules }
  }

  pub fn add_rule(&mut self, rule: ChunkRule) {
    self.rules.insert(rule.r#type, rule);
  }

  pub fn enable_rule(&mut self, rule_type: &ChunkRuleType) {
    if let Some(rule) = self.rules.get_mut(rule_type) {
      rule.enable();
    }
  }

  pub fn disable_rule(&mut self, rule_type: &ChunkRuleType) {
    if let Some(rule) = self.rules.get_mut(rule_type) {
      rule.disable();
    }
  }

  pub fn is_rule_enabled(&self, rule_type: &ChunkRuleType) -> bool {
    self.rules.get(rule_type).map_or(false, |rule| rule.is_enabled())
  }

  pub fn get_rules(&self) -> Vec<&ChunkRule> {
    self.rules.values().collect()
  }

  pub fn insert_stock_rules(&mut self) {
    ChunkRuleType::iterator().for_each(|rule_type| {
      self.add_rule(ChunkRule::new(rule_type));
    });
  }

  pub fn inject_rule_subscribers(&self, event_publisher: &mut EventPublisher) {
    self.rules.values().for_each(|rule| {
      event_publisher.add_subscriber(rule.subscriber.clone());
    });
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use anyhow::Error as AnyError;

  use crate::test::init;

  #[test]
  fn test_chunk_rule_manager() -> Result<(), AnyError> {
    init();
    let mut chunk_rule_manager = Manager::new();
    chunk_rule_manager.insert_stock_rules();
    let mut event_publisher = EventPublisher::new();
    chunk_rule_manager.inject_rule_subscribers(&mut event_publisher);
    Ok(())
  }
}
