use std::collections::HashMap;

use crate::event::EventPublisher;
use crate::lookup_rule::LookupRule;
use crate::lookup_rule::LookupRuleType;

/// The `LookupRuleManager` struct.
#[derive(Clone, Debug, Default)]
pub struct Manager {
  /// The `LookupRule` rules.
  rules: HashMap<LookupRuleType, LookupRule>,
}

impl Manager {
  /// Creates a new `LookupRuleManager`.
  pub fn new() -> Self {
    let rules = HashMap::new();
    Self { rules }
  }

  pub fn add_rule(&mut self, rule: LookupRule) {
    self.rules.insert(rule.lookup_rule_type, rule);
  }

  pub fn enable_rule(&mut self, rule_type: &LookupRuleType) {
    if let Some(rule) = self.rules.get_mut(rule_type) {
      rule.enable();
    }
  }

  pub fn disable_rule(&mut self, rule_type: &LookupRuleType) {
    if let Some(rule) = self.rules.get_mut(rule_type) {
      rule.disable();
    }
  }

  pub fn is_rule_enabled(&self, rule_type: &LookupRuleType) -> bool {
    self.rules.get(rule_type).map_or(false, |rule| rule.is_enabled())
  }

  pub fn get_rules(&self) -> Vec<&LookupRule> {
    self.rules.values().collect()
  }

  pub fn insert_stock_rules(&mut self) {
    LookupRuleType::iterator().for_each(|rule_type| {
      self.add_rule(LookupRule::new(rule_type));
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
  fn test_lookup_rule_manager() -> Result<(), AnyError> {
    init();
    let mut lookup_rule_manager = Manager::new();
    lookup_rule_manager.insert_stock_rules();
    let mut event_publisher = EventPublisher::new();
    lookup_rule_manager.inject_rule_subscribers(&mut event_publisher);
    Ok(())
  }
}
