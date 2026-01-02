//! Rule definitions for data-driven game logic.
//!
//! A Rule combines a pattern (when to fire) with an effect (what to do).

use crate::rules::pattern::Pattern;
use crate::symbol::Symbol;

use super::effect::Effect;

/// When a rule should be evaluated.
#[derive(Debug, Clone)]
pub enum Trigger {
    /// Fire every N ticks.
    Periodic {
        /// Number of ticks between firings.
        interval: u64,
    },
    // Future: OnChange, OnEvent, etc.
}

impl Trigger {
    /// Create a periodic trigger.
    pub fn every(interval: u64) -> Self {
        Trigger::Periodic { interval }
    }

    /// Get the interval for periodic triggers, if applicable.
    pub fn interval(&self) -> Option<u64> {
        match self {
            Trigger::Periodic { interval } => Some(*interval),
        }
    }
}

/// A rule that fires when a pattern matches and a trigger condition is met.
#[derive(Debug, Clone)]
pub struct Rule {
    /// Unique name for this rule.
    pub name: Symbol,
    /// Pattern that must match for the rule to fire.
    pub pattern: Pattern,
    /// When to evaluate this rule.
    pub trigger: Trigger,
    /// What to do when the rule fires.
    pub effect: Effect,
}

impl Rule {
    /// Create a new rule.
    pub fn new(
        name: impl Into<Symbol>,
        pattern: Pattern,
        trigger: Trigger,
        effect: Effect,
    ) -> Self {
        Self {
            name: name.into(),
            pattern,
            trigger,
            effect,
        }
    }

    /// Get a human-readable description of this rule.
    pub fn describe(&self) -> String {
        let trigger_desc = match &self.trigger {
            Trigger::Periodic { interval } => format!("every {interval} ticks"),
        };
        format!(
            "Rule: {}\n  Pattern: {}\n  Trigger: {}\n  Effect: {}",
            self.name.as_str(),
            self.pattern.describe(),
            trigger_desc,
            self.effect.describe()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rule_creation() {
        let rule = Rule::new(
            "test-rule",
            Pattern::entity("?e"),
            Trigger::every(10),
            Effect::emit_message("Hello!"),
        );

        assert_eq!(rule.name.as_str(), "test-rule");
        assert_eq!(rule.trigger.interval(), Some(10));
    }

    #[test]
    fn test_rule_describe() {
        let rule = Rule::new(
            "goat-baas",
            Pattern::and(vec![
                Pattern::entity("?e"),
                Pattern::component_value("?e", "Name", "goat"),
            ]),
            Trigger::every(10),
            Effect::emit_message("The goat says: Baa!"),
        );

        let desc = rule.describe();
        assert!(desc.contains("goat-baas"));
        assert!(desc.contains("every 10 ticks"));
        assert!(desc.contains("goat"));
        assert!(desc.contains("Baa!"));
    }
}
