//! Rule definitions for data-driven game logic.
//!
//! A Rule combines a pattern (when to fire) with an effect (what to do).
//!
//! ## Hook Triggers
//!
//! Hooks are represented as rules with Before/On/After triggers:
//! - `Before(action)`: Runs before action, can veto
//! - `On(action)`: Runs during action, can handle (skip default)
//! - `After(action)`: Runs after action completes
//!
//! Hook rules typically have a pattern that matches a specific entity
//! (the entity the hook is "attached" to).

use crate::rules::pattern::Pattern;
use crate::symbol::Symbol;

use super::effect::Effect;

/// When a rule should be evaluated.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Trigger {
    /// Fire every N ticks.
    Periodic {
        /// Number of ticks between firings.
        interval: u64,
    },

    /// Fire before an action (can veto).
    Before(Symbol),

    /// Fire during an action (can handle/skip default).
    On(Symbol),

    /// Fire after an action completes.
    After(Symbol),

    /// Fire when deriving a property value.
    /// Multiple derivation rules can contribute to a property's final value.
    Derive(Symbol),

    /// A precondition check (e.g., "reachable?", "portable?").
    /// Precondition rules define validation logic for actions.
    Precondition(Symbol),
}

impl Trigger {
    /// Create a periodic trigger.
    pub fn every(interval: u64) -> Self {
        Trigger::Periodic { interval }
    }

    /// Create a Before hook trigger.
    pub fn before(action: impl Into<Symbol>) -> Self {
        Trigger::Before(action.into())
    }

    /// Create an On hook trigger.
    pub fn on(action: impl Into<Symbol>) -> Self {
        Trigger::On(action.into())
    }

    /// Create an After hook trigger.
    pub fn after(action: impl Into<Symbol>) -> Self {
        Trigger::After(action.into())
    }

    /// Create a Derive trigger for computing derived properties.
    pub fn derive(property: impl Into<Symbol>) -> Self {
        Trigger::Derive(property.into())
    }

    /// Create a Precondition trigger.
    pub fn precondition(name: impl Into<Symbol>) -> Self {
        Trigger::Precondition(name.into())
    }

    /// Get the interval for periodic triggers, if applicable.
    pub fn interval(&self) -> Option<u64> {
        match self {
            Trigger::Periodic { interval } => Some(*interval),
            _ => None,
        }
    }

    /// Get the action name for hook triggers, if applicable.
    pub fn action(&self) -> Option<Symbol> {
        match self {
            Trigger::Before(action) | Trigger::On(action) | Trigger::After(action) => Some(*action),
            _ => None,
        }
    }

    /// Get the property name for Derive triggers, if applicable.
    pub fn property(&self) -> Option<Symbol> {
        match self {
            Trigger::Derive(property) => Some(*property),
            _ => None,
        }
    }

    /// Get the precondition name for Precondition triggers, if applicable.
    pub fn precondition_name(&self) -> Option<Symbol> {
        match self {
            Trigger::Precondition(name) => Some(*name),
            _ => None,
        }
    }

    /// Check if this is a Precondition trigger.
    pub fn is_precondition(&self) -> bool {
        matches!(self, Trigger::Precondition(_))
    }

    /// Check if this is a Derive trigger.
    pub fn is_derive(&self) -> bool {
        matches!(self, Trigger::Derive(_))
    }

    /// Check if this is a Before trigger.
    pub fn is_before(&self) -> bool {
        matches!(self, Trigger::Before(_))
    }

    /// Check if this is an On trigger.
    pub fn is_on(&self) -> bool {
        matches!(self, Trigger::On(_))
    }

    /// Check if this is an After trigger.
    pub fn is_after(&self) -> bool {
        matches!(self, Trigger::After(_))
    }

    /// Check if this is a hook trigger (Before, On, or After).
    pub fn is_hook(&self) -> bool {
        matches!(
            self,
            Trigger::Before(_) | Trigger::On(_) | Trigger::After(_)
        )
    }

    /// Get the phase name for hook triggers.
    pub fn phase_name(&self) -> Option<&'static str> {
        match self {
            Trigger::Before(_) => Some("Before"),
            Trigger::On(_) => Some("On"),
            Trigger::After(_) => Some("After"),
            _ => None,
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
    /// Priority for ordering (higher = fires earlier within same trigger bucket).
    pub priority: i32,
}

impl Rule {
    /// Create a new rule with default priority (0).
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
            priority: 0,
        }
    }

    /// Create a new rule with explicit priority.
    pub fn with_priority(
        name: impl Into<Symbol>,
        pattern: Pattern,
        trigger: Trigger,
        effect: Effect,
        priority: i32,
    ) -> Self {
        Self {
            name: name.into(),
            pattern,
            trigger,
            effect,
            priority,
        }
    }

    /// Get a human-readable description of this rule.
    pub fn describe(&self) -> String {
        let trigger_desc = match &self.trigger {
            Trigger::Periodic { interval } => format!("every {interval} ticks"),
            Trigger::Before(action) => format!("before:{}", action.as_str()),
            Trigger::On(action) => format!("on:{}", action.as_str()),
            Trigger::After(action) => format!("after:{}", action.as_str()),
            Trigger::Derive(property) => format!("derive:{}", property.as_str()),
            Trigger::Precondition(name) => format!("precondition:{}", name.as_str()),
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

    #[test]
    fn test_hook_triggers() {
        let before = Trigger::before("take");
        assert!(before.is_before());
        assert!(before.is_hook());
        assert!(!before.is_on());
        assert!(!before.is_after());
        assert_eq!(before.action(), Some(Symbol::new("take")));
        assert_eq!(before.phase_name(), Some("Before"));
        assert_eq!(before.interval(), None);

        let on = Trigger::on("burn");
        assert!(on.is_on());
        assert!(on.is_hook());
        assert!(!on.is_before());
        assert_eq!(on.action(), Some(Symbol::new("burn")));
        assert_eq!(on.phase_name(), Some("On"));

        let after = Trigger::after("open");
        assert!(after.is_after());
        assert!(after.is_hook());
        assert_eq!(after.action(), Some(Symbol::new("open")));
        assert_eq!(after.phase_name(), Some("After"));
    }

    #[test]
    fn test_periodic_trigger_not_hook() {
        let periodic = Trigger::every(10);
        assert!(!periodic.is_hook());
        assert!(!periodic.is_before());
        assert!(!periodic.is_on());
        assert!(!periodic.is_after());
        assert_eq!(periodic.action(), None);
        assert_eq!(periodic.phase_name(), None);
        assert_eq!(periodic.interval(), Some(10));
    }

    #[test]
    fn test_hook_rule_describe() {
        let rule = Rule::new(
            "holy-book-burn",
            Pattern::entity("?e"),
            Trigger::on("burn"),
            Effect::emit_message("Lightning strikes!"),
        );

        let desc = rule.describe();
        assert!(desc.contains("holy-book-burn"));
        assert!(desc.contains("on:burn"));
        assert!(desc.contains("Lightning"));
    }

    #[test]
    fn test_derive_trigger() {
        let derive = Trigger::derive("FireResistance");
        assert!(derive.is_derive());
        assert!(!derive.is_hook());
        assert!(!derive.is_before());
        assert!(!derive.is_on());
        assert!(!derive.is_after());
        assert_eq!(derive.property(), Some(Symbol::new("FireResistance")));
        assert_eq!(derive.action(), None);
        assert_eq!(derive.interval(), None);
    }

    #[test]
    fn test_derive_rule_describe() {
        let rule = Rule::new(
            "fire-resist-base",
            Pattern::has_component("?e", "Ancestry"),
            Trigger::derive("FireResistance"),
            Effect::no_op(), // Derivation rules typically just provide values, not effects
        );

        let desc = rule.describe();
        assert!(desc.contains("fire-resist-base"));
        assert!(desc.contains("derive:FireResistance"));
    }
}
