//! Precondition rules for action validation.
//!
//! Precondition rules define validation checks that must pass before an action
//! can be executed. They combine the rule system with precondition semantics.
//!
//! ## Structure
//!
//! A precondition rule has:
//! - A name (e.g., "reachable?", "portable?")
//! - Parameters (e.g., [actor, target])
//! - A check pattern (the condition that must match)
//! - A failure template (interpolated message on failure)
//!
//! ## Example
//!
//! ```ignore
//! (precondition reachable?
//!   :params (actor target)
//!   :check (in-scope? target actor)
//!   :failure "You can't reach ~(the target).")
//! ```

use crate::core::{EntityId, World};
use crate::lang::SExpr;
use crate::rules::{Effect, Pattern, Rule, Trigger};
use crate::symbol::Symbol;
use std::sync::Arc;

/// A precondition rule definition.
///
/// Precondition rules are a specialized form of rules that:
/// - Are triggered by precondition checks (not actions or ticks)
/// - Have named parameters that are bound at evaluation time
/// - Return Pass/Fail with an optional failure message
#[derive(Debug, Clone)]
pub struct PreconditionRule {
    /// The underlying rule (stores name, pattern, trigger).
    rule: Rule,
    /// Parameter names for the precondition.
    params: Vec<Symbol>,
    /// The failure message template.
    failure_template: Arc<str>,
}

impl PreconditionRule {
    /// Create a new precondition rule.
    ///
    /// # Arguments
    /// * `name` - The precondition name (e.g., "reachable?")
    /// * `params` - Parameter names (e.g., ["actor", "target"])
    /// * `check` - The pattern that must match for the precondition to pass
    /// * `failure_template` - Message template for failures
    pub fn new(
        name: impl Into<Symbol>,
        params: Vec<Symbol>,
        check: Pattern,
        failure_template: impl Into<Arc<str>>,
    ) -> Self {
        let name_sym = name.into();
        let rule = Rule::new(
            name_sym,
            check,
            Trigger::precondition(name_sym),
            Effect::no_op(), // Preconditions don't have effects
        );

        Self {
            rule,
            params,
            failure_template: failure_template.into(),
        }
    }

    /// Create a precondition rule from an S-expression check.
    ///
    /// The check is converted to a predicate pattern.
    pub fn from_sexpr(
        name: impl Into<Symbol>,
        params: Vec<Symbol>,
        _check: SExpr, // TODO: Compile to PredicatePattern for VM-based evaluation
        failure_template: impl Into<Arc<str>>,
    ) -> Self {
        // For now, use a pass-through pattern - the check will be compiled separately
        // In a full implementation, we'd compile the SExpr to a predicate pattern
        let name_sym = name.into();
        let rule = Rule::new(
            name_sym,
            Pattern::entity("?e"), // Placeholder pattern
            Trigger::precondition(name_sym),
            Effect::no_op(),
        );

        // Store the check SExpr as part of the pattern
        // For now, we use the existing pattern system
        // TODO: Integrate with PredicatePattern for VM-based evaluation

        Self {
            rule,
            params,
            failure_template: failure_template.into(),
        }
    }

    /// Get the precondition name.
    pub fn name(&self) -> Symbol {
        self.rule.name
    }

    /// Get the parameter names.
    pub fn params(&self) -> &[Symbol] {
        &self.params
    }

    /// Get the check pattern.
    pub fn pattern(&self) -> &Pattern {
        &self.rule.pattern
    }

    /// Get the failure message template.
    pub fn failure_template(&self) -> &str {
        &self.failure_template
    }

    /// Get the underlying rule.
    pub fn rule(&self) -> &Rule {
        &self.rule
    }

    /// Convert to a Rule for storage in RuleSet.
    pub fn into_rule(self) -> Rule {
        self.rule
    }

    /// Check if the given number of arguments matches the expected parameter count.
    pub fn check_arity(&self, arg_count: usize) -> bool {
        self.params.len() == arg_count
    }

    /// Format a failure message with entity name interpolation.
    ///
    /// Replaces placeholders like `~(the target)` or `~(name actor)` with
    /// actual entity names from the world.
    pub fn format_failure_message(&self, world: &World, bindings: &[(Symbol, EntityId)]) -> String {
        let mut message = self.failure_template.to_string();

        for (param, entity) in bindings {
            // Replace ~(the param) patterns
            let the_placeholder = format!("~(the {})", param.as_str());
            if message.contains(&the_placeholder) {
                let name = get_entity_article_name(world, *entity);
                message = message.replace(&the_placeholder, &name);
            }

            // Replace ~(name param) patterns
            let name_placeholder = format!("~(name {})", param.as_str());
            if message.contains(&name_placeholder) {
                let name = get_entity_name(world, *entity);
                message = message.replace(&name_placeholder, &name);
            }

            // Replace ~(Name param) patterns (capitalized)
            let cap_name_placeholder = format!("~(Name {})", param.as_str());
            if message.contains(&cap_name_placeholder) {
                let name = get_entity_name(world, *entity);
                let capitalized = capitalize_first(&name);
                message = message.replace(&cap_name_placeholder, &capitalized);
            }
        }

        message
    }
}

/// Result of evaluating a precondition rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PreconditionRuleResult {
    /// The precondition passed.
    Passed,
    /// The precondition failed with a message.
    Failed(String),
}

impl PreconditionRuleResult {
    /// Check if the precondition passed.
    pub fn passed(&self) -> bool {
        matches!(self, PreconditionRuleResult::Passed)
    }

    /// Check if the precondition failed.
    pub fn failed(&self) -> bool {
        matches!(self, PreconditionRuleResult::Failed(_))
    }

    /// Get the failure message, if any.
    pub fn message(&self) -> Option<&str> {
        match self {
            PreconditionRuleResult::Passed => None,
            PreconditionRuleResult::Failed(msg) => Some(msg),
        }
    }
}

// ============================================================================
// Helper functions
// ============================================================================

/// Get the name of an entity with article ("the lamp").
fn get_entity_article_name(world: &World, entity: EntityId) -> String {
    let name = get_entity_name(world, entity);
    format!("the {name}")
}

/// Get the name of an entity.
fn get_entity_name(world: &World, entity: EntityId) -> String {
    world
        .get_component(entity, "Name")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .unwrap_or_else(|| "something".to_string())
}

/// Capitalize the first character of a string.
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().chain(chars).collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Value;

    #[test]
    fn test_precondition_rule_creation() {
        let rule = PreconditionRule::new(
            "reachable?",
            vec![Symbol::new("actor"), Symbol::new("target")],
            Pattern::entity("?e"),
            "You can't reach ~(the target).",
        );

        assert_eq!(rule.name(), Symbol::new("reachable?"));
        assert_eq!(rule.params().len(), 2);
        assert_eq!(rule.params()[0], Symbol::new("actor"));
        assert_eq!(rule.params()[1], Symbol::new("target"));
        assert!(rule.failure_template().contains("can't reach"));
    }

    #[test]
    fn test_precondition_rule_arity() {
        let rule = PreconditionRule::new(
            "held?",
            vec![Symbol::new("obj")],
            Pattern::entity("?e"),
            "You're not holding ~(the obj).",
        );

        assert!(rule.check_arity(1));
        assert!(!rule.check_arity(0));
        assert!(!rule.check_arity(2));
    }

    #[test]
    fn test_format_failure_message() {
        use crate::core::World;

        let mut world = World::new();
        let lamp = world.create_entity();
        world.set_component(lamp, "Name", Value::string("brass lamp"));

        let rule = PreconditionRule::new(
            "reachable?",
            vec![Symbol::new("actor"), Symbol::new("target")],
            Pattern::entity("?e"),
            "You can't reach ~(the target).",
        );

        let bindings = vec![(Symbol::new("target"), lamp)];
        let message = rule.format_failure_message(&world, &bindings);
        assert_eq!(message, "You can't reach the brass lamp.");
    }

    #[test]
    fn test_format_failure_message_name_only() {
        use crate::core::World;

        let mut world = World::new();
        let sword = world.create_entity();
        world.set_component(sword, "Name", Value::string("sword"));

        let rule = PreconditionRule::new(
            "held?",
            vec![Symbol::new("obj")],
            Pattern::entity("?e"),
            "The ~(name obj) is not in your inventory.",
        );

        let bindings = vec![(Symbol::new("obj"), sword)];
        let message = rule.format_failure_message(&world, &bindings);
        assert_eq!(message, "The sword is not in your inventory.");
    }

    #[test]
    fn test_into_rule() {
        let precond_rule = PreconditionRule::new(
            "portable?",
            vec![Symbol::new("obj")],
            Pattern::entity("?e"),
            "That's fixed in place.",
        );

        let rule = precond_rule.into_rule();
        assert_eq!(rule.name, Symbol::new("portable?"));
        assert!(rule.trigger.is_precondition());
    }

    #[test]
    fn test_precondition_rule_result() {
        let passed = PreconditionRuleResult::Passed;
        assert!(passed.passed());
        assert!(!passed.failed());
        assert!(passed.message().is_none());

        let failed = PreconditionRuleResult::Failed("Error message".to_string());
        assert!(!failed.passed());
        assert!(failed.failed());
        assert_eq!(failed.message(), Some("Error message"));
    }
}
