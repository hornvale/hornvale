//! Rule system for data-driven game logic.
//!
//! This module provides the infrastructure for defining and evaluating rules
//! as data rather than hardcoded Rust code.
//!
//! ## Components
//!
//! - **Pattern**: Rust-based enumeration and matching (MatchPlan)
//! - **PredicatePattern**: VM-compiled boolean filters
//! - **Effect**: VM-executed actions (mutations, output)
//! - **Rule**: Combines trigger, pattern, and effect
//! - **Derivation**: Computed properties with caching
//!
//! ## Pattern vs PredicatePattern
//!
//! Use `Pattern` (Rust) for enumeration - finding candidate entities:
//! - "Find all entities with HP component"
//! - "Find entities in relation to room"
//!
//! Use `PredicatePattern` (VM) for filtering - checking specific entities:
//! - "Is this entity portable?"
//! - "Does this creature have enough HP?"
//!
//! Combined: Rust enumeration + VM filter for best performance.
//!
//! ## Derivations
//!
//! Use `Trigger::derive()` for computed properties:
//! - Multiple rules can contribute to a property's final value
//! - Values are composed using modes: Add, Multiply, Max, Min, Replace
//! - Results are cached and invalidated based on epochs

pub mod derive;
mod effect;
mod engine;
mod index;
mod pattern;
mod precondition_rule;
pub mod predicate;
mod rule;

// Re-export derivation types from the derive module
pub use derive::{
    CacheEntry, ComposeMode, Contribution, Dependency, DerivationEngine, DerivationError,
    DerivationRule, DerivedCache, DerivedProperty, GenerationContext, GenerationStep, ValueFn,
    compose_values,
};
pub use effect::{Effect, EffectError, EffectResult};
pub use engine::RuleSet;
pub use index::RuleIndex;
pub use pattern::{Pattern, Var};
pub use precondition_rule::{PreconditionRule, PreconditionRuleResult};
pub use predicate::{PatternFilter, PredicateError, PredicatePattern};
pub use rule::{Rule, Trigger};
