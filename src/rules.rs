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

mod effect;
mod engine;
mod pattern;
pub mod predicate;
mod rule;

pub use effect::{Effect, EffectError, EffectResult};
pub use engine::RuleSet;
pub use pattern::{Pattern, Var};
pub use predicate::{PatternFilter, PredicateError, PredicatePattern};
pub use rule::{Rule, Trigger};
