//! Rule system for data-driven game logic.
//!
//! This module provides the infrastructure for defining and evaluating rules
//! as data rather than hardcoded Rust code.
//!
//! ## Phase 2 Components
//! - Pattern matching against world state
//! - Rule definitions with triggers and effects
//! - Rule evaluation engine

mod effect;
mod engine;
mod pattern;
mod rule;

pub use effect::Effect;
pub use engine::RuleSet;
pub use pattern::{Pattern, Var};
pub use rule::{Rule, Trigger};
