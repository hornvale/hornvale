//! Derivation engine for computed properties.
//!
//! This module provides the ability to define derived components that are
//! computed on access rather than stored. Multiple rules can contribute to
//! the same derived property, with their values composed according to
//! composition modes (add, multiply, max, min, override).

pub mod cache;
pub mod compose;
pub mod context;
pub mod engine;
pub mod rule;

pub use cache::{CacheEntry, Dependency, DerivedCache};
pub use compose::{ComposeMode, Contribution, compose_values};
pub use context::{GenerationContext, GenerationStep};
pub use engine::{DerivationEngine, DerivationError};
pub use rule::{DerivationRule, DerivedProperty, ValueFn};
