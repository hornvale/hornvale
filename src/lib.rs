//! Hornvale: A world programming environment.
//!
//! Hornvale is a minimal system for defining entities, rules, and derivations
//! that can express anything from a goat in a room to a procedurally generated galaxy.
//!
//! ## Phase 1: "Goat in a Room"
//!
//! The simplest possible world that does something:
//! - Entities with components
//! - A tick-based simulation loop
//! - Hardcoded rules (goat says "Baa!" every 10 ticks)
//! - Interactive REPL for inspection

pub mod compiler;
pub mod core;
pub mod derive;
pub mod io;
pub mod lang;
pub mod repl;
pub mod rules;
pub mod symbol;
pub mod systems;
pub mod vm;

// Re-export commonly used types at the crate root
pub use core::{
    Cardinality, ComponentTypeId, EntityId, RelationSchema, RelationTypeId, Value, World,
};
pub use derive::{ComposeMode, DerivationEngine, DerivationError, DerivationRule, DerivedProperty};
pub use io::{StdIO, WorldIO};
pub use symbol::Symbol;
