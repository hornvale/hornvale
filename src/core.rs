//! Core world primitives.
//!
//! This module provides the fundamental building blocks for the world VM:
//! - Entities (opaque identifiers)
//! - Values (runtime data)
//! - Components (typed data attached to entities)
//! - Relations (connections between entities)
//! - World (top-level state container)

mod component;
mod entity;
mod relation;
mod value;
mod world;

pub use component::{ComponentStorage, ComponentTypeId};
pub use entity::{EntityAllocator, EntityId};
pub use relation::{Cardinality, RelationRegistry, RelationSchema, RelationTable, RelationTypeId};
pub use value::Value;
pub use world::World;
