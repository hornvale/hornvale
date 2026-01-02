//! Core world primitives.
//!
//! This module provides the fundamental building blocks for the world VM:
//! - Entities (opaque identifiers)
//! - Values (runtime data)
//! - Components (typed data attached to entities)
//! - World (top-level state container)

mod component;
mod entity;
mod value;
mod world;

pub use component::{ComponentStorage, ComponentTypeId};
pub use entity::{EntityAllocator, EntityId};
pub use value::Value;
pub use world::World;
