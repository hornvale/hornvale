//! Core world primitives.
//!
//! This module provides the fundamental building blocks for the world VM:
//! - Entities (opaque identifiers)
//! - Values (runtime data)
//! - Components (typed data attached to entities)
//! - Relations (connections between entities)
//! - World (top-level state container)
//! - Queries (graph traversal over relations)
//! - Layers (entity stratification for mutation rules)
//! - Phases (simulation lifecycle)
//! - Epochs (cache invalidation tracking)

mod component;
mod entity;
mod epoch;
mod layer;
mod phase;
pub mod query;
mod relation;
mod value;
mod world;

pub use component::{ComponentStorage, ComponentTypeId};
pub use entity::{EntityAllocator, EntityId};
pub use epoch::{EpochSnapshot, epoch_types};
pub use layer::{Layer, LayerError};
pub use phase::Phase;
pub use query::TraversalResult;
pub use relation::{Cardinality, RelationRegistry, RelationSchema, RelationTable, RelationTypeId};
pub use value::Value;
pub use world::{World, WorldSnapshot};
