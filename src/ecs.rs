//! # Entity-Component-System (ECS)
//!
//! This module contains the Entity-Component-System (ECS) implementation for
//! the _Hornvale_ project.
//!
//! This implementation owes a lot to Brooks Patton's series of videos [1] and
//! to Catherine West's delightful RustConf talk [2] and accompanying blog post
//! [3].
//!
//! [1]: https://www.youtube.com/playlist?list=PLrmY5pVcnuE_SQSzGPWUJrf9Yo-YNeBYs
//! [2]: https://www.youtube.com/watch?v=aKLntZcp27M
//! [3]: https://kyren.github.io/2018/09/14/rustconf-talk.html

use anyhow::Error as AnyError;
use std::any::Any;

/// Allocator entry.
pub mod allocator_entry;
/// Array entry.
pub mod array_entry;
/// Components.
pub mod component;
use component::AddComponentTuple;
/// A component map.
pub mod component_map;
/// Entities and components.
pub mod entities;
use entities::Entities;
/// Generational index.
pub mod generational_index;
/// Generational index allocator.
pub mod generational_index_allocator;
/// Generational index array.
pub mod generational_index_array;
/// Queries.
pub mod query;
pub use query::Query;
/// Query entities.
pub mod query_entity;
pub use query_entity::QueryEntity;
/// Singleton resources shared across the ECS.
pub mod resources;
use resources::Resources;

/// The Entity-Component-System (ECS) implementation.
#[derive(Debug, Default)]
pub struct ECS {
  /// Entities and components.
  pub entities: Entities,
  /// The shared resources.
  pub resources: Resources,
}

impl ECS {
  /// Create a new ECS.
  pub fn new() -> Self {
    ECS::default()
  }

  /// Add a resource.
  pub fn add_resource(&mut self, resource: impl Any) {
    self.resources.insert(resource);
  }

  /// Get a resource.
  pub fn get_resource<T: Any>(&self) -> Option<&T> {
    self.resources.get::<T>()
  }

  /// Get a mutable reference to a resource.
  pub fn get_resource_mut<T: Any>(&mut self) -> Option<&mut T> {
    self.resources.get_mut::<T>()
  }

  /// Delete a resource.
  pub fn delete_resource<T: Any>(&mut self) {
    self.resources.remove::<T>();
  }

  /// Register a component.
  pub fn register_component<T: Any + 'static>(&mut self) {
    self.entities.register::<T>();
  }

  /// Create an entity.
  pub fn create_entity(&mut self) -> &mut Entities {
    self.entities.create()
  }

  /// Create a batch of entities.
  pub fn create_entities<T: AddComponentTuple + Clone>(&mut self, count: usize, components: T) -> Vec<u32> {
    self.entities.create_batch(count, components)
  }

  /// Delete an entity.
  pub fn delete_entity(&mut self, index: usize) -> Result<(), AnyError> {
    self.entities.delete(index)
  }

  /// Add a component to an entity.
  pub fn add_component<T: Any>(&mut self, data: T, index: usize) -> Result<(), AnyError> {
    self.entities.add(data, index)
  }

  /// Remove a component from an entity.
  pub fn remove_component<T: Any>(&mut self, index: usize) -> Result<(), AnyError> {
    self.entities.remove::<T>(index)
  }

  /// Query for entities and components.
  pub fn query(&self) -> Query {
    self.entities.query()
  }
}

/// The prelude for the ECS.
pub mod prelude {
  pub use super::component::Component;
  pub use super::entities::Entities;
  pub use super::query::Query;
  pub use super::resources::Resources;
  pub use super::ECS;
}
