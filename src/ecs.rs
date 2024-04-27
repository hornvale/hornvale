//! # Entity-Component-System (ECS)

use std::any::Any;

/// Singleton resources shared across the ECS.
pub mod resources;
use resources::Resources;

/// The Entity-Component-System (ECS) implementation.
#[derive(Debug, Default)]
pub struct ECS {
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
}
