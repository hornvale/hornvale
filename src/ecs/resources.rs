use crate::type_map::TypeMap;
use std::any::Any;

/// The resources shared across the ECS.
#[derive(Debug, Default)]
#[repr(transparent)]
pub struct Resources(pub TypeMap);

impl Resources {
  /// Insert a resource into the ECS.
  pub fn insert(&mut self, data: impl Any) {
    self.0.insert(data);
  }

  /// Get a reference to a resource.
  pub fn get<T: Any>(&self) -> Option<&T> {
    self.0.get::<T>()
  }

  /// Get a mutable reference to a resource.
  pub fn get_mut<T: Any>(&mut self) -> Option<&mut T> {
    self.0.get_mut::<T>()
  }

  /// Check if a resource is present.
  pub fn has<T: Any>(&self) -> bool {
    self.0.has::<T>()
  }

  /// Remove a resource from the ECS.
  pub fn remove<T: Any>(&mut self) {
    self.0.remove::<T>();
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::test_utilities::prelude::*;

  #[test]
  fn add_resource() {
    init();
    let resources = initialize_resource();
    let stored_resource = resources.get::<TestResource>().unwrap();
    assert_approx_eq!(stored_resource.0, 100.0);
  }

  #[test]
  fn get_resource() {
    let resources = initialize_resource();
    let test_resource = resources.get::<TestResource>().unwrap();
    assert_approx_eq!(test_resource.0, 100.0);
  }

  #[test]
  fn get_resource_mut() {
    let mut resources = initialize_resource();
    {
      let test_resource: &mut TestResource = resources.get_mut::<TestResource>().unwrap();
      test_resource.0 += 1.0;
    }
    let test_resource = resources.get::<TestResource>().unwrap();
    assert_approx_eq!(test_resource.0, 101.0);
  }

  #[test]
  fn remove_resource() {
    let mut resources = initialize_resource();
    resources.remove::<TestResource>();
    assert!(!resources.has::<TestResource>());
  }

  fn initialize_resource() -> Resources {
    let mut resources = Resources::default();
    let world_width = TestResource(100.0);

    resources.insert(world_width);
    resources
  }

  struct TestResource(f32);
}
