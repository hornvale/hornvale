use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};

/// A map from types to values.
///
/// This is a simple wrapper around `HashMap<TypeId, Box<dyn Any>>` that allows
/// for type-safe access to the stored values.
///
/// This enables type-safe registries, event handlers, dependency injection,
/// and other use cases where you want to store and retrieve values by type.
///
/// @see <https://willcrichton.net/rust-api-type-patterns/registries.html>
#[derive(Default)]
#[repr(transparent)]
pub struct TypeMap(HashMap<TypeId, Box<dyn Any>>);

impl TypeMap {
  /// Create a new, empty `TypeMap`.
  pub fn new() -> Self {
    Self(HashMap::new())
  }

  /// Set a value of type `T`.
  pub fn insert<T: Any + 'static>(&mut self, t: T) {
    self.0.insert(TypeId::of::<T>(), Box::new(t));
  }

  /// Check if a value of type `T` is present.
  pub fn has<T: Any + 'static>(&self) -> bool {
    self.0.contains_key(&TypeId::of::<T>())
  }

  /// Get a reference to a value of type `T`.
  pub fn get<T: Any + 'static>(&self) -> Option<&T> {
    self.0.get(&TypeId::of::<T>()).map(|t| t.downcast_ref::<T>().unwrap())
  }

  /// Get a mutable reference to a value of type `T`.
  pub fn get_mut<T: Any + 'static>(&mut self) -> Option<&mut T> {
    self
      .0
      .get_mut(&TypeId::of::<T>())
      .map(|t| t.downcast_mut::<T>().unwrap())
  }

  /// Delete a value of type `T`.
  pub fn remove<T: Any + 'static>(&mut self) {
    self.0.remove(&TypeId::of::<T>());
  }
}

impl Debug for TypeMap {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "TypeMap {{ ... }}")
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_type_map() {
    let mut map = TypeMap::new();
    assert_eq!(map.has::<i32>(), false);
    map.insert(42);
    assert_eq!(map.has::<i32>(), true);
    assert_eq!(map.get::<i32>(), Some(&42));
    assert_eq!(map.get_mut::<i32>(), Some(&mut 42));
  }
}
