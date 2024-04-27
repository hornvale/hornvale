/// An allocator entry.
pub mod allocator_entry;
/// An entry in an array with a generation number.
pub mod array_entry;
use array_entry::ArrayEntry;
/// A component trait.
pub mod component;
use component::Component;
/// A generational index; a unique identifier for an entity.
pub mod generational_index;
use generational_index::GenerationalIndex;
/// An allocator for generational indices.
pub mod generational_index_allocator;
use generational_index_allocator::GenerationalIndexAllocator;
/// An array indexed by generational indices.
pub mod generational_index_array;
use generational_index_array::GenerationalIndexArray;
/// A type map.
pub mod type_map;
use type_map::TypeMap;

/// The ECS struct.
#[derive(Debug, Default)]
pub struct ECS {
  /// The allocator for generational indices.
  pub allocator: GenerationalIndexAllocator,
  /// The storage for the generational index arrays.
  pub arrays: TypeMap,
  /// The storage for resources.
  pub resources: TypeMap,
}

impl ECS {
  /// Creates a new ECS.
  pub fn new() -> Self {
    ECS::default()
  }

  /// Register a new component type.
  pub fn register_component<C: Component>(&mut self)
  where
    C: Default,
  {
    self
      .arrays
      .insert::<GenerationalIndexArray<C>>(GenerationalIndexArray::default());
  }

  /// Add a new entity with the given components.
  pub fn add_entity(&mut self, components: Option<TypeMap>) -> GenerationalIndex {
    let entity = self.allocator.allocate();
    if let Some(components) = components {
      self.insert_components(entity, components);
    }
    entity
  }

  /// Remove an entity and its associated components.
  pub fn remove_entity(&mut self, entity: GenerationalIndex) {
    if self.allocator.deallocate(entity) {
      for key in self.arrays.iter_mut() {
        if let Some(array) = key.1.downcast_mut::<Option<ArrayEntry<dyn Component>>>() {
          array[entity.index] = None;
        }
      }
    }
  }

  /// Get an array of components by type.
  pub fn get_array<C: 'static + Component>(&self) -> Option<&GenerationalIndexArray<C>> {
    self.arrays.get::<GenerationalIndexArray<C>>()
  }

  /// Get a mutable array of components by type.
  pub fn get_array_mut<C: 'static + Component>(&mut self) -> Option<&mut GenerationalIndexArray<C>> {
    self.arrays.get_mut::<GenerationalIndexArray<C>>()
  }
}

/// The prelude.
pub mod prelude {
  pub use super::allocator_entry::AllocatorEntry;
  pub use super::array_entry::ArrayEntry;
  pub use super::component::Component;
  pub use super::generational_index::GenerationalIndex;
  pub use super::generational_index_allocator::GenerationalIndexAllocator;
  pub use super::generational_index_array::GenerationalIndexArray;
  pub use super::type_map::TypeMap;
  pub use super::ECS;
}
